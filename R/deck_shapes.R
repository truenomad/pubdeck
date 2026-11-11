# shape repair -----------------------------------------------------------------

# a shape placed or sized at the limit of a 32-bit whole number, which is
# how 'rvg' writes a chart mark it has no outline for
.unplaced_shapes <- paste0(
  ".//*[self::p:sp or self::p:pic or self::p:cxnSp][./p:spPr/a:xfrm/*[",
  "@x='2147483647' or @x='-2147483648' or ",
  "@y='2147483647' or @y='-2147483648' or ",
  "@cx='2147483647' or @cx='-2147483648' or ",
  "@cy='2147483647' or @cy='-2147483648']]"
)

#' Drop the Shapes PowerPoint Would Repair
#'
#' Drops every shape, picture or connector whose position or size is
#' written at the limit of a 32-bit whole number, on every slide of the
#' deck. A chart placed as native shapes with [deck_figure()],
#' [deck_place_vector()] or [rvg::dml()] writes such a shape for a
#' rectangle whose sides are both under 0.01 point at the size it is
#' drawn, such as a tile or a rectangle of zero width and height. The
#' shape has no outline, so it shows nothing, but the file is then
#' invalid and PowerPoint offers to repair the deck when it is opened.
#'
#' Call this function once, after the last page is added and before the
#' deck is written for the last time, or call [deck_repair()], which also
#' corrects the paragraphs. It changes nothing else, and a deck with
#' nothing to drop is returned unchanged.
#'
#' @param deck An `rpptx` object.
#' @param quiet Whether to stay silent. Default `FALSE`, which reports how
#'   many shapes were dropped, and on how many slides, when there are any.
#'   No message is shown when none are dropped.
#'
#' @return The deck, without the shapes.
#'
#' @examples
#' chart <- rvg::dml(code = grid::grid.rect(width = 0, height = 0))
#' deck <- officer::read_pptx() |>
#'   officer::add_slide(layout = "Title and Content") |>
#'   officer::ph_with(value = chart, location = officer::ph_location_type())
#' deck <- deck_fix_shapes(deck)
#' @export
deck_fix_shapes <- function(deck, quiet = FALSE) {
  if (!inherits(deck, "rpptx")) {
    cli::cli_abort("{.arg deck} must be an {.cls rpptx} object.")
  }

  dropped <- purrr::map_int(seq_len(length(deck)), \(page) {
    slide <- deck$slide$get_slide(page)$get()
    unplaced <- xml2::xml_find_all(slide, .unplaced_shapes, .slide_ns)
    xml2::xml_remove(unplaced)

    length(unplaced)
  })

  if (!quiet && sum(dropped) > 0) {
    cli::cli_alert_info(
      "Dropped {sum(dropped)} shape{?s} with no position, on
       {sum(dropped > 0)} slide{?s}."
    )
  }

  deck
}

#' Repair a Deck Before It Is Written
#'
#' Runs both repairs of this package on every slide: [deck_fix_shapes()]
#' drops the shapes written with no position, and [deck_fix_paragraphs()]
#' corrects the paragraphs written with content after their end mark.
#' PowerPoint offers to repair a deck that holds either.
#'
#' This is the one call to make before the last write of a deck. Call it
#' after the last page is added, and write the deck it returns. A deck
#' with nothing to repair is returned unchanged.
#'
#' @param deck An `rpptx` object.
#' @param quiet Whether [deck_fix_shapes()] stays silent. Default `FALSE`,
#'   which reports how many shapes were dropped when there are any.
#'
#' @return The deck, repaired.
#'
#' @examples
#' \dontrun{
#' deck <- deck_repair(deck)
#' print(deck, target = "outputs/weekly_review.pptx")
#' }
#' @export
deck_repair <- function(deck, quiet = FALSE) {
  deck |>
    deck_fix_paragraphs() |>
    deck_fix_shapes(quiet = quiet)
}
