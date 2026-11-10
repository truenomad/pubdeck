# paragraph correction ---------------------------------------------------------

# the end mark of a paragraph with content after it, which is how flextable
# writes a cell with no text
.misplaced_end_marks <- ".//a:p/a:endParaRPr[following-sibling::*]"

#' Correct the Paragraphs PowerPoint Would Repair
#'
#' Moves the end mark of a paragraph back to the end of the paragraph,
#' wherever text or other content follows it, on every slide of the deck.
#' 'flextable' writes a table cell with no text, such as an empty string
#' or a missing value, as a paragraph whose run comes after its end mark.
#' The file is then invalid, and PowerPoint offers to repair the deck when
#' it is opened.
#'
#' The page builders of this package write a space into such cells, so a
#' deck built with them alone needs no correction. Call this function when
#' the deck also holds tables built elsewhere, such as a flextable placed
#' with [officer::ph_with()]. Call it once, after the last page is added
#' and before the deck is written for the last time. It changes nothing
#' else, and a deck with nothing to correct is returned unchanged.
#'
#' @param deck An `rpptx` object.
#'
#' @return The deck, with each such paragraph corrected.
#'
#' @examples
#' cell <- flextable::flextable(data.frame(note = ""))
#' deck <- officer::read_pptx() |>
#'   officer::add_slide(layout = "Title and Content") |>
#'   officer::ph_with(value = cell, location = officer::ph_location_type())
#' deck <- deck_fix_paragraphs(deck)
#' @export
deck_fix_paragraphs <- function(deck) {
  if (!inherits(deck, "rpptx")) {
    cli::cli_abort("{.arg deck} must be an {.cls rpptx} object.")
  }

  purrr::walk(seq_len(length(deck)), \(page) {
    slide <- deck$slide$get_slide(page)$get()
    marks <- xml2::xml_find_all(slide, .misplaced_end_marks, .slide_ns)

    purrr::walk(marks, \(mark) {
      xml2::xml_add_child(xml2::xml_parent(mark), mark)
      xml2::xml_remove(mark)
    })
  })

  deck
}
