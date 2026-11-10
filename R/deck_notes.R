# building ---------------------------------------------------------------------

# one cell of text; a note built from several pieces arrives as a list
#' @noRd
.flatten_text <- function(x) {
  out <- .squish_ws(paste(as.character(unlist(x)), collapse = " "))

  if (!length(out) || is.na(out)) "" else out
}

# an optional column of the notes table, or a default for every row
#' @noRd
.note_column <- function(notes, name, default, n) {
  if (name %in% names(notes)) unlist(notes[[name]]) else rep(default, n)
}

# the paragraph for one note: a bold lead phrase, then the body or a link
#' @noRd
.note_paragraph <- function(note, pt, bullet, config) {
  markers <- c(config$note_bullet, config$note_subbullet)
  marker <- if (bullet) paste0(markers[note$level], "  ") else ""

  lead <- flextable::as_chunk(
    paste0(marker, note$lead, if (nzchar(note$lead)) " " else ""),
    props = officer::fp_text(
      font.size = pt, bold = TRUE, color = config$note_accent,
      font.family = config$font
    )
  )

  body <- if (is.na(note$url)) {
    flextable::as_chunk(
      note$body,
      props = officer::fp_text(
        font.size = pt, color = config$note_body,
        font.family = config$font, italic = note$italic
      )
    )
  } else {
    flextable::hyperlink_text(
      note$body, url = note$url,
      props = officer::fp_text(
        font.size = pt, color = config$note_accent,
        font.family = config$font, underlined = TRUE
      )
    )
  }

  flextable::as_paragraph(lead, body)
}

#' Build an Explainer Box
#'
#' Builds a short reading of a page as a filled box, with an optional
#' heading and one entry per note. The box is a one-column table, because
#' paragraph shading does not render in a slide. Place it with
#' [deck_place_note()].
#'
#' @param heading Heading of the box. Default `NULL`, none.
#' @param notes A data frame or list with one element per note in `lead`,
#'   the bold opening phrase, and `body`, the text that follows. Optional
#'   columns: `italic` sets a body in italics, `url` turns a body into a
#'   link, and `level`, 1 or 2, indents an entry under the one above it.
#' @param pt Font size in points. Default `NULL`, the `note_pt` setting of
#'   `config`.
#' @param bullet Logical. Mark each entry with a bullet and space the
#'   entries as a list. Default `FALSE`.
#' @param fill Fill colour of the box. Default `NULL`, the `note_fill`
#'   setting of `config`.
#' @param heading_pt Font size of the heading. Default is two points
#'   above `pt`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return A flextable. It carries the text and size of each row in the
#'   attribute `deck_note_rows`, which [deck_note_height()] reads.
#'
#' @examples
#' notes <- data.frame(
#'   lead = c("Coverage", "Sources"),
#'   body = c("is the share of children reached.", "are listed below.")
#' )
#' deck_note_block("How to read this page", notes, bullet = TRUE)
#' @export
deck_note_block <- function(heading = NULL, notes, pt = NULL, bullet = FALSE,
                            fill = NULL, heading_pt = pt + 2,
                            config = deck_config_get()) {
  pt <- pt %||% config$note_pt
  fill <- fill %||% config$note_fill

  lead <- purrr::map_chr(notes$lead, .flatten_text)
  n <- length(lead)

  entries <- tibble::tibble(
    lead = unname(lead),
    body = unname(purrr::map_chr(notes$body, .flatten_text)),
    italic = as.logical(.note_column(notes, "italic", FALSE, n)),
    url = as.character(.note_column(notes, "url", NA_character_, n)),
    level = as.integer(.note_column(notes, "level", 1L, n))
  )

  # the rows the notes occupy, which start at 2 only when a heading took
  # row 1
  has_heading <- !is.null(heading)
  offset <- as.integer(has_heading)
  note_rows <- seq_len(n) + offset
  pad_x <- config$note_pad_x_pt
  pad_y <- config$note_pad_y_pt

  block <- tibble::tibble(
    text = c(heading, paste(entries$lead, entries$body))
  ) |>
    flextable::flextable() |>
    flextable::delete_part(part = "header") |>
    flextable::border_remove() |>
    flextable::bg(bg = fill, part = "body") |>
    flextable::valign(valign = "top", part = "body") |>
    flextable::padding(
      padding.left = pad_x, padding.right = pad_x,
      padding.top = pad_y, padding.bottom = pad_y,
      part = "body"
    ) |>
    flextable::font(fontname = config$font, part = "body") |>
    flextable::fontsize(size = pt, part = "body") |>
    flextable::color(color = config$note_body, part = "body") |>
    flextable::line_spacing(space = config$note_line_spacing, part = "body")

  # the heading and each lead phrase carry the accent colour, so the box
  # reads as a short list and not as a paragraph
  if (has_heading) {
    block <- block |>
      flextable::bold(i = 1, bold = TRUE) |>
      flextable::color(i = 1, color = config$note_accent) |>
      flextable::fontsize(i = 1, size = heading_pt)
  }

  block <- purrr::reduce(
    seq_len(n),
    \(block, i) {
      flextable::compose(
        block, i = i + offset, j = "text",
        value = .note_paragraph(dplyr::slice(entries, i), pt, bullet, config)
      )
    },
    .init = block
  )

  # padding between the entries, so a note written as a list reads as one,
  # and a sub-point sits in from the line it belongs to
  entry_pad <- round(pt * 0.4)

  if (bullet) {
    block <- block |>
      flextable::padding(
        i = note_rows, padding.top = entry_pad, padding.bottom = entry_pad,
        part = "body"
      ) |>
      flextable::padding(
        i = note_rows[entries$level == 2L],
        padding.left = pad_x + config$note_indent, part = "body"
      )
  }

  row_pad <- if (bullet) 2 * entry_pad else 2 * pad_y
  indent <- dplyr::if_else(
    bullet & entries$level == 2L, config$note_indent, 0
  )

  attr(block, "deck_note_rows") <- tibble::tibble(
    lead = c(if (has_heading) heading, entries$lead),
    body = c(if (has_heading) "", entries$body),
    pt = c(if (has_heading) heading_pt, rep(pt, n)),
    pad_pt = c(if (has_heading) 2 * pad_y, rep(row_pad, n)),
    indent_pt = c(if (has_heading) 0, indent)
  )

  block
}

# measuring --------------------------------------------------------------------

# words of a string, or none for an empty one
#' @noRd
.split_words <- function(x) {
  if (!nzchar(x)) character(0) else strsplit(.squish_ws(x), " ")[[1]]
}

# lines a run of word widths takes when it wraps at a given width
#' @noRd
.count_lines <- function(widths, available) {
  wrapped <- purrr::reduce(
    widths,
    \(state, width) {
      if (state$used + width > available) {
        list(used = width, lines = state$lines + 1)
      } else {
        list(used = state$used + width, lines = state$lines)
      }
    },
    .init = list(used = 0, lines = 1)
  )

  wrapped$lines
}

# height in inches of one row once its text wraps; the lead phrase is bold
# and so runs wider than the body at the same size
#' @noRd
.note_row_height <- function(lead, body, pt, indent_pt, width, config) {
  available <- width - (2 * config$note_pad_x_pt + indent_pt) / 72

  bold <- .split_words(lead)
  plain <- .split_words(body)
  words <- c(bold, plain)
  face <- c(rep(2L, length(bold)), rep(1L, length(plain)))

  graphics::par(ps = pt, family = "sans")

  widths <- purrr::map2_dbl(
    words, face,
    \(word, font) {
      graphics::strwidth(paste0(word, " "), units = "in", font = font)
    }
  )

  .count_lines(widths, available) * pt * config$note_line_spacing / 72
}

#' Height of an Explainer Box at a Given Width
#'
#' Estimates how tall a box from [deck_note_block()] draws once its text
#' wraps inside a box of a given width. The deck typeface is measured as
#' the graphics device's sans face.
#'
#' @param block A flextable from [deck_note_block()].
#' @param width Width of the box, in inches.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return Height in inches.
#'
#' @examples
#' notes <- data.frame(lead = "Coverage", body = "is the share reached.")
#' block <- deck_note_block(notes = notes)
#' deck_note_height(block, width = 3)
#' @export
deck_note_height <- function(block, width, config = deck_config_get()) {
  rows <- attr(block, "deck_note_rows")

  if (is.null(rows)) {
    return(flextable::flextable_dim(block)$heights)
  }

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  drawn <- purrr::pmap_dbl(
    list(rows$lead, rows$body, rows$pt, rows$indent_pt),
    \(lead, body, pt, indent_pt) {
      .note_row_height(lead, body, pt, indent_pt, width, config)
    }
  )

  sum(drawn) + sum(rows$pad_pt) / 72
}

# placing ----------------------------------------------------------------------

#' Place an Explainer Box on a Page
#'
#' Sets an explainer box to the width of its box on the page and places
#' it. A note that outgrows its box would run under the footer without
#' failing the build, so the overflow is reported as a warning.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param block A flextable from [deck_note_block()].
#' @param geom The box to fill, a list of `left`, `top`, `width` and
#'   `height` in inches.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the box placed.
#'
#' @examples
#' \dontrun{
#' deck <- deck_place_note(
#'   deck, block, list(left = 9, top = 1.5, width = 3.5, height = 4)
#' )
#' }
#' @export
deck_place_note <- function(deck, block, geom, config = deck_config_get()) {
  fitted <- block |>
    flextable::width(width = geom$width) |>
    flextable::set_table_properties(layout = "fixed")

  drawn <- deck_note_height(block, geom$width, config)

  if (drawn > geom$height) {
    cli::cli_alert_warning(
      "Explainer box needs {round(drawn, 2)}in but its box is
       {round(geom$height, 2)}in; shorten the note"
    )
  }

  officer::ph_with(deck, value = fitted, location = .box_location(geom))
}
