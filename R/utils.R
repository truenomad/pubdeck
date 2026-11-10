# text -------------------------------------------------------------------------

# trimmed, with every run of whitespace reduced to one space
#' @noRd
.squish_ws <- function(x) {
  gsub("[[:space:]]+", " ", trimws(x))
}

# a date written out in full, with no leading zero on the day
#' @noRd
.format_long_date <- function(date) {
  date <- as.Date(date)

  paste0(as.integer(format(date, "%d")), " ", format(date, "%B %Y"))
}

# one formatted paragraph
#' @noRd
.text_run <- function(text, size, colour, font, bold = FALSE,
                      align = "left") {
  officer::fpar(
    officer::ftext(
      as.character(text),
      officer::fp_text(
        font.size = size, color = colour, bold = bold, font.family = font
      )
    ),
    fp_p = officer::fp_par(text.align = align, padding = 0)
  )
}

# geometry ---------------------------------------------------------------------

# a box of left, top, width and height as an officer location
#' @noRd
.box_location <- function(box, label = NULL) {
  if (is.null(label)) {
    return(officer::ph_location(
      left = box$left, top = box$top, width = box$width, height = box$height
    ))
  }

  officer::ph_location(
    left = box$left, top = box$top, width = box$width, height = box$height,
    newlabel = label
  )
}

# a placeholder addressed by one of the labels the configuration names
#' @noRd
.label_location <- function(name, config) {
  officer::ph_location_label(ph_label = config$labels[[name]])
}

# bands ------------------------------------------------------------------------

# a filled one-cell table, because a slide will not render a paragraph
# border and officer draws no rectangle of its own
#' @noRd
.filled_band <- function(fill) {
  tibble::tibble(band = "") |>
    flextable::flextable() |>
    flextable::delete_part(part = "header") |>
    flextable::border_remove() |>
    flextable::bg(bg = fill, part = "body")
}

# the fixed size a band finishes with
#' @noRd
.size_band <- function(band, width, height) {
  band |>
    flextable::padding(padding = 0, part = "body") |>
    flextable::height(height = height, part = "body") |>
    flextable::hrule(rule = "exact", part = "body") |>
    flextable::width(width = width) |>
    flextable::set_table_properties(layout = "fixed")
}
