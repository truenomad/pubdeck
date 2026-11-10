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

# empty cells ------------------------------------------------------------------

# a space in each cell with no text, because flextable writes an empty cell
# as a paragraph that PowerPoint repairs on opening
#' @noRd
.space_empty_cells <- function(ft, part = "body") {
  empty <- flextable::information_data_chunk(ft) |>
    dplyr::mutate(
      .part = as.character(.data$.part), .col_id = as.character(.data$.col_id)
    ) |>
    dplyr::filter(.data$.part %in% part) |>
    dplyr::summarise(
      empty = all(is.na(.data$txt) | .data$txt == ""),
      .by = c(".part", ".row_id", ".col_id")
    ) |>
    dplyr::filter(.data$empty)

  purrr::reduce(
    seq_len(nrow(empty)),
    \(ft, k) {
      cell <- dplyr::slice(empty, k)

      flextable::compose(
        ft, i = cell$.row_id, j = cell$.col_id, part = cell$.part,
        value = flextable::as_paragraph(" ")
      )
    },
    .init = ft
  )
}

# bands ------------------------------------------------------------------------

# a filled one-cell table, because a slide will not render a paragraph
# border and officer draws no rectangle of its own; the space keeps the
# cell's paragraph valid
#' @noRd
.filled_band <- function(fill) {
  tibble::tibble(band = " ") |>
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
