# cells ------------------------------------------------------------------------

#' Draw a Proportional Bar Behind a Value
#'
#' Builds an HTML table cell that shows a value with a bar proportional to
#' a share, so a column of proportions reads at a glance without a
#' separate chart. Use the result in a gt column formatted with
#' `gt::fmt_markdown()`.
#'
#' @param value Vector of values to print.
#' @param share Numeric vector from 0 to 1 giving the length of each bar.
#'   Missing shares draw no bar, and values outside the range are clamped.
#' @param label Text to print in place of `value`. Default `NULL`.
#' @param fill,track Colours of the bar and of the track behind it.
#'   Default `NULL`, the `table_bar` and `table_bar_track` settings of the
#'   configuration in force.
#'
#' @return Character vector of HTML, one element per value.
#'
#' @examples
#' bar_cell(c("12%", "80%"), share = c(0.12, 0.8))
#' @export
bar_cell <- function(value, share, label = NULL, fill = NULL, track = NULL) {
  fill <- fill %||% .cfg("table_bar")
  track <- track %||% .cfg("table_bar_track")
  share <- pmin(pmax(dplyr::coalesce(share, 0), 0), 1)
  label <- label %||% value

  paste0(
    "<div style=\"display:flex;align-items:center;gap:6px;\">",
    "<div style=\"flex:0 0 46px;text-align:right;",
    "font-variant-numeric:tabular-nums;\">", label, "</div>",
    "<div style=\"flex:1 1 auto;height:9px;background:", track,
    ";border-radius:2px;overflow:hidden;\">",
    "<div style=\"width:", round(100 * share, 1), "%;height:100%;background:",
    fill, ";\"></div></div></div>"
  )
}

#' Draw a Coloured Dot for a Category
#'
#' Builds an HTML table cell with a coloured dot, for a categorical
#' reading that would otherwise cost a column.
#'
#' @param label Vector of category labels.
#' @param colours Named character vector giving a colour for each label. A
#'   label with no colour takes a neutral grey.
#' @param show_label Logical. Print the label beside the dot. Default
#'   `TRUE`.
#' @param missing_colour Colour for a label that `colours` does not name.
#'
#' @return Character vector of HTML, one element per label.
#'
#' @examples
#' dot_cell(c("AFP", "Env"), pub_palette(named = c("Env", "AFP")))
#' @export
dot_cell <- function(label, colours, show_label = TRUE,
                     missing_colour = "grey72") {
  colour <- dplyr::coalesce(
    unname(colours[as.character(label)]), missing_colour
  )
  align <- if (show_label) "flex-start" else "center"
  text <- if (show_label) paste0("<span>", label, "</span>") else ""

  paste0(
    "<div style=\"display:flex;align-items:center;gap:6px;",
    "justify-content:", align, ";\">",
    "<span style=\"flex:0 0 9px;height:9px;border-radius:50%;background:",
    colour, ";display:inline-block;\"></span>", text, "</div>"
  )
}

#' Draw a Filled Pill for a Banded Reading
#'
#' Builds an HTML table cell with a filled pill. The fill carries the
#' order of the bands and the ink is chosen with [contrast_ink()].
#'
#' @param label Vector of band labels.
#' @param colours Named character vector giving a fill for each label. A
#'   label with no fill takes a neutral tone.
#' @param width_px Minimum width of the pill in pixels. Default `74`.
#' @param font_pct Font size as a percentage of the cell's. Default `100`.
#' @param missing_fill Fill for a label that `colours` does not name.
#'
#' @return Character vector of HTML, one element per label.
#'
#' @examples
#' chip_cell(c("Low", "High"), c(Low = "#FFF5F0", High = "#67000D"))
#' @export
chip_cell <- function(label, colours, width_px = 74, font_pct = 100,
                      missing_fill = "#EFEDE8") {
  fill <- dplyr::coalesce(unname(colours[as.character(label)]), missing_fill)
  ink <- contrast_ink(fill)

  paste0(
    "<div style=\"display:flex;justify-content:center;\">",
    "<span style=\"display:inline-block;box-sizing:border-box;",
    "font-size:", font_pct, "%;",
    "min-width:", width_px, "px;padding:2px 8px;text-align:center;",
    "border-radius:9px;background:", fill, ";color:", ink,
    ";border:1.5px solid #FFFFFF;",
    "box-shadow:0 0 0 1px rgba(26,26,26,0.35);",
    "font-weight:600;letter-spacing:0.01em;white-space:nowrap;\">",
    label, "</span></div>"
  )
}

# one series as an inline SVG line with a dot on its last point
#' @noRd
.spark_svg <- function(values, floor, width, height, colour, min_span) {
  if (!length(values) || all(is.na(values))) {
    return("")
  }

  logged <- log10(pmin(pmax(values, floor), 1))
  span <- range(logged)

  if (diff(span) < min_span) {
    span <- mean(span) + c(-1, 1) * min_span / 2
  }

  scaled <- (logged - span[1]) / diff(span)
  x <- seq(1, width - 1, length.out = length(values))
  y <- (height - 2) - scaled * (height - 4)

  paste0(
    "<svg width=\"", width, "\" height=\"", height,
    "\" viewBox=\"0 0 ", width, " ", height, "\">",
    "<polyline points=\"",
    paste0(round(x, 1), ",", round(y, 1), collapse = " "),
    "\" fill=\"none\" stroke=\"", colour,
    "\" stroke-width=\"1.3\" stroke-linejoin=\"round\"/>",
    "<circle cx=\"", round(utils::tail(x, 1), 1), "\" cy=\"",
    round(utils::tail(y, 1), 1), "\" r=\"1.9\" fill=\"", colour,
    "\"/></svg>"
  )
}

#' Draw a Sparkline in a Cell
#'
#' Builds an inline SVG line for each series of proportions, so a table
#' row carries its own direction. Values are drawn on a log scale between
#' `floor` and 1. Each line is scaled to its own range, with a minimum
#' span that keeps a series that barely moves looking flat.
#'
#' @param series List of numeric vectors, one per row.
#' @param floor Smallest value drawn. Values below it are raised to it.
#' @param width,height Size of the drawing in pixels.
#' @param colour Line colour. Default `NULL`, the `table_bar` setting of
#'   the configuration in force.
#' @param min_span Smallest range, in log10 units, a line is scaled to.
#'   Default `NULL`, the `spark_min_span` setting.
#'
#' @return Character vector of SVG, one element per series. An empty or
#'   all-missing series gives an empty string.
#'
#' @examples
#' spark_cell(list(c(0.01, 0.02, 0.08), c(0.2, 0.2, 0.2)), floor = 0.001)
#' @export
spark_cell <- function(series, floor, width = 62, height = 16,
                       colour = NULL, min_span = NULL) {
  colour <- colour %||% .cfg("table_bar")
  min_span <- min_span %||% .cfg("spark_min_span")

  purrr::map_chr(
    series,
    \(values) .spark_svg(values, floor, width, height, colour, min_span)
  )
}

#' Show a Change in Rank
#'
#' Builds an HTML table cell that shows how far a row has moved since the
#' previous period. A move up the list takes the alarm colour, because up
#' is worse on a list ranked by risk. A row that was not on the previous
#' list reads "new".
#'
#' @param change Numeric vector of places moved. Positive is up the list.
#' @param is_new Logical vector. The row was not on the previous list.
#' @param alarm Colour for a move up and for a new row. Default
#'   `pub_alarm(config)`, the `alarm` setting of `config` or the built-in
#'   alarm colour where it is not set.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return Character vector of HTML, one element per row.
#'
#' @examples
#' move_cell(change = c(2, -1, 0, NA), is_new = c(FALSE, FALSE, FALSE, TRUE))
#' @export
move_cell <- function(change, is_new, alarm = pub_alarm(config),
                      config = deck_config_get()) {
  arrow <- dplyr::case_when(
    is_new ~ "new",
    change > 0 ~ paste0("&#9650; ", change),
    change < 0 ~ paste0("&#9660; ", abs(change)),
    TRUE ~ "&ndash;"
  )

  ink <- dplyr::case_when(
    is_new | change > 0 ~ alarm,
    change < 0 ~ "#7A7A78",
    TRUE ~ "#9A9A98"
  )

  paste0(
    "<span style=\"color:", ink, ";white-space:nowrap;\">", arrow, "</span>"
  )
}

# whole tables -----------------------------------------------------------------

#' Apply the House Look to a gt Table
#'
#' Styles the column labels, spanners and row groups of a gt table and
#' removes the outer borders, so the tables in a report read as one set.
#'
#' @param tbl A gt table.
#' @param mono_columns Columns to set in a monospaced face, as a character
#'   vector of names. Default `NULL`, none.
#' @param header Colour of the header fill and of the row group text.
#'   Default `NULL`, the `table_header` setting of the configuration in
#'   force.
#' @param font,mono_font Names of the Google fonts for the table and for
#'   `mono_columns`.
#' @param group_fill Fill of the row group rows.
#' @param rule_colour Colour of the lines between body rows.
#' @param row_padding Vertical padding of body rows, in pixels.
#'
#' @return The gt table, styled.
#'
#' @examples
#' gt::gt(head(mtcars[, 1:4])) |>
#'   style_deck_table(mono_columns = "mpg")
#' @export
style_deck_table <- function(tbl, mono_columns = NULL, header = NULL,
                             font = "Source Sans 3",
                             mono_font = "Roboto Mono",
                             group_fill = "#EDEDEA",
                             rule_colour = "#E6E6E3",
                             row_padding = 4) {
  header <- header %||% .cfg("table_header")

  header_style <- list(
    gt::cell_fill(color = header),
    gt::cell_text(color = "white", weight = "bold")
  )

  out <- tbl |>
    gt::tab_style(
      style = header_style,
      locations = gt::cells_column_labels(columns = gt::everything())
    ) |>
    gt::tab_style(
      style = header_style,
      locations = gt::cells_column_spanners(spanners = gt::everything())
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_fill(color = group_fill),
        gt::cell_text(weight = "bold", color = header)
      ),
      locations = gt::cells_row_groups()
    ) |>
    gt::opt_table_font(font = gt::google_font(font)) |>
    gt::tab_options(
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      table_body.hlines.color = rule_colour,
      column_labels.border.bottom.style = "none",
      data_row.padding = gt::px(row_padding)
    )

  if (is.null(mono_columns)) {
    return(out)
  }

  gt::tab_style(
    out,
    style = gt::cell_text(
      font = gt::google_font(mono_font), size = gt::pct(90)
    ),
    locations = gt::cells_body(columns = dplyr::all_of(mono_columns))
  )
}
