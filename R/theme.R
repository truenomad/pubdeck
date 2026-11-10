# type scale -------------------------------------------------------------------

# three sizes mapped to role, not to available space
.pub_base_size <- 10
.pub_title_size <- .pub_base_size * 1.2
.pub_small_size <- .pub_base_size * 0.8

# themes -----------------------------------------------------------------------

#' House Theme for Charts
#'
#' A minimal theme with a left-aligned, regular-weight title, a legend at
#' the bottom with its title above the keys, and light major gridlines.
#' Write the title in sentence case and put the metric name on the axis.
#'
#' @param base_size Base font size in points. Default `10`.
#' @param grid Which major gridlines to draw: `"both"`, `"x"`, `"y"` or
#'   `"none"`.
#'
#' @return A ggplot2 theme.
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point(colour = pub_palette(1)) +
#'   ggplot2::labs(title = "Fuel economy by weight") +
#'   theme_pub(grid = "y")
#' @export
theme_pub <- function(base_size = 10, grid = c("both", "x", "y", "none")) {
  grid <- rlang::arg_match(grid)

  base <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = .pub_title_size,
        face = "plain",
        hjust = 0,
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size * 0.95,
        colour = "grey30",
        hjust = 0,
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        size = .pub_small_size,
        colour = "grey40",
        hjust = 0,
        margin = ggplot2::margin(t = 8)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.title.x = ggplot2::element_text(
        size = base_size, margin = ggplot2::margin(t = 13)
      ),
      axis.title.y = ggplot2::element_text(
        size = base_size, margin = ggplot2::margin(r = 13)
      ),
      axis.text.x = ggplot2::element_text(
        size = base_size * 0.9, colour = "grey20",
        margin = ggplot2::margin(t = 4)
      ),
      axis.text.y = ggplot2::element_text(
        size = base_size * 0.9, colour = "grey20",
        margin = ggplot2::margin(r = 4)
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        colour = "grey92", linewidth = 0.3
      ),
      strip.text = ggplot2::element_text(
        size = base_size, face = "bold", hjust = 0,
        margin = ggplot2::margin(b = 4)
      ),
      legend.position = "bottom",
      legend.title.position = "top",
      legend.text.position = "bottom",
      legend.title = ggplot2::element_text(
        size = base_size * 0.9, hjust = 0
      ),
      legend.text = ggplot2::element_text(size = base_size * 0.85),
      legend.key.height = ggplot2::unit(0.32, "cm"),
      legend.margin = ggplot2::margin(t = 2),
      plot.margin = ggplot2::margin(t = 8, r = 10, b = 6, l = 8)
    )

  drop <- switch(
    grid,
    both = NULL,
    x = ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()),
    y = ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()),
    none = ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  )

  if (is.null(drop)) base else base + drop
}

#' Clear the Painted Backgrounds of a Plot
#'
#' Returns theme settings that blank the plot, legend, legend key and strip
#' backgrounds, for a figure placed over a coloured slide. With
#' `transparent = FALSE` it returns an empty theme, so it can be added
#' unconditionally.
#'
#' @param transparent Logical. Blank the backgrounds.
#'
#' @return A ggplot2 theme.
#'
#' @examples
#' theme_pub() + theme_pub_clear(transparent = TRUE)
#' @export
theme_pub_clear <- function(transparent) {
  if (!transparent) {
    return(ggplot2::theme())
  }

  clear <- ggplot2::element_rect(fill = NA, colour = NA)

  ggplot2::theme(
    plot.background = clear,
    legend.background = clear,
    legend.key = clear,
    strip.background = clear
  )
}

#' House Theme for Maps
#'
#' [theme_pub()] without gridlines, axis text, axis titles or ticks, for a
#' map drawn from simple features, where those carry nothing.
#'
#' @param base_size Base font size in points. Default `10`.
#' @param water Fill for the panel behind the land. Use `NA` for no fill.
#' @param frame Logical. Draw a thin frame around the panel. Default
#'   `TRUE`.
#' @param transparent Logical. Blank the painted backgrounds, as
#'   [theme_pub_clear()] does. Default `FALSE`.
#'
#' @return A ggplot2 theme.
#'
#' @examples
#' theme_pub_map(frame = FALSE)
#' @export
theme_pub_map <- function(base_size = 10,
                          water = "#DCE7EF",
                          frame = TRUE,
                          transparent = FALSE) {
  panel_fill <- if (is.na(water)) {
    ggplot2::element_blank()
  } else {
    ggplot2::element_rect(fill = water, colour = NA)
  }

  panel_frame <- if (frame) {
    ggplot2::element_rect(colour = "grey45", fill = NA, linewidth = 0.4)
  } else {
    ggplot2::element_blank()
  }

  theme_pub(base_size = base_size, grid = "none") +
    ggplot2::theme(
      # blanked per axis, not through the parent: a specific setting already
      # in the theme beats the generic one, and coord_sf() would print both
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = panel_fill,
      panel.border = panel_frame,
      plot.margin = ggplot2::margin(t = 4, r = 4, b = 2, l = 4)
    ) +
    theme_pub_clear(transparent)
}

#' House Theme for a Small Panel
#'
#' [theme_pub()] or [theme_pub_map()] at a smaller type size, for a panel
#' that shares a page with several others. The title is scaled to the
#' panel and room is left under it.
#'
#' @param map Logical. Use the map theme. Default `FALSE`.
#' @param base_size Base font size in points. Default `7.5`.
#' @param ... Passed to [theme_pub()] when `map` is `FALSE`.
#'
#' @return A ggplot2 theme.
#'
#' @examples
#' theme_pub_panel(grid = "y")
#' @export
theme_pub_panel <- function(map = FALSE, base_size = 7.5, ...) {
  base <- if (map) {
    theme_pub_map(base_size = base_size)
  } else {
    theme_pub(base_size = base_size, ...)
  }

  base +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = base_size * 1.2, margin = ggplot2::margin(b = 9)
      ),
      legend.key.height = ggplot2::unit(0.22, "cm"),
      legend.box.spacing = ggplot2::unit(3, "pt"),
      plot.margin = ggplot2::margin(t = 4, r = 6, b = 2, l = 4)
    )
}

#' House Theme for Columns over Rounds or Dates
#'
#' [theme_pub()] with horizontal gridlines only, angled labels on the x
#' axis and the legend set to the right of the bottom edge. Use it when the
#' x axis carries a round identifier or a date.
#'
#' @param base_size Base font size in points. Default `10`.
#' @param angle Angle of the x axis labels in degrees. Default `45`.
#' @param tick_size Font size of the x axis labels. Default is 85 per cent
#'   of `base_size`.
#'
#' @return A ggplot2 theme.
#'
#' @examples
#' theme_pub_cols(angle = 0)
#' @export
theme_pub_cols <- function(base_size = 10, angle = 45,
                           tick_size = base_size * 0.85) {
  # vjust = 1 stops ggplot reserving an upright label's bounding box, which
  # leaves dead space under the panel
  x_text <- if (angle == 0) {
    ggplot2::element_text(size = tick_size, colour = "grey20")
  } else {
    ggplot2::element_text(
      size = tick_size, colour = "grey20",
      angle = angle, hjust = 1, vjust = 1
    )
  }

  theme_pub(base_size = base_size, grid = "y") +
    ggplot2::theme(
      axis.text.x = x_text,
      axis.title.y = ggplot2::element_text(
        size = base_size, hjust = 0, margin = ggplot2::margin(r = 12)
      ),
      legend.position = "bottom",
      legend.justification = "right",
      legend.key.height = ggplot2::unit(0.28, "cm")
    )
}

# scales and legends -----------------------------------------------------------

#' Label Numbers by Magnitude
#'
#' A label function that writes 4.2M and 120K in place of comma-grouped
#' numerals.
#'
#' @param accuracy Rounding unit. Default `1`. Use `0.1` where the decimal
#'   matters.
#' @param ... Passed to `scales::label_number()`.
#'
#' @return A function that formats a numeric vector.
#'
#' @examples
#' label_mag()(c(1200, 4200000))
#' label_mag(accuracy = 0.1)(c(1250, 4260000))
#' @export
label_mag <- function(accuracy = 1, ...) {
  scales::label_number(
    accuracy = accuracy,
    scale_cut = scales::cut_short_scale(),
    ...
  )
}

#' Axis Padding
#'
#' Axis expansion that keeps a marker or a label from touching the panel
#' border.
#'
#' @param mult Share of the axis range added at each end. Default `0.04`.
#'
#' @return A numeric vector for the `expand` argument of a scale.
#'
#' @examples
#' pub_expansion()
#' @export
pub_expansion <- function(mult = 0.04) {
  ggplot2::expansion(mult = mult)
}

#' Continuous Scales with House Labels and Padding
#'
#' Continuous position scales that label by magnitude with [label_mag()]
#' and pad the axis with [pub_expansion()].
#'
#' @param ... Passed to `ggplot2::scale_x_continuous()` or
#'   `ggplot2::scale_y_continuous()`.
#' @param accuracy Rounding unit for the labels. Default `1`.
#' @param mult Share of the axis range added at each end. Default `0.04`.
#'
#' @return A ggplot2 scale.
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, disp * 1000)) +
#'   ggplot2::geom_point() +
#'   scale_y_pub() +
#'   theme_pub()
#' @export
scale_x_pub <- function(..., accuracy = 1, mult = 0.04) {
  ggplot2::scale_x_continuous(
    labels = label_mag(accuracy = accuracy),
    expand = pub_expansion(mult),
    ...
  )
}

#' @rdname scale_x_pub
#' @export
scale_y_pub <- function(..., accuracy = 1, mult = 0.04) {
  ggplot2::scale_y_continuous(
    labels = label_mag(accuracy = accuracy),
    expand = pub_expansion(mult),
    ...
  )
}

#' House Legend Guide
#'
#' A horizontal legend with the title above the keys and the labels below.
#' [theme_pub()] sets this for the whole plot, so use the guide only to
#' override one legend.
#'
#' @param nrow Number of rows of keys. Default `1`.
#' @param ... Passed to `ggplot2::guide_legend()`.
#'
#' @return A ggplot2 guide.
#'
#' @examples
#' guide_legend_bottom(nrow = 2)
#' @export
guide_legend_bottom <- function(nrow = 1, ...) {
  ggplot2::guide_legend(
    title.position = "top",
    label.position = "bottom",
    direction = "horizontal",
    nrow = nrow,
    ...
  )
}
