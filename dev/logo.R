# Draws the package logo to man/figures/logo.png. Run from the package root:
#   Rscript dev/logo.R

slate <- "#2E4053"
focal <- "#A9552B"
paper <- "#FFFFFF"
back_paper <- "#DCE3E9"
rule <- "#9AA3AB"
strip <- "#5B7186"
bar_colours <- c("#4E6E5D", focal, "#2F5061", "#B08A3E")

# shapes -----------------------------------------------------------------------

hexagon <- function(radius) {
  angle <- (seq(30, 330, by = 60) + 60) * pi / 180

  tibble::tibble(x = radius * cos(angle), y = radius * sin(angle))
}

# a rectangle given in the slide's own coordinates, turned and moved into place
placed <- function(xmin, xmax, ymin, ymax, centre, degrees) {
  turn <- degrees * pi / 180
  x <- c(xmin, xmax, xmax, xmin)
  y <- c(ymin, ymin, ymax, ymax)

  tibble::tibble(
    x = centre[1] + x * cos(turn) - y * sin(turn),
    y = centre[2] + x * sin(turn) + y * cos(turn)
  )
}

slide_width <- 1.02
slide_height <- slide_width * 9 / 16
half_w <- slide_width / 2
half_h <- slide_height / 2

front <- list(centre = c(-0.03, 0.2), degrees = -4)
back <- list(centre = c(0.05, 0.29), degrees = 5)

on_front <- function(xmin, xmax, ymin, ymax) {
  placed(xmin, xmax, ymin, ymax, front$centre, front$degrees)
}

bar_width <- 0.13
bar_gap <- 0.07
bar_left <- -0.33 + (seq_along(bar_colours) - 1) * (bar_width + bar_gap)
bar_heights <- c(0.14, 0.27, 0.2, 0.1)
baseline <- -half_h + 0.13

bars <- purrr::pmap(
  list(bar_left, bar_heights, bar_colours),
  \(left, height, colour) {
    dplyr::mutate(
      on_front(left, left + bar_width, baseline, baseline + height),
      colour = colour
    )
  }
) |>
  purrr::list_rbind(names_to = "bar")

# drawing ----------------------------------------------------------------------

layer <- function(shape, fill, ...) {
  ggplot2::geom_polygon(
    data = shape, ggplot2::aes(.data$x, .data$y), fill = fill, ...
  )
}

logo <- ggplot2::ggplot() +
  layer(hexagon(0.985), focal) +
  layer(hexagon(0.915), slate) +
  layer(
    placed(-half_w, half_w, -half_h, half_h, back$centre, back$degrees),
    back_paper
  ) +
  layer(
    placed(
      -half_w, half_w, -half_h, half_h, front$centre + c(0.015, -0.02),
      front$degrees
    ),
    "#1B2836"
  ) +
  layer(on_front(-half_w, half_w, -half_h, half_h), paper) +
  layer(on_front(-half_w + 0.08, -0.06, half_h - 0.115, half_h - 0.075), rule) +
  layer(
    on_front(-half_w + 0.08, half_w - 0.08, baseline - 0.008, baseline),
    rule
  ) +
  ggplot2::geom_polygon(
    data = bars,
    ggplot2::aes(.data$x, .data$y, group = .data$bar, fill = .data$colour)
  ) +
  layer(on_front(-half_w, half_w, -half_h, -half_h + 0.06), strip) +
  ggplot2::annotate(
    "text", x = 0, y = -0.47, label = "pubdeck", colour = paper,
    family = "Source Sans 3", fontface = "bold", size = 6.6
  ) +
  ggplot2::scale_fill_identity() +
  ggplot2::coord_fixed(
    xlim = c(-sqrt(3) / 2, sqrt(3) / 2), ylim = c(-1, 1), expand = FALSE
  ) +
  ggplot2::theme_void()

fs::dir_create(fs::path("man", "figures"))

ggplot2::ggsave(
  fs::path("man", "figures", "logo.png"), logo,
  width = 518, height = 600, units = "px", dpi = 300, bg = "transparent"
)
