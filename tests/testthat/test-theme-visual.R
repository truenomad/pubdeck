# drawn snapshots of the theme family, on data sets that ship with R

cylinder_scatter <- function() {
  ggplot2::ggplot(
    datasets::mtcars,
    ggplot2::aes(.data$wt, .data$mpg, colour = factor(.data$cyl))
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = pub_palette(3)) +
    ggplot2::labs(
      title = "Fuel economy by weight",
      subtitle = "Thirty-two cars, by number of cylinders",
      x = "Weight (1,000 lb)", y = "Miles per gallon", colour = "Cylinders"
    )
}

test_that("theme_pub draws a scatter with a title and a legend", {
  skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger(
    "theme-pub-scatter", cylinder_scatter() + theme_pub()
  )
})

test_that("theme_pub_cols draws columns with angled labels", {
  skip_if_not_installed("vdiffr")

  monthly <- datasets::airquality |>
    dplyr::summarise(temp = mean(.data$Temp), .by = "Month") |>
    dplyr::mutate(month = factor(month.name[.data$Month], month.name))

  plot <- ggplot2::ggplot(monthly, ggplot2::aes(.data$month, .data$temp)) +
    ggplot2::geom_col(fill = pub_palette(1)) +
    ggplot2::labs(
      title = "Mean temperature by month",
      x = NULL, y = "Degrees Fahrenheit"
    ) +
    theme_pub_cols()

  vdiffr::expect_doppelganger("theme-pub-cols", plot)
})

test_that("theme_pub_panel draws a small panel", {
  skip_if_not_installed("vdiffr")

  plot <- ggplot2::ggplot(
    datasets::pressure, ggplot2::aes(.data$temperature, .data$pressure)
  ) +
    ggplot2::geom_line(colour = pub_palette(1)) +
    ggplot2::labs(
      title = "Vapour pressure of mercury",
      x = "Temperature (C)", y = "Pressure (mm)"
    ) +
    theme_pub_panel()

  vdiffr::expect_doppelganger("theme-pub-panel", plot)
})

test_that("theme_pub_clear blanks the backgrounds", {
  skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger(
    "theme-pub-clear",
    cylinder_scatter() + theme_pub() + theme_pub_clear(transparent = TRUE)
  )
})

test_that("guide_legend_bottom sets a legend in rows under the plot", {
  skip_if_not_installed("vdiffr")

  plot <- cylinder_scatter() +
    theme_pub() +
    ggplot2::guides(colour = guide_legend_bottom(nrow = 2))

  vdiffr::expect_doppelganger("guide-legend-bottom", plot)
})

test_that("the position scales label by magnitude and pad the axes", {
  skip_if_not_installed("vdiffr")

  states <- tibble::as_tibble(datasets::state.x77) |>
    dplyr::mutate(people = .data$Population * 1000)

  plot <- ggplot2::ggplot(
    states, ggplot2::aes(.data$Area, .data$people, size = .data$people)
  ) +
    ggplot2::geom_point(colour = pub_palette(1), alpha = 0.6) +
    scale_x_pub() +
    scale_y_pub(accuracy = 0.1) +
    ggplot2::scale_size_area(labels = label_mag()) +
    ggplot2::labs(
      title = "Population by land area, United States",
      x = "Area (square miles)", y = "People", size = "People"
    ) +
    theme_pub()

  vdiffr::expect_doppelganger("scale-pub-label-mag", plot)
})
