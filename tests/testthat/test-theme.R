test_that("theme_pub returns a complete theme with the house legend", {
  theme <- theme_pub()

  expect_s3_class(theme, "theme")
  expect_equal(theme$legend.position, "bottom")
  expect_equal(theme$plot.title$hjust, 0)
  expect_equal(theme$plot.title$face, "plain")
})

test_that("theme_pub keeps the title size when the base size changes", {
  expect_equal(theme_pub(base_size = 7)$plot.title$size, 12)
  expect_equal(theme_pub(base_size = 7)$axis.title.x$size, 7)
})

test_that("theme_pub drops the gridlines it is asked to drop", {
  expect_s3_class(theme_pub(grid = "x")$panel.grid.major.y, "element_blank")
  expect_s3_class(theme_pub(grid = "y")$panel.grid.major.x, "element_blank")
  expect_s3_class(theme_pub(grid = "none")$panel.grid.major, "element_blank")
  expect_s3_class(theme_pub(grid = "both")$panel.grid.major, "element_line")
  expect_error(theme_pub(grid = "diagonal"))
})

test_that("theme_pub_clear blanks backgrounds only when asked", {
  expect_length(theme_pub_clear(transparent = FALSE), 0)
  expect_true(is.na(theme_pub_clear(transparent = TRUE)$plot.background$fill))
})

test_that("theme_pub_map removes axes and takes a water fill and frame", {
  map <- theme_pub_map()

  expect_s3_class(map$axis.text.x, "element_blank")
  expect_s3_class(map$axis.ticks.y, "element_blank")
  expect_equal(map$panel.background$fill, "#DCE7EF")
  expect_s3_class(theme_pub_map(water = NA)$panel.background, "element_blank")
  expect_s3_class(theme_pub_map(frame = FALSE)$panel.border, "element_blank")
})

test_that("theme_pub_panel scales the title to the panel", {
  panel <- theme_pub_panel()

  expect_equal(panel$plot.title$size, 7.5 * 1.2)
  expect_s3_class(theme_pub_panel(map = TRUE)$axis.text.x, "element_blank")
  expect_s3_class(
    theme_pub_panel(grid = "none")$panel.grid.major, "element_blank"
  )
})

test_that("theme_pub_cols angles the x labels unless the angle is zero", {
  expect_equal(theme_pub_cols()$axis.text.x$angle, 45)
  expect_null(theme_pub_cols(angle = 0)$axis.text.x$angle)
  expect_equal(theme_pub_cols()$legend.justification, "right")
})

test_that("theme_pub draws a panel frame only when asked", {
  framed <- theme_pub(frame = TRUE)$panel.border
  styled <- theme_pub(
    frame = TRUE, frame_colour = "grey40", frame_linewidth = 1
  )$panel.border

  expect_s3_class(theme_pub()$panel.border, "element_blank")
  expect_s3_class(theme_pub(frame = FALSE)$panel.border, "element_blank")
  expect_s3_class(framed, "element_rect")
  expect_equal(framed$colour, "black")
  expect_equal(framed$linewidth, 0.43)
  expect_true(is.na(framed$fill))
  expect_equal(styled$colour, "grey40")
  expect_equal(styled$linewidth, 1)
})

test_that("theme_pub takes its typeface, title position and margins", {
  theme <- theme_pub(
    family = "serif", title_position = "panel",
    plot_margin = list(t = 18, r = 24, b = 14, l = 18)
  )

  expect_equal(theme$text$family, "serif")
  expect_equal(theme$plot.title.position, "panel")
  expect_equal(as.numeric(theme$plot.margin), c(18, 24, 14, 18))
  expect_equal(
    as.numeric(theme_pub(plot_margin = list(r = 30))$plot.margin),
    c(8, 30, 6, 8)
  )
})

test_that("theme_pub reads its settings from the configuration", {
  config <- deck_config(
    theme_frame = TRUE, theme_frame_colour = "grey20",
    theme_frame_linewidth = 0.8, theme_family = "mono",
    theme_title_position = "panel", theme_plot_margin = list(l = 30)
  )
  theme <- theme_pub(config = config)

  expect_equal(theme$panel.border$colour, "grey20")
  expect_equal(theme$panel.border$linewidth, 0.8)
  expect_equal(theme$text$family, "mono")
  expect_equal(theme$plot.title.position, "panel")
  expect_equal(as.numeric(theme$plot.margin), c(8, 10, 6, 30))
  expect_s3_class(
    theme_pub(frame = FALSE, config = config)$panel.border, "element_blank"
  )
  expect_equal(
    theme_pub(family = "serif", config = config)$text$family, "serif"
  )
  expect_equal(with_deck_config(config, theme_pub()$text$family), "mono")
})

test_that("theme_pub is unchanged by settings left at their defaults", {
  stated <- theme_pub(
    frame = FALSE, family = "", title_position = "plot",
    plot_margin = list(t = 8, r = 10, b = 6, l = 8)
  )

  expect_identical(theme_pub(), stated)
  expect_identical(theme_pub(grid = "y"), theme_pub(10, "y", FALSE))
  expect_equal(as.numeric(theme_pub()$plot.margin), c(8, 10, 6, 8))
  expect_equal(theme_pub()$plot.title.position, "plot")
  expect_equal(theme_pub()$text$family, "")
})

test_that("the map, panel and column themes follow the configuration", {
  config <- deck_config(theme_family = "mono", theme_frame = TRUE)

  expect_equal(theme_pub_map(config = config)$text$family, "mono")
  expect_equal(theme_pub_map(config = config)$panel.border$colour, "grey45")
  expect_equal(theme_pub_cols(config = config)$text$family, "mono")
  expect_s3_class(theme_pub_cols(config = config)$panel.border, "element_rect")
  expect_equal(theme_pub_panel(config = config)$text$family, "mono")
  expect_equal(
    theme_pub_panel(map = TRUE, config = config)$text$family, "mono"
  )
})

test_that("the map and column themes pass their settings to theme_pub", {
  expect_equal(theme_pub_map(family = "serif")$text$family, "serif")
  expect_equal(
    theme_pub_map(title_position = "panel")$plot.title.position, "panel"
  )
  expect_equal(theme_pub_cols(family = "serif")$text$family, "serif")
  expect_s3_class(theme_pub_cols(frame = TRUE)$panel.border, "element_rect")
  expect_equal(
    as.numeric(theme_pub_cols(plot_margin = list(t = 20))$plot.margin),
    c(20, 10, 6, 8)
  )
  expect_identical(theme_pub_map(), theme_pub_map(family = ""))
  expect_identical(theme_pub_cols(), theme_pub_cols(frame = FALSE))
})

test_that("theme_pub_panel takes its margins as given and from the config", {
  given <- theme_pub_panel(plot_margin = list(t = 12, r = 16, b = 10, l = 12))
  from_config <- theme_pub_panel(
    config = deck_config(theme_panel_margin = list(b = 9))
  )

  on_map <- theme_pub_panel(map = TRUE, plot_margin = list(l = 1))

  expect_equal(as.numeric(theme_pub_panel()$plot.margin), c(4, 6, 2, 4))
  expect_equal(as.numeric(given$plot.margin), c(12, 16, 10, 12))
  expect_equal(as.numeric(from_config$plot.margin), c(4, 6, 9, 4))
  expect_equal(as.numeric(on_map$plot.margin), c(4, 6, 2, 1))
  expect_s3_class(theme_pub_panel(frame = TRUE)$panel.border, "element_rect")
})

test_that("label_mag writes thousands and millions in short form", {
  expect_equal(label_mag()(c(1200, 4200000)), c("1K", "4M"))
  expect_equal(label_mag(accuracy = 0.1)(c(1250, 4260000)), c("1.2K", "4.3M"))
})

test_that("the position scales carry the house labels and padding", {
  scale_x <- scale_x_pub()
  scale_y <- scale_y_pub(accuracy = 0.1, mult = 0.1)

  expect_s3_class(scale_x, "ScaleContinuousPosition")
  expect_equal(scale_x$labels(1200), "1K")
  expect_equal(scale_y$labels(1250), "1.2K")
  expect_equal(scale_y$expand, pub_expansion(0.1))
  expect_true("x" %in% scale_x$aesthetics)
  expect_true("y" %in% scale_y$aesthetics)
})

test_that("guide_legend_bottom is a horizontal legend guide", {
  guide <- guide_legend_bottom(nrow = 2)

  expect_s3_class(guide, "Guide")
  expect_equal(guide$params$nrow, 2)
  expect_equal(guide$params$direction, "horizontal")
})

test_that("a plot builds with the theme and scales", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, disp * 1000)) +
    ggplot2::geom_point(colour = pub_palette(1)) +
    scale_y_pub() +
    theme_pub_cols()

  expect_no_error(ggplot2::ggplotGrob(plot))
})
