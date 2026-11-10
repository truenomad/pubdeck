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
