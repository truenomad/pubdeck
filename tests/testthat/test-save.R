test_that("ggsave_pub writes the figure and keeps its grob", {
  dir <- withr::local_tempdir()
  path <- save_test_chart(dir)

  expect_true(file.exists(path))
  expect_true(file.exists(file.path(dir, "chart.qs2")))

  saved <- qs2::qs_read(file.path(dir, "chart.qs2"))

  expect_s3_class(saved$grob, "grob")
  expect_equal(c(saved$width, saved$height), c(6, 3))
  expect_true("ggplot2" %in% saved$namespaces)
})

test_that("ggsave_pub takes its resolution from the configuration", {
  dir <- withr::local_tempdir()
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  default <- file.path(dir, "default.png")
  coarse <- file.path(dir, "coarse.png")
  stated <- file.path(dir, "stated.png")

  suppressMessages({
    ggsave_pub(plot, default, width = 4, height = 2)
    ggsave_pub(
      plot, coarse, width = 4, height = 2, config = deck_config(dpi = 72)
    )
    ggsave_pub(plot, stated, width = 4, height = 2, dpi = 100)
  })

  expect_equal(magick::image_info(magick::image_read(default))$width, 1200)
  expect_equal(magick::image_info(magick::image_read(coarse))$width, 288)
  expect_equal(magick::image_info(magick::image_read(stated))$width, 400)
})

test_that("ggsave_pub needs a width and a height", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg))

  expect_error(
    ggsave_pub(plot, file.path(tempdir(), "no_size.png")),
    "required"
  )
})

test_that("ggsave_pub saves a map at the map resolution with no grob", {
  skip_if_not_installed("sf")

  dir <- withr::local_tempdir()
  points <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
  )
  map <- ggplot2::ggplot(points) +
    ggplot2::geom_sf() +
    theme_pub_map()
  path <- file.path(dir, "map.png")

  suppressMessages(ggsave_pub(map, path, width = 4, height = 2))

  expect_equal(magick::image_info(magick::image_read(path))$width, 800)
  expect_false(file.exists(file.path(dir, "map.qs2")))
})

test_that("ggsave_pub keeps no grob when told not to", {
  dir <- withr::local_tempdir()
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  path <- file.path(dir, "chart.png")
  left_beside <- file.path(dir, "chart.qs2")
  writeLines("kept", left_beside)

  suppressMessages(
    ggsave_pub(plot, path, width = 4, height = 2, grob = FALSE)
  )

  expect_true(file.exists(path))
  expect_equal(readLines(left_beside), "kept")
})

test_that("ggsave_pub takes the grob setting from the configuration", {
  dir <- withr::local_tempdir()
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  pictures_only <- deck_config(save_grob = FALSE)
  path <- file.path(dir, "chart.png")

  suppressMessages(
    ggsave_pub(plot, path, width = 4, height = 2, config = pictures_only)
  )

  expect_false(file.exists(file.path(dir, "chart.qs2")))

  suppressMessages(ggsave_pub(
    plot, path, width = 4, height = 2, grob = TRUE, config = pictures_only
  ))

  expect_true(file.exists(file.path(dir, "chart.qs2")))
})

test_that("ggsave_pub saves a map at the chart resolution without a map one", {
  skip_if_not_installed("sf")

  dir <- withr::local_tempdir()
  points <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
  )
  map <- ggplot2::ggplot(points) +
    ggplot2::geom_sf() +
    theme_pub_map()
  path <- file.path(dir, "map.png")

  suppressMessages(ggsave_pub(
    map, path, width = 4, height = 2,
    config = deck_config(dpi = 100, map_dpi = NULL)
  ))

  expect_equal(magick::image_info(magick::image_read(path))$width, 400)
})

test_that("ggsave_pub reports the path from the root it is given", {
  dir <- withr::local_tempdir()
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  expect_message(
    ggsave_pub(
      plot, file.path(dir, "plots", "chart.png"), width = 4, height = 2,
      config = deck_config(path_root = dir)
    ),
    "plots/chart.png"
  )
})

test_that("save_pub_grob removes a stale grob when none is kept", {
  dir <- withr::local_tempdir()
  path <- save_test_chart(dir)
  grob_path <- file.path(dir, "chart.qs2")
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg))

  expect_true(file.exists(grob_path))
  expect_null(save_pub_grob(plot, path, 6, 3, is_map = TRUE))
  expect_false(file.exists(grob_path))
  expect_null(
    save_pub_grob(plot, file.path(dir, "chart.pdf"), 6, 3, is_map = FALSE)
  )
})

test_that("compress_pub leaves missing and non-PNG files alone", {
  dir <- withr::local_tempdir()
  text_file <- file.path(dir, "notes.txt")
  writeLines("a", text_file)

  expect_equal(compress_pub(file.path(dir, "absent.png")), file.path(
    dir, "absent.png"
  ))
  expect_equal(compress_pub(text_file), text_file)
  expect_equal(readLines(text_file), "a")
})

test_that("trim_png cuts the margin down to the border", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "dot.png")

  magick::image_blank(200, 200, color = "white") |>
    magick::image_composite(
      magick::image_blank(20, 20, color = "black"), offset = "+90+90"
    ) |>
    magick::image_write(path)

  trim_png(path, border = 5)
  trimmed <- magick::image_info(magick::image_read(path))

  expect_equal(c(trimmed$width, trimmed$height), c(30, 30))
})

test_that("a PNG is drawn with ragg unless the call or a setting says so", {
  expect_identical(.ggsave_extras("chart.png", "ragg")$device, ragg::agg_png)
  expect_identical(.ggsave_extras("CHART.PNG", "ragg")$device, ragg::agg_png)
  expect_identical(.ggsave_extras("chart.png", "default"), list())
  expect_identical(.ggsave_extras("chart.pdf", "ragg"), list())
  expect_identical(
    .ggsave_extras("chart.png", "ragg", device = "png", limitsize = FALSE),
    list(device = "png", limitsize = FALSE)
  )
  expect_identical(
    .ggsave_extras("chart.png", "ragg", limitsize = FALSE),
    list(limitsize = FALSE, device = ragg::agg_png)
  )
})

test_that("arguments ragg does not take are dropped when ragg is named", {
  expect_identical(
    .ggsave_extras(
      "chart.png", "ragg", type = "cairo", antialias = "none",
      limitsize = FALSE
    ),
    list(limitsize = FALSE, device = ragg::agg_png)
  )
  expect_identical(
    .ggsave_extras("chart.png", "default", type = "cairo"),
    list(type = "cairo")
  )

  dir <- withr::local_tempdir()
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  path <- file.path(dir, "chart.png")

  expect_no_error(suppressMessages(ggsave_pub(
    plot, path, width = 4, height = 2, grob = FALSE, type = "cairo"
  )))
  expect_true(file.exists(path))
})

test_that("ggsave_pub writes the same picture with either PNG device", {
  dir <- withr::local_tempdir()
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    theme_pub()
  named <- file.path(dir, "named.png")
  left <- file.path(dir, "left.png")

  suppressMessages({
    ggsave_pub(plot, named, width = 4, height = 2, grob = FALSE)
    ggsave_pub(
      plot, left, width = 4, height = 2, grob = FALSE, png_device = "default"
    )
  })

  expect_equal(deck_config()$png_device, "ragg")
  expect_identical(image_pixels(named), image_pixels(left))
})

test_that("ggsave_pub takes its PNG device three ways", {
  dir <- withr::local_tempdir()
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  path <- file.path(dir, "chart.png")
  unknown <- deck_config(png_device = "cairo")
  save <- \(...) {
    suppressMessages(
      ggsave_pub(plot, path, width = 4, height = 2, grob = FALSE, ...)
    )
  }

  expect_error(save(png_device = "cairo"), "png_device")
  expect_error(save(config = unknown), "png_device")
  expect_error(with_deck_config(unknown, save()), "png_device")
  expect_false(file.exists(path))
  expect_no_error(save(png_device = "default", config = unknown))
  expect_true(file.exists(path))
})

test_that("ggsave_pub uses a device named in the call as it is given", {
  dir <- withr::local_tempdir()
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  path <- file.path(dir, "chart.png")
  opened <- 0
  counted <- function(filename, width, height, units, res, ...) {
    opened <<- opened + 1
    ragg::agg_png(
      filename, width = width, height = height, units = units, res = res,
      ...
    )
  }

  suppressMessages(ggsave_pub(
    plot, path, width = 4, height = 2, grob = FALSE, device = counted
  ))

  expect_equal(opened, 1)
  expect_equal(magick::image_info(magick::image_read(path))$width, 1200)
})
