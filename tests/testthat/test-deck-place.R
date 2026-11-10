test_that("deck_fit_image keeps the aspect and stays inside the box", {
  dir <- withr::local_tempdir()
  wide <- file.path(dir, "wide.png")
  magick::image_write(magick::image_blank(400, 100, color = "white"), wide)

  by_width <- deck_fit_image(wide, max_width = 8, max_height = 5)
  by_height <- deck_fit_image(wide, max_width = 8, max_height = 1)

  expect_equal(by_width, list(width = 8, height = 2))
  expect_equal(by_height, list(width = 4, height = 1))
})

test_that("deck_fit_images_common uses one scale for every image", {
  dir <- withr::local_tempdir()
  small <- file.path(dir, "small.png")
  large <- file.path(dir, "large.png")
  magick::image_write(magick::image_blank(200, 100, color = "white"), small)
  magick::image_write(magick::image_blank(400, 400, color = "white"), large)

  fits <- deck_fit_images_common(c(small, large), max_width = 4, max_height = 2)

  expect_named(fits, c(small, large))
  expect_equal(fits[[large]], list(width = 2, height = 2))
  expect_equal(fits[[small]], list(width = 1, height = 0.5))
})

test_that("deck_stacked_height caps the image at a share of the column", {
  dir <- withr::local_tempdir()
  square <- file.path(dir, "square.png")
  magick::image_write(magick::image_blank(100, 100, color = "white"), square)

  expect_equal(deck_stacked_height(square, 2, 10, max_share = 0.5), 2)
  expect_equal(deck_stacked_height(square, 6, 10, max_share = 0.5), 5)
})

test_that("the box helpers derive a box from another", {
  box <- list(left = 1, top = 1, width = 6, height = 3)

  expect_equal(
    deck_inset_box(box, 0.5),
    list(left = 1.5, top = 1.5, width = 5, height = 2)
  )
  expect_equal(
    deck_grow_box(box, top_shift = 0.2, extra_height = 0.5),
    list(left = 1, top = 1.2, width = 6, height = 3.5)
  )
  expect_equal(deck_grow_box(box), box)
})

test_that("deck_split_box cuts a box into equal columns with a gap", {
  box <- list(left = 1, top = 1, width = 6.2, height = 3)
  columns <- deck_split_box(box, n = 2, gap = 0.2)

  expect_length(columns, 2)
  expect_equal(columns[[1]], list(left = 1, top = 1, width = 3, height = 3))
  expect_equal(columns[[2]]$left, 4.2)
  expect_equal(deck_split_box(box, n = 1)[[1]], box)
})

test_that("deck_place_in_box aligns an image inside its box", {
  fit <- list(width = 4, height = 2)
  box <- list(left = 1, top = 1, width = 6, height = 3)

  centred <- deck_place_in_box(fit, box)
  right_top <- deck_place_in_box(fit, box, align = "right", valign = "top")
  left <- deck_place_in_box(fit, box, align = "left")

  expect_equal(c(centred$left, centred$top), c(2, 1.5))
  expect_equal(c(right_top$left, right_top$top), c(3, 1))
  expect_equal(left$left, 1)
  expect_equal(c(centred$width, centred$height), c(4, 2))
  expect_error(deck_place_in_box(fit, box, align = "middle"))
})

test_that("deck_box_location turns a box into a slide location", {
  box <- list(left = 1, top = 2, width = 6, height = 3)
  plain <- deck_box_location(box)
  named <- deck_box_location(box, label = "trend")

  expect_s3_class(plain, "location_manual")
  expect_equal(
    c(plain$left, plain$top, plain$width, plain$height), c(1, 2, 6, 3)
  )
  expect_equal(plain$ph_label, "")
  expect_equal(named$ph_label, "trend")
  expect_equal(named$left, 1)
})

test_that("deck_figure places native shapes near the drawn size", {
  path <- save_test_chart(withr::local_tempdir())

  expect_s3_class(deck_figure(path, width = 6, height = 3), "dml")
  expect_s3_class(deck_figure(path, width = 6.3, height = 3.15), "dml")
})

test_that("deck_figure falls back to the picture", {
  dir <- withr::local_tempdir()
  path <- save_test_chart(dir)

  expect_s3_class(deck_figure(path, width = 3, height = 1.5), "external_img")
  expect_s3_class(
    deck_figure(
      path, width = 6, height = 3,
      config = deck_config(vector_max_bytes = 10)
    ),
    "external_img"
  )

  file.remove(file.path(dir, "chart.qs2"))

  expect_s3_class(deck_figure(path, width = 6, height = 3), "external_img")
})

test_that("deck_cover_plot is an empty white panel", {
  panel <- deck_cover_plot()

  expect_s3_class(panel, "ggplot")
  expect_length(panel$layers, 0)
  expect_equal(panel$theme$plot.background$fill, "white")
})

test_that("deck_cover_box adds a labelled panel to the slide", {
  config <- test_config()
  deck <- build_test_deck(config) |>
    deck_cover_box(
      list(left = 1, top = 6, width = 4, height = 0.3),
      label = "cover_note", config = config
    )

  shapes <- officer::slide_summary(deck)

  expect_true("cover_note" %in% shapes$ph_label)
})

test_that("deck_figure passes alternative text to the picture", {
  dir <- withr::local_tempdir()
  path <- save_test_chart(dir)

  described <- deck_figure(
    path, width = 3, height = 1.5, alt = "Fuel economy by weight"
  )

  expect_s3_class(described, "external_img")
  expect_equal(attr(described, "alt"), "Fuel economy by weight")
  expect_identical(
    deck_figure(path, width = 3, height = 1.5),
    officer::external_img(path, width = 3, height = 1.5)
  )
  expect_s3_class(
    deck_figure(path, width = 6, height = 3, alt = "Fuel economy"), "dml"
  )
})
