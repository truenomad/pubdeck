test_that("a rectangle of no size is placed as an unplaced shape", {
  local_null_device()
  path <- save_unplaced_chart(withr::local_tempdir())

  deck <- blank_page() |>
    deck_place_vector(path, test_box(height = 3), "chart")

  expect_length(unplaced_shapes(deck), 1)
})

test_that("deck_fix_shapes drops only the unplaced shape of a chart", {
  local_null_device()
  path <- save_unplaced_chart(withr::local_tempdir())

  deck <- blank_page() |>
    deck_place_vector(path, test_box(height = 3), "chart")
  before <- length(shape_names(deck))

  deck <- deck_fix_shapes(deck, quiet = TRUE)

  expect_length(unplaced_shapes(deck), 0)
  expect_equal(length(shape_names(deck)), before - 1)
})

test_that("deck_fix_shapes drops a shape at either limit in any field", {
  fields <- tibble::tibble(
    attr = rep(c("x", "y", "cx", "cy"), each = 2),
    value = rep(c("2147483647", "-2147483648"), times = 4)
  )

  purrr::pwalk(fields, \(attr, value) {
    deck <- unplaced_shape_page(attr = attr, value = value)
    expect_length(unplaced_shapes(deck), 1)

    names_left <- shape_names(deck_fix_shapes(deck, quiet = TRUE))

    expect_true("kept" %in% names_left)
    expect_false("lost" %in% names_left)
  })
})

test_that("deck_fix_shapes keeps shapes with ordinary positions", {
  deck <- unplaced_shape_page(attr = "x", value = "-914400")
  before <- shape_names(deck)

  expect_identical(shape_names(deck_fix_shapes(deck)), before)
})

test_that("deck_fix_shapes leaves a deck with nothing to drop", {
  config <- test_config()
  deck <- build_test_deck(config)
  before <- written_slides(deck)

  expect_silent(deck <- deck_fix_shapes(deck))
  expect_identical(written_slides(deck), before)
})

test_that("deck_fix_shapes reports what it drops unless quiet", {
  expect_message(
    deck_fix_shapes(unplaced_shape_page()), "Dropped 1 shape with no position"
  )
  expect_silent(deck_fix_shapes(unplaced_shape_page(), quiet = TRUE))
})

test_that("a deck with shapes dropped reads back with the same slides", {
  config <- test_config()
  deck <- build_test_deck(config) |>
    officer::add_slide(
      layout = config$layouts$text_page, master = config$master
    ) |>
    officer::ph_with(
      value = "Lost", location = .box_location(test_box(), "lost")
    )
  xfrm <- xml2::xml_find_first(
    deck$slide$get_slide(deck$cursor)$get(),
    "//p:sp[p:nvSpPr/p:cNvPr/@name='lost']/p:spPr/a:xfrm/a:ext", .slide_ns
  )
  xml2::xml_set_attr(xfrm, "cy", "-2147483648")
  before <- length(deck)

  path <- file.path(withr::local_tempdir(), "deck.pptx")
  print(deck_fix_shapes(deck, quiet = TRUE), target = path)
  saved <- officer::read_pptx(path)

  expect_equal(length(saved), before)
  expect_length(unplaced_shapes(saved), 0)
})

test_that("deck_repair drops the shapes and corrects the paragraphs", {
  config <- test_config()
  cell <- flextable::flextable(tibble::tibble(note = "")) |>
    flextable::delete_part(part = "header")

  deck <- unplaced_shape_page(config) |>
    officer::ph_with(
      value = cell, location = .box_location(test_box(top = 6))
    ) |>
    deck_repair(quiet = TRUE)

  expect_length(unplaced_shapes(deck), 0)
  expect_true("kept" %in% shape_names(deck))
  expect_equal(saved_malformed_paragraphs(deck), 0)
})

test_that("deck_repair leaves a deck with nothing to repair", {
  config <- test_config()
  deck <- build_test_deck(config)
  before <- written_slides(deck)

  expect_identical(written_slides(deck_repair(deck)), before)
})

test_that("deck_fix_shapes stops on an object that is not a deck", {
  expect_error(deck_fix_shapes(list()), class = "rlang_error")
  expect_error(deck_repair(list()), class = "rlang_error")
})
