test_notes <- function() {
  tibble::tibble(
    lead = c("Coverage", "Sources"),
    body = c(
      "is the share of children reached in the round.",
      "are listed at the foot of each page."
    )
  )
}

test_that("deck_note_block builds a row for the heading and each note", {
  block <- deck_note_block("How to read this page", test_notes())
  rows <- attr(block, "deck_note_rows")

  expect_s3_class(block, "flextable")
  expect_equal(nrow(rows), 3)
  expect_equal(rows$lead, c("How to read this page", "Coverage", "Sources"))
  expect_equal(rows$pt, c(10.5, 8.5, 8.5))
  expect_equal(flextable::nrow_part(block, "body"), 3)
})

test_that("deck_note_block works without a heading", {
  rows <- attr(deck_note_block(notes = test_notes()), "deck_note_rows")

  expect_equal(nrow(rows), 2)
  expect_equal(rows$lead, c("Coverage", "Sources"))
})

test_that("deck_note_block flattens a note built from several pieces", {
  notes <- tibble::tibble(
    lead = list(c("Coverage", " by  round")),
    body = list(c("is the share", "reached."))
  )
  rows <- attr(deck_note_block(notes = notes), "deck_note_rows")

  expect_equal(rows$lead, "Coverage by round")
  expect_equal(rows$body, "is the share reached.")
})

test_that("deck_note_block indents a second-level bullet", {
  notes <- dplyr::mutate(test_notes(), level = c(1L, 2L))
  rows <- attr(deck_note_block(notes = notes, bullet = TRUE), "deck_note_rows")

  expect_equal(rows$indent_pt, c(0, 14))
  expect_equal(
    attr(deck_note_block(notes = notes), "deck_note_rows")$indent_pt,
    c(0, 0)
  )
})

test_that("deck_note_block takes sizes and spacing from the configuration", {
  config <- deck_config(note_pt = 12, note_pad_y_pt = 10, note_indent = 20)
  notes <- dplyr::mutate(test_notes(), level = c(1L, 2L))

  plain <- attr(
    deck_note_block("Heading", notes, config = config), "deck_note_rows"
  )
  listed <- attr(
    deck_note_block(notes = notes, bullet = TRUE, config = config),
    "deck_note_rows"
  )

  expect_equal(plain$pt, c(14, 12, 12))
  expect_equal(plain$pad_pt, c(20, 20, 20))
  expect_equal(listed$indent_pt, c(0, 20))
})

test_that("deck_note_block accepts italic and url columns", {
  notes <- dplyr::mutate(
    test_notes(),
    italic = c(TRUE, FALSE),
    url = c(NA, "https://example.org")
  )

  expect_s3_class(deck_note_block(notes = notes), "flextable")
})

test_that("deck_note_height grows as the box narrows", {
  block <- deck_note_block("How to read this page", test_notes())

  wide <- deck_note_height(block, width = 6)
  narrow <- deck_note_height(block, width = 1.5)

  expect_gt(narrow, wide)
  expect_gt(wide, 0)
  expect_null(grDevices::dev.list())
})

test_that("deck_note_height measures a table that is not a note block", {
  plain <- flextable::flextable(head(mtcars[, 1:2]))

  expect_equal(
    deck_note_height(plain, width = 3),
    flextable::flextable_dim(plain)$heights
  )
})

test_that("deck_place_note places the box and warns when it overflows", {
  config <- test_config()
  block <- deck_note_block("How to read this page", test_notes())
  deck <- deck_open(config = config) |>
    officer::add_slide(layout = "two_plots", master = config$master)

  expect_no_message(
    placed <- deck_place_note(
      deck, block, list(left = 5, top = 1.5, width = 4, height = 4), config
    )
  )
  expect_equal(nrow(officer::slide_summary(placed)), 1)
  expect_message(
    deck_place_note(
      deck, block, list(left = 5, top = 1.5, width = 4, height = 0.1), config
    ),
    "shorten the note"
  )
})
