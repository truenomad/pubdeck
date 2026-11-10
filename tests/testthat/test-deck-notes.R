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

test_sentence <- function() {
  paste(
    "Coverage is the share of children reached in the round, as the",
    "campaign reported it for every district that sent a tally."
  )
}

test_that("the shaper and systemfonts agree on the width of a line", {
  font <- deck_config()$font
  runs <- tibble::tibble(text = test_sentence(), weight = "normal")
  shaped <- sum(.shape_row(runs, 8.5, font)$advance) / 72

  expect_equal(
    shaped, line_width(test_sentence(), font, 8.5), tolerance = 0.01
  )
})

test_that("a row wraps onto more lines as its box narrows", {
  font <- deck_config()$font
  one_line <- line_width(test_sentence(), font, 8.5)
  lines_at <- \(share) {
    .typeface_lines("", test_sentence(), 8.5, one_line * share, font)
  }

  expect_equal(lines_at(1.02), 1)
  expect_equal(lines_at(0.55), 2)
  expect_gte(lines_at(0.3), 4)
  expect_gt(lines_at(0.15), lines_at(0.3))
  expect_gt(lines_at(0.05), lines_at(0.15))
})

test_that("a bold lead never takes fewer lines than the same text regular", {
  font <- deck_config()$font
  skip_if_not(
    tolower(font) %in% tolower(systemfonts::system_fonts()$family),
    "the deck typeface is not installed"
  )

  lead <- "Coverage reported by the campaign"
  body <- "is the share of children reached in the round."
  whole <- paste(lead, body)
  one_line <- line_width(whole, font, 8.5)
  widths <- seq(0.25, 6, by = 0.25)

  bold <- purrr::map_dbl(
    widths, \(width) .typeface_lines(lead, body, 8.5, width, font)
  )
  regular <- purrr::map_dbl(
    widths, \(width) .typeface_lines("", whole, 8.5, width, font)
  )

  expect_true(all(bold >= regular))
  expect_equal(.typeface_lines("", whole, 8.5, one_line * 1.01, font), 1)
  expect_equal(.typeface_lines(lead, body, 8.5, one_line * 1.01, font), 2)
})

test_that("a row with no text or no room to wrap in is still counted", {
  font <- deck_config()$font

  expect_equal(.typeface_lines("", "", 8.5, 3, font), 1)
  expect_equal(.typeface_lines("Sources", "", 8.5, 3, font), 1)
  expect_equal(.typeface_lines("", "Wide", 8.5, 0, font), 4)
  expect_equal(.typeface_lines("", "Wide", 8.5, -1, font), 4)
})

test_that("marks set over a letter are not counted as lines", {
  font <- deck_config()$font

  # an Arabic word whose vowel marks sit above and below its letters
  word <- "\u0645\u064e\u0631\u0652\u062d\u064e\u0628\u064b\u0627"
  marked <- paste(rep(word, 12), collapse = " ")
  runs <- tibble::tibble(text = marked, weight = "normal")
  one_line <- sum(.shape_row(runs, 8.5, font)$advance) / 72

  expect_equal(.typeface_lines("", marked, 8.5, one_line * 1.02, font), 1)
  expect_equal(.typeface_lines("", marked, 8.5, one_line * 0.55, font), 2)
})

test_that("deck_note_height keeps the row formula and counts the lines", {
  config <- deck_config()
  notes <- tibble::tibble(lead = "", body = test_sentence())
  block <- deck_note_block(notes = notes, config = config)

  one_line <- line_width(test_sentence(), config$font, config$note_pt)
  inset <- 2 * config$note_pad_x_pt / 72
  line <- config$note_pt * config$note_line_spacing / 72
  padding <- 2 * config$note_pad_y_pt / 72

  expect_equal(
    deck_note_height(block, one_line * 1.02 + inset), line + padding
  )
  expect_equal(
    deck_note_height(block, one_line * 0.55 + inset), 2 * line + padding
  )
})

test_that("deck_note_height takes its measure three ways", {
  block <- deck_note_block("How to read this page", test_notes())
  device <- deck_config(note_measure = "device")

  # a box with no room to wrap in, where the two measures cannot agree
  narrow <- 0.1
  by_argument <- deck_note_height(block, narrow, measure = "device")
  by_typeface <- deck_note_height(block, narrow)

  expect_equal(deck_config()$note_measure, "typeface")
  expect_gt(by_typeface, by_argument)
  expect_identical(deck_note_height(block, narrow, device), by_argument)
  expect_identical(
    with_deck_config(device, deck_note_height(block, narrow)), by_argument
  )
  expect_identical(
    deck_note_height(block, narrow, device, measure = "typeface"),
    by_typeface
  )
  expect_error(
    deck_note_height(block, narrow, measure = "pixels"), "must be one of"
  )
  expect_error(
    deck_note_height(block, narrow, deck_config(note_measure = "pixels")),
    "must be one of"
  )
  expect_null(grDevices::dev.list())
})

test_that("a typeface that is installed is not reported", {
  local_typeface_record()
  installed <- systemfonts::system_fonts()$family
  skip_if(length(installed) == 0, "no typeface is installed")

  expect_no_message(found <- .check_typeface(toupper(installed[[1]])))
  expect_true(found)
  expect_no_message(.check_typeface(tolower(installed[[1]])))
  expect_null(.state$typeface_reported)
})

test_that("a typeface that is not installed is reported once in a session", {
  local_typeface_record()
  config <- deck_config(font = "No Such Typeface")
  block <- deck_note_block("How to read this page", test_notes())

  expect_message(
    height <- deck_note_height(block, 3, config), "is not installed"
  )
  expect_gt(height, 0)
  expect_no_message(deck_note_height(block, 3, config))
  expect_false(.check_typeface("No Such Typeface"))
  expect_message(.check_typeface("Another Missing Face"), "Another Missing")
  expect_equal(
    .state$typeface_reported, c("No Such Typeface", "Another Missing Face")
  )
})

test_that("the device measure does not look for the typeface", {
  local_typeface_record()
  config <- deck_config(font = "No Such Typeface", note_measure = "device")
  block <- deck_note_block("How to read this page", test_notes())

  expect_no_message(deck_note_height(block, 3, config))
  expect_null(.state$typeface_reported)
})

test_that("deck_place_note takes the measure for a call and draws one box", {
  config <- test_config()
  block <- deck_note_block("How to read this page", test_notes())
  geom <- list(left = 5, top = 1.5, width = 4, height = 4)
  place <- \(...) {
    deck_open(config = config) |>
      officer::add_slide(layout = "two_plots", master = config$master) |>
      deck_place_note(block, geom, config, ...)
  }

  expect_identical(
    written_slides(place(note_measure = "device")), written_slides(place())
  )
  expect_error(place(note_size = 1), "Unknown setting")
  expect_error(place(note_measure = "pixels"), "must be one of")
})
