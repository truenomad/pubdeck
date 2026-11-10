test_that("a flextable cell with no text is saved as a malformed paragraph", {
  expect_equal(saved_malformed_paragraphs(empty_cell_page()), 1)
})

test_that("deck_cover_box and the contents bands save valid paragraphs", {
  config <- test_config()
  deck <- build_test_deck(config)
  entries <- deck_contents_entries(deck, config)

  deck <- deck_contents_slide(deck, entries, config = config) |>
    deck_cover_box(
      list(left = 1, top = 6, width = 4, height = 0.3),
      label = "cover_note", config = config
    )

  expect_equal(saved_malformed_paragraphs(deck), 0)
})

test_that("deck_text_box saves a valid paragraph with no text", {
  config <- test_config()

  deck <- blank_page(config) |>
    deck_text_box(test_box(), config = config) |>
    deck_text_box(
      test_box(top = 4), text = NA_character_, label = "missing",
      config = config
    )

  expect_equal(saved_malformed_paragraphs(deck), 0)
})

test_that("deck_hrule saves a valid paragraph", {
  config <- test_config()

  deck <- blank_page(config) |>
    deck_hrule(left = 1, top = 6, width = 3, config = config)

  expect_equal(saved_malformed_paragraphs(deck), 0)
})

test_that("the text blocks save valid paragraphs for empty lines", {
  config <- test_config()

  deck <- blank_page(config) |>
    deck_panel(
      test_box(), heading = "What changed", lines = c("Rose", "", "Fell"),
      config = config
    ) |>
    deck_callout(
      test_box(top = 3.6), label = "", text = "A sentence", config = config
    ) |>
    deck_rule_note(
      test_box(top = 5.2), label = "Next", text = "", config = config
    ) |>
    deck_kpi(
      test_box(left = 6), label = "Cases", period_label = "",
      rows = c("2024: 10", ""), change = "-5%", config = config
    )

  expect_equal(saved_malformed_paragraphs(deck), 0)
})

test_that("deck_table saves valid paragraphs for empty and missing cells", {
  config <- test_config()
  data <- tibble::tibble(
    place = c("North", "", NA), cases = c(1.5, NA, 2)
  )

  deck <- blank_page(config) |>
    deck_table(test_box(height = 3), data, widths = c(2, 2), config = config)

  expect_equal(saved_malformed_paragraphs(deck), 0)
})

test_that("deck_table writes a space only into the cells with no text", {
  config <- test_config()
  data <- tibble::tibble(place = c("North", ""), cases = c(1.5, NA))

  table <- blank_page(config) |>
    deck_table(test_box(), data, widths = c(2, 2), config = config) |>
    shape_xml(config$shape_labels$table)

  expect_equal(count_matches(table, "<a:t> </a:t>"), 2)
  expect_equal(count_matches(table, "<a:t>1.5</a:t>"), 1)
  expect_equal(count_matches(table, "<a:t>North</a:t>"), 1)
})

test_that("deck_note_block saves valid paragraphs for an empty heading", {
  config <- test_config()
  notes <- tibble::tibble(
    lead = c("Coverage", ""), body = c("is the share reached.", "")
  )
  block <- deck_note_block("", notes, config = config)

  deck <- blank_page(config) |>
    deck_place_note(block, test_box(height = 4), config = config)

  expect_equal(saved_malformed_paragraphs(deck), 0)
  expect_equal(attr(block, "deck_note_rows")$lead, c("", "Coverage", ""))
})

test_that("the bands keep their size with the space in them", {
  config <- test_config()
  box <- list(left = 1, top = 6, width = 4, height = 0.3)

  deck <- blank_page(config) |>
    deck_cover_box(box, label = "cover_note", config = config)
  band <- .filled_band("#FFFFFF") |>
    .size_band(width = 4, height = 0.3)

  expect_equal(shape_box(deck, "cover_note"), tibble::as_tibble(box))
  expect_equal(flextable::flextable_dim(band)$heights, 0.3)
  expect_equal(table_runs(band)$txt, " ")
})

test_that("deck_fix_paragraphs corrects a table a caller built", {
  deck <- deck_fix_paragraphs(empty_cell_page())

  expect_equal(saved_malformed_paragraphs(deck), 0)
})

test_that("a corrected deck reads back with the same slides", {
  config <- test_config()
  deck <- build_test_deck(config) |>
    officer::add_slide(
      layout = config$layouts$text_page, master = config$master
    ) |>
    officer::ph_with(
      value = flextable::flextable(tibble::tibble(note = c("", NA))),
      location = .box_location(test_box())
    )
  before <- length(deck)

  path <- file.path(withr::local_tempdir(), "deck.pptx")
  print(deck_fix_paragraphs(deck), target = path)
  saved <- officer::read_pptx(path)

  expect_equal(length(saved), before)
  expect_equal(saved_malformed_paragraphs(saved), 0)
})

test_that("deck_fix_paragraphs leaves a deck with nothing to correct", {
  config <- test_config()
  deck <- build_test_deck(config)
  before <- written_slides(deck)

  expect_identical(written_slides(deck_fix_paragraphs(deck)), before)
})

test_that("deck_fix_paragraphs moves the end mark and keeps the text", {
  deck <- empty_cell_page()
  slide <- deck$slide$get_slide(deck$cursor)$get()

  deck_fix_paragraphs(deck)
  paragraph <- xml2::xml_find_first(slide, "//a:tbl//a:p")
  last <- xml2::xml_find_first(paragraph, "./*[last()]")

  expect_equal(xml2::xml_name(last), "endParaRPr")
  expect_length(xml2::xml_find_all(paragraph, "./a:endParaRPr"), 1)
  expect_length(xml2::xml_find_all(paragraph, "./a:r"), 1)
})

test_that("deck_fix_paragraphs stops on an object that is not a deck", {
  expect_error(deck_fix_paragraphs(list()), class = "rlang_error")
})
