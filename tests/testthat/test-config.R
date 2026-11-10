test_that("deck_config returns every default when nothing is passed", {
  config <- deck_config()

  expect_s3_class(config, "pubdeck_config")
  expect_null(config$template)
  expect_equal(config$master, "Office Theme")
  expect_equal(config$cover_title_pt, 28)
  expect_equal(config$contents_slide$n_cols, 2L)
})

test_that("deck_config replaces a setting and keeps the rest", {
  config <- deck_config(title = "Weekly review", cover_title_pt = 32)

  expect_equal(config$title, "Weekly review")
  expect_equal(config$cover_title_pt, 32)
  expect_equal(config$cover_date_pt, deck_config()$cover_date_pt)
})

test_that("deck_config merges one field of a nested setting", {
  config <- deck_config(
    contents_slide = list(n_cols = 3L),
    layouts = list(cover = "Title Slide")
  )

  expect_equal(config$contents_slide$n_cols, 3L)
  expect_equal(config$contents_slide$layout, "summary_text")
  expect_equal(config$layouts$cover, "Title Slide")
  expect_equal(config$layouts$section, "section_page_main")
})

test_that("deck_config replaces a table and a vector whole", {
  sections <- test_sections()
  config <- deck_config(
    sections = sections,
    source_labels = c(polis = "POLIS")
  )

  expect_identical(config$sections, sections)
  expect_equal(config$source_labels, c(polis = "POLIS"))
})

test_that("deck_config starts from a base configuration", {
  base <- deck_config(title = "Weekly review", country = "Testland")
  annex <- deck_config(title = "Annex", base = base)

  expect_equal(annex$title, "Annex")
  expect_equal(annex$country, "Testland")
  expect_equal(base$title, "Weekly review")
})

test_that("deck_config keeps a setting that is set to NULL", {
  config <- deck_config(
    cover_footer = NULL,
    base = deck_config(cover_footer = "Compiled by the data team")
  )

  expect_true("cover_footer" %in% names(config))
  expect_null(config$cover_footer)
})

test_that("deck_config rejects unknown and unnamed settings", {
  expect_error(deck_config(tittle = "Weekly review"), "Unknown setting")
  expect_error(deck_config("Weekly review"), "must be named")
  expect_error(
    deck_config(contents_slide = list(columns = 3L)),
    "Unknown field"
  )
  expect_error(deck_config(base = list(title = "x")), "deck_config")
})

test_that("deck_config_set returns the configuration it replaced", {
  first <- deck_config(title = "First")
  second <- deck_config(title = "Second")

  original <- deck_config_set(first)
  withr::defer(deck_config_set(original))

  previous <- deck_config_set(second)

  expect_equal(previous$title, "First")
  expect_equal(deck_config_get()$title, "Second")
  expect_error(deck_config_set(list(title = "x")), "deck_config")
})

test_that("with_deck_config restores the configuration afterwards", {
  before <- deck_config_get()

  inside <- with_deck_config(
    deck_config(title = "Annex"),
    deck_config_get()$title
  )

  expect_equal(inside, "Annex")
  expect_identical(deck_config_get(), before)
})

test_that("with_deck_config restores the configuration after an error", {
  before <- deck_config_get()

  expect_error(
    with_deck_config(deck_config(title = "Annex"), stop("failed")),
    "failed"
  )
  expect_identical(deck_config_get(), before)
})

test_that("a builder stops on a setting that has no default", {
  expect_error(deck_open(config = deck_config()), "has no")
  expect_error(
    deck_section_number("epi", config = deck_config()),
    "sections"
  )
  expect_error(
    deck_section_number(
      "epi",
      config = deck_config(sections = data.frame(key = "epi"))
    ),
    "lacks column"
  )
})
