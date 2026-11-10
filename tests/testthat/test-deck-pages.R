test_that("deck_subtitle formats a line from the configuration", {
  line <- deck_subtitle("Cases by week", config = test_config())
  run <- line$chunks[[1]]

  expect_s3_class(line, "fpar")
  expect_equal(run$value, "Cases by week")
  expect_equal(run$pr$font.size, 11)
  expect_equal(run$pr$color, "#4A4A48")
  expect_false(run$pr$bold)
})

test_that("deck_subtitle takes its size, colour and typeface as given", {
  line <- deck_subtitle(
    "Cases", size = 20, colour = "#000000", bold = TRUE, align = "right",
    config = deck_config(font = "Georgia")
  )
  run <- line$chunks[[1]]

  expect_equal(run$pr$font.size, 20)
  expect_equal(run$pr$color, "#000000")
  expect_equal(run$pr$font.family, "Georgia")
  expect_true(run$pr$bold)
  expect_equal(line$fp_p$text.align, "right")
})

test_that("deck_page_subtitle matches the page subtitle settings", {
  run <- deck_page_subtitle("Cases", test_config())$chunks[[1]]

  expect_equal(run$pr$font.size, 14)
  expect_equal(run$pr$color, "#366092")
  expect_true(run$pr$bold)
})

test_that("deck_title_slide writes the title, date line and footer", {
  config <- test_config()
  deck <- deck_open(config = config) |>
    deck_title_slide(date = as.Date("2025-03-09"), config = config)

  text <- slide_text(deck, 1)

  expect_length(deck, 1)
  expect_true("Weekly review" %in% text)
  expect_true("Testland, report through 9 March 2025" %in% text)
  expect_true("Compiled by the data team" %in% text)
})

test_that("deck_title_slide takes its wording from its arguments", {
  config <- test_config(cover_footer = NULL)
  deck <- deck_open(config = config) |>
    deck_title_slide(
      title = "Annex", subtitle = "Supplementary tables",
      corner = "Draft",
      corner_box = list(left = 0.5, top = 0.5, width = 2, height = 0.3),
      config = config
    )

  text <- slide_text(deck, 1)

  expect_setequal(text, c("Annex", "Supplementary tables", "Draft"))
})

test_that("deck_title_slide needs a title and a box for the corner text", {
  config <- test_config(title = NULL)
  deck <- deck_open(config = config)

  expect_error(deck_title_slide(deck, config = config), "title")
  expect_error(
    deck_title_slide(deck, title = "Annex", corner = "Draft", config = config),
    "corner_box"
  )
})

test_that("deck_section_number counts top-level sections only", {
  config <- test_config()

  expect_equal(deck_section_number("epi", config = config), 1)
  expect_equal(deck_section_number("campaigns", config = config), 2)
  expect_error(deck_section_number("absent", config = config), "Unknown")
})

test_that("deck_section_number reads the dividers a deck already holds", {
  config <- test_config()
  deck <- deck_open(config = config) |>
    deck_section_slide("campaigns", config = config)

  expect_equal(deck_section_number("epi", deck, config), 2)
})

test_that("deck_section_slide numbers top-level sections as they are built", {
  config <- test_config()
  deck <- build_test_deck(config)

  expect_setequal(slide_text(deck, 3), c("Section 1", "Epidemiology"))
  expect_setequal(slide_text(deck, 7), c("Section 2", "Campaigns"))
})

test_that("deck_section_slide gives a subsection a title and no number", {
  deck <- build_test_deck()

  expect_equal(slide_text(deck, 5), "AFP surveillance")
})

test_that("deck_section_slide takes the prefix from the configuration", {
  config <- test_config(section_prefix = "Part")
  deck <- deck_open(config = config) |>
    deck_section_slide("epi", config = config)

  expect_true("Part 1" %in% slide_text(deck, 1))
})

test_that("deck_thanks_slide adds the closing page", {
  config <- test_config()
  deck <- deck_open(config = config) |>
    deck_thanks_slide(config = config) |>
    deck_thanks_slide(title = "Questions", config = config)

  expect_equal(slide_text(deck, 1), "Thank you!")
  expect_equal(slide_text(deck, 2), "Questions")
})

test_that("deck_source_line names sources in order, then clauses and notes", {
  config <- test_config()

  expect_equal(
    deck_source_line(c("survey", "polis"), config = config),
    "Source: Post-campaign survey, POLIS"
  )
  expect_equal(
    deck_source_line(
      c("polis", "births"), as_of = "Last updated: 9 March 2025",
      notes = "two districts pending", config = config
    ),
    paste0(
      "Source: POLIS; births estimated from the census; ",
      "two districts pending. Last updated: 9 March 2025"
    )
  )
})

test_that("deck_source_line takes labels as given and stops on unknown keys", {
  expect_equal(
    deck_source_line("dhis2", labels = c(dhis2 = "DHIS2")),
    "Source: DHIS2"
  )
  expect_error(
    deck_source_line("census", config = test_config()),
    "Unknown page source"
  )
  expect_error(deck_source_line("polis", config = deck_config()), "has no")
})

test_that("deck_page_footer fills the section strip and the source line", {
  deck <- build_test_deck()
  shapes <- officer::slide_summary(officer::on_slide(deck, index = 4))

  expect_equal(shapes$text[shapes$ph_label == "section"], "Epidemiology")
  expect_equal(shapes$text[shapes$ph_label == "page_source"], "Source: POLIS")
})

test_that("deck_page_footer stops when a page names no source", {
  config <- test_config()
  deck <- deck_open(config = config) |>
    officer::add_slide(layout = "two_plots", master = config$master)

  expect_error(deck_page_footer(deck, "epi", config = config), "no source")
})

test_that("deck_page_footer warns when the source line outgrows the strip", {
  config <- test_config(foot_source_max_chars = 10L)
  deck <- deck_open(config = config) |>
    officer::add_slide(layout = "two_plots", master = config$master)

  expect_message(
    deck_page_footer(deck, "epi", sources = "polis", config = config),
    "name fewer sources"
  )
})

test_that("deck_page_footer takes a label in place of the section's", {
  config <- test_config()
  deck <- deck_open(config = config) |>
    officer::add_slide(layout = "two_plots", master = config$master) |>
    deck_page_footer(
      "epi", label = "All sections", sources = "polis", config = config
    )

  expect_true("All sections" %in% slide_text(deck, 1))
})
