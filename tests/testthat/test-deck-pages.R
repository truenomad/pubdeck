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

test_that("deck_subtitle sets a weight in place of the bold flag", {
  semibold <- deck_subtitle("Cases", weight = "semibold")$chunks[[1]]
  named <- deck_subtitle(
    "Cases", weight = "semibold", config = deck_config(use_semibold = TRUE)
  )$chunks[[1]]
  regular <- deck_subtitle("Cases", bold = TRUE, weight = "regular")
  run <- regular$chunks[[1]]

  expect_true(semibold$pr$bold)
  expect_equal(semibold$pr$font.family, "Arial")
  expect_false(named$pr$bold)
  expect_equal(named$pr$font.family, "Source Sans 3 Semibold")
  expect_false(run$pr$bold)
  expect_error(deck_subtitle("Cases", weight = "heavy"))
})

test_that("deck_subtitle gives the same line for a weight as for the flag", {
  expect_identical(
    deck_subtitle("Cases", weight = "bold"), deck_subtitle("Cases", bold = TRUE)
  )
  expect_identical(
    deck_subtitle("Cases", 9, "#000000", "right", weight = "regular"),
    deck_subtitle("Cases", 9, "#000000", "right", FALSE)
  )
})

test_that("deck_section_slide leaves the number off when told to", {
  config <- test_config()
  deck <- deck_open(config = config) |>
    deck_section_slide("epi", config = config, numbered = FALSE) |>
    deck_section_slide(
      "campaigns", config = test_config(section_numbered = FALSE)
    ) |>
    deck_section_slide("campaigns", config = config)

  expect_equal(slide_text(deck, 1), "Epidemiology")
  expect_equal(slide_layout(deck, 1), "section_page")
  expect_equal(slide_text(deck, 2), "Campaigns")
  expect_setequal(slide_text(deck, 3), c("Section 3", "Campaigns"))
  expect_equal(slide_layout(deck, 3), "section_page_main")
})

test_that("deck_section_slide takes the layout it is given", {
  config <- test_config()
  deck <- deck_open(config = config) |>
    deck_section_slide(
      "epi", config = config, numbered = FALSE, layout = "section_page_main"
    ) |>
    deck_section_slide("afp", config = config, layout = "section_page_main")

  expect_equal(slide_layout(deck, 1), "section_page_main")
  expect_equal(slide_text(deck, 1), "Epidemiology")
  expect_equal(slide_layout(deck, 2), "section_page_main")
  expect_equal(slide_text(deck, 2), "AFP surveillance")
})

test_that("deck_section_slide numbers a table that has no parent column", {
  config <- test_config(
    sections = dplyr::select(test_sections(), "key", "label")
  )
  deck <- deck_open(config = config) |>
    deck_section_slide("epi", config = config) |>
    deck_section_slide("afp", config = config)

  expect_setequal(slide_text(deck, 2), c("Section 2", "AFP surveillance"))
})

test_that("deck_page_footer takes a framing page that names no source", {
  config <- test_config()
  deck <- deck_open(config = config) |>
    officer::add_slide(layout = "two_plots", master = config$master) |>
    deck_page_footer("epi", config = config, framing = TRUE)
  shapes <- slide_shapes(deck)

  expect_equal(shapes$text[shapes$ph_label == "section"], "Epidemiology")
  expect_false("page_source" %in% shapes$ph_label)
})

test_that("deck_page_footer writes a framing page's sources when it has any", {
  config <- test_config()
  deck <- blank_page(config) |>
    deck_page_footer(
      "epi", sources = "polis", config = config, framing = TRUE
    )

  expect_equal(shape_text(deck, "page_source"), "Source: POLIS")
})

test_that("deck_page_footer draws the section label in a box it is given", {
  box <- list(left = 0.1, top = 7.2, width = 2.5, height = 0.3)
  config <- test_config(
    foot_section_box = box, foot_section_pt = 9,
    foot_section_colour = "#112233", foot_section_weight = "bold"
  )
  deck <- blank_page(config) |>
    deck_page_footer("epi", sources = "polis", config = config)
  placed <- shape_box(deck, "section")
  xml <- shape_xml(deck, "section")

  expect_equal(shape_text(deck, "section"), "Epidemiology")
  expect_equal(c(placed$left, placed$top, placed$width), c(0.1, 7.2, 2.5))
  expect_match(xml, "sz=\"900\"", fixed = TRUE)
  expect_match(xml, "112233", fixed = TRUE)
  expect_match(xml, "b=\"1\"", fixed = TRUE)
  expect_no_match(xml, "type=\"ftr\"", fixed = TRUE)
})

test_that("deck_page_footer takes the section box for one call", {
  config <- test_config()
  box <- list(left = 0.5, top = 7, width = 3, height = 0.3)
  deck <- blank_page(config) |>
    deck_page_footer(
      "epi", config = config, framing = TRUE, section_box = box
    )
  placed <- shape_box(deck, "section")

  expect_equal(c(placed$left, placed$top), c(0.5, 7))
  expect_match(shape_xml(deck, "section"), "sz=\"1200\"", fixed = TRUE)
  expect_match(shape_xml(deck, "section"), "b=\"0\"", fixed = TRUE)
  expect_error(
    deck_page_footer(
      deck, "absent", sources = "polis", config = config, section_box = box
    ),
    "Unknown deck section"
  )
})

test_that("deck_page_footer sets the source line at the weight it is given", {
  config <- test_config()
  bold <- blank_page(config) |>
    deck_page_footer("epi", sources = "polis", config = config)
  regular <- blank_page(config) |>
    deck_page_footer(
      "epi", sources = "polis", config = config, source_weight = "regular"
    )
  light <- test_config(foot_source_weight = "regular")
  from_config <- blank_page(light) |>
    deck_page_footer("epi", sources = "polis", config = light)

  expect_match(shape_xml(bold, "page_source"), "b=\"1\"", fixed = TRUE)
  expect_match(shape_xml(regular, "page_source"), "b=\"0\"", fixed = TRUE)
  expect_match(shape_xml(from_config, "page_source"), "b=\"0\"", fixed = TRUE)
})

test_that("deck_page_footer takes settings for one call", {
  config <- test_config()
  box <- list(left = 0.5, top = 7, width = 3, height = 0.3)
  deck <- blank_page(config) |>
    deck_page_footer(
      "epi", sources = "polis", config = config, section_box = box,
      foot_section_pt = 20, foot_section_colour = "#112233",
      foot_source_pt = 15, source_lead = "From: "
    )

  expect_match(shape_xml(deck, "section"), "sz=\"2000\"", fixed = TRUE)
  expect_match(shape_xml(deck, "section"), "112233", fixed = TRUE)
  expect_match(shape_xml(deck, "page_source"), "sz=\"1500\"", fixed = TRUE)
  expect_equal(shape_text(deck, "page_source"), "From: POLIS")
  expect_error(
    deck_page_footer(
      deck, "epi", sources = "polis", config = config, foot_size = 20
    ),
    "Unknown setting"
  )
})
