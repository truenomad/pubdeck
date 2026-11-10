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

test_that("deck_config holds the settings of the page builders", {
  config <- deck_config()

  expect_equal(config$layouts$text_page, "section2_overview_maps")
  expect_equal(config$labels$page_subtitle, "page_subtitle")
  expect_equal(config$labels$cover_author, "author")
  expect_equal(config$shape_labels$bullets, "bullets")
  expect_equal(config$font_semibold, "Source Sans 3 Semibold")
  expect_equal(config$page_title_pt, 26)
  expect_equal(config$panel_padding$left, 8)
  expect_equal(config$contents_fill$row_height, 0.22)
  expect_equal(config$bullet_mark, "\u2022\t")
  expect_null(config$path_root)
})

test_that("settings added to earlier builders keep their behaviour", {
  config <- deck_config()

  expect_false(config$theme_frame)
  expect_equal(config$theme_family, "")
  expect_equal(config$theme_title_position, "plot")
  expect_equal(config$theme_plot_margin, list(t = 8, r = 10, b = 6, l = 8))
  expect_equal(config$theme_panel_margin, list(t = 4, r = 6, b = 2, l = 4))
  expect_true(config$save_grob)
  expect_equal(config$map_dpi, 200)
  expect_equal(config$foot_source_weight, "bold")
  expect_null(config$foot_section_box)
  expect_true(config$section_numbered)
  expect_false(config$use_semibold)
})

test_that("deck_config merges one field of the builders' nested settings", {
  config <- deck_config(
    panel_padding = list(left = 2),
    theme_plot_margin = list(t = 20),
    hrule_geometry = list(pt = 2),
    shape_labels = list(kpi = "tile"),
    contents_fill = list(gap = 1)
  )

  expect_equal(
    config$panel_padding, list(left = 2, right = 8, top = 3, bottom = 3)
  )
  expect_equal(config$theme_plot_margin$t, 20)
  expect_equal(config$theme_plot_margin$r, 10)
  expect_equal(config$hrule_geometry$row_height, 0.02)
  expect_equal(config$shape_labels$table, "table")
  expect_equal(config$contents_fill$row_height, 0.22)
  expect_error(deck_config(kpi_padding = list(inner = 1)), "Unknown field")
})

test_that("a box setting with no default takes the box it is given", {
  box <- list(left = 0.1, top = 7.2, width = 2.5, height = 0.3)

  expect_equal(deck_config(foot_section_box = box)$foot_section_box, box)
})

test_that("a sections table needs no parent column", {
  config <- deck_config(
    sections = tibble::tibble(key = c("a", "b"), label = c("A", "B"))
  )

  expect_equal(deck_section_number("b", config = config), 2)
  expect_equal(.sections(config)$parent, c(NA_character_, NA_character_))
})

test_that("settings passed for one call replace those of the configuration", {
  config <- deck_config(title = "Weekly review", panel_pt = 12)
  once <- .call_config(
    config, panel_pt = 20, panel_padding = list(top = 9)
  )

  expect_identical(.call_config(config), config)
  expect_s3_class(once, "pubdeck_config")
  expect_equal(once$panel_pt, 20)
  expect_equal(once$panel_padding$top, 9)
  expect_equal(once$panel_padding$left, 8)
  expect_equal(once$title, "Weekly review")
  expect_equal(config$panel_pt, 12)
  expect_error(.call_config(config, panel_size = 20), "Unknown setting")
  expect_error(.call_config(config, 20), "must be named")
  expect_error(.call_config(list(), panel_pt = 20), "deck_config")
})

test_that("a nested setting given for one call is merged into the default", {
  config <- deck_config()

  expect_equal(.nested(NULL, "kpi_padding", config), config$kpi_padding)
  expect_equal(
    .nested(list(top = 4), "kpi_padding", config),
    list(left = 0, right = 0, top = 4, bottom = 1)
  )
  expect_error(.nested(list(inner = 4), "kpi_padding", config), "Unknown field")
})
