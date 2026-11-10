test_that("deck_fp_text sets the size, colour and typeface of a run", {
  run <- deck_fp_text(12, "#1F497D", config = test_config())

  expect_s3_class(run, "fp_text")
  expect_equal(run$font.size, 12)
  expect_equal(run$color, "#1F497D")
  expect_equal(run$font.family, "Arial")
  expect_false(run$bold)
})

test_that("deck_fp_text sets bold with the flag and rejects other weights", {
  expect_true(deck_fp_text(12, "#000000", "bold")$bold)
  expect_false(deck_fp_text(12, "#000000", "regular")$bold)
  expect_error(deck_fp_text(12, "#000000", "heavy"))
})

test_that("deck_fp_text sets semibold in bold unless the family is used", {
  stand_in <- deck_fp_text(12, "#000000", "semibold")
  named <- deck_fp_text(12, "#000000", "semibold", use_semibold = TRUE)

  expect_true(stand_in$bold)
  expect_equal(stand_in$font.family, "Arial")
  expect_false(named$bold)
  expect_equal(named$font.family, "Source Sans 3 Semibold")
})

test_that("deck_fp_text takes its families as given and from the config", {
  config <- deck_config(
    font = "Georgia", font_semibold = "Georgia Semibold", use_semibold = TRUE
  )
  family <- function(...) {
    deck_fp_text(10, "#000000", ..., config = config)$font.family
  }

  expect_equal(family(), "Georgia")
  expect_equal(family("semibold"), "Georgia Semibold")
  expect_equal(family(font = "Verdana"), "Verdana")
  expect_equal(
    family("semibold", semibold_font = "Verdana Semibold"),
    "Verdana Semibold"
  )
  expect_true(
    deck_fp_text(
      10, "#000000", "semibold", use_semibold = FALSE, config = config
    )$bold
  )
})

test_that("deck_fp_text follows the configuration in force", {
  family <- with_deck_config(
    deck_config(font = "Georgia"), deck_fp_text(10, "#000000")$font.family
  )

  expect_equal(family, "Georgia")
})

test_that("deck_flex_semibold sets rows in bold or in the semibold family", {
  table <- flextable::flextable(head(mtcars[, 1:2], 3))

  bold <- table_runs(deck_flex_semibold(table, i = 1))
  named <- table_runs(deck_flex_semibold(table, i = 2, use_semibold = TRUE))
  other <- table_runs(deck_flex_semibold(
    table, i = 2, semibold_font = "Georgia Semibold", use_semibold = TRUE
  ))
  from_config <- table_runs(deck_flex_semibold(
    table, i = 3,
    config = deck_config(use_semibold = TRUE, font_semibold = "Verdana")
  ))

  expect_equal(bold$bold, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_false(any(named$bold))
  expect_equal(
    unique(named$font.family[named$.row_id == 2]), "Source Sans 3 Semibold"
  )
  expect_equal(
    unique(other$font.family[other$.row_id == 2]), "Georgia Semibold"
  )
  expect_equal(
    unique(from_config$font.family[from_config$.row_id == 3]), "Verdana"
  )
})

test_that("deck_sized formats a heading in the page title colour", {
  line <- deck_sized("Cases by week", 26)
  run <- line$chunks[[1]]

  expect_s3_class(line, "fpar")
  expect_equal(run$value, "Cases by week")
  expect_equal(run$pr$font.size, 26)
  expect_equal(run$pr$color, "#156082")
  expect_true(run$pr$bold)
})

test_that("deck_sized takes its weight and colour as given", {
  plain <- deck_sized("Cases", 18, "regular", colour = "#000000")$chunks[[1]]
  from_config <- deck_sized(
    "Cases", 18, config = deck_config(page_title_colour = "#112233")
  )$chunks[[1]]

  expect_false(plain$pr$bold)
  expect_equal(plain$pr$color, "#000000")
  expect_equal(from_config$pr$color, "#112233")
})

test_that("deck_marked sets the phrases between marks in another weight", {
  line <- deck_marked("Cases fell **after** the campaign", 18)
  bold <- purrr::map_lgl(line$chunks, \(run) run$pr$bold)
  text <- purrr::map_chr(line$chunks, \(run) run$value)

  expect_equal(text, c("Cases fell ", "after", " the campaign"))
  expect_equal(bold, c(FALSE, TRUE, FALSE))
  expect_equal(line$chunks[[1]]$pr$color, "#366092")
})

test_that("deck_marked takes its colour, weights and mark as given", {
  line <- deck_marked(
    "Cases __fell__", 14, colour = "#000000", weight = "regular",
    base_weight = "bold", mark = "__"
  )
  from_config <- deck_marked(
    "Cases ~fell~", 14,
    config = deck_config(emphasis_mark = "~", page_subtitle_colour = "#112233")
  )

  expect_equal(purrr::map_lgl(line$chunks, \(run) run$pr$bold), c(TRUE, FALSE))
  expect_equal(line$chunks[[1]]$pr$color, "#000000")
  expect_equal(from_config$chunks[[2]]$value, "fell")
  expect_equal(from_config$chunks[[2]]$pr$color, "#112233")
})

test_that("deck_check_heading is silent for a heading that fits", {
  expect_silent(deck_check_heading("Cases by week"))
  expect_silent(deck_check_heading("Cases by **week**", "subtitle"))
  expect_equal(deck_check_heading("Cases by week"), "Cases by week")
})

test_that("deck_check_heading reports a heading past its character limit", {
  long <- strrep("Cases by week of onset ", 4)

  expect_message(deck_check_heading(long), "Title may wrap")
  expect_message(deck_check_heading(long, "subtitle"), "Subtitle may wrap")
  expect_error(deck_check_heading("Cases", "footer"))
})

test_that("deck_check_heading does not count the emphasis marks", {
  marked <- "**Cases** by **week**"
  config <- deck_config(page_subtitle_max_chars = 14L)

  expect_silent(deck_check_heading(marked, "subtitle", config = config))
  expect_message(
    deck_check_heading(paste0(marked, "ly"), "subtitle", config = config),
    "15 characters"
  )
})

test_that("deck_check_heading takes its limits for one call", {
  expect_message(
    deck_check_heading("Cases by week", page_title_max_chars = 5L),
    "may wrap"
  )
  expect_message(
    deck_check_heading(
      "Cases by week of onset",
      page_title_box = list(left = 0, top = 0, width = 1.5, height = 0.5)
    ),
    "may wrap"
  )
  expect_message(
    deck_check_heading("Cases by week of onset", heading_fill = 0.01),
    "may wrap"
  )
  expect_error(
    deck_check_heading("Cases", page_title_chars = 5L), "Unknown setting"
  )
})

test_that("deck_title_pt returns the page title size after the check", {
  expect_equal(deck_title_pt("Cases by week"), 26)
  expect_equal(deck_title_pt("Cases by week", page_title_pt = 30), 30)
  expect_equal(
    deck_title_pt("Cases", config = deck_config(page_title_pt = 22)), 22
  )
  expect_message(deck_title_pt(strrep("Cases by week ", 6)), "may wrap")
})

test_that("deck_text_box places a filled, labelled box of text", {
  deck <- blank_page() |>
    deck_text_box(test_box(), "Figure pending")
  box <- shape_box(deck, "cover")
  xml <- shape_xml(deck, "cover")

  expect_equal(shape_text(deck, "cover"), "Figure pending")
  expect_equal(c(box$left, box$top, box$width), c(1, 2, 4))
  expect_match(xml, "FFFFFF", fixed = TRUE)
  expect_match(xml, "sz=\"1200\"", fixed = TRUE)
  expect_match(xml, "algn=\"ctr\"", fixed = TRUE)
})

test_that("deck_text_box takes its fill, ink, size and label as given", {
  deck <- blank_page() |>
    deck_text_box(
      test_box(), "Pending", fill = "#F2F5F7", colour = "#112233",
      size = 9, label = "pending", align = "left", valign = "top"
    )
  xml <- shape_xml(deck, "pending")

  expect_match(xml, "F2F5F7", fixed = TRUE)
  expect_match(xml, "112233", fixed = TRUE)
  expect_match(xml, "sz=\"900\"", fixed = TRUE)
  expect_match(xml, "algn=\"l\"", fixed = TRUE)
  expect_match(xml, "anchor=\"t\"", fixed = TRUE)
})

test_that("deck_text_box reads the configuration and settings for one call", {
  config <- test_config(
    text_box_fill = "#ABCDEF", text_box_colour = "#123456", text_box_pt = 20,
    shape_labels = list(text_box = "band")
  )
  deck <- blank_page(config) |>
    deck_text_box(test_box(), "From config", config = config) |>
    deck_text_box(
      test_box(top = 4), "One call", label = "once", font = "Georgia",
      config = config
    )

  expect_match(shape_xml(deck, "band"), "ABCDEF", fixed = TRUE)
  expect_match(shape_xml(deck, "band"), "123456", fixed = TRUE)
  expect_match(shape_xml(deck, "band"), "sz=\"2000\"", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "Georgia", fixed = TRUE)
  expect_error(
    deck_text_box(deck, test_box(), "x", fount = "Georgia", config = config),
    "Unknown setting"
  )
})

test_that("deck_message places one line in the heading colour", {
  deck <- blank_page() |>
    deck_message(test_box(), "Coverage fell in two rounds.")
  xml <- shape_xml(deck, "message")

  expect_equal(shape_text(deck, "message"), "Coverage fell in two rounds.")
  expect_match(xml, "1F497D", fixed = TRUE)
  expect_match(xml, "sz=\"1600\"", fixed = TRUE)
  expect_match(xml, "b=\"1\"", fixed = TRUE)
})

test_that("deck_message takes its label, size, colour and weight", {
  config <- test_config(message_pt = 20, heading_colour = "#445566")
  deck <- blank_page(config) |>
    deck_message(test_box(), "From config", config = config) |>
    deck_message(
      test_box(top = 4), "As given", label = "closing", size = 11,
      colour = "#000000", weight = "regular", config = config
    ) |>
    deck_message(
      test_box(top = 5), "One call", label = "once", message_pt = 30,
      config = config
    )

  expect_match(shape_xml(deck, "message"), "sz=\"2000\"", fixed = TRUE)
  expect_match(shape_xml(deck, "message"), "445566", fixed = TRUE)
  expect_match(shape_xml(deck, "closing"), "sz=\"1100\"", fixed = TRUE)
  expect_match(shape_xml(deck, "closing"), "b=\"0\"", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "sz=\"3000\"", fixed = TRUE)
})
