test_that("deck_panel places a fill, a heading and marked lines", {
  deck <- blank_page() |>
    deck_panel(
      test_box(), "What changed",
      c("Coverage rose", "\u2192 see page 4", "In detail:", "")
    )
  shapes <- slide_shapes(deck)
  xml <- shape_xml(deck, "panel")

  expect_setequal(shapes$ph_label, c("panel_bg", "panel"))
  expect_equal(
    shape_text(deck, "panel"),
    "What changed\u25aa Coverage rose\u2192 see page 4In detail:"
  )
  expect_match(shape_xml(deck, "panel_bg"), "F2F5F7", fixed = TRUE)
  expect_match(xml, "sz=\"1800\"", fixed = TRUE)
  expect_match(xml, "sz=\"1600\"", fixed = TRUE)
  expect_match(xml, "1F497D", fixed = TRUE)
  expect_match(xml, "4A4A48", fixed = TRUE)
})

test_that("deck_panel takes its fill, size and label as given", {
  deck <- blank_page() |>
    deck_panel(
      test_box(), "Heading", "One line", fill = "#ABCDEF", size = 10,
      label = "left_panel"
    )

  expect_setequal(
    slide_shapes(deck)$ph_label, c("left_panel_bg", "left_panel")
  )
  expect_match(shape_xml(deck, "left_panel_bg"), "ABCDEF", fixed = TRUE)
  expect_match(shape_xml(deck, "left_panel"), "sz=\"1000\"", fixed = TRUE)
  expect_match(shape_xml(deck, "left_panel"), "sz=\"1200\"", fixed = TRUE)
})

test_that("deck_panel reads the configuration and settings for one call", {
  config <- test_config(
    panel_fill = "#101010", panel_pt = 12, panel_marker = "- ",
    heading_colour = "#202020", body_colour = "#303030",
    shape_labels = list(panel = "card", panel_fill_suffix = "_fill")
  )
  deck <- blank_page(config) |>
    deck_panel(test_box(), "Heading", "One line", config = config) |>
    deck_panel(
      test_box(left = 6), "Heading", c("One line", "Stands alone"),
      label = "once", heading_step_pt = 8, plain_line_pattern = "^Stands",
      panel_padding = list(left = 20), config = config
    )

  expect_true(all(c("card", "card_fill") %in% slide_shapes(deck)$ph_label))
  expect_equal(shape_text(deck, "card"), "Heading- One line")
  expect_match(shape_xml(deck, "card_fill"), "101010", fixed = TRUE)
  expect_match(shape_xml(deck, "card"), "202020", fixed = TRUE)
  expect_match(shape_xml(deck, "card"), "303030", fixed = TRUE)
  expect_equal(shape_text(deck, "once"), "Heading- One lineStands alone")
  expect_match(shape_xml(deck, "once"), "sz=\"2000\"", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "marL=\"254000\"", fixed = TRUE)
  expect_error(
    deck_panel(deck, test_box(), "H", "l", panel_size = 3, config = config),
    "Unknown setting"
  )
})

test_that("deck_bullets places headings, bullets and sub-bullets", {
  blocks <- list(
    list(
      heading = "Coverage",
      lines = c("Rose in **two** regions", "- by 4 points", "In detail:")
    ),
    list(lines = "Two districts were missed")
  )
  deck <- blank_page() |>
    deck_bullets(test_box(left = 1, width = 6, height = 3), blocks)
  box <- shape_box(deck, "bullets")
  xml <- shape_xml(deck, "bullets")

  expect_equal(c(box$left, box$width), c(0.75, 6.25))
  expect_equal(
    shape_text(deck, "bullets"),
    paste0(
      "CoverageRose in two regionsby 4 pointsIn detail: ",
      "Two districts were missed"
    )
  )
  expect_equal(count_matches(xml, "<a:buChar"), 3)
  expect_match(xml, "char=\"\u2022\"", fixed = TRUE)
  expect_match(xml, "char=\"\u2013\"", fixed = TRUE)
  expect_match(xml, "marL=\"228600\" marR=\"0\" indent=\"-228600\"")
  expect_match(xml, "marL=\"502920\" marR=\"0\" indent=\"-164592\"")
  expect_match(xml, "marL=\"228600\" marR=\"0\" indent=\"0\"")
  expect_no_match(xml, "\t", fixed = TRUE)
  expect_match(xml, "6B6B6B", fixed = TRUE)
  expect_match(xml, "sz=\"1500\"", fixed = TRUE)
  expect_match(xml, "sz=\"800\"", fixed = TRUE)
})

test_that("deck_bullets sets marked text and headings in the emphasis", {
  blocks <- list(list(heading = "Head", lines = "Plain **marked** plain"))
  bold <- function(deck) {
    xml <- shape_xml(deck, "bullets")
    regmatches(xml, gregexpr(" b=\"[01]\"", xml))[[1]]
  }

  strong <- blank_page() |>
    deck_bullets(test_box(), blocks)
  light <- blank_page() |>
    deck_bullets(test_box(), blocks, emphasis = "regular")
  named <- blank_page() |>
    deck_bullets(test_box(), blocks, use_semibold = TRUE)

  expect_equal(bold(strong), c(" b=\"1\"", " b=\"0\"", " b=\"1\"", " b=\"0\""))
  expect_equal(unique(bold(light)), " b=\"0\"")
  expect_equal(unique(bold(named)), " b=\"0\"")
  expect_match(
    shape_xml(named, "bullets"), "Source Sans 3 Semibold", fixed = TRUE
  )
})

test_that("deck_bullets takes its size, spacing, groups and label", {
  blocks <- list(list(lines = c("One", "Two", "Three")))
  deck <- blank_page() |>
    deck_bullets(
      test_box(), blocks, size = 10, space_after = 6, gap_before = 3,
      gap_pt = 14, label = "notes"
    )
  xml <- shape_xml(deck, "notes")

  expect_match(xml, "sz=\"1000\"", fixed = TRUE)
  expect_equal(count_matches(xml, "<a:spcPts val=\"600\"/>"), 3)
  expect_equal(count_matches(xml, "<a:spcPts val=\"1400\"/>"), 1)
})

test_that("deck_bullets counts gap_before across blocks", {
  blocks <- list(list(lines = c("One", "Two")), list(lines = "Three"))
  deck <- blank_page() |>
    deck_bullets(test_box(), blocks, gap_before = 3, gap_pt = 14)
  xml <- shape_xml(deck, "bullets")
  paragraphs <- strsplit(xml, "<a:p>", fixed = TRUE)[[1]]
  spaced <- grepl("<a:spcPts val=\"1400\"/>", paragraphs, fixed = TRUE)

  expect_equal(sum(spaced), 1)
  expect_match(paragraphs[spaced], "Three", fixed = TRUE)
})

test_that("deck_bullets reads the configuration and settings for one call", {
  config <- test_config(
    bullet_pt = 12, bullet_space_after = 5, bullet_group_gap = 9,
    bullet_hang = 0.5, bullet_char = "*", sub_char = "+", sub_prefix = "> ",
    sub_indent = 0.5, sub_hang = 0.25, sub_pt_drop = 2,
    muted_colour = "#101010", body_colour = "#202020",
    shape_labels = list(bullets = "points")
  )
  blocks <- list(list(heading = "Head", lines = c("One", "> under one")))
  deck <- blank_page(config) |>
    deck_bullets(
      test_box(left = 2), blocks, gap_before = 1, config = config
    ) |>
    deck_bullets(
      test_box(left = 2, top = 4), blocks, label = "once",
      heading_step_pt = 6, bullet_block_gap = 1, heading_colour = "#303030",
      config = config
    )
  xml <- shape_xml(deck, "points")

  expect_equal(shape_box(deck, "points")$left, 1.5)
  expect_match(xml, "sz=\"1200\"", fixed = TRUE)
  expect_match(xml, "sz=\"1000\"", fixed = TRUE)
  expect_match(xml, "char=\"*\"", fixed = TRUE)
  expect_match(xml, "char=\"+\"", fixed = TRUE)
  expect_match(xml, "marL=\"457200\" marR=\"0\" indent=\"-457200\"")
  expect_match(xml, "marL=\"914400\" marR=\"0\" indent=\"-228600\"")
  expect_match(xml, "<a:spcPts val=\"900\"/>", fixed = TRUE)
  expect_match(xml, "<a:spcPts val=\"500\"/>", fixed = TRUE)
  expect_match(xml, "101010", fixed = TRUE)
  expect_match(xml, "202020", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "sz=\"1800\"", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "303030", fixed = TRUE)
})

test_that("deck_hang_bullets swaps marks for bullets in a text shape", {
  marked <- officer::block_list(
    officer::fpar(officer::ftext("\u2022\tA bullet")),
    officer::fpar(officer::ftext("\u2013\tA sub-bullet")),
    officer::fpar(officer::ftext("A plain line"))
  )
  deck <- blank_page() |>
    officer::ph_with(marked, location = deck_box_location(test_box(), "notes"))

  before <- shape_xml(deck, "notes")
  deck <- deck_hang_bullets(deck, "notes")
  after <- shape_xml(deck, "notes")

  expect_no_match(before, "buChar", fixed = TRUE)
  expect_equal(count_matches(after, "<a:buChar"), 2)
  expect_equal(count_matches(after, "<a:buNone"), 1)
  expect_match(after, "<a:t>A bullet</a:t>", fixed = TRUE)
  expect_match(after, "<a:t>A sub-bullet</a:t>", fixed = TRUE)
  expect_match(after, "typeface=\"Arial\"", fixed = TRUE)
})

test_that("deck_hang_bullets takes its marks and hang for one call", {
  marked <- officer::block_list(
    officer::fpar(officer::ftext("@@A bullet")),
    officer::fpar(officer::ftext("\u2022\tLeft as written"))
  )
  deck <- blank_page() |>
    officer::ph_with(
      marked, location = deck_box_location(test_box(), "notes")
    ) |>
    deck_hang_bullets(
      "notes", bullet_mark = "@@", bullet_char = "o", bullet_hang = 1,
      font = "Georgia"
    )
  xml <- shape_xml(deck, "notes")

  expect_match(xml, "<a:t>A bullet</a:t>", fixed = TRUE)
  expect_match(xml, "char=\"o\"", fixed = TRUE)
  expect_match(xml, "indent=\"-914400\"", fixed = TRUE)
  expect_match(xml, "<a:buFont typeface=\"Georgia\"/>", fixed = TRUE)
  expect_equal(count_matches(xml, "<a:buChar"), 1)
})

test_that("deck_hang_bullets stops when the slide has no such shape", {
  expect_error(deck_hang_bullets(blank_page(), "absent"), "No text shape")
})

test_that("deck_callout places a label over a sentence in a band", {
  deck <- blank_page() |>
    deck_callout(test_box(), "The question", "How much remains?")
  xml <- shape_xml(deck, "callout")

  expect_equal(shape_text(deck, "callout"), "The questionHow much remains?")
  expect_match(xml, "EAF4FB", fixed = TRUE)
  expect_match(xml, "009ADE", fixed = TRUE)
  expect_match(xml, "0C2A5E", fixed = TRUE)
  expect_match(xml, "sz=\"1200\"", fixed = TRUE)
  expect_match(xml, "sz=\"1700\"", fixed = TRUE)
})

test_that("deck_callout takes its label, size and colours as given", {
  deck <- blank_page() |>
    deck_callout(
      test_box(), "Label", "Sentence", label_name = "question",
      text_size = 11, fill = "#101010", label_colour = "#202020",
      text_colour = "#303030"
    )
  xml <- shape_xml(deck, "question")

  expect_match(xml, "sz=\"1100\"", fixed = TRUE)
  expect_match(xml, "101010", fixed = TRUE)
  expect_match(xml, "202020", fixed = TRUE)
  expect_match(xml, "303030", fixed = TRUE)
})

test_that("deck_callout reads the configuration and settings for one call", {
  config <- test_config(
    callout_fill = "#101010", callout_label_pt = 9, callout_text_pt = 13,
    accent_colour = "#202020", strong_colour = "#303030",
    shape_labels = list(callout = "band")
  )
  deck <- blank_page(config) |>
    deck_callout(test_box(), "Label", "Sentence", config = config) |>
    deck_callout(
      test_box(top = 4), "Label", "Sentence", label_name = "once",
      callout_label_pt = 8, callout_padding = list(left = 30),
      config = config
    )
  xml <- shape_xml(deck, "band")

  expect_match(xml, "101010", fixed = TRUE)
  expect_match(xml, "202020", fixed = TRUE)
  expect_match(xml, "303030", fixed = TRUE)
  expect_match(xml, "sz=\"900\"", fixed = TRUE)
  expect_match(xml, "sz=\"1300\"", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "sz=\"800\"", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "marL=\"381000\"", fixed = TRUE)
})

test_that("deck_rule_note places a label and sentence against a rule", {
  deck <- blank_page() |>
    deck_rule_note(test_box(), "Next", "Where did the campaign reach?")
  xml <- shape_xml(deck, "next")

  expect_equal(
    shape_text(deck, "next"), "NextWhere did the campaign reach?"
  )
  expect_match(xml, "<a:lnL[^>]* w=\"31750\"")
  expect_match(xml, "009ADE", fixed = TRUE)
  expect_match(xml, "0C2A5E", fixed = TRUE)
  expect_match(xml, "sz=\"1000\"", fixed = TRUE)
  expect_match(xml, "sz=\"1800\"", fixed = TRUE)
})

test_that("deck_rule_note takes its settings as given and for one call", {
  config <- test_config(
    rule_label_pt = 8, rule_text_pt = 12, rule_width = 4,
    shape_labels = list(rule_note = "handover")
  )
  deck <- blank_page(config) |>
    deck_rule_note(test_box(), "Next", "A question", config = config) |>
    deck_rule_note(
      test_box(top = 4), "Next", "A question", label_name = "once",
      text_size = 20, accent_colour = "#101010", strong_colour = "#202020",
      rule_padding = list(left = 30), config = config
    )
  xml <- shape_xml(deck, "handover")
  once <- shape_xml(deck, "once")

  expect_match(xml, "<a:lnL[^>]* w=\"50800\"")
  expect_match(xml, "sz=\"800\"", fixed = TRUE)
  expect_match(xml, "sz=\"1200\"", fixed = TRUE)
  expect_match(once, "sz=\"2000\"", fixed = TRUE)
  expect_match(once, "101010", fixed = TRUE)
  expect_match(once, "202020", fixed = TRUE)
  expect_match(once, "marL=\"381000\"", fixed = TRUE)
})

test_that("deck_hrule draws a rule of a stated length", {
  deck <- blank_page() |>
    deck_hrule(left = 1, top = 5, width = 3)
  box <- shape_box(deck, "rule")
  xml <- shape_xml(deck, "rule")

  expect_equal(c(box$left, box$top, box$width, box$height), c(1, 5, 3, 0.05))
  expect_match(xml, "<a:lnT[^>]* w=\"12700\"")
  expect_match(xml, "009ADE", fixed = TRUE)
})

test_that("deck_hrule takes its colour, weight, label and geometry", {
  config <- test_config(
    hrule_weight = 3, accent_colour = "#101010",
    shape_labels = list(hrule = "divider")
  )
  deck <- blank_page(config) |>
    deck_hrule(1, 5, 3, config = config) |>
    deck_hrule(
      1, 6, 3, colour = "#202020", weight = 2, label = "once",
      hrule_geometry = list(box_height = 0.5), config = config
    )

  expect_match(shape_xml(deck, "divider"), "<a:lnT[^>]* w=\"38100\"")
  expect_match(shape_xml(deck, "divider"), "101010", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "<a:lnT[^>]* w=\"25400\"")
  expect_match(shape_xml(deck, "once"), "202020", fixed = TRUE)
  expect_equal(shape_box(deck, "once")$height, 0.5)
})

test_that("deck_ellipse draws an unfilled ring as native shapes", {
  deck <- blank_page() |>
    deck_ellipse(test_box(left = 2, top = 3, width = 1, height = 2))
  xml <- shape_xml(deck, "highlight")

  expect_true("highlight" %in% slide_shapes(deck)$ph_label)
  expect_match(xml, "grpSp", fixed = TRUE)
  expect_match(xml, "009ADE", fixed = TRUE)
  expect_match(xml, "<a:ln w=\"14287\"", fixed = TRUE)
})

test_that("deck_ellipse takes its colour, label, width and geometry", {
  config <- test_config(
    ellipse_lwd = 3, accent_colour = "#101010",
    shape_labels = list(ellipse = "ring")
  )
  deck <- blank_page(config) |>
    deck_ellipse(test_box(), config = config) |>
    deck_ellipse(
      test_box(left = 6), colour = "#FFED19", label = "once",
      ellipse_lwd = 1, ellipse_geometry = list(points = 9L),
      config = config
    )

  expect_match(shape_xml(deck, "ring"), "101010", fixed = TRUE)
  expect_match(shape_xml(deck, "ring"), "<a:ln w=\"28575\"", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "FFED19", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "<a:ln w=\"9525\"", fixed = TRUE)
  expect_lt(
    count_matches(shape_xml(deck, "once"), "<a:lnTo>"),
    count_matches(shape_xml(deck, "ring"), "<a:lnTo>")
  )
})

test_that("deck_kpi places a label, a note, value rows and a change", {
  deck <- blank_page() |>
    deck_kpi(
      test_box(), "Suspected cases", "Weeks 1 to 20",
      c("2024: 120", "2025: 90"), "-25%"
    )
  xml <- shape_xml(deck, "kpi")

  expect_equal(
    shape_text(deck, "kpi"),
    "Suspected casesWeeks 1 to 202024: 1202025: 90-25%"
  )
  expect_match(xml, "sz=\"1200\"", fixed = TRUE)
  expect_match(xml, "sz=\"1100\"", fixed = TRUE)
  expect_match(xml, "sz=\"1500\"", fixed = TRUE)
  expect_match(xml, "sz=\"2600\"", fixed = TRUE)
  expect_match(xml, "009ADE", fixed = TRUE)
  expect_match(xml, "6B6B6B", fixed = TRUE)
  expect_match(xml, "0C2A5E", fixed = TRUE)
})

test_that("deck_kpi places a label and a note alone", {
  deck <- blank_page() |>
    deck_kpi(
      test_box(), "Positivity", "By month", character(), character(),
      kpi_label = "positivity"
    )
  xml <- shape_xml(deck, "positivity")

  expect_equal(shape_text(deck, "positivity"), "PositivityBy month")
  expect_no_match(xml, "sz=\"2600\"", fixed = TRUE)
  expect_no_match(xml, "sz=\"1500\"", fixed = TRUE)
})

test_that("deck_kpi reads the configuration and settings for one call", {
  config <- test_config(
    kpi_label_pt = 8, kpi_note_pt = 7, kpi_row_pt = 9, kpi_change_pt = 30,
    accent_colour = "#101010", muted_colour = "#202020",
    body_colour = "#303030", strong_colour = "#404040",
    shape_labels = list(kpi = "tile")
  )
  deck <- blank_page(config) |>
    deck_kpi(test_box(), "Label", "Note", "Row", "Change", config = config) |>
    deck_kpi(
      test_box(top = 4), "Label", "Note", "Row", "Change",
      kpi_label = "once", kpi_change_pt = 40,
      kpi_padding = list(left = 30), config = config
    )
  xml <- shape_xml(deck, "tile")

  expect_match(xml, "sz=\"800\"", fixed = TRUE)
  expect_match(xml, "sz=\"700\"", fixed = TRUE)
  expect_match(xml, "sz=\"900\"", fixed = TRUE)
  expect_match(xml, "sz=\"3000\"", fixed = TRUE)
  expect_match(xml, "101010", fixed = TRUE)
  expect_match(xml, "202020", fixed = TRUE)
  expect_match(xml, "303030", fixed = TRUE)
  expect_match(xml, "404040", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "sz=\"4000\"", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "marL=\"381000\"", fixed = TRUE)
})

test_that("deck_table places a data frame with a ruled header", {
  data <- tibble::tibble(
    Measure = c("First", "Second"), Value = c("12", "3.4")
  )
  deck <- blank_page() |>
    deck_table(test_box(), data, widths = c(2.5, 1.5))
  xml <- shape_xml(deck, "table")

  expect_equal(shape_text(deck, "table"), "MeasureValueFirst12Second3.4")
  expect_match(xml, "<a:gridCol w=\"2286000\"/>", fixed = TRUE)
  expect_match(xml, "<a:gridCol w=\"1371600\"/>", fixed = TRUE)
  expect_match(xml, "sz=\"1100\"", fixed = TRUE)
  expect_match(xml, "D0D5DB", fixed = TRUE)
  expect_match(xml, "009ADE", fixed = TRUE)
  expect_match(xml, "1F497D", fixed = TRUE)
  expect_match(xml, "4A4A48", fixed = TRUE)
})

test_that("deck_table takes its size, label and settings for one call", {
  data <- tibble::tibble(Measure = "First", Value = "12")
  config <- test_config(
    slide_table_pt = 8, slide_table_rule_colour = "#101010",
    slide_table_rule_width = 2, slide_table_head_rule_width = 3,
    shape_labels = list(table = "grid")
  )
  deck <- blank_page(config) |>
    deck_table(test_box(), data, widths = c(2, 2), config = config) |>
    deck_table(
      test_box(top = 4), data, widths = c(2, 2), size = 14, label = "once",
      slide_table_padding = list(left = 30), heading_colour = "#202020",
      config = config
    )
  xml <- shape_xml(deck, "grid")

  expect_match(xml, "sz=\"800\"", fixed = TRUE)
  expect_match(xml, "101010", fixed = TRUE)
  expect_match(xml, "w=\"25400\"", fixed = TRUE)
  expect_match(xml, "w=\"38100\"", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "sz=\"1400\"", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "202020", fixed = TRUE)
  expect_match(shape_xml(deck, "once"), "marL=\"381000\"", fixed = TRUE)
})
