test_that("deck_contents_entries lists sections and the runs inside them", {
  entries <- deck_contents_entries(build_test_deck(), test_config())

  expect_named(entries, c("kind", "number", "label", "page"))
  expect_equal(entries$kind, c("section", "run", "section"))
  expect_equal(entries$number, c(1L, NA, 2L))
  expect_equal(
    entries$label, c("Epidemiology", "AFP surveillance", "Campaigns")
  )
  expect_equal(entries$page, c(3L, 6L, 7L))
})

test_that("deck_contents_entries leaves out the labels it is told to skip", {
  config <- test_config(contents_skip = "AFP surveillance")
  entries <- deck_contents_entries(build_test_deck(config), config)

  expect_equal(entries$label, c("Epidemiology", "Campaigns"))
})

test_that("deck_contents_entries numbers the sections a partial deck holds", {
  config <- test_config()
  deck <- deck_open(config = config) |>
    deck_section_slide("campaigns", config = config) |>
    add_test_page("Coverage by round", "campaigns", config)

  entries <- deck_contents_entries(deck, config)

  expect_equal(entries$number, 1L)
  expect_equal(entries$label, "Campaigns")
})

test_that("deck_contents_entries stops on a deck with no section divider", {
  config <- test_config()
  deck <- deck_open(config = config) |>
    deck_title_slide(config = config)

  expect_error(deck_contents_entries(deck, config), "No section divider")
})

test_that("deck_contents_layout keeps a section with its runs", {
  entries <- tibble::tibble(
    kind = c("section", "run", "run", "section", "run"),
    number = c(1L, NA, NA, 2L, NA),
    label = c("One", "a", "b", "Two", "c"),
    page = c(3L, 4L, 5L, 6L, 7L)
  )
  box <- list(left = 1, top = 2, width = 8.5, height = 4)
  page <- deck_config()$contents_slide

  rows <- deck_contents_layout(entries, box, config = deck_config())

  expect_equal(nrow(rows), 5)
  expect_named(rows, c(names(entries), "left", "width", "top"))
  expect_equal(rows$top[1], box$top)
  expect_equal(
    diff(rows$top[1:3]),
    c(page$row_height + page$section_pad, page$row_height)
  )
  expect_equal(unique(rows$width), (8.5 - page$gap) / 2)
  expect_true(all(rows$left[1:3] == rows$left[1]))
})

test_that("deck_contents_layout opens a new column at the target depth", {
  entries <- tibble::tibble(
    kind = rep("section", 4),
    number = 1:4,
    label = paste("Section", 1:4),
    page = 3:6
  )
  box <- list(left = 1, top = 2, width = 8.5, height = 4)

  rows <- deck_contents_layout(entries, box, config = deck_config())

  expect_equal(dplyr::n_distinct(rows$left), 2)
  expect_equal(rows$top[1], box$top)
  expect_equal(rows$top[4], box$top)
  expect_gt(rows$left[4], rows$left[1])
})

test_that("deck_contents_layout moves a block that would overflow the box", {
  entries <- tibble::tibble(
    kind = c("section", "run", "run", "section", "run", "run"),
    number = c(1L, NA, NA, 2L, NA, NA),
    label = letters[1:6],
    page = 3:8
  )
  box <- list(left = 1, top = 2, width = 8.5, height = 1)

  rows <- deck_contents_layout(entries, box, config = deck_config())

  expect_equal(rows$top[c(1, 4)], c(box$top, box$top))
  expect_true(all(rows$top + 0.24 <= box$top + box$height + 1e-8))
})

test_that("deck_contents_layout takes the page settings as given", {
  entries <- tibble::tibble(
    kind = "section", number = 1L, label = "One", page = 3L
  )
  box <- list(left = 1, top = 2, width = 9, height = 4)
  page <- deck_config(contents_slide = list(n_cols = 3L, gap = 0))

  rows <- deck_contents_layout(entries, box, page = page$contents_slide)

  expect_equal(rows$width, 3)
})

test_that("deck_contents_layout stops when the entries do not fit", {
  entries <- tibble::tibble(
    kind = rep("section", 30),
    number = 1:30,
    label = paste("Section", 1:30),
    page = 3:32
  )
  box <- list(left = 1, top = 2, width = 8.5, height = 1)

  expect_error(
    deck_contents_layout(entries, box, config = deck_config()),
    "does not fit"
  )
})

test_that("deck_contents_placeholder adds a titled page", {
  deck <- build_test_deck()

  expect_equal(slide_text(deck, 2), "Contents")
})

test_that("deck_contents_slide draws a linked row for every entry", {
  config <- test_config()
  deck <- build_test_deck(config)
  entries <- deck_contents_entries(deck, config)

  deck <- deck_contents_slide(deck, entries, config = config)
  shapes <- officer::slide_summary(officer::on_slide(deck, index = 2))
  labels <- shapes$ph_label

  expect_equal(sum(startsWith(labels, "contents_label_")), 3)
  expect_equal(sum(startsWith(labels, "contents_page_")), 3)
  expect_equal(sum(startsWith(labels, "contents_band_")), 2)
  expect_true("1. Epidemiology" %in% shapes$text)
  expect_true(all(c("3", "6", "7") %in% shapes$text))
})

test_that("deck_contents_slide replaces the rows of an earlier build", {
  config <- test_config()
  deck <- build_test_deck(config)
  entries <- deck_contents_entries(deck, config)

  once <- deck_contents_slide(deck, entries, config = config)
  twice <- deck_contents_slide(once, entries, config = config)

  count_rows <- function(deck) {
    labels <- officer::slide_summary(officer::on_slide(deck, index = 2))
    sum(startsWith(labels$ph_label, "contents_"))
  }

  expect_equal(count_rows(twice), count_rows(once))
  expect_length(twice, 8)
})

test_that("deck_contents_title_entries lists the pages under each divider", {
  entries <- deck_contents_title_entries(
    build_test_deck(), config = test_config()
  )

  expect_named(entries, c("kind", "number", "label", "page"))
  expect_equal(entries$kind, rep(c("section", "run"), 3))
  expect_equal(entries$number, c(1L, NA, 2L, NA, 3L, NA))
  expect_equal(
    entries$label,
    c(
      "Epidemiology", "Detections by week", "AFP surveillance",
      "AFP cases by month", "Campaigns", "Coverage by round"
    )
  )
  expect_equal(entries$page, 3:8)
})

test_that("deck_contents_title_entries collapses and drops sections", {
  config <- test_config()
  deck <- build_test_deck(config)

  collapsed <- deck_contents_title_entries(
    deck, collapsed = "Campaigns", config = config
  )
  dropped <- deck_contents_title_entries(
    deck, dropped = "AFP surveillance", config = config
  )

  expect_equal(nrow(collapsed), 5)
  expect_false("Coverage by round" %in% collapsed$label)
  expect_equal(
    dropped$label,
    c("Epidemiology", "Detections by week", "Campaigns", "Coverage by round")
  )
  expect_equal(dropped$number, c(1L, NA, 3L, NA))
})

test_that("deck_contents_title_entries lists a page as its own section", {
  config <- test_config()
  entries <- deck_contents_title_entries(
    build_test_deck(config), standalone = "Detections by week",
    config = config
  )

  expect_equal(entries$kind[1:3], c("section", "section", "section"))
  expect_equal(entries$label[2], "Detections by week")
  expect_equal(entries$number[1:3], 1:3)
  expect_equal(entries$page[2], 4L)
})

test_that("deck_contents_title_entries reads its lists from the config", {
  config <- test_config(
    contents_collapsed = "Epidemiology", contents_dropped = "Campaigns",
    contents_standalone = "Contents"
  )
  entries <- deck_contents_title_entries(
    build_test_deck(config), config = config
  )

  expect_equal(
    entries$label,
    c("Contents", "Epidemiology", "AFP surveillance", "AFP cases by month")
  )
  expect_equal(entries$page, c(2L, 3L, 5L, 6L))
})

test_that("deck_contents_title_entries stops on a deck with no divider", {
  config <- test_config()
  deck <- deck_open(config = config) |>
    deck_title_slide(config = config)

  expect_error(
    deck_contents_title_entries(deck, config = config), "No section divider"
  )
})

test_that("deck_contents_fill draws a linked line for every entry", {
  config <- test_config()
  deck <- build_test_deck(config) |>
    deck_contents_fill(config = config)
  shapes <- slide_shapes(deck)
  labels <- shapes$ph_label

  expect_length(deck, 8)
  expect_equal(sum(startsWith(labels, "contents_label_")), 6)
  expect_equal(sum(startsWith(labels, "contents_page_")), 6)
  expect_false(any(startsWith(labels, "contents_band_")))
  expect_true("1. Epidemiology" %in% shapes$text)
  expect_true("Detections by week" %in% shapes$text)
  expect_true(all(as.character(3:8) %in% shapes$text))

  section <- shape_xml(deck, "contents_label_1")
  run <- shape_xml(deck, "contents_label_2")

  expect_match(run, "hlinkClick", fixed = TRUE)
  expect_match(section, "A9552B", fixed = TRUE)
  expect_match(section, "b=\"1\"", fixed = TRUE)
  expect_match(run, "b=\"0\"", fixed = TRUE)
})

test_that("deck_contents_fill balances the sections over two columns", {
  config <- test_config()
  deck <- build_test_deck(config) |>
    deck_contents_fill(config = config)
  place <- function(i) shape_box(deck, paste0("contents_label_", i))

  near <- function(actual, expected) {
    expect_equal(actual, expected, tolerance = 1e-5)
  }

  near(c(place(1)$left, place(1)$top), c(0.41, 1.45))
  near(c(place(2)$left, place(2)$top), c(0.71, 1.75))
  near(c(place(3)$left, place(3)$top), c(6.965, 1.45))
  near(place(5)$top, 2.11)
  near(place(6)$top, 2.41)
  near(shape_box(deck, "contents_page_1")$width, 0.45)
})

test_that("deck_contents_fill takes its entries, box and spacing as given", {
  config <- test_config()
  deck <- build_test_deck(config)
  entries <- deck_contents_title_entries(
    deck, collapsed = c("Epidemiology", "AFP surveillance"), config = config
  )

  deck <- deck_contents_fill(
    deck, entries = entries,
    box = list(left = 1, top = 2, width = 9, height = 4),
    page = list(gap = 1, row_height = 0.3), config = config
  )
  labels <- slide_shapes(deck)$ph_label
  place <- function(i) shape_box(deck, paste0("contents_label_", i))

  expect_equal(sum(startsWith(labels, "contents_label_")), 4)
  expect_equal(c(place(1)$left, place(1)$top), c(1, 2))
  expect_equal(place(1)$height, 0.3, tolerance = 1e-5)
  expect_equal(place(2)$top, 2 + 0.3 + 0.08 + 0.14, tolerance = 1e-5)
  expect_equal(c(place(3)$left, place(3)$top), c(6, 2))
})

test_that("deck_contents_fill passes its lists on to the entries", {
  config <- test_config()
  deck <- build_test_deck(config) |>
    deck_contents_fill(
      collapsed = "Campaigns", dropped = "AFP surveillance",
      standalone = "Contents", index = 1, config = config
    )
  shapes <- slide_shapes(deck)

  expect_true(all(c("1. Contents", "2. Epidemiology") %in% shapes$text))
  expect_true("4. Campaigns" %in% shapes$text)
  expect_false("Coverage by round" %in% shapes$text)
  expect_false("3. AFP surveillance" %in% shapes$text)
  expect_true("Weekly review" %in% shapes$text)
})

test_that("deck_contents_fill replaces the rows of an earlier build", {
  config <- test_config()
  once <- deck_contents_fill(build_test_deck(config), config = config)
  count_once <- sum(startsWith(slide_shapes(once)$ph_label, "contents_"))
  twice <- deck_contents_fill(once, config = config)

  expect_equal(
    sum(startsWith(slide_shapes(twice)$ph_label, "contents_")), count_once
  )
})

test_that("deck_contents_fill stops when the entries do not fit", {
  config <- test_config()

  expect_error(
    deck_contents_fill(
      build_test_deck(config),
      box = list(left = 1, top = 2, width = 9, height = 0.3),
      config = config
    ),
    "does not fit"
  )
})

test_that("deck_contents_fill reads the config and settings for one call", {
  config <- test_config(
    contents_fill = list(indent = 1), contents_section_pt = 20,
    contents_number_colour = "#101010", contents_section_colour = "#202020",
    contents_run_colour = "#303030",
    content_box = list(left = 2, top = 2, width = 10, height = 4)
  )
  deck <- build_test_deck(config) |>
    deck_contents_fill(config = config)

  expect_equal(shape_box(deck, "contents_label_1")$left, 2)
  expect_equal(shape_box(deck, "contents_label_2")$left, 3)
  expect_match(shape_xml(deck, "contents_label_1"), "sz=\"2000\"", fixed = TRUE)
  expect_match(shape_xml(deck, "contents_label_1"), "101010", fixed = TRUE)
  expect_match(shape_xml(deck, "contents_label_1"), "202020", fixed = TRUE)
  expect_match(shape_xml(deck, "contents_label_2"), "303030", fixed = TRUE)

  deck <- deck_contents_fill(
    deck, section_weight = "regular", contents_run_pt = 8,
    contents_page = 1L, config = config
  )

  expect_match(shape_xml(deck, "contents_label_1"), "b=\"0\"", fixed = TRUE)
  expect_match(shape_xml(deck, "contents_label_2"), "sz=\"800\"", fixed = TRUE)
  expect_true("Weekly review" %in% slide_shapes(deck)$text)
  expect_error(
    deck_contents_fill(deck, contents_rows = 2, config = config),
    "Unknown setting"
  )
})
