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
