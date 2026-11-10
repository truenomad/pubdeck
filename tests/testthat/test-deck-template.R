test_that("deck_open reads the template and stops when there is none", {
  deck <- deck_open(config = test_config())

  expect_s3_class(deck, "rpptx")
  expect_length(deck, 0)
  expect_error(deck_open("no_such_template.pptx"), "No template")
})

test_that("deck_check_template passes a template that has everything", {
  expect_message(
    result <- deck_check_template(config = test_config()),
    "every layout"
  )
  expect_true(all(result$found))
  expect_named(result, c("use", "layout", "placeholder", "found"))
})

test_that("deck_check_template names what a template lacks", {
  config <- test_config(
    layouts = list(cover = "front_page"),
    labels = list(section_title = "divider_title")
  )

  expect_error(deck_check_template(config = config), "front_page")
  expect_error(deck_check_template(config = config), "divider_title")

  result <- deck_check_template(config = config, error = FALSE)

  expect_false(result$found[result$use == "cover"])
  expect_true(all(result$found[result$use == "contents"]))
})

test_that("deck_check_template reads the master from the configuration", {
  config <- test_config(master = "Another Master")

  expect_error(deck_check_template(config = config), "Another Master")
})

test_that("deck_layout_box reads a placeholder's box off the template", {
  box <- deck_layout_box("two_plots", label = "plot", config = test_config())

  expect_named(box, c("left", "top", "width", "height"))
  expect_true(all(unlist(box) > 0))
  expect_error(
    deck_layout_box("two_plots", label = "absent", config = test_config()),
    "0 placeholders"
  )
})

test_that("deck_page_labels reads each page back in order", {
  pages <- deck_page_labels(build_test_deck(), test_config())

  expect_equal(nrow(pages), 8)
  expect_equal(
    pages$divider[c(3, 5, 7)],
    c("Epidemiology", "AFP surveillance", "Campaigns")
  )
  expect_equal(pages$title[c(2, 4)], c("Contents", "Detections by week"))
  expect_equal(pages$foot[c(4, 6, 8)], c(
    "Epidemiology", "AFP surveillance", "Campaigns"
  ))
  expect_equal(pages$source[4], "Source: POLIS")
  expect_true(is.na(pages$divider[1]))
})

test_that("deck_page_titles lists page and divider titles of a saved deck", {
  path <- file.path(withr::local_tempdir(), "deck.pptx")
  print(build_test_deck(), target = path)

  titles <- deck_page_titles(path, test_config())

  expect_setequal(titles, c(
    "Contents", "Epidemiology", "Detections by week", "AFP surveillance",
    "AFP cases by month", "Campaigns", "Coverage by round"
  ))
})

test_that("deck_reopen drops the pages a previous run added", {
  path <- file.path(withr::local_tempdir(), "deck.pptx")
  print(build_test_deck(), target = path)

  expect_length(deck_reopen(path, list(base_slides = 8)), 8)
  expect_message(
    shorter <- deck_reopen(path, list(base_slides = 6)),
    "Dropping 2 pages"
  )
  expect_length(shorter, 6)
  expect_message(
    reread <- deck_reopen(path, list(base_slides = 4), reread = TRUE),
    "Dropping 4 pages"
  )
  expect_length(reread, 4)
  expect_length(officer::read_pptx(path), 4)
})

test_that("deck_reopen stops on a deck shorter than its predecessor left", {
  path <- file.path(withr::local_tempdir(), "deck.pptx")
  print(build_test_deck(), target = path)

  expect_error(deck_reopen(path, list(base_slides = 12)), "fewer than")
})

test_that("deck_carries_page matches in either direction, ignoring case", {
  titles <- c("12 AFP cases awaiting classification", "Summary")

  expect_true(deck_carries_page("AFP cases", titles))
  expect_true(deck_carries_page("summary of epidemiology", titles))
  expect_false(deck_carries_page("Campaigns", titles))
})

test_that("deck_carries_page is FALSE for an empty page or no titles", {
  expect_false(deck_carries_page("", "Summary"))
  expect_false(deck_carries_page(NA_character_, "Summary"))
  expect_false(deck_carries_page("Summary", c(NA, "")))
  expect_false(deck_carries_page(c("a", "b"), "Summary"))
})
