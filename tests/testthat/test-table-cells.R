test_that("bar_cell draws a bar as long as the share", {
  cells <- bar_cell(c("12%", "80%"), share = c(0.12, 0.8))

  expect_length(cells, 2)
  expect_match(cells[1], "width:12%", fixed = TRUE)
  expect_match(cells[2], "width:80%", fixed = TRUE)
  expect_match(cells[1], ">12%</div>", fixed = TRUE)
})

test_that("bar_cell clamps the share and draws nothing for a missing one", {
  cells <- bar_cell(c("a", "b", "c"), share = c(NA, 1.4, -0.2))

  expect_match(cells[1], "width:0%", fixed = TRUE)
  expect_match(cells[2], "width:100%", fixed = TRUE)
  expect_match(cells[3], "width:0%", fixed = TRUE)
})

test_that("bar_cell takes its label and colours from its arguments", {
  cell <- bar_cell(
    0.5, share = 0.5, label = "half", fill = "#111111", track = "#EEEEEE"
  )

  expect_match(cell, ">half</div>", fixed = TRUE)
  expect_match(cell, "background:#111111", fixed = TRUE)
  expect_match(cell, "background:#EEEEEE", fixed = TRUE)
})

test_that("bar_cell falls back to the configuration for its colours", {
  cell <- with_deck_config(
    deck_config(table_bar = "#123456"),
    bar_cell("1", share = 1)
  )

  expect_match(cell, "background:#123456", fixed = TRUE)
})

test_that("dot_cell colours each label and greys an unknown one", {
  cells <- dot_cell(c("AFP", "Other"), c(AFP = "#A9552B"))

  expect_match(cells[1], "background:#A9552B", fixed = TRUE)
  expect_match(cells[2], "background:grey72", fixed = TRUE)
  expect_match(cells[1], "<span>AFP</span>", fixed = TRUE)
})

test_that("dot_cell can drop the label and centre the dot", {
  cell <- dot_cell("AFP", c(AFP = "#A9552B"), show_label = FALSE)

  expect_match(cell, "justify-content:center", fixed = TRUE)
  expect_no_match(cell, "<span>AFP</span>", fixed = TRUE)
})

test_that("chip_cell fills the pill and picks readable ink", {
  cells <- chip_cell(c("Low", "High"), c(Low = "#FFF5F0", High = "#67000D"))

  expect_match(cells[1], "background:#FFF5F0;color:#1A1A1A", fixed = TRUE)
  expect_match(cells[2], "background:#67000D;color:#FFFFFF", fixed = TRUE)
  expect_match(
    chip_cell("Other", c(Low = "#FFF5F0"), missing_fill = "#CCCCCC"),
    "background:#CCCCCC",
    fixed = TRUE
  )
})

test_that("spark_cell draws a line per series and nothing for an empty one", {
  cells <- spark_cell(
    list(c(0.01, 0.02, 0.08), c(NA_real_, NA_real_), numeric(0)),
    floor = 0.001
  )

  expect_length(cells, 3)
  expect_match(cells[1], "^<svg")
  expect_match(cells[1], "<polyline")
  expect_equal(cells[2:3], c("", ""))
})

test_that("spark_cell keeps a series that barely moves flat", {
  flat <- spark_cell(list(c(0.2, 0.2, 0.2)), floor = 0.001)
  points <- sub(".*points=\"([^\"]*)\".*", "\\1", flat)
  heights <- as.numeric(sub(".*,", "", strsplit(points, " ")[[1]]))

  expect_equal(length(unique(heights)), 1)
})

test_that("move_cell marks a move up and a new row with the alarm colour", {
  cells <- move_cell(
    change = c(2, -1, 0, NA), is_new = c(FALSE, FALSE, FALSE, TRUE)
  )

  expect_match(cells[1], pub_alarm(), fixed = TRUE)
  expect_match(cells[1], "&#9650; 2", fixed = TRUE)
  expect_match(cells[2], "&#9660; 1", fixed = TRUE)
  expect_match(cells[3], "&ndash;", fixed = TRUE)
  expect_match(cells[4], ">new<", fixed = TRUE)
  expect_match(cells[4], pub_alarm(), fixed = TRUE)
})

test_that("style_deck_table returns a gt table that renders", {
  styled <- gt::gt(head(mtcars[, 1:4])) |>
    style_deck_table(mono_columns = "mpg", header = "#123456")

  expect_s3_class(styled, "gt_tbl")
  expect_match(gt::as_raw_html(styled), "#123456", fixed = TRUE)
})
