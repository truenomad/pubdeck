test_that("pub_palette returns the categorical colours in order", {
  all_colours <- pub_palette()

  expect_length(all_colours, 8)
  expect_equal(pub_palette(3), all_colours[1:3])
  expect_true(all(grepl("^#[0-9A-F]{6}$", all_colours)))
})

test_that("pub_palette binds colours to named entities", {
  bound <- pub_palette(named = c("Env", "AFP", "Contact"))

  expect_named(bound, c("Env", "AFP", "Contact"))
  expect_equal(unname(bound), pub_palette(3))
})

test_that("pub_palette stops when more colours are asked for than exist", {
  expect_error(pub_palette(9), "8 colours")
  expect_error(pub_palette(named = letters[1:9]), "8 colours")
})

test_that("pub_ramp returns each ramp and interpolates to n", {
  expect_length(pub_ramp("coverage"), 6)
  expect_length(pub_ramp("harm"), 5)
  expect_length(pub_ramp("silence"), 5)
  expect_length(pub_ramp("diverging"), 5)

  stretched <- pub_ramp("harm", n = 9)

  expect_length(stretched, 9)
  expect_equal(stretched[c(1, 9)], pub_ramp("harm")[c(1, 5)])
  expect_error(pub_ramp("rainbow"))
})

test_that("pub_alarm is not one of the categorical colours", {
  expect_false(pub_alarm() %in% pub_palette())
})

test_that("pub_map_colours names every map colour", {
  colours <- pub_map_colours()

  expect_true(all(
    c("not_measured", "no_detection", "focus_land", "contested") %in%
      names(colours)
  ))
  expect_type(colours, "character")
})

test_that("tint lightens colours and keeps their names", {
  colours <- pub_palette(named = c("AFP", "Env"))
  tinted <- tint(colours)

  expect_named(tinted, c("AFP", "Env"))
  expect_true(all(
    colSums(grDevices::col2rgb(tinted)) > colSums(grDevices::col2rgb(colours))
  ))
  expect_equal(unname(tint("#000000", weight = 1)), "#FFFFFF")
  expect_equal(unname(tint("#336699", weight = 0)), "#336699")
})

test_that("contrast_ink picks dark ink on pale fills and light on deep", {
  expect_equal(
    contrast_ink(c("#FFF5F0", "#67000D")),
    c("#1A1A1A", "#FFFFFF")
  )
  expect_equal(
    contrast_ink("#FFFFFF", dark = "#000000", light = "#EEEEEE"),
    "#000000"
  )
  expect_equal(
    contrast_ink(c(low = "#FFF5F0", high = "#67000D")),
    c(low = "#1A1A1A", high = "#FFFFFF")
  )
})
