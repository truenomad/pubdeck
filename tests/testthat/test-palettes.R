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

test_that("the colour settings default to the built-in colours", {
  config <- deck_config()

  expect_null(config$palette)
  expect_null(config$ramps)
  expect_null(config$alarm)
  expect_null(config$map_colours)
  expect_identical(pub_palette(config = config), pub_palette())
  expect_identical(pub_alarm(config = config), "#B2182B")
  expect_identical(pub_ramp(), pub_ramp("coverage"))
  expect_identical(pub_map_colours(config = config), pub_map_colours())
})

test_that("pub_palette takes its colours three ways", {
  own <- c("#B5693F", "#476074", "#6B7F4E")
  config <- deck_config(palette = own)

  expect_identical(pub_palette(config = config), own)
  expect_identical(pub_palette(2, config = config), own[1:2])
  expect_identical(
    pub_palette(named = c("a", "b"), config = config),
    c(a = "#B5693F", b = "#476074")
  )
  expect_error(pub_palette(4, config = config), "3 colours, not 4")
  expect_identical(with_deck_config(config, pub_palette()), own)
  expect_length(pub_palette(), 8)
  expect_error(
    pub_palette(config = deck_config(palette = 1:3)), "palette"
  )
})

test_that("pub_ramp replaces a ramp by name and adds a new one", {
  blue <- c("#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")
  config <- deck_config(
    ramps = list(harm = c("#FFFFFF", "#000000"), seq5_blue = blue)
  )

  expect_identical(pub_ramp("harm", config = config), c("#FFFFFF", "#000000"))
  expect_identical(pub_ramp("seq5_blue", config = config), blue)
  expect_identical(
    pub_ramp("coverage", config = config), pub_ramp("coverage")
  )
  expect_length(pub_ramp("seq5_blue", n = 9, config = config), 9)
  expect_identical(with_deck_config(config, pub_ramp("seq5_blue")), blue)
  expect_error(pub_ramp("seq5_blue"), "coverage")
  expect_error(pub_ramp("rainbow", config = config), "seq5_blue")
  expect_error(
    pub_ramp(config = deck_config(ramps = list("#FFFFFF"))), "ramps"
  )
})

test_that("a ramps setting over a base merges and may add a name", {
  first <- deck_config(ramps = list(one = c("#FFFFFF", "#000000")))
  second <- deck_config(
    ramps = list(two = c("#000000", "#FFFFFF")), base = first
  )

  expect_named(second$ramps, c("one", "two"))
  expect_error(
    deck_config(contents_slide = list(columns = 2L)), "Unknown field"
  )
})

test_that("pub_alarm takes its colour three ways", {
  config <- deck_config(alarm = "#C0392B")

  expect_identical(pub_alarm(config = config), "#C0392B")
  expect_identical(with_deck_config(config, pub_alarm()), "#C0392B")
  expect_identical(pub_alarm(), "#B2182B")
  expect_error(
    pub_alarm(config = deck_config(alarm = c("#C0392B", "#000000"))),
    "single colour"
  )
})

test_that("move_cell follows the alarm setting", {
  config <- deck_config(alarm = "#C0392B")
  change <- c(2, -1)
  is_new <- c(FALSE, FALSE)

  expect_match(
    move_cell(change, is_new, config = config)[1], "#C0392B", fixed = TRUE
  )
  expect_match(
    with_deck_config(config, move_cell(change, is_new))[1], "#C0392B",
    fixed = TRUE
  )
  expect_match(
    move_cell(change, is_new, alarm = "#000000", config = config)[1],
    "#000000", fixed = TRUE
  )
})

test_that("pub_map_colours merges the map_colours setting", {
  config <- deck_config(
    map_colours = c(not_measured = "grey80", water = "#DCE7EF")
  )
  colours <- pub_map_colours(config = config)

  expect_identical(colours[["not_measured"]], "grey80")
  expect_identical(colours[["water"]], "#DCE7EF")
  expect_identical(
    colours[names(pub_map_colours())][-1], pub_map_colours()[-1]
  )
  expect_identical(
    with_deck_config(config, pub_map_colours()), colours
  )
  expect_error(
    pub_map_colours(config = deck_config(map_colours = "grey80")),
    "named"
  )
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
