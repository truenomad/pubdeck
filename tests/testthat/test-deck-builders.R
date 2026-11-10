test_that("deck_cover_slide writes a labelled title, subtitle and author", {
  config <- test_config(
    cover_subtitle = "Evidence for 2027", cover_author = "The data team"
  )
  deck <- deck_open(config = config) |>
    deck_cover_slide(config = config)
  shapes <- slide_shapes(deck)

  expect_length(deck, 1)
  expect_equal(shape_text(deck, "main_title"), "Weekly review")
  expect_equal(shape_text(deck, "subtitle"), "Evidence for 2027")
  expect_equal(shape_text(deck, "author"), "The data team")
  expect_equal(shapes$offx[shapes$ph_label == "main_title"], 1)
  expect_equal(shapes$offy[shapes$ph_label == "subtitle"], 3.93)
  expect_match(shape_xml(deck, "main_title"), "b=\"1\"", fixed = TRUE)
  expect_match(shape_xml(deck, "subtitle"), "b=\"0\"", fixed = TRUE)
  expect_match(shape_xml(deck, "subtitle"), "9DC3E6", fixed = TRUE)
})

test_that("deck_cover_slide leaves out a line that has no text", {
  config <- test_config()
  deck <- deck_open(config = config) |>
    deck_cover_slide(config = config)

  expect_equal(slide_shapes(deck)$ph_label, "main_title")
  expect_error(
    deck_cover_slide(
      deck_open(config = config), config = test_config(title = NULL)
    ),
    "title"
  )
})

test_that("deck_cover_slide takes its wording and weights as given", {
  config <- test_config(cover_author = "The data team")
  deck <- deck_open(config = config) |>
    deck_cover_slide(
      title = "Annex", subtitle = "Supplementary pages", author = "A. N.",
      title_weight = "regular", subtitle_weight = "bold",
      author_weight = "regular", config = config
    )

  expect_equal(shape_text(deck, "main_title"), "Annex")
  expect_equal(shape_text(deck, "subtitle"), "Supplementary pages")
  expect_equal(shape_text(deck, "author"), "A. N.")
  expect_match(shape_xml(deck, "main_title"), "b=\"0\"", fixed = TRUE)
  expect_match(shape_xml(deck, "subtitle"), "b=\"1\"", fixed = TRUE)
  expect_match(shape_xml(deck, "author"), "b=\"0\"", fixed = TRUE)
})

test_that("deck_cover_slide takes settings for one call", {
  config <- test_config(cover_author = "The data team")
  deck <- deck_open(config = config) |>
    deck_cover_slide(
      subtitle = "Evidence",
      cover_author = NULL,
      cover_subtitle_pt = 22,
      cover_subtitle_colour = "#112233",
      cover_subtitle_box = list(left = 2, top = 5, width = 6, height = 0.4),
      labels = list(cover_subtitle = "strapline"),
      config = config
    )
  shapes <- slide_shapes(deck)

  expect_setequal(shapes$ph_label, c("main_title", "strapline"))
  expect_equal(shapes$offy[shapes$ph_label == "strapline"], 5)
  expect_match(shape_xml(deck, "strapline"), "sz=\"2200\"", fixed = TRUE)
  expect_match(shape_xml(deck, "strapline"), "112233", fixed = TRUE)
  expect_error(
    deck_cover_slide(deck, cover_subtitle_size = 22, config = config),
    "Unknown setting"
  )
})

test_that("deck_figure_page places the headings, the figure and the footer", {
  config <- test_config()
  figure <- test_png(withr::local_tempdir())
  deck <- deck_open(config = config) |>
    deck_figure_page(
      layout = "two_plots", title = "Cases by week",
      subtitle = "Cases fell **after** the campaign", figure = figure,
      section = "epi", sources = "polis", as_of = "To 9 March",
      config = config
    )
  shapes <- slide_shapes(deck)

  expect_equal(shape_text(deck, "page_title"), "Cases by week")
  expect_equal(
    shape_text(deck, "page_subtitle"), "Cases fell after the campaign"
  )
  expect_equal(shape_text(deck, "section"), "Epidemiology")
  expect_equal(shape_text(deck, "page_source"), "Source: POLIS. To 9 March")
  expect_equal(nrow(slide_pictures(deck)), 1)
  expect_false("figure_pending" %in% shapes$ph_label)
  expect_match(shape_xml(deck, "page_title"), "sz=\"2600\"", fixed = TRUE)
  expect_match(shape_xml(deck, "page_subtitle"), "b=\"1\"", fixed = TRUE)
})

test_that("deck_figure_page keeps the figure below the subtitle", {
  config <- test_config()
  figure <- test_png(withr::local_tempdir(), width = 100, height = 400)
  box <- deck_layout_box("two_plots", "plot", config = config)
  deck <- deck_open(config = config) |>
    deck_figure_page(
      "two_plots", "Cases", "By week", figure, "epi", "polis",
      plot_top_min = box$top + 1, config = config
    )
  picture <- slide_pictures(deck)

  expect_equal(picture$top, box$top + 1, tolerance = 1e-5)
  expect_equal(picture$height, box$height - 1, tolerance = 1e-5)
})

test_that("deck_figure_page stands a panel in for a figure not yet drawn", {
  dir <- withr::local_tempdir()
  config <- test_config(path_root = dir)
  deck <- deck_open(config = config) |>
    deck_figure_page(
      "two_plots", "Cases", "By week", file.path(dir, "plots", "trend.png"),
      "epi", "polis", config = config
    )
  xml <- shape_xml(deck, "figure_pending")

  expect_equal(
    shape_text(deck, "figure_pending"), "Figure pending: plots/trend.png"
  )
  expect_match(xml, "F2F5F7", fixed = TRUE)
  expect_match(xml, "sz=\"1200\"", fixed = TRUE)
})

test_that("deck_figure_page takes the pending wording and size for a call", {
  dir <- withr::local_tempdir()
  config <- test_config(path_root = dir)
  deck <- deck_open(config = config) |>
    deck_figure_page(
      "two_plots", "Cases", "By week", file.path(dir, "trend.png"), "epi",
      framing = TRUE, pending_lead = "To draw: ", pending_page_pt = 9,
      pending_fill = "#ABCDEF", config = config
    )
  xml <- shape_xml(deck, "figure_pending")

  expect_equal(shape_text(deck, "figure_pending"), "To draw: trend.png")
  expect_match(xml, "ABCDEF", fixed = TRUE)
  expect_match(xml, "sz=\"900\"", fixed = TRUE)
  expect_false("page_source" %in% slide_shapes(deck)$ph_label)
})

test_that("deck_figure_page stops when a page names no source", {
  config <- test_config()
  figure <- test_png(withr::local_tempdir())

  expect_error(
    deck_figure_page(
      deck_open(config = config), "two_plots", "Cases", "By week", figure,
      "epi", config = config
    ),
    "no source"
  )
})

test_that("deck_text_page adds the headings and footer on the text layout", {
  config <- test_config()
  deck <- deck_open(config = config) |>
    deck_text_page(
      "What the review covers", "Three questions", "campaigns", "survey",
      config = config
    )
  shapes <- slide_shapes(deck)

  expect_length(deck, 1)
  expect_equal(shape_text(deck, "page_title"), "What the review covers")
  expect_equal(shape_text(deck, "page_subtitle"), "Three questions")
  expect_equal(shape_text(deck, "section"), "Campaigns")
  expect_equal(
    shape_text(deck, "page_source"), "Source: Post-campaign survey"
  )
  expect_equal(shapes$offx[shapes$ph_label == "page_title"], 0.41)
  expect_equal(shapes$offy[shapes$ph_label == "page_subtitle"], 0.74)
  expect_match(shape_xml(deck, "page_subtitle"), "b=\"0\"", fixed = TRUE)
})

test_that("deck_text_page takes a framing page with no source", {
  config <- test_config()
  deck <- deck_open(config = config) |>
    deck_text_page(
      "What the review covers", "Three questions", "epi", framing = TRUE,
      config = config
    )

  expect_false("page_source" %in% slide_shapes(deck)$ph_label)
  expect_equal(shape_text(deck, "section"), "Epidemiology")
  expect_error(
    deck_text_page(
      deck, "What the review covers", "Three questions", "epi",
      config = config
    ),
    "no source"
  )
})

test_that("deck_text_page takes its layout, boxes and date as given", {
  config <- test_config()
  title_box <- list(left = 1, top = 0.3, width = 8, height = 1)
  subtitle_box <- list(left = 1, top = 1.4, width = 8, height = 0.5)
  deck <- deck_open(config = config) |>
    deck_text_page(
      "Cases", "By week", "epi", "polis", layout = "two_plots",
      title_box = title_box, subtitle_box = subtitle_box,
      as_of = "To 9 March", config = config
    )
  shapes <- slide_shapes(deck)

  expect_equal(shapes$offx[shapes$ph_label == "page_title"], 1)
  expect_equal(shapes$cy[shapes$ph_label == "page_title"], 1)
  expect_equal(shapes$offy[shapes$ph_label == "page_subtitle"], 1.4)
  expect_equal(shape_text(deck, "page_source"), "Source: POLIS. To 9 March")
})

test_that("deck_text_page checks the title unless it has a box of its own", {
  config <- test_config()
  long <- strrep("Cases by week of onset ", 4)
  roomy <- list(left = 0.41, top = 0.2, width = 12, height = 1.2)

  expect_message(
    deck_text_page(
      deck_open(config = config), long, "By week", "epi", "polis",
      config = config
    ),
    "Title may wrap"
  )
  expect_no_message(
    deck_text_page(
      deck_open(config = config), long, "By week", "epi", "polis",
      title_box = roomy, config = config
    )
  )
})

test_that("deck_text_page reads the configuration and settings for a call", {
  config <- test_config(
    layouts = list(text_page = "two_plots"), page_title_pt = 30,
    page_title_colour = "#112233", page_subtitle_weight = "bold",
    labels = list(page_subtitle = "strapline")
  )
  deck <- deck_open(config = config) |>
    deck_text_page("Cases", "By week", "epi", "polis", config = config) |>
    deck_text_page(
      "Cases", "By week", "epi", "polis", page_title_pt = 20,
      page_subtitle_pt = 10, config = config
    )

  expect_match(shape_xml(deck, "page_title"), "sz=\"2000\"", fixed = TRUE)
  expect_match(shape_xml(deck, "strapline"), "sz=\"1000\"", fixed = TRUE)

  first <- officer::on_slide(deck, index = 1)

  expect_match(shape_xml(first, "page_title"), "sz=\"3000\"", fixed = TRUE)
  expect_match(shape_xml(first, "page_title"), "112233", fixed = TRUE)
  expect_match(shape_xml(first, "strapline"), "b=\"1\"", fixed = TRUE)
  expect_true("plot" %in% officer::layout_properties(
    first, layout = "two_plots"
  )$ph_label)
})

test_that("deck_below_subtitle moves a box down and keeps its bottom edge", {
  box <- list(left = 1, top = 1.2, width = 6, height = 4)

  expect_equal(
    deck_below_subtitle(box),
    list(left = 1, top = 1.7, width = 6, height = 3.5)
  )
  expect_equal(deck_below_subtitle(box, top_min = 2)$top, 2)
  expect_equal(deck_below_subtitle(box, top_min = 2)$height, 3.2)
  expect_equal(
    deck_below_subtitle(box, config = deck_config(plot_top_min = 3))$top, 3
  )
})

test_that("deck_below_subtitle leaves a box that starts below the line", {
  box <- list(left = 1, top = 2.5, width = 6, height = 4, label = "kept")

  expect_equal(deck_below_subtitle(box), box)
})

test_that("deck_columns splits a box at the gap the configuration holds", {
  box <- list(left = 1, top = 1, width = 6.5, height = 3)
  columns <- deck_columns(box, 3)

  expect_length(columns, 3)
  expect_equal(columns[[1]], list(left = 1, top = 1, width = 2, height = 3))
  expect_equal(columns[[2]]$left, 3.25)
  expect_equal(deck_columns(box, 2, gap = 0.5)[[1]]$width, 3)
  expect_equal(
    deck_columns(box, 2, config = deck_config(column_gap = 0.1))[[1]]$width,
    3.2
  )
  expect_equal(deck_columns(box, 3), deck_split_box(box, n = 3, gap = 0.25))
})

test_that("deck_place_figure fits a drawn figure into its box", {
  figure <- test_png(withr::local_tempdir())
  deck <- blank_page() |>
    deck_place_figure(figure, test_box(top = 2, width = 4, height = 4))
  picture <- slide_pictures(deck)

  expect_equal(nrow(picture), 1)
  expect_equal(c(picture$width, picture$height), c(4, 2))
  expect_equal(c(picture$left, picture$top), c(1, 3))
})

test_that("deck_place_figure moves its box below the subtitle", {
  figure <- test_png(withr::local_tempdir(), width = 100, height = 400)
  deck <- blank_page() |>
    deck_place_figure(figure, test_box(top = 1, height = 3)) |>
    deck_place_figure(figure, test_box(top = 1, height = 3), plot_top_min = 3)
  picture <- slide_pictures(deck)

  expect_equal(picture$top, c(1.7, 3), tolerance = 1e-5)
  expect_equal(picture$height, c(2.3, 1), tolerance = 1e-5)
})

test_that("deck_place_figure stands a panel in for a missing figure", {
  dir <- withr::local_tempdir()
  config <- test_config(path_root = dir)
  missing_figure <- file.path(dir, "trend.png")
  deck <- blank_page(config) |>
    deck_place_figure(missing_figure, test_box(), config = config) |>
    deck_place_figure(
      missing_figure, test_box(left = 6), label = "trend", pending_pt = 14,
      pending_lead = "Missing: ", config = config
    )

  expect_equal(shape_text(deck, "figure_pending"), "Figure pending: trend.png")
  expect_match(shape_xml(deck, "figure_pending"), "sz=\"1000\"", fixed = TRUE)
  expect_equal(shape_text(deck, "trend"), "Missing: trend.png")
  expect_match(shape_xml(deck, "trend"), "sz=\"1400\"", fixed = TRUE)
})

test_that("deck_place_vector places a saved grob as native shapes", {
  local_null_device()
  dir <- withr::local_tempdir()
  grob_path <- file.path(dir, "chart.rds")
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  saveRDS(ggplot2::ggplotGrob(plot), grob_path)

  deck <- blank_page() |>
    deck_place_vector(grob_path, test_box(top = 2, height = 3), "trend")

  expect_equal(nrow(slide_pictures(deck)), 0)
  expect_match(shape_xml(deck, "trend"), "grpSp", fixed = TRUE)
})

test_that("deck_place_vector reads the grob ggsave_pub keeps", {
  local_null_device()
  dir <- withr::local_tempdir()
  path <- save_test_chart(dir)

  deck <- blank_page() |>
    deck_place_vector(file.path(dir, "chart.qs2"), test_box(), "trend")

  expect_match(shape_xml(deck, "trend"), "grpSp", fixed = TRUE)
})

test_that("deck_place_vector places the picture when the grob is large", {
  local_null_device()
  dir <- withr::local_tempdir()
  path <- save_test_chart(dir)
  config <- test_config(vector_max_bytes = 10)

  deck <- blank_page(config) |>
    deck_place_vector(
      file.path(dir, "chart.qs2"), test_box(top = 2, width = 4, height = 4),
      "trend", config = config
    ) |>
    deck_place_vector(
      file.path(dir, "chart.qs2"), test_box(top = 2, width = 4, height = 4),
      "trend_small", vector_max_bytes = 5e6, config = config
    )
  picture <- slide_pictures(deck)

  expect_equal(nrow(picture), 1)
  expect_equal(c(picture$width, picture$height), c(4, 2))
  expect_match(shape_xml(deck, "trend_small"), "grpSp", fixed = TRUE)
})

test_that("deck_place_vector keeps a large grob that has no picture", {
  dir <- withr::local_tempdir()
  grob_path <- file.path(dir, "alone.rds")
  saveRDS(grid::rectGrob(), grob_path)
  config <- test_config(vector_max_bytes = 10)

  deck <- blank_page(config) |>
    deck_place_vector(grob_path, test_box(), "alone", config = config)

  expect_true("alone" %in% slide_shapes(deck)$ph_label)
})
