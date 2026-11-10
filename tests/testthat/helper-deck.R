# a neutral template, sections and configuration for the deck tests; the
# template is built by fixtures/make_template.R

test_template <- function() {
  testthat::test_path("fixtures", "template.pptx")
}

test_sections <- function() {
  tibble::tibble(
    key = c("epi", "afp", "campaigns"),
    label = c("Epidemiology", "AFP surveillance", "Campaigns"),
    parent = c(NA, "epi", NA)
  )
}

test_config <- function(...) {
  deck_config(
    template = test_template(),
    sections = test_sections(),
    title = "Weekly review",
    country = "Testland",
    cover_footer = "Compiled by the data team",
    source_labels = c(polis = "POLIS", survey = "Post-campaign survey"),
    source_clauses = c(births = "births estimated from the census"),
    cover_title_box = list(left = 1, top = 3, width = 8, height = 0.7),
    cover_date_box = list(left = 1, top = 3.8, width = 8, height = 0.4),
    cover_footer_box = list(left = 1, top = 6.8, width = 8, height = 0.3),
    foot_source_box = list(left = 3, top = 6.9, width = 6.5, height = 0.3),
    ...
  )
}

# a content page with a title and a foot strip
add_test_page <- function(deck, title, section, config) {
  deck |>
    officer::add_slide(layout = "two_plots", master = config$master) |>
    officer::ph_with(
      value = title,
      location = officer::ph_location_label(ph_label = "page_title")
    ) |>
    deck_page_footer(section, sources = "polis", config = config)
}

# cover, contents, then two top-level sections with a subsection in the first
build_test_deck <- function(config = test_config()) {
  deck_open(config = config) |>
    deck_title_slide(date = as.Date("2025-03-09"), config = config) |>
    deck_contents_placeholder(config = config) |>
    deck_section_slide("epi", config = config) |>
    add_test_page("Detections by week", "epi", config) |>
    deck_section_slide("afp", config = config) |>
    add_test_page("AFP cases by month", "afp", config) |>
    deck_section_slide("campaigns", config = config) |>
    add_test_page("Coverage by round", "campaigns", config)
}

# the text of every shape on one slide
slide_text <- function(deck, index) {
  officer::slide_summary(officer::on_slide(deck, index = index))$text
}

# a small chart saved with its grob, for the placement tests
save_test_chart <- function(dir, width = 6, height = 3) {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    theme_pub()

  path <- file.path(dir, "chart.png")
  suppressMessages(ggsave_pub(plot, path, width = width, height = height))

  path
}
