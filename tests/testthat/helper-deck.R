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

# an empty page on the layout the builders place their boxes on
blank_page <- function(config = test_config()) {
  deck_open(config = config) |>
    officer::add_slide(
      layout = config$layouts$text_page, master = config$master
    )
}

# the name of the layout a slide is built on
slide_layout <- function(deck, index) {
  file <- deck$slide$get_slide(index)$layout_name()
  layouts <- deck$slideLayouts$get_metadata()

  layouts$name[basename(layouts$filename) == file]
}

# the shapes on the current slide
slide_shapes <- function(deck) {
  officer::slide_summary(deck)
}

# the text of one labelled shape on the current slide
shape_text <- function(deck, label) {
  shapes <- slide_shapes(deck)

  shapes$text[shapes$ph_label == label]
}

# the XML of one labelled shape on the current slide
shape_xml <- function(deck, label) {
  slide <- deck$slide$get_slide(deck$cursor)$get()
  shape <- xml2::xml_find_first(
    slide, sprintf("//p:cNvPr[@name='%s']/../..", label)
  )

  as.character(shape)
}

# position and size in inches of a shape, read from its XML
xfrm_box <- function(shape) {
  offset <- xml2::xml_find_first(shape, ".//a:off")
  extent <- xml2::xml_find_first(shape, ".//a:ext")
  emu_per_inch <- 914400

  tibble::tibble(
    left = as.numeric(xml2::xml_attr(offset, "x")) / emu_per_inch,
    top = as.numeric(xml2::xml_attr(offset, "y")) / emu_per_inch,
    width = as.numeric(xml2::xml_attr(extent, "cx")) / emu_per_inch,
    height = as.numeric(xml2::xml_attr(extent, "cy")) / emu_per_inch
  )
}

# position and size of one labelled shape on the current slide
shape_box <- function(deck, label) {
  slide <- deck$slide$get_slide(deck$cursor)$get()

  xfrm_box(xml2::xml_find_first(
    slide, sprintf("//p:cNvPr[@name='%s']/../..", label)
  ))
}

# position and size of every picture on the current slide
slide_pictures <- function(deck) {
  slide <- deck$slide$get_slide(deck$cursor)$get()

  xml2::xml_find_all(slide, "//p:pic") |>
    purrr::map(xfrm_box) |>
    purrr::list_rbind()
}

# how many times a pattern occurs in a string
count_matches <- function(text, pattern) {
  lengths(regmatches(text, gregexpr(pattern, text)))
}

# the text runs of a flextable's body, one row per cell
table_runs <- function(ft) {
  flextable::information_data_chunk(ft) |>
    dplyr::filter(.data$.part == "body")
}

# a box in inches
test_box <- function(left = 1, top = 2, width = 4, height = 1.5) {
  list(left = left, top = top, width = width, height = height)
}

# a plain white picture
test_png <- function(dir, name = "figure.png", width = 400, height = 200) {
  path <- file.path(dir, name)
  magick::image_write(
    magick::image_blank(width, height, color = "white"), path
  )

  path
}

# a null graphics device for the length of a test, so that drawing a grob
# leaves no device open behind it
local_null_device <- function(env = parent.frame()) {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off(), envir = env)
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
