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

# the XML of each slide of a deck, as it is written to a file
written_slides <- function(deck, dir = withr::local_tempdir()) {
  path <- file.path(dir, "deck.pptx")
  print(deck, target = path)
  utils::unzip(path, exdir = dir)

  file.path(dir, "ppt", "slides") |>
    list.files(pattern = "[.]xml$", full.names = TRUE) |>
    sort() |>
    purrr::map_chr(
      \(file) paste(readLines(file, warn = FALSE), collapse = "\n")
    )
}

# paragraphs of the saved deck where a run follows the end mark, which
# PowerPoint repairs on opening
saved_malformed_paragraphs <- function(deck) {
  written_slides(deck) |>
    purrr::map_int(\(xml) {
      xml2::read_xml(xml) |>
        xml2::xml_find_all("//a:p/a:endParaRPr[following-sibling::a:r]") |>
        length()
    }) |>
    sum()
}

# a page holding a flextable cell with no text, as a caller might build it
empty_cell_page <- function(config = test_config()) {
  cell <- flextable::flextable(tibble::tibble(note = "")) |>
    flextable::delete_part(part = "header")

  blank_page(config) |>
    officer::ph_with(value = cell, location = .box_location(test_box()))
}

# the record of typefaces already looked up, emptied for the length of a
# test
local_typeface_record <- function(env = parent.frame()) {
  installed <- .state$typeface_installed
  reported <- .state$typeface_reported
  .state$typeface_installed <- NULL
  .state$typeface_reported <- NULL

  withr::defer(
    {
      .state$typeface_installed <- installed
      .state$typeface_reported <- reported
    },
    envir = env
  )
}

# width in inches of one line of regular text, measured with systemfonts
# at a resolution where rounding to whole pixels is too small to matter
line_width <- function(text, font, pt, res = 7200) {
  systemfonts::string_width(text, family = font, size = pt, res = res) / res
}

# the pixels of an image file
image_pixels <- function(path) {
  as.integer(magick::image_data(magick::image_read(path)))
}
