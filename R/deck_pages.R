# text -------------------------------------------------------------------------

#' Format a Line of Page Text
#'
#' `deck_subtitle()` formats a line for a page whose layout carries no
#' subtitle placeholder, so every such page reads at the same size and
#' weight. `deck_page_subtitle()` formats a line to match the subtitle
#' placeholder of the template, for a layout that reserves none.
#'
#' @param text The text.
#' @param size Font size in points. Default `NULL`, the `subtitle_pt`
#'   setting of `config`.
#' @param colour Text colour. Default `NULL`, the `subtitle_colour`
#'   setting of `config`.
#' @param align Text alignment. Default `"left"`.
#' @param bold Logical. Default `FALSE`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return An `officer::fpar()` paragraph.
#'
#' @examples
#' deck_subtitle("Cases by week of onset")
#' deck_page_subtitle("Cases by week of onset")
#' @export
deck_subtitle <- function(text, size = NULL, colour = NULL, align = "left",
                          bold = FALSE, config = deck_config_get()) {
  .text_run(
    text,
    size = size %||% config$subtitle_pt,
    colour = colour %||% config$subtitle_colour,
    font = config$font,
    bold = bold,
    align = align
  )
}

#' @rdname deck_subtitle
#' @export
deck_page_subtitle <- function(text, config = deck_config_get()) {
  .text_run(
    text,
    size = config$page_subtitle_pt,
    colour = config$page_subtitle_colour,
    font = config$font,
    bold = TRUE
  )
}

# cover ------------------------------------------------------------------------

#' Add the Cover Page
#'
#' Adds the cover and writes its title, date line and footer. Every line
#' is placed from its own box in the configuration and not dropped into a
#' placeholder, so the cover reproduces the approved layout exactly.
#'
#' @param deck An `rpptx` object.
#' @param title Deck title. Default `NULL`, the `title` setting of
#'   `config`.
#' @param date Date the report runs to. Default is today. Pass the
#'   reporting date, so a rebuilt deck cannot claim a period it did not
#'   cover.
#' @param country Country name for the date line. Default `NULL`, the
#'   `country` setting of `config`.
#' @param compiled_by Footer line. Default `NULL`, the `cover_footer`
#'   setting of `config`. The line is left out when both are `NULL`.
#' @param subtitle Text for the date line, in place of the country and
#'   date. Default `NULL`.
#' @param corner Optional text for a corner of the cover. Default `NULL`.
#' @param corner_box Box for `corner`, a list of `left`, `top`, `width`
#'   and `height` in inches. Required with `corner`.
#' @param corner_colour Colour of `corner`. Default `NULL`, the
#'   `cover_band_colour` setting of `config`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the cover added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_open() |>
#'   deck_title_slide(date = as.Date("2025-03-09"))
#' }
#' @export
deck_title_slide <- function(deck, title = NULL, date = Sys.Date(),
                             country = NULL, compiled_by = NULL,
                             subtitle = NULL, corner = NULL,
                             corner_box = NULL, corner_colour = NULL,
                             config = deck_config_get()) {
  title <- title %||% .cfg_required("title", config)
  country <- country %||% config$country
  compiled_by <- compiled_by %||% config$cover_footer

  band_line <- function(text, size, bold, align = "center",
                        colour = config$cover_band_colour) {
    .text_run(
      text, size = size, colour = colour, font = config$font, bold = bold,
      align = align
    )
  }

  date_line <- subtitle %||% paste(
    c(country, paste(config$cover_date_lead, .format_long_date(date))),
    collapse = ", "
  )

  deck <- deck |>
    officer::add_slide(layout = config$layouts$cover, master = config$master) |>
    officer::ph_with(
      value = band_line(title, config$cover_title_pt, bold = TRUE),
      location = .box_location(config$cover_title_box)
    ) |>
    officer::ph_with(
      value = band_line(date_line, config$cover_date_pt, bold = FALSE),
      location = .box_location(config$cover_date_box)
    )

  if (!is.null(compiled_by)) {
    deck <- officer::ph_with(
      deck,
      value = band_line(compiled_by, config$cover_footer_pt, bold = FALSE),
      location = .box_location(config$cover_footer_box)
    )
  }

  if (is.null(corner)) {
    return(deck)
  }

  if (is.null(corner_box)) {
    cli::cli_abort("{.arg corner_box} is required with {.arg corner}.")
  }

  officer::ph_with(
    deck,
    value = band_line(
      corner, config$cover_footer_pt, bold = TRUE, align = "left",
      colour = corner_colour %||% config$cover_band_colour
    ),
    location = .box_location(corner_box)
  )
}

# section dividers -------------------------------------------------------------

# TRUE for a section that sits inside another
#' @noRd
.section_is_child <- function(section, sections) {
  parent <- sections$parent[sections$key == section]

  length(parent) == 1 && !is.na(parent)
}

#' Number of a Top-Level Section
#'
#' Returns the number a section divider carries. Only top-level sections
#' are counted. Without `deck`, the number is the section's place in the
#' configuration. With `deck`, it is one more than the number of top-level
#' dividers the deck already holds, so a section left out of a build
#' leaves no gap in the numbering.
#'
#' @param section Section key.
#' @param deck An `rpptx` object, read before the divider is added.
#'   Default `NULL`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return An integer.
#'
#' @examples
#' sections <- data.frame(
#'   key = c("epi", "afp", "campaigns"),
#'   label = c("Epidemiology", "AFP surveillance", "Campaigns"),
#'   parent = c(NA, "epi", NA)
#' )
#' deck_section_number("campaigns", config = deck_config(sections = sections))
#' @export
deck_section_number <- function(section, deck = NULL,
                                config = deck_config_get()) {
  sections <- .sections(config)
  .check_section(section, sections)

  top <- dplyr::filter(sections, is.na(.data$parent))

  if (is.null(deck)) {
    return(which(top$key == section))
  }

  pages <- deck_page_labels(deck, config)

  sum(!is.na(pages$divider) & pages$divider %in% top$label) + 1L
}

#' Add a Section Divider
#'
#' Adds the divider page that opens a section. A top-level section takes
#' the numbered layout and a number read off the deck. A section inside
#' another takes the subsection layout, with a title and no number.
#'
#' @param deck An `rpptx` object.
#' @param section Section key.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the divider added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_section_slide(deck, "campaigns")
#' }
#' @export
deck_section_slide <- function(deck, section, config = deck_config_get()) {
  sections <- .sections(config)
  .check_section(section, sections)

  label <- sections$label[sections$key == section]

  if (.section_is_child(section, sections)) {
    return(
      deck |>
        officer::add_slide(
          layout = config$layouts$subsection, master = config$master
        ) |>
        officer::ph_with(
          value = label, location = .label_location("section_title", config)
        )
    )
  }

  # read off the deck before the page goes on, so the section cannot count
  # its own divider
  number <- deck_section_number(section, deck, config)

  deck |>
    officer::add_slide(
      layout = config$layouts$section, master = config$master
    ) |>
    officer::ph_with(
      value = paste0(config$section_prefix, " ", number),
      location = .label_location("section", config)
    ) |>
    officer::ph_with(
      value = label, location = .label_location("section_title", config)
    )
}

#' Add the Closing Page
#'
#' Adds the page a deck ends on, on the divider layout, with a title and
#' no number.
#'
#' @param deck An `rpptx` object.
#' @param title Text of the page. Default `NULL`, the `closing_title`
#'   setting of `config`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the closing page added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_thanks_slide(deck)
#' }
#' @export
deck_thanks_slide <- function(deck, title = NULL,
                              config = deck_config_get()) {
  deck |>
    officer::add_slide(
      layout = config$layouts$closing, master = config$master
    ) |>
    officer::ph_with(
      value = title %||% config$closing_title,
      location = .label_location("section_title", config)
    )
}

# page footer ------------------------------------------------------------------

#' Write the Source Line of a Page
#'
#' Builds the line that says what a page is built from: the data sources
#' in the order given, then any clause about how the page was built, then
#' any note the page computes, then the date the data were last updated.
#'
#' @param sources Character vector of keys. Each is a name in `labels` or
#'   in `clauses`.
#' @param as_of Text appended as a final sentence, such as the date of
#'   the last update. Default `NULL`.
#' @param labels Named character vector of source labels. Default `NULL`,
#'   the `source_labels` setting of `config`.
#' @param clauses Named character vector of clauses. Default `NULL`, the
#'   `source_clauses` setting of `config`.
#' @param notes Character vector of further clauses. Default `NULL`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return A single string.
#'
#' @examples
#' deck_source_line(
#'   sources = c("polis", "survey"),
#'   as_of = "Last updated: 9 March 2025",
#'   labels = c(polis = "POLIS", survey = "Post-campaign survey")
#' )
#' @export
deck_source_line <- function(sources, as_of = NULL, labels = NULL,
                             clauses = NULL, notes = NULL,
                             config = deck_config_get()) {
  labels <- labels %||% .cfg_required("source_labels", config)
  clauses <- clauses %||% config$source_clauses

  known <- c(names(labels), names(clauses))
  unknown <- setdiff(sources, known)

  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown page source{?s} {.val {unknown}}.",
      "i" = "One of {.val {known}}."
    ))
  }

  named <- labels[intersect(sources, names(labels))]
  said <- c(clauses[intersect(sources, names(clauses))], notes)

  line <- paste0(config$source_lead, paste(named, collapse = ", "))

  if (length(said) > 0) {
    line <- paste0(line, "; ", paste(said, collapse = "; "))
  }

  if (!is.null(as_of)) {
    line <- paste0(line, ". ", as.character(as_of))
  }

  line
}

#' Write the Foot Strip of a Page
#'
#' Fills the strip at the foot of a page: the section on the left, and on
#' the right what the page is built from and when. A page that names no
#' source stops the build, because a slide with no source line reads as
#' though it needs none.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param section Section key.
#' @param label Text for the left of the strip. Default `NULL`, the label
#'   of `section` in the configuration.
#' @param sources Character vector of source keys, passed to
#'   [deck_source_line()]. Required.
#' @param as_of Text appended to the source line. Default `NULL`.
#' @param notes Character vector of further clauses. Default `NULL`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the foot strip filled.
#'
#' @examples
#' \dontrun{
#' deck <- deck_page_footer(
#'   deck, "campaigns", sources = "polis", as_of = "Last updated: 9 March"
#' )
#' }
#' @export
deck_page_footer <- function(deck, section, label = NULL, sources = NULL,
                             as_of = NULL, notes = NULL,
                             config = deck_config_get()) {
  if (is.null(sources)) {
    cli::cli_abort(c(
      "The {.val {section}} page footer names no source.",
      "i" = "Pass {.arg sources}, keys from the {.field source_labels}
             setting."
    ))
  }

  if (is.null(label)) {
    sections <- .sections(config)
    label <- sections$label[sections$key == section]
  }

  line <- deck_source_line(sources, as_of, notes = notes, config = config)
  limit <- config$foot_source_max_chars

  if (nchar(line) > limit) {
    cli::cli_alert_warning(
      "Source line runs {nchar(line)} characters against a foot strip
       holding about {limit}; name fewer sources"
    )
  }

  deck |>
    officer::ph_with(
      value = as.character(label),
      location = .label_location("section", config)
    ) |>
    officer::ph_with(
      value = deck_subtitle(
        line,
        size = config$foot_source_pt,
        colour = config$foot_source_colour,
        align = "right",
        bold = TRUE,
        config = config
      ),
      location = .box_location(config$foot_source_box, config$labels$source)
    )
}
