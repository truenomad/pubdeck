# the configuration in force for the session
.state <- new.env(parent = emptyenv())

# defaults ---------------------------------------------------------------------

#' @noRd
.config_defaults <- function() {
  list(
    # template
    template = NULL,
    master = "Office Theme",
    layouts = list(
      cover = "main_title",
      section = "section_page_main",
      subsection = "section_page",
      closing = "section_page_main"
    ),
    labels = list(
      page_title = "page_title",
      section_title = "section_title",
      section = "section",
      source = "page_source"
    ),
    font = "Arial",
    # wording
    sections = NULL,
    section_prefix = "Section",
    title = NULL,
    country = NULL,
    cover_date_lead = "report through",
    cover_footer = NULL,
    closing_title = "Thank you!",
    contents_title = "Contents",
    contents_page = 2L,
    contents_skip = character(0),
    source_labels = NULL,
    source_clauses = character(0),
    source_lead = "Source: ",
    # cover
    cover_title_pt = 28,
    cover_date_pt = 18,
    cover_footer_pt = 9.5,
    cover_band_colour = "#FFFFFF",
    cover_title_box = list(
      left = 2.536, top = 3.218, width = 10.861, height = 0.74
    ),
    cover_date_box = list(
      left = 3.859, top = 3.828, width = 7.434, height = 0.4
    ),
    cover_footer_box = list(
      left = 2.03, top = 7.235, width = 7.434, height = 0.34
    ),
    # page text and foot strip
    subtitle_pt = 11,
    subtitle_colour = "#4A4A48",
    page_subtitle_pt = 14,
    page_subtitle_colour = "#366092",
    foot_source_box = list(
      left = 2.62, top = 7.23, width = 10.71, height = 0.27
    ),
    foot_source_pt = 8,
    foot_source_colour = "#FFFFFF",
    foot_source_max_chars = 150L,
    # contents page
    contents_slide = list(
      layout = "summary_text",
      box = "Text Placeholder 5",
      n_cols = 2L,
      gap = 0.5,
      row_height = 0.24,
      section_lead = 0.22,
      section_pad = 0.06,
      indent = 0.3,
      number_width = 0.45
    ),
    contents_section_pt = 14,
    contents_run_pt = 11.5,
    contents_section_colour = "#2E4053",
    contents_run_colour = "#4A4A48",
    contents_number_colour = "#A9552B",
    contents_run_marker = "\u25aa",
    contents_section_band = "#F2F5F7",
    # explainer boxes
    note_pt = 8.5,
    note_pad_x_pt = 8,
    note_pad_y_pt = 5,
    note_line_spacing = 1.15,
    note_fill = "#F2F5F7",
    note_accent = "#2E4053",
    note_body = "#4A4A48",
    note_bullet = "\u25aa",
    note_subbullet = "\u00b7",
    note_indent = 14,
    page_bg = "#FFFFFF",
    # tables
    table_header = "#2E4053",
    table_row_padding = 6L,
    table_bar = "#A9552B",
    table_bar_track = "#EDEDEA",
    spark_min_span = 0.35,
    # figures
    dpi = 300,
    map_dpi = 200,
    vector_max_bytes = 5e5,
    vector_scale_tolerance = 0.1
  )
}

# a nested setting is merged into its default, so one field can be changed
# without restating the rest; a table or a vector replaces the default
#' @noRd
.merge_setting <- function(default, value) {
  nested <- is.list(default) && !is.data.frame(default) &&
    is.list(value) && !is.data.frame(value)

  if (!nested) {
    return(value)
  }

  unknown <- setdiff(names(value), names(default))

  if (length(unknown) > 0) {
    cli::cli_abort("Unknown field{?s} {.val {unknown}} in a nested setting.")
  }

  utils::modifyList(default, value)
}

# constructor ------------------------------------------------------------------

#' Build a Deck Configuration
#'
#' Collects the wording and styling a deck uses into one object. Every
#' setting has a default, so a call names only what differs. Nothing in
#' the package is fixed to one report: each builder takes a `config`
#' argument, which defaults to the configuration in force set with
#' [deck_config_set()], and most builders also take the settings they use
#' most often as arguments of their own.
#'
#' @details
#' Settings fall into five groups.
#'
#' **Template.** `template` is the path to the `.pptx` template and has no
#' default. `master` names its slide master. `layouts` names the layouts
#' used for the `cover`, a numbered `section` divider, a `subsection`
#' divider and the `closing` page. `labels` names the placeholders for the
#' `page_title`, the `section_title`, the `section` strip at the foot of a
#' page and the `source` line the footer draws. `font` is the typeface.
#'
#' **Wording.** `sections` is a data frame with one row per section and the
#' columns `key`, `label` and `parent`, where `parent` is `NA` for a
#' top-level section. `section_prefix` is the word before a section number.
#' `title`, `country`, `cover_date_lead` and `cover_footer` fill the cover.
#' `closing_title` and `contents_title` title those pages, `contents_page`
#' is the slide the contents sits on, and `contents_skip` lists foot-strip
#' labels the contents leaves out. `source_labels` is a named character
#' vector of the data sources a page can cite, `source_clauses` holds named
#' clauses about how a page was built, and `source_lead` opens the line.
#'
#' **Cover and page text.** `cover_title_pt`, `cover_date_pt`,
#' `cover_footer_pt` and `cover_band_colour` style the cover.
#' `cover_title_box`, `cover_date_box` and `cover_footer_box` place its
#' lines, each a list of `left`, `top`, `width` and `height` in inches.
#' `subtitle_pt`, `subtitle_colour`, `page_subtitle_pt` and
#' `page_subtitle_colour` style subtitles. `foot_source_box`,
#' `foot_source_pt`, `foot_source_colour` and `foot_source_max_chars` place
#' and style the source line.
#'
#' **Contents and explainer boxes.** `contents_slide` is a list holding the
#' `layout`, the `box` placeholder, `n_cols`, and the spacing fields `gap`,
#' `row_height`, `section_lead`, `section_pad`, `indent` and `number_width`
#' in inches. The `contents_*` settings style its rows. The `note_*`
#' settings style explainer boxes, and `page_bg` is the page background.
#'
#' **Tables and figures.** `table_header`, `table_row_padding`, `table_bar`,
#' `table_bar_track` and `spark_min_span` style tables. `dpi` and `map_dpi`
#' are the resolutions for charts and maps. `vector_max_bytes` and
#' `vector_scale_tolerance` decide when a chart is placed as native shapes.
#'
#' A nested setting such as `contents_slide` or `layouts` is merged into
#' its default, so one field can be changed alone.
#'
#' The default layout names, placeholder labels and boxes describe the
#' template the package was first written for. A report with its own
#' template sets its own, and [deck_check_template()] confirms the two
#' agree.
#'
#' @param ... Named settings that replace the defaults.
#' @param base A configuration to start from in place of the defaults, so
#'   one report can vary another's settings. Default `NULL`.
#'
#' @return A list of class `pubdeck_config`.
#'
#' @examples
#' config <- deck_config(
#'   title = "Weekly review",
#'   country = "Afghanistan",
#'   contents_slide = list(n_cols = 3L)
#' )
#' config$contents_slide$n_cols
#' config$cover_title_pt
#'
#' # a second report that differs in one setting
#' annex <- deck_config(title = "Annex", base = config)
#' annex$country
#' @export
deck_config <- function(..., base = NULL) {
  overrides <- rlang::list2(...)
  known <- names(.config_defaults())

  if (!is.null(base)) {
    .check_config(base, arg = "base")
  }

  if (length(overrides) > 0 && !rlang::is_named(overrides)) {
    cli::cli_abort("Every setting passed to {.fn deck_config} must be named.")
  }

  unknown <- setdiff(names(overrides), known)

  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown setting{?s} {.val {unknown}}.",
      "i" = "See {.fn deck_config} for the settings a deck takes."
    ))
  }

  config <- unclass(base) %||% .config_defaults()

  merged <- purrr::imap(
    overrides, \(value, name) .merge_setting(config[[name]], value)
  )

  # assigned by single bracket so a setting given as NULL stays in the list
  config[names(merged)] <- merged

  structure(config, class = "pubdeck_config")
}

# stops unless the object came from deck_config()
#' @noRd
.check_config <- function(config, arg = "config") {
  if (!inherits(config, "pubdeck_config")) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be built with {.fn deck_config}.",
        "x" = "It has class {.cls {class(config)}}."
      ),
      call = rlang::caller_env()
    )
  }

  invisible(config)
}

# active configuration ---------------------------------------------------------

#' Set, Read or Temporarily Change the Deck Configuration in Force
#'
#' `deck_config_set()` makes a configuration the one the builders read by
#' default for the rest of the session. `deck_config_get()` returns it.
#' `with_deck_config()` runs code under a configuration and restores the
#' previous one afterwards, for a session that builds more than one
#' report. Until one is set, the defaults of [deck_config()] are in force.
#'
#' @param config A configuration built with [deck_config()].
#' @param code Code to run with `config` in force.
#'
#' @return `deck_config_set()` returns the configuration it replaced,
#'   invisibly, so it can be restored. `deck_config_get()` returns the
#'   configuration in force. `with_deck_config()` returns the value of
#'   `code`.
#'
#' @examples
#' previous <- deck_config_set(deck_config(title = "Weekly review"))
#' deck_config_get()$title
#' deck_config_set(previous)
#'
#' with_deck_config(
#'   deck_config(title = "Annex"),
#'   deck_config_get()$title
#' )
#' @export
deck_config_set <- function(config) {
  .check_config(config)

  previous <- deck_config_get()
  .state$config <- config

  invisible(previous)
}

#' @rdname deck_config_set
#' @export
deck_config_get <- function() {
  .state$config %||% deck_config()
}

#' @rdname deck_config_set
#' @export
with_deck_config <- function(config, code) {
  previous <- deck_config_set(config)
  on.exit(deck_config_set(previous), add = TRUE)

  force(code)
}

# reading settings -------------------------------------------------------------

# one setting of a configuration
#' @noRd
.cfg <- function(name, config = deck_config_get()) {
  config[[name]]
}

# a setting with no default, which a builder cannot work without
#' @noRd
.cfg_required <- function(name, config = deck_config_get()) {
  value <- config[[name]]

  if (is.null(value)) {
    cli::cli_abort(
      c(
        "The deck configuration has no {.field {name}}.",
        "i" = "Set it with {.code deck_config({name} = ...)}."
      ),
      call = rlang::caller_env()
    )
  }

  value
}

# the sections table, checked for the columns the builders read
#' @noRd
.sections <- function(config = deck_config_get()) {
  sections <- .cfg_required("sections", config)
  missing <- setdiff(c("key", "label", "parent"), names(sections))

  if (length(missing) > 0) {
    cli::cli_abort(
      "The {.field sections} setting lacks column{?s} {.val {missing}}.",
      call = rlang::caller_env()
    )
  }

  sections
}

# stops on a section key the configuration does not hold
#' @noRd
.check_section <- function(section, sections) {
  if (!section %in% sections$key) {
    cli::cli_abort(
      c(
        "Unknown deck section {.val {section}}.",
        "i" = "One of {.val {sections$key}}."
      ),
      call = rlang::caller_env()
    )
  }

  invisible(section)
}
