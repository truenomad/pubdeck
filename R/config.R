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
      closing = "section_page_main",
      text_page = "section2_overview_maps"
    ),
    labels = list(
      page_title = "page_title",
      section_title = "section_title",
      section = "section",
      source = "page_source",
      page_subtitle = "page_subtitle",
      plot = "plot",
      cover_title = "main_title",
      cover_subtitle = "subtitle",
      cover_author = "author"
    ),
    shape_labels = list(
      text_box = "cover",
      pending = "figure_pending",
      panel = "panel",
      panel_fill_suffix = "_bg",
      bullets = "bullets",
      message = "message",
      callout = "callout",
      rule_note = "next",
      ellipse = "highlight",
      hrule = "rule",
      kpi = "kpi",
      table = "table"
    ),
    font = "Arial",
    font_semibold = "Source Sans 3 Semibold",
    use_semibold = FALSE,
    path_root = NULL,
    # wording
    sections = NULL,
    section_prefix = "Section",
    section_numbered = TRUE,
    title = NULL,
    country = NULL,
    cover_date_lead = "report through",
    cover_footer = NULL,
    cover_subtitle = NULL,
    cover_author = NULL,
    closing_title = "Thank you!",
    contents_title = "Contents",
    contents_page = 2L,
    contents_skip = character(0),
    contents_collapsed = character(0),
    contents_dropped = character(0),
    contents_standalone = character(0),
    source_labels = NULL,
    source_clauses = character(0),
    source_lead = "Source: ",
    pending_lead = "Figure pending: ",
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
    cover_subtitle_pt = 14,
    cover_subtitle_colour = "#9DC3E6",
    cover_subtitle_box = list(
      left = 2.947, top = 3.93, width = 7.439, height = 0.296
    ),
    cover_author_pt = 14,
    cover_author_colour = "#9DC3E6",
    cover_author_box = list(
      left = 2.848, top = 4.226, width = 6.704, height = 0.337
    ),
    # page text and foot strip
    subtitle_pt = 11,
    subtitle_colour = "#4A4A48",
    page_subtitle_pt = 14,
    page_subtitle_colour = "#366092",
    page_subtitle_weight = "regular",
    page_subtitle_box = list(
      left = 0.41, top = 0.74, width = 10.64, height = 0.58
    ),
    page_subtitle_max_chars = 90L,
    page_title_pt = 26,
    page_title_colour = "#156082",
    page_title_weight = "bold",
    page_title_box = list(
      left = 0.41, top = 0.2, width = 10.64, height = 0.52
    ),
    page_title_max_chars = 55L,
    heading_fill = 0.92,
    box_inset = 0.1,
    emphasis_mark = "**",
    foot_source_box = list(
      left = 2.62, top = 7.23, width = 10.71, height = 0.27
    ),
    foot_source_pt = 8,
    foot_source_colour = "#FFFFFF",
    foot_source_weight = "bold",
    foot_source_max_chars = 150L,
    foot_section_box = NULL,
    foot_section_pt = 12,
    foot_section_colour = "#FFFFFF",
    foot_section_weight = "regular",
    # page builders: shared text colours
    body_colour = "#4A4A48",
    heading_colour = "#1F497D",
    accent_colour = "#009ADE",
    strong_colour = "#0C2A5E",
    muted_colour = "#6B6B6B",
    # page builders: figures and text boxes
    content_box = list(
      left = 0.41, top = 1.45, width = 12.51, height = 5.65
    ),
    plot_top_min = 1.7,
    column_gap = 0.25,
    pending_fill = "#F2F5F7",
    pending_pt = 10,
    pending_page_pt = 12,
    text_box_fill = "#FFFFFF",
    text_box_colour = "#4A4A48",
    text_box_pt = 12,
    # page builders: panels and bullets
    panel_fill = "#F2F5F7",
    panel_pt = 16,
    panel_marker = "\u25aa ",
    panel_padding = list(left = 8, right = 8, top = 3, bottom = 3),
    heading_step_pt = 2,
    plain_line_pattern = "^\u2192|:$",
    bullet_pt = 16,
    bullet_space_after = 3,
    bullet_group_gap = 10,
    bullet_block_gap = 0.5,
    bullet_mark = "\u2022\t",
    bullet_char = "\u2022",
    bullet_hang = 0.25,
    sub_prefix = "- ",
    sub_mark = "\u2013\t",
    sub_char = "\u2013",
    sub_indent = 0.3,
    sub_hang = 0.18,
    sub_pt_drop = 1,
    message_pt = 16,
    # page builders: callouts, rules and marks
    callout_fill = "#EAF4FB",
    callout_label_pt = 12,
    callout_text_pt = 17,
    callout_padding = list(
      left = 12, right = 12, top = 6, bottom = 2, text_bottom = 8
    ),
    rule_label_pt = 10,
    rule_text_pt = 18,
    rule_width = 2.5,
    rule_padding = list(left = 12, right = 4, top = 2, bottom = 4),
    hrule_weight = 1,
    hrule_geometry = list(row_height = 0.02, box_height = 0.05, pt = 1),
    ellipse_lwd = 1.5,
    ellipse_geometry = list(radius = 0.49, points = 121L),
    # page builders: indicators and tables
    kpi_label_pt = 12,
    kpi_note_pt = 11,
    kpi_row_pt = 15,
    kpi_change_pt = 26,
    kpi_padding = list(left = 0, right = 0, top = 1, bottom = 1),
    slide_table_pt = 11,
    slide_table_rule_colour = "#D0D5DB",
    slide_table_rule_width = 0.75,
    slide_table_head_rule_width = 1,
    slide_table_padding = list(left = 5, right = 5, top = 2, bottom = 2),
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
    contents_fill = list(
      gap = 0.6,
      row_height = 0.22,
      section_lead = 0.14,
      section_gap = 0.08,
      top_offset = 0,
      indent = 0.3,
      number_width = 0.45
    ),
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
    note_measure = "typeface",
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
    save_grob = TRUE,
    png_device = "ragg",
    compress_png = TRUE,
    vector_max_bytes = 5e5,
    vector_scale_tolerance = 0.1,
    # colours
    palette = NULL,
    ramps = NULL,
    alarm = NULL,
    map_colours = NULL,
    # theme
    theme_family = "",
    theme_title_position = "plot",
    theme_plot_margin = list(t = 8, r = 10, b = 6, l = 8),
    theme_panel_margin = list(t = 4, r = 6, b = 2, l = 4),
    theme_frame = FALSE,
    theme_frame_colour = "black",
    theme_frame_linewidth = 0.43
  )
}

# settings whose fields are named by the report, so a new field is added
#' @noRd
.open_settings <- c("ramps")

# a nested setting is merged into its default, so one field can be changed
# without restating the rest; a table or a vector replaces the default
#' @noRd
.merge_setting <- function(default, value, open = FALSE) {
  nested <- is.list(default) && !is.data.frame(default) &&
    is.list(value) && !is.data.frame(value)

  if (!nested) {
    return(value)
  }

  unknown <- setdiff(names(value), names(default))

  if (length(unknown) > 0 && !open) {
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
#' most often as arguments of their own. The page builders also take any
#' setting by name for one call, as in
#' `deck_panel(deck, geom, heading, lines, panel_marker = "- ")`.
#'
#' @details
#' Settings fall into eight groups.
#'
#' **Template.** `template` is the path to the `.pptx` template and has no
#' default. `master` names its slide master. `layouts` names the layouts
#' used for the `cover`, a numbered `section` divider, a `subsection`
#' divider, the `closing` page and a `text_page` whose content is placed
#' in boxes. `labels` names the placeholders and shapes the builders
#' address: the `page_title`, the `page_subtitle`, the `section_title`,
#' the `section` strip at the foot of a page, the `source` line the footer
#' draws, the `plot` box of a figure layout, and the `cover_title`,
#' `cover_subtitle` and `cover_author` lines of [deck_cover_slide()].
#' `shape_labels` holds the label a block builder gives its shape when a
#' call names none: `text_box`, `pending`, `panel`, `bullets`, `message`,
#' `callout`, `rule_note`, `ellipse`, `hrule`, `kpi` and `table`, with
#' `panel_fill_suffix` added to a panel's label to name its fill. `font`
#' is the typeface. `font_semibold` is the family of its semibold weight,
#' and `use_semibold` says whether text in that weight uses the family or
#' is set in bold. `path_root` is the folder a file path is written
#' relative to in a pending panel or a message, and is the working
#' directory when `NULL`.
#'
#' **Wording.** `sections` is a data frame with one row per section and the
#' columns `key` and `label`. An optional `parent` column holds the key of
#' the section a section sits inside, and `NA` for a top-level section.
#' `section_prefix` is the word before a section number, and
#' `section_numbered` says whether a top-level divider carries a number.
#' `title`, `country`, `cover_date_lead` and `cover_footer` fill the cover
#' [deck_title_slide()] draws, and `title`, `cover_subtitle` and
#' `cover_author` fill the one [deck_cover_slide()] draws. `closing_title`
#' and `contents_title` title those pages, `contents_page` is the slide
#' the contents sits on, and `contents_skip` lists foot-strip labels the
#' contents leaves out. `contents_collapsed`, `contents_dropped` and
#' `contents_standalone` steer [deck_contents_title_entries()]: sections
#' listed without their pages, sections left out, and page titles listed
#' as sections of their own. `source_labels` is a named character vector
#' of the data sources a page can cite, `source_clauses` holds named
#' clauses about how a page was built, and `source_lead` opens the line.
#' `pending_lead` opens the text of a panel that stands in for a figure
#' not yet drawn.
#'
#' **Cover and page text.** `cover_title_pt`, `cover_date_pt`,
#' `cover_footer_pt` and `cover_band_colour` style the cover.
#' `cover_title_box`, `cover_date_box` and `cover_footer_box` place its
#' lines, each a list of `left`, `top`, `width` and `height` in inches.
#' `cover_subtitle_pt`, `cover_subtitle_colour`, `cover_subtitle_box`,
#' `cover_author_pt`, `cover_author_colour` and `cover_author_box` style
#' and place the subtitle and author of [deck_cover_slide()].
#' `subtitle_pt` and `subtitle_colour` style a line from
#' [deck_subtitle()]. `page_title_pt`, `page_title_colour`,
#' `page_title_weight` and `page_title_box` style and place the title a
#' page builder writes, and `page_subtitle_pt`, `page_subtitle_colour`,
#' `page_subtitle_weight` and `page_subtitle_box` its subtitle.
#' `page_title_max_chars`, `page_subtitle_max_chars`, `heading_fill`, the
#' share of its box a heading may fill, and `box_inset`, the text inset
#' on each side of a box in inches, decide when [deck_check_heading()]
#' reports a heading that may wrap. `emphasis_mark` brackets text to
#' emphasise. `foot_source_box`, `foot_source_pt`, `foot_source_colour`,
#' `foot_source_weight` and `foot_source_max_chars` place and style the
#' source line. `foot_section_box` is `NULL` where the section label goes
#' into the template's placeholder; given a box, the label is drawn there
#' at `foot_section_pt`, `foot_section_colour` and `foot_section_weight`.
#' A weight is `"regular"`, `"semibold"` or `"bold"`.
#'
#' **Page builders.** `body_colour`, `heading_colour`, `accent_colour`,
#' `strong_colour` and `muted_colour` are the text colours the builders
#' share: body text, headings, labels and rules, the sentence a callout
#' carries, and secondary lines. `content_box` is the area between the
#' subtitle and the foot strip. `plot_top_min` is the line no figure
#' starts above, in inches, and `column_gap` the gap [deck_columns()]
#' leaves. `pending_fill` fills a pending panel, set at `pending_pt` in a
#' box and at `pending_page_pt` on a figure page. `text_box_fill`,
#' `text_box_colour` and `text_box_pt` style [deck_text_box()].
#' `panel_fill`, `panel_pt`, `panel_marker` and `panel_padding` style
#' [deck_panel()]. `heading_step_pt` is how much larger a heading is than
#' its text, and `plain_line_pattern` is a regular expression for a line
#' that takes no bullet. `bullet_pt`, `bullet_space_after`,
#' `bullet_group_gap` and `bullet_block_gap`, the space between blocks as
#' a share of the text size, style [deck_bullets()]. `bullet_mark` marks
#' a line for a native bullet until the slide is written, `bullet_char`
#' is the bullet drawn and `bullet_hang` its hang in inches. `sub_prefix`
#' opens a sub-bullet in the input, and `sub_mark`, `sub_char`,
#' `sub_indent`, `sub_hang` and `sub_pt_drop` set its mark, bullet,
#' indent, hang and drop in size. `message_pt` sizes [deck_message()].
#' `callout_fill`, `callout_label_pt`, `callout_text_pt` and
#' `callout_padding` style [deck_callout()], and `rule_label_pt`,
#' `rule_text_pt`, `rule_width` and `rule_padding` style
#' [deck_rule_note()]. `hrule_weight` and `hrule_geometry`, a list of
#' `row_height`, `box_height` and `pt`, draw [deck_hrule()].
#' `ellipse_lwd` and `ellipse_geometry`, a list of `radius` and `points`,
#' draw [deck_ellipse()]. `kpi_label_pt`, `kpi_note_pt`, `kpi_row_pt`,
#' `kpi_change_pt` and `kpi_padding` style [deck_kpi()].
#' `slide_table_pt`, `slide_table_rule_colour`, `slide_table_rule_width`,
#' `slide_table_head_rule_width` and `slide_table_padding` style
#' [deck_table()]. A padding is a list of `left`, `right`, `top` and
#' `bottom` in points, and `callout_padding` also holds `text_bottom`,
#' the space under the sentence.
#'
#' **Contents and explainer boxes.** `contents_slide` is a list holding the
#' `layout`, the `box` placeholder, `n_cols`, and the spacing fields `gap`,
#' `row_height`, `section_lead`, `section_pad`, `indent` and `number_width`
#' in inches. `contents_fill` holds the spacing [deck_contents_fill()]
#' uses: `gap`, `row_height`, `section_lead`, `section_gap`, `top_offset`,
#' `indent` and `number_width`. The other `contents_*` settings style the
#' rows of both. The `note_*` settings style explainer boxes, and
#' `page_bg` is the page background. `note_measure` is how
#' [deck_note_height()] measures the text of a box: `"typeface"` wraps it
#' in `font`, and `"device"` measures it in the sans face of the graphics
#' device.
#'
#' **Tables and figures.** `table_header`, `table_row_padding`, `table_bar`,
#' `table_bar_track` and `spark_min_span` style tables. `dpi` and `map_dpi`
#' are the resolutions for charts and maps; a `map_dpi` of `NULL` saves
#' maps at `dpi`. `save_grob` says whether [ggsave_pub()] keeps the drawn
#' chart beside its PNG. `png_device` is the device [ggsave_pub()] draws a
#' PNG with: `"ragg"` names `ragg::agg_png()`, and `"default"` leaves the
#' choice to 'ggplot2'. `compress_png` says whether [compress_pub()]
#' compresses a PNG, and so whether [ggsave_pub()], [gtsave_pub()] and
#' [trim_png()] compress the files they write; it is `TRUE` unless set.
#' `vector_max_bytes` and `vector_scale_tolerance` decide when a chart is
#' placed as native shapes.
#'
#' **Colours.** Each colour setting is `NULL` unless set, which means the
#' built-in colours. `palette` is a character vector of categorical
#' colours that replaces the one [pub_palette()] returns, the first colour
#' being the focal series. `ramps` is a named list of character vectors
#' read by [pub_ramp()]: a name the package already has replaces that
#' ramp, and a new name adds a ramp. `alarm` is the single colour
#' [pub_alarm()] returns, which [move_cell()] also takes. `map_colours` is
#' a named character vector merged into the colours [pub_map_colours()]
#' returns, so a name given replaces that colour and a new name adds one.
#'
#' **Theme.** `theme_family` is the typeface of [theme_pub()], and the
#' device default when empty. `theme_title_position` is `"plot"` or
#' `"panel"`. `theme_plot_margin` and `theme_panel_margin` are the plot
#' margins of [theme_pub()] and [theme_pub_panel()], each a list of `t`,
#' `r`, `b` and `l` in points. `theme_frame` draws a border round the
#' panel, in `theme_frame_colour` at `theme_frame_linewidth`.
#'
#' A nested setting such as `contents_slide` or `layouts` is merged into
#' its default, so one field can be changed alone. `ramps` given over a
#' `base` that holds ramps is merged the same way, and may add a name.
#'
#' The default layout names, placeholder labels, boxes, sizes and colours
#' describe the templates the builders were first written for. A report
#' with its own template sets its own, and [deck_check_template()]
#' confirms the layouts and placeholders agree.
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

  merged <- purrr::imap(overrides, \(value, name) {
    .merge_setting(config[[name]], value, open = name %in% .open_settings)
  })

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

# the configuration for one call: settings passed by name replace its own
#' @noRd
.call_config <- function(config, ...) {
  .check_config(config)

  if (...length() == 0) {
    return(config)
  }

  deck_config(..., base = config)
}

# a nested setting given for one call, merged into the configuration's
#' @noRd
.nested <- function(value, name, config) {
  if (is.null(value)) {
    return(config[[name]])
  }

  .merge_setting(config[[name]], value)
}

# a path as it is written in a message or on a slide
#' @noRd
.path_rel <- function(path, config) {
  fs::path_rel(path, start = config$path_root %||% ".")
}

# the sections table, checked for the columns the builders read
#' @noRd
.sections <- function(config = deck_config_get()) {
  sections <- .cfg_required("sections", config)
  missing <- setdiff(c("key", "label"), names(sections))

  if (length(missing) > 0) {
    cli::cli_abort(
      "The {.field sections} setting lacks column{?s} {.val {missing}}.",
      call = rlang::caller_env()
    )
  }

  # a table with no parent column holds top-level sections only
  if (!"parent" %in% names(sections)) {
    sections <- dplyr::mutate(sections, parent = NA_character_)
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
