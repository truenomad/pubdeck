# shared pieces ----------------------------------------------------------------

# English Metric Units to the inch, the unit a slide measures indents in
.emu_per_inch <- 914400

# the namespaces a slide's shapes and their text are written in
.slide_ns <- c(
  a = "http://schemas.openxmlformats.org/drawingml/2006/main",
  p = "http://schemas.openxmlformats.org/presentationml/2006/main"
)

# inches as a whole number of EMU
#' @noRd
.emu <- function(inches) {
  round(inches * .emu_per_inch)
}

# a whole number as text, written without an exponent
#' @noRd
.whole <- function(x) {
  sprintf("%.0f", x)
}

# pads the cells of a flextable from a padding setting, in points
#' @noRd
.pad_cells <- function(ft, padding, part = "body") {
  flextable::padding(
    ft,
    padding.left = padding$left, padding.right = padding$right,
    padding.top = padding$top, padding.bottom = padding$bottom,
    part = part
  )
}

# a one-column flextable with no header and no borders
#' @noRd
.text_rows <- function(text) {
  tibble::tibble(text = text) |>
    flextable::flextable() |>
    flextable::delete_part(part = "header") |>
    flextable::border_remove()
}

# the fixed width a block finishes with
#' @noRd
.fix_width <- function(ft, width) {
  ft |>
    flextable::width(width = width) |>
    flextable::set_table_properties(layout = "fixed")
}

# panels -----------------------------------------------------------------------

#' Place a Filled Panel with a Heading and Lines
#'
#' Places a filled panel: a heading row in the emphasis weight, then one
#' row per line. A line that matches the `plain_line_pattern` setting, by
#' default one that starts with an arrow or ends with a colon, stands as
#' written, and so does an empty line. Any other line takes the
#' `panel_marker`. The fill is drawn as a box of its own, so panels in a
#' row reach the same depth whatever their text.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param geom The box to fill, a list of `left`, `top`, `width` and
#'   `height` in inches.
#' @param heading Heading of the panel.
#' @param lines Character vector of lines.
#' @param fill Fill colour. Default `NULL`, the `panel_fill` setting of
#'   `config`.
#' @param size Font size of the lines in points. Default `NULL`, the
#'   `panel_pt` setting of `config`. The heading is larger by the
#'   `heading_step_pt` setting.
#' @param label Label given to the text. Default `NULL`, the `panel`
#'   field of the `shape_labels` setting. The fill takes the same label
#'   with the `panel_fill_suffix` field added.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `panel_marker`, `panel_padding`,
#'   `heading_colour` or `body_colour`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the panel added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_panel(
#'   deck, list(left = 0.5, top = 1.7, width = 4, height = 3),
#'   heading = "What changed",
#'   lines = c("Coverage rose in the East", "Two districts were missed")
#' )
#' }
#' @export
deck_panel <- function(deck, geom, heading, lines, fill = NULL, size = NULL,
                       label = NULL, ..., config = deck_config_get()) {
  config <- .call_config(config, ...)
  size <- size %||% config$panel_pt
  label <- label %||% config$shape_labels$panel

  plain <- grepl(config$plain_line_pattern, lines) | lines == ""
  marked <- dplyr::if_else(plain, lines, paste0(config$panel_marker, lines))

  # the fill is drawn as its own box so every panel reaches the same depth
  deck <- deck_text_box(
    deck, geom,
    fill = fill %||% config$panel_fill,
    label = paste0(label, config$shape_labels$panel_fill_suffix),
    config = config
  )

  block <- .text_rows(c(heading, marked)) |>
    flextable::font(fontname = config$font, part = "body") |>
    flextable::fontsize(size = size, part = "body") |>
    flextable::color(color = config$body_colour, part = "body") |>
    deck_flex_semibold(i = 1, config = config) |>
    flextable::color(i = 1, color = config$heading_colour, part = "body") |>
    flextable::fontsize(
      i = 1, size = size + config$heading_step_pt, part = "body"
    ) |>
    flextable::align(align = "left", part = "body") |>
    flextable::valign(valign = "top", part = "body") |>
    .pad_cells(config$panel_padding) |>
    .fix_width(geom$width)

  officer::ph_with(deck, value = block, location = .box_location(geom, label))
}

# bullets ----------------------------------------------------------------------

# one run of bullet text
#' @noRd
.bullet_run <- function(text, size, colour, weight, config) {
  officer::ftext(text, deck_fp_text(size, colour, weight, config = config))
}

# one paragraph of a bullet block, with the space below and above it
#' @noRd
.bullet_paragraph <- function(runs, space_after, top = 0) {
  spacing <- officer::fp_par(padding.bottom = space_after, padding.top = top)

  do.call(officer::fpar, c(runs, list(fp_p = spacing)))
}

# the paragraph for one line: marked for a bullet or a sub-bullet unless it
# stands as written, with the text between emphasis marks in another weight
#' @noRd
.bullet_line <- function(line, number, style) {
  config <- style$config
  is_sub <- startsWith(line, config$sub_prefix)

  text <- if (is_sub) {
    paste0(config$sub_mark, substring(line, nchar(config$sub_prefix) + 1))
  } else if (grepl(config$plain_line_pattern, line)) {
    line
  } else {
    paste0(config$bullet_mark, line)
  }

  size <- if (is_sub) style$size - config$sub_pt_drop else style$size
  colour <- if (is_sub) config$muted_colour else config$body_colour
  parts <- strsplit(text, config$emphasis_mark, fixed = TRUE)[[1]]

  runs <- purrr::imap(parts, \(part, i) {
    weight <- if (i %% 2 == 0) style$emphasis else "regular"
    .bullet_run(part, size, colour, weight, config)
  })

  # a line listed in gap_before opens a new group with space above it
  top <- if (number %in% style$gap_before) style$gap_pt else 0

  .bullet_paragraph(runs, style$space_after, top)
}

# the paragraphs of one block: a spacer after the first block, the heading
# when there is one, then the lines, numbered across blocks
#' @noRd
.bullet_block <- function(block, index, offset, style) {
  config <- style$config

  spacer <- if (index > 1) {
    gap <- .bullet_run(
      " ", style$size * config$bullet_block_gap, config$body_colour,
      "regular", config
    )
    list(.bullet_paragraph(list(gap), style$space_after))
  }

  heading <- if (!is.null(block$heading)) {
    title <- .bullet_run(
      block$heading, style$size + config$heading_step_pt,
      config$heading_colour, style$emphasis, config
    )
    list(.bullet_paragraph(list(title), style$space_after))
  }

  lines <- purrr::map2(
    block$lines, seq_along(block$lines),
    \(line, i) .bullet_line(line, offset + i, style)
  )

  c(spacer, heading, lines)
}

#' Place Bulleted Text
#'
#' Places plain bulleted text with native, hanging bullets. Each block is
#' a list with an optional `heading` and its `lines`. A line that opens
#' with the `sub_prefix` setting, by default a hyphen and a space, is a
#' sub-bullet. A line that matches the `plain_line_pattern` setting
#' stands as written with no bullet. Text between pairs of the emphasis
#' mark is set in the emphasis weight.
#'
#' The box starts one bullet hang to the left of `geom`, so the text and
#' not the bullet lines up with the page title.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param geom The box the text fills, a list of `left`, `top`, `width`
#'   and `height` in inches.
#' @param blocks A list of blocks, each a list with `lines`, a character
#'   vector, and optionally `heading`.
#' @param size Font size of the lines in points. Default `NULL`, the
#'   `bullet_pt` setting of `config`.
#' @param space_after Space below each paragraph in points. Default
#'   `NULL`, the `bullet_space_after` setting of `config`.
#' @param gap_before Numbers of the lines that open a new group, counted
#'   across blocks. Default `integer()`, none.
#' @param gap_pt Space above a line in `gap_before`, in points. Default
#'   `NULL`, the `bullet_group_gap` setting of `config`.
#' @param label Label given to the new shape. Default `NULL`, the
#'   `bullets` field of the `shape_labels` setting.
#' @param emphasis Weight of headings and of text between emphasis marks.
#'   Default `"semibold"`.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `bullet_hang`, `bullet_char`, `sub_indent`
#'   or `muted_colour`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the text added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_bullets(
#'   deck, list(left = 0.5, top = 1.7, width = 8, height = 4),
#'   blocks = list(
#'     list(
#'       heading = "Coverage",
#'       lines = c("Rose in **two** regions", "- by 4 points in the East")
#'     ),
#'     list(lines = "Two districts were missed")
#'   )
#' )
#' }
#' @export
deck_bullets <- function(deck, geom, blocks, size = NULL, space_after = NULL,
                         gap_before = integer(), gap_pt = NULL, label = NULL,
                         emphasis = "semibold", ...,
                         config = deck_config_get()) {
  config <- .call_config(config, ...)
  label <- label %||% config$shape_labels$bullets
  hang <- config$bullet_hang

  style <- list(
    size = size %||% config$bullet_pt,
    space_after = space_after %||% config$bullet_space_after,
    gap_before = gap_before,
    gap_pt = gap_pt %||% config$bullet_group_gap,
    emphasis = emphasis,
    config = config
  )

  blocks <- unname(blocks)
  line_counts <- purrr::map_int(blocks, \(block) length(block$lines))
  offsets <- cumsum(c(0L, utils::head(line_counts, -1)))

  paragraphs <- purrr::pmap(
    list(blocks, seq_along(blocks), offsets),
    \(block, index, offset) .bullet_block(block, index, offset, style)
  ) |>
    purrr::list_flatten()

  # the box starts one hang to the left, so the text lines up with the title
  deck <- officer::ph_with(
    deck,
    value = do.call(officer::block_list, paragraphs),
    location = officer::ph_location(
      left = geom$left - hang, top = geom$top,
      width = geom$width + hang, height = geom$height,
      newlabel = label
    )
  )

  deck_hang_bullets(deck, label, config = config)
}

# sets one paragraph's margin and, for a marked line, swaps the mark for a
# native bullet that hangs in the margin
#' @noRd
.hang_paragraph <- function(paragraph, config) {
  properties <- xml2::xml_find_first(paragraph, "./a:pPr", .slide_ns)

  if (inherits(properties, "xml_missing")) {
    xml2::xml_add_child(paragraph, "a:pPr", .where = 0)
    properties <- xml2::xml_find_first(paragraph, "./a:pPr", .slide_ns)
  }

  hang <- .emu(config$bullet_hang)

  # every paragraph starts one hang in, so headings, plain lines and bullet
  # text share a left edge
  xml2::xml_set_attr(properties, "marL", .whole(hang))
  xml2::xml_set_attr(properties, "indent", "0")

  first <- xml2::xml_find_first(paragraph, ".//a:t", .slide_ns)

  if (inherits(first, "xml_missing")) {
    return(invisible(paragraph))
  }

  text <- xml2::xml_text(first)
  is_sub <- startsWith(text, config$sub_mark)

  if (!startsWith(text, config$bullet_mark) && !is_sub) {
    return(invisible(paragraph))
  }

  mark <- if (is_sub) config$sub_mark else config$bullet_mark
  xml2::xml_text(first) <- substring(text, nchar(mark) + 1)

  if (is_sub) {
    xml2::xml_set_attr(
      properties, "marL", .whole(hang + .emu(config$sub_indent))
    )
    xml2::xml_set_attr(
      properties, "indent", paste0("-", .whole(.emu(config$sub_hang)))
    )
  } else {
    xml2::xml_set_attr(properties, "indent", paste0("-", .whole(hang)))
  }

  # officer writes buNone into every paragraph, and a second bullet setting
  # makes the file invalid
  xml2::xml_remove(xml2::xml_find_all(properties, "./a:buNone", .slide_ns))
  xml2::xml_add_child(properties, "a:buFont", typeface = config$font)
  xml2::xml_add_child(
    properties, "a:buChar",
    char = if (is_sub) config$sub_char else config$bullet_char
  )

  invisible(paragraph)
}

#' Turn Marked Paragraphs into Hanging Bullets
#'
#' Rewrites the paragraphs of a text shape on the current slide so that
#' every line of text starts at one left edge and the bullets hang in the
#' margin. A paragraph that opens with the `bullet_mark` or `sub_mark`
#' setting loses the mark and takes a native bullet. [deck_bullets()]
#' calls this for the text it places.
#'
#' @param deck An `rpptx` object, on the slide that holds the shape.
#' @param label Label of the text shape.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`. The bullets read `bullet_mark`, `bullet_char`,
#'   `bullet_hang`, `sub_mark`, `sub_char`, `sub_indent`, `sub_hang` and
#'   `font`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the shape's paragraphs rewritten.
#'
#' @examples
#' \dontrun{
#' deck <- deck_hang_bullets(deck, "bullets")
#' }
#' @export
deck_hang_bullets <- function(deck, label, ..., config = deck_config_get()) {
  config <- .call_config(config, ...)

  slide <- deck$slide$get_slide(deck$cursor)$get()
  shape <- xml2::xml_find_first(
    slide, sprintf("//p:sp[p:nvSpPr/p:cNvPr[@name='%s']]", label), .slide_ns
  )

  if (inherits(shape, "xml_missing")) {
    cli::cli_abort("No text shape labelled {.val {label}} on the slide.")
  }

  purrr::walk(
    xml2::xml_find_all(shape, ".//a:p", .slide_ns),
    \(paragraph) .hang_paragraph(paragraph, config)
  )

  deck
}

# callouts and rules -----------------------------------------------------------

#' Place a Callout
#'
#' Places a pale band holding a small label over one sentence, for the
#' question a page leads into, set apart from the bullets without a heavy
#' box.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param geom The box to fill, a list of `left`, `top`, `width` and
#'   `height` in inches.
#' @param label The small label over the sentence.
#' @param text The sentence.
#' @param label_name Label given to the new shape. Default `NULL`, the
#'   `callout` field of the `shape_labels` setting.
#' @param text_size Font size of the sentence in points. Default `NULL`,
#'   the `callout_text_pt` setting of `config`.
#' @param fill Fill colour. Default `NULL`, the `callout_fill` setting of
#'   `config`.
#' @param label_colour Colour of the label. Default `NULL`, the
#'   `accent_colour` setting of `config`.
#' @param text_colour Colour of the sentence. Default `NULL`, the
#'   `strong_colour` setting of `config`.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `callout_label_pt` or `callout_padding`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the callout added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_callout(
#'   deck, list(left = 0.5, top = 5, width = 6, height = 1.1),
#'   label = "The question for 2027",
#'   text = "How much susceptibility remains, and where?"
#' )
#' }
#' @export
deck_callout <- function(deck, geom, label, text, label_name = NULL,
                         text_size = NULL, fill = NULL, label_colour = NULL,
                         text_colour = NULL, ...,
                         config = deck_config_get()) {
  config <- .call_config(config, ...)
  padding <- config$callout_padding

  block <- .text_rows(c(label, text)) |>
    flextable::bg(bg = fill %||% config$callout_fill, part = "body") |>
    deck_flex_semibold(i = 1, config = config) |>
    flextable::fontsize(
      i = 1, size = config$callout_label_pt, part = "body"
    ) |>
    flextable::color(
      i = 1, color = label_colour %||% config$accent_colour, part = "body"
    ) |>
    flextable::font(i = 2, fontname = config$font, part = "body") |>
    flextable::fontsize(
      i = 2, size = text_size %||% config$callout_text_pt, part = "body"
    ) |>
    flextable::color(
      i = 2, color = text_colour %||% config$strong_colour, part = "body"
    ) |>
    flextable::align(align = "left", part = "body") |>
    flextable::valign(valign = "top", part = "body") |>
    .pad_cells(padding) |>
    flextable::padding(
      i = 2, padding.bottom = padding$text_bottom, part = "body"
    ) |>
    .fix_width(geom$width)

  officer::ph_with(
    deck,
    value = block,
    location = .box_location(
      geom, label_name %||% config$shape_labels$callout
    )
  )
}

#' Place a Note Against a Rule
#'
#' Places a small label over one sentence, set against a thin rule on the
#' left, for the question a page hands on to the next.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param geom The box to fill, a list of `left`, `top`, `width` and
#'   `height` in inches.
#' @param label The small label over the sentence.
#' @param text The sentence.
#' @param label_name Label given to the new shape. Default `NULL`, the
#'   `rule_note` field of the `shape_labels` setting.
#' @param text_size Font size of the sentence in points. Default `NULL`,
#'   the `rule_text_pt` setting of `config`.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `rule_width`, `rule_label_pt`,
#'   `rule_padding`, `accent_colour` or `strong_colour`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the note added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_rule_note(
#'   deck, list(left = 0.5, top = 6, width = 8, height = 0.9),
#'   label = "Next", text = "Where did the campaign reach?"
#' )
#' }
#' @export
deck_rule_note <- function(deck, geom, label, text, label_name = NULL,
                           text_size = NULL, ...,
                           config = deck_config_get()) {
  config <- .call_config(config, ...)

  rule <- officer::fp_border(
    color = config$accent_colour, width = config$rule_width
  )
  none <- officer::fp_border(width = 0)

  block <- .text_rows(c(label, text)) |>
    flextable::border(
      border.left = rule, border.top = none, border.bottom = none,
      border.right = none, part = "body"
    ) |>
    deck_flex_semibold(i = 1, config = config) |>
    flextable::fontsize(i = 1, size = config$rule_label_pt, part = "body") |>
    flextable::color(i = 1, color = config$accent_colour, part = "body") |>
    flextable::font(fontname = config$font, part = "body") |>
    flextable::fontsize(
      i = 2, size = text_size %||% config$rule_text_pt, part = "body"
    ) |>
    flextable::color(i = 2, color = config$strong_colour, part = "body") |>
    flextable::align(align = "left", part = "body") |>
    .pad_cells(config$rule_padding) |>
    .fix_width(geom$width)

  officer::ph_with(
    deck,
    value = block,
    location = .box_location(
      geom, label_name %||% config$shape_labels$rule_note
    )
  )
}

#' Draw a Horizontal Rule
#'
#' Draws a thin horizontal rule as an empty one-cell table with a top
#' border, because 'officer' draws no line of its own.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param left,top Position of the rule's left end, in inches.
#' @param width Length of the rule, in inches.
#' @param colour Colour of the rule. Default `NULL`, the `accent_colour`
#'   setting of `config`.
#' @param weight Width of the rule in points. Default `NULL`, the
#'   `hrule_weight` setting of `config`.
#' @param label Label given to the new shape. Default `NULL`, the `hrule`
#'   field of the `shape_labels` setting.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `hrule_geometry`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the rule added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_hrule(deck, left = 0.5, top = 6.4, width = 2.75)
#' }
#' @export
deck_hrule <- function(deck, left, top, width, colour = NULL, weight = NULL,
                       label = NULL, ..., config = deck_config_get()) {
  config <- .call_config(config, ...)
  geometry <- config$hrule_geometry

  line <- officer::fp_border(
    color = colour %||% config$accent_colour,
    width = weight %||% config$hrule_weight
  )

  rule <- tibble::tibble(x = "") |>
    flextable::flextable() |>
    flextable::delete_part(part = "header") |>
    flextable::border_remove() |>
    flextable::border(border.top = line, part = "body") |>
    flextable::fontsize(size = geometry$pt, part = "body") |>
    flextable::padding(padding = 0, part = "body") |>
    flextable::height(height = geometry$row_height, part = "body") |>
    flextable::hrule(rule = "exact", part = "body") |>
    .fix_width(width)

  officer::ph_with(
    deck,
    value = rule,
    location = officer::ph_location(
      left = left, top = top, width = width,
      height = geometry$box_height,
      newlabel = label %||% config$shape_labels$hrule
    )
  )
}

#' Draw a Ring Round Part of a Chart
#'
#' Draws an unfilled ellipse as native shapes, to mark one part of a
#' chart placed on the slide.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param geom The box the ellipse fills, a list of `left`, `top`,
#'   `width` and `height` in inches.
#' @param colour Colour of the line. Default `NULL`, the `accent_colour`
#'   setting of `config`.
#' @param label Label given to the new shape. Default `NULL`, the
#'   `ellipse` field of the `shape_labels` setting.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `ellipse_lwd` or `ellipse_geometry`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the ellipse added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_ellipse(
#'   deck, list(left = 10.9, top = 3.6, width = 0.6, height = 1.4),
#'   colour = "#FFED19"
#' )
#' }
#' @export
deck_ellipse <- function(deck, geom, colour = NULL, label = NULL, ...,
                         config = deck_config_get()) {
  config <- .call_config(config, ...)
  geometry <- config$ellipse_geometry
  angle <- seq(0, 2 * pi, length.out = geometry$points)

  ring <- grid::polygonGrob(
    x = 0.5 + geometry$radius * cos(angle),
    y = 0.5 + geometry$radius * sin(angle),
    gp = grid::gpar(
      col = colour %||% config$accent_colour, fill = NA,
      lwd = config$ellipse_lwd
    )
  )

  officer::ph_with(
    deck,
    value = .grob_dml(ring),
    location = .box_location(geom, label %||% config$shape_labels$ellipse)
  )
}

# indicators and tables --------------------------------------------------------

#' Place a Headline Indicator
#'
#' Places one headline indicator as a borderless table: a small label, a
#' note on the period, the value rows and the change between them. The
#' value rows and the change line are optional, so a block can be a label
#' and a note alone.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param geom The box to fill, a list of `left`, `top`, `width` and
#'   `height` in inches.
#' @param label The indicator's label.
#' @param period_label The note under the label, such as the periods
#'   compared.
#' @param rows Character vector of value rows. Use `character()` for
#'   none.
#' @param change The change line, set largest and in bold. Use
#'   `character()` for none.
#' @param kpi_label Label given to the new shape. Default `NULL`, the
#'   `kpi` field of the `shape_labels` setting.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `kpi_change_pt`, `kpi_padding` or
#'   `accent_colour`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the indicator added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_kpi(
#'   deck, list(left = 9.6, top = 1.7, width = 3.2, height = 1.3),
#'   label = "Suspected cases", period_label = "Weeks 1 to 20",
#'   rows = c("2024: 12,400", "2025: 8,100"), change = "-35%"
#' )
#' }
#' @export
deck_kpi <- function(deck, geom, label, period_label, rows, change,
                     kpi_label = NULL, ..., config = deck_config_get()) {
  config <- .call_config(config, ...)
  text <- c(label, period_label, rows, change)
  last <- length(text)
  value_rows <- seq_along(rows) + 2

  block <- .text_rows(text) |>
    flextable::font(fontname = config$font, part = "body") |>
    flextable::align(align = "left", part = "body") |>
    .pad_cells(config$kpi_padding) |>
    deck_flex_semibold(i = 1, config = config) |>
    flextable::fontsize(i = 1, size = config$kpi_label_pt, part = "body") |>
    flextable::color(i = 1, color = config$accent_colour, part = "body") |>
    flextable::fontsize(i = 2, size = config$kpi_note_pt, part = "body") |>
    flextable::color(i = 2, color = config$muted_colour, part = "body")

  if (length(value_rows) > 0) {
    block <- block |>
      flextable::fontsize(
        i = value_rows, size = config$kpi_row_pt, part = "body"
      ) |>
      flextable::color(
        i = value_rows, color = config$body_colour, part = "body"
      )
  }

  if (length(change) > 0) {
    block <- block |>
      flextable::fontsize(
        i = last, size = config$kpi_change_pt, part = "body"
      ) |>
      flextable::bold(i = last, part = "body") |>
      flextable::color(i = last, color = config$strong_colour, part = "body")
  }

  officer::ph_with(
    deck,
    value = .fix_width(block, geom$width),
    location = .box_location(geom, kpi_label %||% config$shape_labels$kpi)
  )
}

#' Place a Data Table
#'
#' Places a data frame as a table on white: a bold header over a rule in
#' the accent colour, thin rules between the body rows and the first
#' column in bold.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param geom The box the table sits in, a list of `left`, `top`,
#'   `width` and `height` in inches.
#' @param data A data frame. Its column names are the header.
#' @param widths Column widths in inches, one per column.
#' @param size Font size in points. Default `NULL`, the `slide_table_pt`
#'   setting of `config`.
#' @param label Label given to the new shape. Default `NULL`, the `table`
#'   field of the `shape_labels` setting.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `slide_table_rule_colour`,
#'   `slide_table_padding`, `heading_colour` or `body_colour`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the table added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_table(
#'   deck, list(left = 0.5, top = 1.7, width = 6, height = 3),
#'   data = head(mtcars[, 1:3]), widths = c(2, 2, 2)
#' )
#' }
#' @export
deck_table <- function(deck, geom, data, widths, size = NULL, label = NULL,
                       ..., config = deck_config_get()) {
  config <- .call_config(config, ...)

  rule <- officer::fp_border(
    color = config$slide_table_rule_colour,
    width = config$slide_table_rule_width
  )
  head_rule <- officer::fp_border(
    color = config$accent_colour,
    width = config$slide_table_head_rule_width
  )

  table <- flextable::flextable(data) |>
    flextable::border_remove() |>
    flextable::hline(border = rule, part = "body") |>
    flextable::hline_bottom(border = head_rule, part = "header") |>
    flextable::font(fontname = config$font, part = "all") |>
    flextable::fontsize(
      size = size %||% config$slide_table_pt, part = "all"
    ) |>
    flextable::color(color = config$body_colour, part = "body") |>
    flextable::color(color = config$heading_colour, part = "header") |>
    flextable::bold(part = "header") |>
    flextable::bold(j = 1, part = "body") |>
    flextable::color(j = 1, color = config$heading_colour, part = "body") |>
    flextable::align(align = "left", part = "all") |>
    flextable::valign(valign = "top", part = "body") |>
    .pad_cells(config$slide_table_padding, part = "all") |>
    .fix_width(widths)

  officer::ph_with(
    deck,
    value = table,
    location = .box_location(geom, label %||% config$shape_labels$table)
  )
}
