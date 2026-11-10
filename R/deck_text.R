# weights ----------------------------------------------------------------------

#' Text Properties for a Weight
#'
#' Returns the run properties for text at a size, colour and weight. An
#' office suite treats semibold as a family of its own, so that weight is
#' set by naming the family and not with the bold flag. Where the family
#' is not installed, the `use_semibold` setting is `FALSE` and semibold
#' text is set in bold.
#'
#' @param size Font size in points.
#' @param colour Text colour.
#' @param weight Text weight: `"regular"`, `"semibold"` or `"bold"`.
#'   Default `"regular"`.
#' @param font Typeface of regular and bold text. Default `NULL`, the
#'   `font` setting of `config`.
#' @param semibold_font Family of the semibold weight. Default `NULL`, the
#'   `font_semibold` setting of `config`.
#' @param use_semibold Logical. Set semibold text in `semibold_font`. With
#'   `FALSE` it is set in bold. Default `NULL`, the `use_semibold` setting
#'   of `config`, which is `FALSE` unless set.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return An `officer::fp_text()` object.
#'
#' @examples
#' deck_fp_text(12, "#1F497D", "bold")
#' deck_fp_text(12, "#1F497D", "semibold", use_semibold = TRUE)$font.family
#' @export
deck_fp_text <- function(size, colour,
                         weight = c("regular", "semibold", "bold"),
                         font = NULL, semibold_font = NULL,
                         use_semibold = NULL, config = deck_config_get()) {
  weight <- rlang::arg_match(weight)

  # an office suite does not always find the semibold family; bold stands in
  if (weight == "semibold" && !(use_semibold %||% config$use_semibold)) {
    weight <- "bold"
  }

  family <- if (weight == "semibold") {
    semibold_font %||% config$font_semibold
  } else {
    font %||% config$font
  }

  officer::fp_text(
    font.size = size, color = colour, bold = weight == "bold",
    font.family = family
  )
}

#' Set Table Rows in the Emphasis Weight
#'
#' Sets rows of a flextable in the semibold family, or in bold where the
#' `use_semibold` setting is `FALSE`. The block builders use it for the
#' heading or label row of a block.
#'
#' @param ft A flextable.
#' @param i Rows of the body to set.
#' @param semibold_font Family of the semibold weight. Default `NULL`, the
#'   `font_semibold` setting of `config`.
#' @param use_semibold Logical. Use `semibold_font`. With `FALSE` the rows
#'   are set in bold. Default `NULL`, the `use_semibold` setting of
#'   `config`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The flextable.
#'
#' @examples
#' flextable::flextable(head(mtcars[, 1:3])) |>
#'   deck_flex_semibold(i = 1)
#' @export
deck_flex_semibold <- function(ft, i, semibold_font = NULL,
                               use_semibold = NULL,
                               config = deck_config_get()) {
  if (use_semibold %||% config$use_semibold) {
    return(flextable::font(
      ft, i = i, fontname = semibold_font %||% config$font_semibold,
      part = "body"
    ))
  }

  flextable::bold(ft, i = i, part = "body")
}

# page headings ----------------------------------------------------------------

#' Format the Text of a Page Heading
#'
#' `deck_sized()` formats a title or subtitle at a stated size, in the
#' colour of the page title. A size alone is not written into a slide, so
#' the colour and typeface are restated. `deck_marked()` formats a
#' subtitle in which the phrases between pairs of the emphasis mark are
#' set in another weight.
#'
#' @param text The text.
#' @param size Font size in points.
#' @param weight Text weight for `deck_sized()`, and the weight of the
#'   marked phrases for `deck_marked()`. Default `"bold"`.
#' @param colour Text colour. Default `NULL`, the `page_title_colour`
#'   setting of `config` for `deck_sized()` and the `page_subtitle_colour`
#'   setting for `deck_marked()`.
#' @param base_weight Weight of the text outside the marks. Default
#'   `"regular"`.
#' @param mark The emphasis mark. Default `NULL`, the `emphasis_mark`
#'   setting of `config`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return An `officer::fpar()` paragraph.
#'
#' @examples
#' deck_sized("Cases by week of onset", size = 26)
#' deck_marked("Cases fell **after** the campaign", size = 18)
#' @export
deck_sized <- function(text, size, weight = "bold", colour = NULL,
                       config = deck_config_get()) {
  officer::fpar(officer::ftext(
    as.character(text),
    deck_fp_text(
      size, colour %||% config$page_title_colour, weight, config = config
    )
  ))
}

#' @rdname deck_sized
#' @export
deck_marked <- function(text, size, colour = NULL, weight = "bold",
                        base_weight = "regular", mark = NULL,
                        config = deck_config_get()) {
  colour <- colour %||% config$page_subtitle_colour
  mark <- mark %||% config$emphasis_mark
  parts <- strsplit(as.character(text), mark, fixed = TRUE)[[1]]

  runs <- purrr::imap(parts, \(part, i) {
    officer::ftext(
      part,
      deck_fp_text(
        size, colour, if (i %% 2 == 0) weight else base_weight,
        config = config
      )
    )
  })

  do.call(officer::fpar, runs)
}

# the limit, size, box and weight a kind of heading is checked against
#' @noRd
.heading_settings <- function(kind, config) {
  if (kind == "title") {
    return(list(
      name = "Title",
      max_chars = config$page_title_max_chars,
      pt = config$page_title_pt,
      box = config$page_title_box,
      weight = config$page_title_weight
    ))
  }

  list(
    name = "Subtitle",
    max_chars = config$page_subtitle_max_chars,
    pt = config$page_subtitle_pt,
    box = config$page_subtitle_box,
    weight = config$page_subtitle_weight
  )
}

# width of a line of text in inches, or NA where the font cannot be measured
#' @noRd
.string_width_in <- function(text, font, size, weight) {
  face <- if (weight == "regular") "normal" else weight

  tryCatch(
    systemfonts::string_width(
      text, family = font, size = size, res = 72, weight = face
    ) / 72,
    error = \(e) NA_real_
  )
}

#' Report a Page Heading That May Wrap
#'
#' Reports a title or subtitle that is past its character limit, or wider
#' than the share of its box the `heading_fill` setting allows when
#' measured in the deck typeface. The heading is reported and not shrunk,
#' so every page keeps one title size.
#'
#' @param text The heading.
#' @param kind Which heading: `"title"` or `"subtitle"`. Default
#'   `"title"`.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`. The check reads `page_title_max_chars`,
#'   `page_title_pt`, `page_title_box` and `page_title_weight` or their
#'   `page_subtitle_*` counterparts, with `heading_fill`, `box_inset`,
#'   `emphasis_mark` and `font`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return `text`, invisibly.
#'
#' @examples
#' deck_check_heading("Cases by week of onset")
#' deck_check_heading("Cases by week of onset", page_title_max_chars = 10L)
#' @export
deck_check_heading <- function(text, kind = c("title", "subtitle"), ...,
                               config = deck_config_get()) {
  kind <- rlang::arg_match(kind)
  config <- .call_config(config, ...)
  heading <- .heading_settings(kind, config)

  plain <- gsub(config$emphasis_mark, "", as.character(text), fixed = TRUE)
  width <- .string_width_in(plain, config$font, heading$pt, heading$weight)
  usable <- heading$box$width - 2 * config$box_inset

  too_long <- nchar(plain) > heading$max_chars
  too_wide <- !is.na(width) && width > config$heading_fill * usable

  if (too_long || too_wide) {
    cli::cli_alert_warning(
      "{heading$name} may wrap ({nchar(plain)} characters); shorten it:
       {.val {plain}}"
    )
  }

  invisible(text)
}

#' Size of a Page Title
#'
#' Returns the one size every page title is set at, after checking the
#' title with [deck_check_heading()].
#'
#' @param title The title.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `page_title_pt`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return Font size in points.
#'
#' @examples
#' deck_title_pt("Cases by week of onset")
#' @export
deck_title_pt <- function(title, ..., config = deck_config_get()) {
  config <- .call_config(config, ...)
  deck_check_heading(title, "title", config = config)

  config$page_title_pt
}

# boxes of text ----------------------------------------------------------------

#' Place a Filled Box of Text
#'
#' Places a filled box, with or without a line of text centred in it. The
#' box is a one-cell table, because 'officer' draws no rectangle of its
#' own.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param geom The box to fill, a list of `left`, `top`, `width` and
#'   `height` in inches.
#' @param text Text of the box. Default `""`, none.
#' @param fill Fill colour. Default `NULL`, the `text_box_fill` setting of
#'   `config`.
#' @param colour Text colour. Default `NULL`, the `text_box_colour`
#'   setting of `config`.
#' @param size Font size in points. Default `NULL`, the `text_box_pt`
#'   setting of `config`.
#' @param label Label given to the new shape. Default `NULL`, the
#'   `text_box` field of the `shape_labels` setting.
#' @param align,valign Position of the text in the box. Default
#'   `"center"` for both.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `font`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the box added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_text_box(
#'   deck, list(left = 1, top = 2, width = 4, height = 1),
#'   text = "Figure pending", fill = "#F2F5F7"
#' )
#' }
#' @export
deck_text_box <- function(deck, geom, text = "", fill = NULL, colour = NULL,
                          size = NULL, label = NULL, align = "center",
                          valign = "center", ...,
                          config = deck_config_get()) {
  config <- .call_config(config, ...)

  band <- tibble::tibble(band = text) |>
    flextable::flextable() |>
    flextable::delete_part(part = "header") |>
    flextable::border_remove() |>
    .space_empty_cells() |>
    flextable::bg(bg = fill %||% config$text_box_fill, part = "body") |>
    flextable::color(
      color = colour %||% config$text_box_colour, part = "body"
    ) |>
    flextable::fontsize(size = size %||% config$text_box_pt, part = "body") |>
    flextable::font(fontname = config$font, part = "body") |>
    flextable::align(align = align, part = "body") |>
    flextable::valign(valign = valign, part = "body") |>
    flextable::height(height = geom$height, part = "body") |>
    flextable::hrule(rule = "exact", part = "body") |>
    flextable::width(width = geom$width) |>
    flextable::set_table_properties(layout = "fixed")

  officer::ph_with(
    deck,
    value = band,
    location = .box_location(geom, label %||% config$shape_labels$text_box)
  )
}

#' Place a Closing Line
#'
#' Places one line of text in the heading colour and the emphasis weight,
#' with no fill, for the sentence a page closes on.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param geom The box the line sits in, a list of `left`, `top`, `width`
#'   and `height` in inches.
#' @param text The line.
#' @param label Label given to the new shape. Default `NULL`, the
#'   `message` field of the `shape_labels` setting.
#' @param size Font size in points. Default `NULL`, the `message_pt`
#'   setting of `config`.
#' @param colour Text colour. Default `NULL`, the `heading_colour` setting
#'   of `config`.
#' @param weight Text weight. Default `"semibold"`.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the line added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_message(
#'   deck, list(left = 0.5, top = 6.2, width = 12, height = 0.5),
#'   "Coverage fell in the two rounds after the campaign paused."
#' )
#' }
#' @export
deck_message <- function(deck, geom, text, label = NULL, size = NULL,
                         colour = NULL, weight = "semibold", ...,
                         config = deck_config_get()) {
  config <- .call_config(config, ...)

  officer::ph_with(
    deck,
    value = deck_subtitle(
      text,
      size = size %||% config$message_pt,
      colour = colour %||% config$heading_colour,
      config = config,
      weight = weight
    ),
    location = .box_location(geom, label %||% config$shape_labels$message)
  )
}
