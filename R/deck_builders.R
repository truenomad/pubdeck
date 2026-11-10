# cover ------------------------------------------------------------------------

# one line of the cover in its own labelled box; a line with no text is
# left out
#' @noRd
.cover_line <- function(deck, text, size, colour, weight, box, label,
                        config) {
  if (is.null(text)) {
    return(deck)
  }

  officer::ph_with(
    deck,
    value = deck_subtitle(
      text, size = size, colour = colour, config = config, weight = weight
    ),
    location = .box_location(box, label)
  )
}

#' Add a Cover with a Title, Subtitle and Author
#'
#' Adds the cover and writes its title, a subtitle and the author, each
#' left aligned in a box of its own and labelled, so a later step can
#' find or remove a line. [deck_title_slide()] is the cover for a deck
#' that carries a reporting date.
#'
#' @param deck An `rpptx` object.
#' @param title Deck title. Default `NULL`, the `title` setting of
#'   `config`.
#' @param subtitle Line under the title. Default `NULL`, the
#'   `cover_subtitle` setting of `config`. The line is left out when both
#'   are `NULL`.
#' @param author Line under the subtitle. Default `NULL`, the
#'   `cover_author` setting of `config`. The line is left out when both
#'   are `NULL`.
#' @param title_weight,subtitle_weight,author_weight Weight of each line:
#'   `"regular"`, `"semibold"` or `"bold"`. Defaults `"bold"`,
#'   `"regular"` and `"bold"`.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `cover_title_pt` or `cover_subtitle_box`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force. The boxes, sizes and colours default to
#'   those of the template the builder was first written for.
#'
#' @return The deck, with the cover added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_open() |>
#'   deck_cover_slide(subtitle = "Evidence for the 2027 campaign")
#' }
#' @export
deck_cover_slide <- function(deck, title = NULL, subtitle = NULL,
                             author = NULL, title_weight = "bold",
                             subtitle_weight = "regular",
                             author_weight = "bold", ...,
                             config = deck_config_get()) {
  config <- .call_config(config, ...)
  labels <- config$labels

  deck |>
    officer::add_slide(layout = config$layouts$cover, master = config$master) |>
    .cover_line(
      title %||% .cfg_required("title", config),
      config$cover_title_pt, config$cover_band_colour, title_weight,
      config$cover_title_box, labels$cover_title, config
    ) |>
    .cover_line(
      subtitle %||% config$cover_subtitle,
      config$cover_subtitle_pt, config$cover_subtitle_colour,
      subtitle_weight, config$cover_subtitle_box, labels$cover_subtitle,
      config
    ) |>
    .cover_line(
      author %||% config$cover_author,
      config$cover_author_pt, config$cover_author_colour, author_weight,
      config$cover_author_box, labels$cover_author, config
    )
}

# pages ------------------------------------------------------------------------

# the subtitle of a page, with any marked phrases in the weight of the title
#' @noRd
.page_subtitle_text <- function(subtitle, config) {
  if (grepl(config$emphasis_mark, subtitle, fixed = TRUE)) {
    return(deck_marked(
      subtitle, config$page_subtitle_pt,
      weight = config$page_title_weight,
      base_weight = config$page_subtitle_weight,
      config = config
    ))
  }

  deck_sized(
    subtitle, config$page_subtitle_pt, config$page_subtitle_weight,
    config$page_subtitle_colour, config = config
  )
}

# the title and subtitle of a page, each in its labelled box
#' @noRd
.page_headings <- function(deck, title, subtitle, config, title_box = NULL,
                           subtitle_box = NULL) {
  title_box <- title_box %||% config$page_title_box

  # a page given its own title box has room for a title that wraps
  title_pt <- if (identical(title_box, config$page_title_box)) {
    deck_title_pt(title, config = config)
  } else {
    config$page_title_pt
  }

  deck <- officer::ph_with(
    deck,
    value = deck_sized(
      title, title_pt, config$page_title_weight, config$page_title_colour,
      config = config
    ),
    location = .box_location(title_box, config$labels$page_title)
  )

  deck_check_heading(subtitle, "subtitle", config = config)

  officer::ph_with(
    deck,
    value = .page_subtitle_text(subtitle, config),
    location = .box_location(
      subtitle_box %||% config$page_subtitle_box,
      config$labels$page_subtitle
    )
  )
}

# a saved figure fitted into a box, or a panel naming the file still to draw
#' @noRd
.place_figure <- function(deck, path, geom, label, pending_pt, config) {
  if (!fs::file_exists(path)) {
    return(deck_text_box(
      deck, geom,
      text = paste0(config$pending_lead, .path_rel(path, config)),
      fill = config$pending_fill, size = pending_pt, label = label,
      config = config
    ))
  }

  fit <- deck_fit_image(path, geom$width, geom$height)

  officer::ph_with(
    deck,
    value = officer::external_img(
      path, width = fit$width, height = fit$height
    ),
    location = deck_place_in_box(fit, geom)
  )
}

#' Add a Page That Carries One Figure
#'
#' Adds a content page: the title, the subtitle, the figure fitted into
#' the plot box its layout reserves, and the foot strip. Where the figure
#' has not been drawn yet, a panel naming the file stands in for it, so a
#' deck can be laid out before its figures exist.
#'
#' @param deck An `rpptx` object.
#' @param layout Name of the layout. It must carry the plot placeholder
#'   the `plot` field of the `labels` setting names.
#' @param title Page title, checked with [deck_check_heading()].
#' @param subtitle Page subtitle. Phrases between pairs of the emphasis
#'   mark are set in the weight of the title.
#' @param figure Path to the figure.
#' @param section Section key, for the foot strip.
#' @param sources Character vector of source keys, passed to
#'   [deck_page_footer()]. Required unless `framing` is `TRUE`.
#' @param as_of Text appended to the source line. Default `NULL`.
#' @param framing Logical. The page carries no finding, so it may name no
#'   source. Default `FALSE`.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `page_title_box` or `pending_page_pt`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force. The heading boxes, sizes and colours
#'   default to those of the template the builder was first written for.
#'
#' @return The deck, with the page added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_figure_page(
#'   deck, layout = "one_plot",
#'   title = "Cases by week of onset",
#'   subtitle = "Cases fell **after** the campaign",
#'   figure = "outputs/trend.png",
#'   section = "epi", sources = "polis"
#' )
#' }
#' @export
deck_figure_page <- function(deck, layout, title, subtitle, figure, section,
                             sources = NULL, as_of = NULL, framing = FALSE,
                             ..., config = deck_config_get()) {
  config <- .call_config(config, ...)

  deck <- deck |>
    officer::add_slide(layout = layout, master = config$master) |>
    .page_headings(title, subtitle, config)

  box <- deck_layout_box(layout, config$labels$plot, config = config) |>
    deck_below_subtitle(config = config)

  deck |>
    .place_figure(
      figure, box, config$shape_labels$pending, config$pending_page_pt,
      config
    ) |>
    deck_page_footer(
      section, sources = sources, as_of = as_of, config = config,
      framing = framing
    )
}

#' Add a Page Built from Boxes
#'
#' Adds a page with a title, a subtitle and the foot strip and nothing
#' else placed, for a page assembled from panels, bullets and figures in
#' boxes of their own.
#'
#' @param deck An `rpptx` object.
#' @param title Page title. It is checked with [deck_check_heading()]
#'   unless the page has a title box of its own, which has room for a
#'   title that wraps.
#' @param subtitle Page subtitle. Phrases between pairs of the emphasis
#'   mark are set in the weight of the title.
#' @param section Section key, for the foot strip.
#' @param sources Character vector of source keys, passed to
#'   [deck_page_footer()]. Required unless `framing` is `TRUE`.
#' @param layout Name of the layout. Default `NULL`, the `text_page`
#'   field of the `layouts` setting.
#' @param framing Logical. The page carries no finding, so it may name no
#'   source. Default `FALSE`.
#' @param title_box,subtitle_box Boxes for the two headings, each a list
#'   of `left`, `top`, `width` and `height` in inches. Default `NULL`,
#'   the `page_title_box` and `page_subtitle_box` settings of `config`.
#' @param as_of Text appended to the source line. Default `NULL`.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `page_title_pt`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force. The layout name, heading boxes, sizes
#'   and colours default to those of the template the builder was first
#'   written for.
#'
#' @return The deck, with the page added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_text_page(
#'   deck, "What the review covers", "Three questions for 2027",
#'   section = "context", framing = TRUE
#' )
#' }
#' @export
deck_text_page <- function(deck, title, subtitle, section, sources = NULL,
                           layout = NULL, framing = FALSE, title_box = NULL,
                           subtitle_box = NULL, as_of = NULL, ...,
                           config = deck_config_get()) {
  config <- .call_config(config, ...)

  deck |>
    officer::add_slide(
      layout = layout %||% config$layouts$text_page, master = config$master
    ) |>
    .page_headings(title, subtitle, config, title_box, subtitle_box) |>
    deck_page_footer(
      section, sources = sources, as_of = as_of, config = config,
      framing = framing
    )
}

# boxes on a page --------------------------------------------------------------

#' Move a Box Below the Subtitle
#'
#' Moves the top of a box down to the line no figure starts above and
#' keeps its bottom edge, so a figure never crowds the subtitle. A box
#' that already starts below the line is returned unchanged.
#'
#' @param geom A box, a list of `left`, `top`, `width` and `height` in
#'   inches.
#' @param top_min The line, in inches from the top of the slide. Default
#'   `NULL`, the `plot_top_min` setting of `config`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The box.
#'
#' @examples
#' deck_below_subtitle(list(left = 1, top = 1.2, width = 6, height = 4))
#' deck_below_subtitle(
#'   list(left = 1, top = 1.2, width = 6, height = 4), top_min = 2
#' )
#' @export
deck_below_subtitle <- function(geom, top_min = NULL,
                                config = deck_config_get()) {
  shift <- max(0, (top_min %||% config$plot_top_min) - geom$top)

  utils::modifyList(
    geom, list(top = geom$top + shift, height = geom$height - shift)
  )
}

#' Split a Box into Columns at the House Gap
#'
#' Cuts a box into columns, as [deck_split_box()] does, with the gap
#' between them taken from the configuration, so every page built from
#' columns keeps one gutter.
#'
#' @param box A box, a list of `left`, `top`, `width` and `height` in
#'   inches.
#' @param n Number of columns.
#' @param gap Space between columns, in inches. Default `NULL`, the
#'   `column_gap` setting of `config`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return A list of `n` boxes, left to right.
#'
#' @examples
#' deck_columns(list(left = 0.5, top = 1.5, width = 12, height = 5), n = 3)
#' @export
deck_columns <- function(box, n, gap = NULL, config = deck_config_get()) {
  deck_split_box(box, n = n, gap = gap %||% config$column_gap)
}

#' Place a Saved Figure in a Box
#'
#' Fits a saved figure into a box below the subtitle and places it as a
#' picture. Where the file does not exist yet, a panel naming it stands
#' in, so a page can be laid out before its figures are drawn.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param path Path to the figure.
#' @param geom The box to fill, a list of `left`, `top`, `width` and
#'   `height` in inches. It is moved below the subtitle with
#'   [deck_below_subtitle()].
#' @param label Label given to the pending panel. Default `NULL`, the
#'   `pending` field of the `shape_labels` setting.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `pending_pt` or `plot_top_min`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the figure or the pending panel added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_place_figure(
#'   deck, "outputs/trend.png",
#'   list(left = 0.5, top = 1.7, width = 6, height = 4)
#' )
#' }
#' @export
deck_place_figure <- function(deck, path, geom, label = NULL, ...,
                              config = deck_config_get()) {
  config <- .call_config(config, ...)

  .place_figure(
    deck, path,
    deck_below_subtitle(geom, config = config),
    label %||% config$shape_labels$pending,
    config$pending_pt, config
  )
}

# a grob read from an .rds file, or from the record save_pub_grob() writes
#' @noRd
.read_grob <- function(path) {
  if (fs::path_ext(path) != "qs2") {
    return(readRDS(path))
  }

  saved <- qs2::qs_read(path)

  if (inherits(saved, "grob")) {
    return(saved)
  }

  .load_namespaces(saved[["namespaces"]])

  saved[["grob"]]
}

#' Place a Saved Chart as Native Shapes
#'
#' Places a chart saved as a grob as native PowerPoint shapes and text,
#' drawn at the size of its box, so small type stays sharp at any zoom.
#' A grob file larger than the `vector_max_bytes` setting is placed as
#' the PNG beside it instead, because the polygons of a map become
#' millions of points in the slide.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param path Path to the grob: an `.rds` file holding a grob, or the
#'   `.qs2` file [save_pub_grob()] writes. A PNG with the same stem is
#'   used when the grob is too large.
#' @param geom The box to fill, a list of `left`, `top`, `width` and
#'   `height` in inches. It is moved below the subtitle with
#'   [deck_below_subtitle()].
#' @param label Label given to the new shape.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `vector_max_bytes`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the chart added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_place_vector(
#'   deck, "outputs/trend.rds",
#'   list(left = 0.5, top = 1.7, width = 6, height = 4), label = "trend"
#' )
#' }
#' @export
deck_place_vector <- function(deck, path, geom, label, ...,
                              config = deck_config_get()) {
  config <- .call_config(config, ...)
  geom <- deck_below_subtitle(geom, config = config)
  png_path <- fs::path_ext_set(path, "png")

  too_large <- fs::file_size(path) > fs::fs_bytes(config$vector_max_bytes)

  if (too_large && fs::file_exists(png_path)) {
    fit <- deck_fit_image(png_path, geom$width, geom$height)

    return(officer::ph_with(
      deck,
      value = officer::external_img(png_path, fit$width, fit$height),
      location = deck_place_in_box(fit, geom)
    ))
  }

  officer::ph_with(
    deck,
    value = .grob_dml(.read_grob(path)),
    location = .box_location(geom, label)
  )
}
