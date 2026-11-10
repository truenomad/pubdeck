# fitting images ---------------------------------------------------------------

# pixel width and height of an image file
#' @noRd
.image_size <- function(path) {
  magick::image_info(magick::image_read(path))
}

#' Fit an Image to a Box Without Distorting It
#'
#' Returns the largest size at which an image fits inside a box while
#' keeping its own aspect ratio. A rendered table's aspect is not known
#' until it is drawn, and stretching it to the box would distort the type.
#'
#' @param path Path to the image.
#' @param max_width,max_height Size of the box, in inches.
#'
#' @return A list of `width` and `height`, in inches.
#'
#' @examples
#' \dontrun{
#' deck_fit_image("outputs/table.png", max_width = 6, max_height = 4)
#' }
#' @export
deck_fit_image <- function(path, max_width, max_height) {
  size <- .image_size(path)
  ratio <- size$width / size$height

  width <- max_width
  height <- width / ratio

  if (height > max_height) {
    height <- max_height
    width <- height * ratio
  }

  list(width = width, height = height)
}

#' Fit Several Images at One Scale
#'
#' Returns sizes for a set of images that all fit a box at the same scale,
#' so tables on consecutive pages keep the same type size.
#'
#' @param paths Character vector of image paths.
#' @param max_width,max_height Size of the box, in inches.
#'
#' @return A list named by path, each element a list of `width` and
#'   `height`.
#'
#' @examples
#' \dontrun{
#' deck_fit_images_common(c("a.png", "b.png"), max_width = 6, max_height = 4)
#' }
#' @export
deck_fit_images_common <- function(paths, max_width, max_height) {
  sizes <- purrr::map(paths, .image_size)

  scale <- min(purrr::map_dbl(
    sizes, \(size) min(max_width / size$width, max_height / size$height)
  ))

  sizes |>
    purrr::map(
      \(size) list(width = size$width * scale, height = size$height * scale)
    ) |>
    stats::setNames(paths)
}

#' Height of an Image Stacked in a Column
#'
#' Returns the height an image takes at the full width of a column, capped
#' at a share of the column, so a figure stacked beneath it can take what
#' is left.
#'
#' @param path Path to the image.
#' @param width Width of the column, in inches.
#' @param column_height Height of the column, in inches.
#' @param max_share Largest share of the column the image may take, from 0
#'   to 1.
#'
#' @return Height in inches.
#'
#' @examples
#' \dontrun{
#' deck_stacked_height("table.png", 6, column_height = 5, max_share = 0.4)
#' }
#' @export
deck_stacked_height <- function(path, width, column_height, max_share) {
  min(deck_fit_image(path, width, Inf)$height, column_height * max_share)
}

# a picture for a slide; alternative text is passed on only when it is
# given, so a picture without it is the one officer builds by default
#' @noRd
.external_img <- function(path, width, height, alt = NULL) {
  if (is.null(alt)) {
    return(officer::external_img(path, width = width, height = height))
  }

  officer::external_img(path, width = width, height = height, alt = alt)
}

#' Place a Chart as Native Shapes Where Possible
#'
#' Returns the object to place on a slide for a saved figure. When
#' [ggsave_pub()] left a current grob beside the PNG, the grob is small
#' enough and the slot is close to the size the chart was drawn at, the
#' chart is returned as editable PowerPoint shapes. Otherwise the PNG is
#' returned as a picture.
#'
#' @param path Path to the PNG.
#' @param width,height Size of the slot, in inches.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force. Its `vector_max_bytes` and
#'   `vector_scale_tolerance` settings decide when shapes are used.
#' @param alt Alternative text for the picture, which a screen reader
#'   reads out. Default `NULL`, none. It does not apply to a chart placed
#'   as native shapes.
#'
#' @return An `rvg::dml()` object or an `officer::external_img()`.
#'
#' @examples
#' \dontrun{
#' deck_figure("outputs/trend.png", width = 6, height = 3)
#' deck_figure(
#'   "outputs/trend.png", width = 6, height = 3,
#'   alt = "Line chart of cases by week of onset"
#' )
#' }
#' @export
deck_figure <- function(path, width, height, config = deck_config_get(),
                        alt = NULL) {
  grob_path <- fs::path_ext_set(path, "qs2")

  usable <- fs::file_exists(grob_path) &&
    fs::file_info(grob_path)$modification_time >=
      fs::file_info(path)$modification_time &&
    fs::file_size(grob_path) <= fs::fs_bytes(config$vector_max_bytes)

  saved <- if (usable) qs2::qs_read(grob_path)

  off_scale <- is.null(saved) ||
    abs(width / saved$width - 1) > config$vector_scale_tolerance

  if (off_scale) {
    return(.external_img(path, width, height, alt))
  }

  .load_namespaces(saved$namespaces)

  .grob_dml(saved$grob)
}

# loads the namespaces a saved grob draws through
#' @noRd
.load_namespaces <- function(namespaces) {
  purrr::walk(
    setdiff(namespaces, loadedNamespaces()),
    \(ns) suppressPackageStartupMessages(requireNamespace(ns, quietly = TRUE))
  )
}

# a grob as native shapes; dml draws later, elsewhere, so the grob travels
# inside a bare ggplot that draws it over the whole device
#' @noRd
.grob_dml <- function(grob) {
  holder <- ggplot2::ggplot() +
    ggplot2::annotation_custom(grob) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

  rvg::dml(ggobj = holder, bg = "transparent")
}

# boxes ------------------------------------------------------------------------

#' Slide Location of a Box
#'
#' Turns a box in inches into the location 'officer' places a shape at.
#' With a label, the shape is named, so the slide can be read back and the
#' shape found, replaced or removed.
#'
#' @param box A list of `left`, `top`, `width` and `height`, in inches.
#' @param label Label given to the shape placed there. Default `NULL`,
#'   none.
#'
#' @return An `officer::ph_location()` object.
#'
#' @examples
#' deck_box_location(
#'   list(left = 1, top = 1, width = 6, height = 3), label = "trend"
#' )
#' @export
deck_box_location <- function(box, label = NULL) {
  .box_location(box, label)
}

#' Adjust a Box
#'
#' Helpers that derive a box from the one a layout reserves.
#' `deck_inset_box()` pulls a box in on every side, where a layout runs
#' its placeholder to the slide edge. `deck_grow_box()` moves a box down
#' and makes it taller, where a layout reserves less room than the page
#' has. `deck_split_box()` cuts a box into columns with a gap between, for
#' a page that carries more than one figure.
#'
#' @param box A list of `left`, `top`, `width` and `height`, in inches, as
#'   [deck_layout_box()] returns.
#' @param inset Distance to pull each side in, in inches.
#' @param top_shift Distance to move the top down, in inches. Default `0`.
#' @param extra_height Height to add, in inches. Default `0`.
#' @param n Number of columns. Default `2`.
#' @param gap Space between columns, in inches. Default `0.2`.
#'
#' @return `deck_inset_box()` and `deck_grow_box()` return a box.
#'   `deck_split_box()` returns a list of `n` boxes, left to right.
#'
#' @examples
#' box <- list(left = 1, top = 1, width = 6, height = 3)
#' deck_inset_box(box, inset = 0.1)
#' deck_grow_box(box, top_shift = 0.2, extra_height = 0.5)
#' deck_split_box(box, n = 2)
#' @name deck_box
NULL

#' @rdname deck_box
#' @export
deck_inset_box <- function(box, inset) {
  list(
    left = box$left + inset,
    top = box$top + inset,
    width = box$width - 2 * inset,
    height = box$height - 2 * inset
  )
}

#' @rdname deck_box
#' @export
deck_grow_box <- function(box, top_shift = 0, extra_height = 0) {
  list(
    left = box$left,
    top = box$top + top_shift,
    width = box$width,
    height = box$height + extra_height
  )
}

#' @rdname deck_box
#' @export
deck_split_box <- function(box, n = 2, gap = 0.2) {
  width <- (box$width - gap * (n - 1)) / n

  purrr::map(seq_len(n) - 1, \(column) {
    list(
      left = box$left + column * (width + gap),
      top = box$top,
      width = width,
      height = box$height
    )
  })
}

#' Position an Image Inside a Box
#'
#' Returns the slide location for an image of a given size inside a box,
#' at the image's own aspect.
#'
#' @param fit A list of `width` and `height`, as [deck_fit_image()]
#'   returns.
#' @param box A list of `left`, `top`, `width` and `height`, in inches.
#' @param align Horizontal position in the box: `"centre"`, `"right"` or
#'   `"left"`.
#' @param valign Vertical position in the box: `"centre"`, `"top"` or
#'   `"bottom"`. Default `"centre"`.
#'
#' @return An `officer::ph_location()` object.
#'
#' @examples
#' deck_place_in_box(
#'   fit = list(width = 4, height = 2),
#'   box = list(left = 1, top = 1, width = 6, height = 3),
#'   align = "right"
#' )
#'
#' # resting on the foot of the box
#' deck_place_in_box(
#'   fit = list(width = 4, height = 2),
#'   box = list(left = 1, top = 1, width = 6, height = 3),
#'   valign = "bottom"
#' )
#' @export
deck_place_in_box <- function(fit, box,
                              align = c("centre", "right", "left"),
                              valign = c("centre", "top", "bottom")) {
  align <- rlang::arg_match(align)
  valign <- rlang::arg_match(valign)

  left <- switch(
    align,
    centre = box$left + (box$width - fit$width) / 2,
    right = box$left + box$width - fit$width,
    left = box$left
  )

  top <- switch(
    valign,
    centre = box$top + (box$height - fit$height) / 2,
    top = box$top,
    bottom = box$top + (box$height - fit$height)
  )

  officer::ph_location(
    left = left, top = top, width = fit$width, height = fit$height
  )
}

# covering what a layout draws -------------------------------------------------

#' A Blank White Panel
#'
#' Returns an empty plot with a white background, to place over text that
#' lives in a layout and that 'officer' cannot clear.
#'
#' @return A ggplot.
#'
#' @examples
#' deck_cover_plot()
#' @export
deck_cover_plot <- function() {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

#' Cover Part of a Slide with an Opaque Panel
#'
#' Places a filled panel over a shape the layout draws and 'officer'
#' cannot remove, such as a footnote written for the page the layout was
#' first cut for.
#'
#' @param deck An `rpptx` object, on the slide to change.
#' @param geom A list of `left`, `top`, `width` and `height`, in inches.
#' @param fill Fill colour. Default `NULL`, the `page_bg` setting of
#'   `config`.
#' @param label Label given to the new shape. Default `"cover"`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck, with the panel added.
#'
#' @examples
#' \dontrun{
#' deck <- deck_cover_box(
#'   deck, list(left = 0.5, top = 6.8, width = 12, height = 0.3)
#' )
#' }
#' @export
deck_cover_box <- function(deck, geom, fill = NULL, label = "cover",
                           config = deck_config_get()) {
  band <- .filled_band(fill %||% config$page_bg) |>
    .size_band(width = geom$width, height = geom$height)

  officer::ph_with(deck, value = band, location = .box_location(geom, label))
}
