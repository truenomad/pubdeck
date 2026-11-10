# figures ----------------------------------------------------------------------

# a plot drawing simple features anywhere, its patchwork parts included
#' @noRd
.plot_has_sf <- function(plot) {
  if (!inherits(plot, "ggplot")) {
    return(FALSE)
  }

  any(purrr::map_lgl(plot$layers, \(layer) inherits(layer$geom, "GeomSf"))) ||
    (inherits(plot, "patchwork") &&
       any(purrr::map_lgl(plot$patches$plots, .plot_has_sf)))
}

# the drawn plot as a grob, or NULL when it cannot be built
#' @noRd
.as_grob <- function(plot) {
  tryCatch(
    if (inherits(plot, "patchwork")) {
      patchwork::patchworkGrob(plot)
    } else if (inherits(plot, "ggplot")) {
      ggplot2::ggplotGrob(plot)
    } else if (inherits(plot, "grob")) {
      plot
    },
    error = \(e) NULL
  )
}

#' Save a Figure at an Explicit Size
#'
#' Saves a plot with `ggplot2::ggsave()` at a stated width and height, so
#' the file that is checked is the file that ships. A PNG is compressed
#' with [compress_pub()]. For a chart saved as PNG, the drawn chart is also
#' kept beside the file with [save_pub_grob()], so [deck_figure()] can
#' place it as native PowerPoint shapes. A report that places pictures
#' only turns that off with `grob` or the `save_grob` setting.
#'
#' @param plot A ggplot, patchwork or grob.
#' @param path File path. The format comes from the extension.
#' @param width,height Size in inches. Both are required.
#' @param dpi Resolution. Default `NULL`, the `map_dpi` setting of
#'   `config` for a plot that draws simple features and the `dpi` setting
#'   for any other. Where `map_dpi` is `NULL`, a map takes `dpi` as well.
#' @param bg Background colour. Default `"white"`.
#' @param scale Passed to `ggplot2::ggsave()`. Default `1`.
#' @param ... Further arguments passed to `ggplot2::ggsave()`.
#' @param grob Logical. Keep the drawn chart beside a PNG. With `FALSE`
#'   no grob file is written or removed. Default `NULL`, the `save_grob`
#'   setting of `config`, which is `TRUE` unless set.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The path, invisibly.
#'
#' @examples
#' plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() +
#'   theme_pub()
#' path <- file.path(tempdir(), "fuel_economy.png")
#' ggsave_pub(plot, path, width = 6, height = 3)
#'
#' # the picture alone
#' ggsave_pub(plot, path, width = 6, height = 3, grob = FALSE)
#' @export
ggsave_pub <- function(plot, path, width, height, dpi = NULL, bg = "white",
                       scale = 1, ..., grob = NULL,
                       config = deck_config_get()) {
  if (missing(width) || missing(height)) {
    cli::cli_abort(c(
      "{.arg width} and {.arg height} are required.",
      "i" = "A figure checked at one size and shipped at another is not
             checked."
    ))
  }

  is_map <- .plot_has_sf(plot)
  map_dpi <- config$map_dpi %||% config$dpi
  dpi <- dpi %||% if (is_map) map_dpi else config$dpi

  fs::dir_create(fs::path_dir(path))

  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    scale = scale,
    bg = bg,
    ...
  )

  compress_pub(path)

  if (grob %||% config$save_grob) {
    save_pub_grob(plot, path, width * scale, height * scale, is_map)
  }

  cli::cli_alert_success(
    "Wrote {.path {(.path_rel(path, config))}} ({width}x{height} in)"
  )

  invisible(path)
}

#' Keep a Drawn Chart Beside Its PNG
#'
#' Writes the chart as a grob to a `.qs2` file with the same stem as the
#' PNG, together with its size and the namespaces loaded when it was
#' drawn. A map is left as a picture, and any grob an earlier run saved
#' for the same path is deleted.
#'
#' @param plot A ggplot, patchwork or grob.
#' @param path Path of the PNG the chart was saved to.
#' @param width,height Size the chart was drawn at, in inches.
#' @param is_map Logical. The plot is a map, so no grob is kept.
#'
#' @return The path of the grob file, invisibly, or `NULL` when none was
#'   written.
#'
#' @examples
#' plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#' path <- file.path(tempdir(), "chart.png")
#' save_pub_grob(plot, path, width = 6, height = 3, is_map = FALSE)
#' @export
save_pub_grob <- function(plot, path, width, height, is_map) {
  grob_path <- fs::path_ext_set(path, "qs2")
  grob <- if (is_map || fs::path_ext(path) != "png") NULL else .as_grob(plot)

  if (is.null(grob)) {
    if (fs::file_exists(grob_path)) {
      fs::file_delete(grob_path)
    }
    return(invisible(NULL))
  }

  # a grob draws through methods of the packages that built it, so they
  # travel with it and are loaded before it is placed
  qs2::qs_save(
    list(
      grob = grob, width = width, height = height,
      namespaces = loadedNamespaces()
    ),
    grob_path
  )

  invisible(grob_path)
}

#' Compress a PNG in Place
#'
#' Compresses a PNG with `poliprep::compress_png()`. Any other file, a
#' missing file, and a session without the 'poliprep' package are left
#' untouched. A failed compression is reported as a warning and does not
#' stop the run.
#'
#' @param path File path.
#'
#' @return The path, invisibly.
#'
#' @examples
#' compress_pub(file.path(tempdir(), "no_such_figure.png"))
#' @export
compress_pub <- function(path) {
  is_png <- fs::file_exists(path) && fs::path_ext(path) == "png"

  if (!is_png || !rlang::is_installed("poliprep")) {
    return(invisible(path))
  }

  tryCatch(
    poliprep::compress_png(path, png_overwrite = TRUE, verbosity = FALSE),
    error = \(e) {
      cli::cli_alert_warning(
        "Could not compress {.path {fs::path_rel(path)}}:
         {conditionMessage(e)}"
      )
    }
  )

  invisible(path)
}

#' Trim the Margin of a PNG
#'
#' Trims the empty margin around an image to a thin border, then
#' compresses the file with [compress_pub()].
#'
#' @param path Path of the PNG, which is overwritten.
#' @param border Width of the border left around the image, in pixels.
#'   Default `6`.
#' @param border_colour Colour of the border. Default `"white"`.
#'
#' @return The path, invisibly.
#'
#' @examples
#' \dontrun{
#' trim_png("outputs/table.png")
#' }
#' @export
trim_png <- function(path, border = 6, border_colour = "white") {
  magick::image_read(path) |>
    magick::image_trim() |>
    magick::image_border(border_colour, paste0(border, "x", border)) |>
    magick::image_write(path)

  compress_pub(path)

  invisible(path)
}

# tables -----------------------------------------------------------------------

#' Save a gt Table as a PNG
#'
#' Renders a gt table at a stated row padding and compresses the file. gt
#' sizes a table to its content, so a short table stays short and a slide
#' can place it at the top of its box.
#'
#' @param build A function that takes the row padding in pixels and returns
#'   a gt table.
#' @param path Path of the PNG to write.
#' @param width,height Size of the rendering viewport, in inches.
#' @param pad Row padding in pixels. Default `NULL`, the
#'   `table_row_padding` setting of `config`.
#' @param px_per_in Pixels per inch of the viewport. Default `132`.
#' @param zoom Zoom factor passed to `gt::gtsave()`. Default `3`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return A list with the `padding` used and the `aspect` ratio of the
#'   saved image.
#'
#' @examples
#' \dontrun{
#' build <- function(pad) {
#'   gt::gt(head(mtcars)) |>
#'     gt::tab_options(data_row.padding = gt::px(pad))
#' }
#' gtsave_pub(build, "outputs/cars.png", width = 8, height = 4)
#' }
#' @export
gtsave_pub <- function(build, path, width, height, pad = NULL,
                       px_per_in = 132, zoom = 3,
                       config = deck_config_get()) {
  pad <- pad %||% config$table_row_padding

  fs::dir_create(fs::path_dir(path))

  gt::gtsave(
    build(pad),
    filename = path,
    vwidth = round(width * px_per_in),
    vheight = round(height * px_per_in),
    zoom = zoom
  )

  compress_pub(path)

  final <- magick::image_info(magick::image_read(path))
  aspect <- final$width / final$height

  cli::cli_alert_success(
    "Wrote {.path {(.path_rel(path, config))}}
     (padding {pad}px, aspect {round(aspect, 2)})"
  )

  list(padding = pad, aspect = aspect)
}
