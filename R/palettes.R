# colours ----------------------------------------------------------------------

# the first colour is the focal series
.pub_colours <- c(
  "#A9552B", "#4E6E5D", "#2F5061", "#B08A3E", "#7A5C7B", "#5C6B73",
  "#8A9A5B", "#96604A"
)

.pub_ramps <- list(
  # coverage bands, low red to high blue
  coverage = c(
    "#B2182B", "#EF8A62", "#FDDBC7", "#D1E5F0", "#67A9CF", "#2166AC"
  ),
  # counts of harm or disagreement, white to dark red
  harm = c("#FFF5F0", "#FCBBA1", "#FB6A4A", "#CB181D", "#67000D"),
  # elapsed time, pale to deep purple, so it does not read as an alarm scale
  silence = c("#F2EFF7", "#CBC0DD", "#9E9AC8", "#6A51A3", "#3F007D"),
  # binned diverging fill, low blue to high red, centred on the middle band
  diverging = c("#2166AC", "#92C5DE", "#F7F7F7", "#F4A582", "#B2182B")
)

.pub_map_colours <- c(
  not_measured = "grey85",
  no_detection = "#F4F4F2",
  focus_land = "#FBF7F2",
  context_land = "#F6F6F4",
  focus_label = "grey30",
  context_label = "grey55",
  context_border = "grey72",
  context_adm1 = "grey88",
  contested = "grey93",
  contested_border = "grey80"
)

.pub_alarm <- "#B2182B"

# a colour setting, checked as a character vector of colours
#' @noRd
.colour_setting <- function(config, name, single = FALSE, named = FALSE) {
  value <- config[[name]]

  if (is.null(value)) {
    return(NULL)
  }

  shape_ok <- is.character(value) && length(value) > 0 &&
    !anyNA(value) && (!single || length(value) == 1) &&
    (!named || rlang::is_named(value))

  if (!shape_ok) {
    what <- if (single) "a single colour" else "a character vector of colours"
    what <- if (named) "a named character vector of colours" else what

    cli::cli_abort(
      "The {.field {name}} setting must be {what}.",
      call = rlang::caller_env()
    )
  }

  value
}

# the ramps a configuration holds: the built-in ones, with any of the same
# name replaced and any new name added
#' @noRd
.config_ramps <- function(config) {
  ramps <- config$ramps

  if (is.null(ramps)) {
    return(.pub_ramps)
  }

  ramps_ok <- is.list(ramps) && rlang::is_named(ramps) &&
    all(purrr::map_lgl(ramps, \(ramp) is.character(ramp) && !anyNA(ramp)))

  if (!ramps_ok) {
    cli::cli_abort(
      "The {.field ramps} setting must be a named list of colour vectors.",
      call = rlang::caller_env()
    )
  }

  utils::modifyList(.pub_ramps, ramps)
}

#' Categorical House Palette
#'
#' Returns the categorical colours used for series, eight unless the
#' `palette` setting holds a palette of its own. The first colour is the
#' focal series. Bind a colour to an entity once with `named`, then reuse
#' the result across a report so each entity keeps its colour.
#'
#' @param n Number of colours to return. Default `NULL`, all of them.
#' @param named Character vector of entity names. When given, the result
#'   is a named vector with one colour per entity, and `n` is ignored.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force. Its `palette` setting replaces the
#'   built-in colours.
#'
#' @return Character vector of hex colours, named when `named` is given.
#'
#' @examples
#' pub_palette(3)
#' pub_palette(named = c("Env", "AFP", "Contact"))
#' pub_palette(config = deck_config(palette = c("#B5693F", "#476074")))
#' @export
pub_palette <- function(n = NULL, named = NULL, config = deck_config_get()) {
  colours <- .colour_setting(config, "palette") %||% .pub_colours
  available <- length(colours)

  if (!is.null(named)) {
    if (length(named) > available) {
      cli::cli_abort(c(
        "The palette carries {available} colours.",
        "x" = "{length(named)} entities were asked for."
      ))
    }
    return(stats::setNames(colours[seq_along(named)], named))
  }

  if (is.null(n)) {
    return(colours)
  }

  if (n > available) {
    cli::cli_abort("The palette carries {available} colours, not {n}.")
  }

  colours[seq_len(n)]
}

#' Ordered House Colour Ramps
#'
#' Returns one of the ordered ramps. `"coverage"` runs from low red to high
#' blue and is the same on every map, so a reader learns it once. `"harm"`
#' runs from white to dark red. `"silence"` runs from pale to deep purple
#' and is for elapsed time, which is not an alarm scale. `"diverging"` runs
#' from low blue to high red around a neutral middle band. The `ramps`
#' setting replaces any of these by name and adds ramps of its own.
#'
#' @param name Name of a ramp the configuration holds: `"coverage"`,
#'   `"harm"`, `"silence"`, `"diverging"`, or a name the `ramps` setting
#'   adds. Default `"coverage"`.
#' @param n Number of colours. Default `NULL`, the ramp as defined. With
#'   `n`, the ramp is interpolated to exactly `n` colours.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return Character vector of hex colours.
#'
#' @examples
#' pub_ramp("coverage")
#' pub_ramp("harm", n = 7)
#'
#' # a ramp of the report's own
#' config <- deck_config(ramps = list(seq5_blue = c("#F7F7F7", "#2166AC")))
#' pub_ramp("seq5_blue", config = config)
#' @export
pub_ramp <- function(name = "coverage", n = NULL,
                     config = deck_config_get()) {
  ramps <- .config_ramps(config)

  if (!rlang::is_string(name) || !name %in% names(ramps)) {
    cli::cli_abort(c(
      "{.arg name} must name a ramp the configuration holds.",
      "i" = "The ramps are {.val {names(ramps)}}."
    ))
  }

  colours <- ramps[[name]]

  if (is.null(n)) {
    return(colours)
  }

  grDevices::colorRampPalette(colours)(n)
}

#' Alarm Colour
#'
#' Returns the colour reserved for flagged or anomalous marks. It is never
#' used for an ordinary series.
#'
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force. Its `alarm` setting replaces the built-in
#'   colour.
#'
#' @return A single hex colour.
#'
#' @examples
#' pub_alarm()
#' pub_alarm(config = deck_config(alarm = "#C0392B"))
#' @export
pub_alarm <- function(config = deck_config_get()) {
  .colour_setting(config, "alarm", single = TRUE) %||% .pub_alarm
}

#' Map Fills and Outline Colours
#'
#' Returns the colours a map uses for ground that carries no series colour.
#' `not_measured` is for an area outside the round, which is shown and
#' never dropped. `no_detection` is for a counted zero, and is neutral
#' because green against a red ramp is unreadable with a colour vision
#' deficiency. The `focus_*` colours are for the reporting country and the
#' `context_*` colours for a neighbour shown for context. The `contested*`
#' colours are for ground that carries no data. The `map_colours` setting
#' replaces any of these by name and adds colours of its own.
#'
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return Named character vector of colours.
#'
#' @examples
#' pub_map_colours()[["not_measured"]]
#' pub_map_colours(
#'   config = deck_config(map_colours = c(not_measured = "grey80"))
#' )[["not_measured"]]
#' @export
pub_map_colours <- function(config = deck_config_get()) {
  given <- .colour_setting(config, "map_colours", named = TRUE)

  if (is.null(given)) {
    return(.pub_map_colours)
  }

  utils::modifyList(as.list(.pub_map_colours), as.list(given)) |>
    unlist()
}

#' Tint Colours Towards White
#'
#' Returns a light tint of each colour, for a table cell that stays
#' readable behind black text.
#'
#' @param colours Character vector of colours. Names are kept.
#' @param weight How far each colour is pulled towards white, from 0 to 1.
#'   Default `0.82`.
#'
#' @return Character vector of hex colours, with the names of `colours`.
#'
#' @examples
#' tint(pub_palette(named = c("AFP", "Env")))
#' @export
tint <- function(colours, weight = 0.82) {
  mixed <- t(grDevices::col2rgb(colours)) * (1 - weight) + 255 * weight
  out <- grDevices::rgb(mixed, maxColorValue = 255)

  stats::setNames(out, names(colours))
}

# relative luminance of an sRGB channel, as WCAG defines it
#' @noRd
.channel_luminance <- function(x) {
  x <- x / 255

  dplyr::if_else(x <= 0.03928, x / 12.92, ((x + 0.055) / 1.055)^2.4)
}

#' @noRd
.luminance <- function(colour) {
  rgb <- grDevices::col2rgb(colour)

  0.2126 * .channel_luminance(rgb[1, ]) +
    0.7152 * .channel_luminance(rgb[2, ]) +
    0.0722 * .channel_luminance(rgb[3, ])
}

#' @noRd
.contrast_ratio <- function(a, b) {
  (max(a, b) + 0.05) / (min(a, b) + 0.05)
}

#' Ink That Reads Against a Fill
#'
#' Chooses dark or light text for each fill by WCAG contrast ratio, so a
#' pale band takes dark ink and a saturated one takes light ink.
#'
#' @param fill Character vector of fill colours. Names are kept.
#' @param dark,light The two ink colours to choose between.
#'
#' @return Character vector the length of `fill`, with its names.
#'
#' @examples
#' contrast_ink(c("#FFF5F0", "#67000D"))
#' @export
contrast_ink <- function(fill, dark = "#1A1A1A", light = "#FFFFFF") {
  dark_luminance <- .luminance(dark)[[1]]

  ink <- purrr::map_chr(unname(.luminance(fill)), \(luminance) {
    on_dark <- .contrast_ratio(luminance, dark_luminance)
    on_white <- .contrast_ratio(luminance, 1)

    if (on_dark >= on_white) dark else light
  })

  stats::setNames(ink, names(fill))
}
