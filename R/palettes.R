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

#' Categorical House Palette
#'
#' Returns the eight categorical colours used for series. The first colour
#' is the focal series. Bind a colour to an entity once with `named`, then
#' reuse the result across a report so each entity keeps its colour.
#'
#' @param n Number of colours to return. Default `NULL`, all eight.
#' @param named Character vector of entity names. When given, the result
#'   is a named vector with one colour per entity, and `n` is ignored.
#'
#' @return Character vector of hex colours, named when `named` is given.
#'
#' @examples
#' pub_palette(3)
#' pub_palette(named = c("Env", "AFP", "Contact"))
#' @export
pub_palette <- function(n = NULL, named = NULL) {
  available <- length(.pub_colours)

  if (!is.null(named)) {
    if (length(named) > available) {
      cli::cli_abort(c(
        "The palette carries {available} colours.",
        "x" = "{length(named)} entities were asked for."
      ))
    }
    return(stats::setNames(.pub_colours[seq_along(named)], named))
  }

  if (is.null(n)) {
    return(.pub_colours)
  }

  if (n > available) {
    cli::cli_abort("The palette carries {available} colours, not {n}.")
  }

  .pub_colours[seq_len(n)]
}

#' Ordered House Colour Ramps
#'
#' Returns one of the ordered ramps. `"coverage"` runs from low red to high
#' blue and is the same on every map, so a reader learns it once. `"harm"`
#' runs from white to dark red. `"silence"` runs from pale to deep purple
#' and is for elapsed time, which is not an alarm scale. `"diverging"` runs
#' from low blue to high red around a neutral middle band.
#'
#' @param name One of `"coverage"`, `"harm"`, `"silence"` or `"diverging"`.
#' @param n Number of colours. Default `NULL`, the ramp as defined. With
#'   `n`, the ramp is interpolated to exactly `n` colours.
#'
#' @return Character vector of hex colours.
#'
#' @examples
#' pub_ramp("coverage")
#' pub_ramp("harm", n = 7)
#' @export
pub_ramp <- function(name = c("coverage", "harm", "silence", "diverging"),
                     n = NULL) {
  name <- rlang::arg_match(name)
  colours <- .pub_ramps[[name]]

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
#' @return A single hex colour.
#'
#' @examples
#' pub_alarm()
#' @export
pub_alarm <- function() {
  "#B2182B"
}

#' Map Fills and Outline Colours
#'
#' Returns the colours a map uses for ground that carries no series colour.
#' `not_measured` is for an area outside the round, which is shown and
#' never dropped. `no_detection` is for a counted zero, and is neutral
#' because green against a red ramp is unreadable with a colour vision
#' deficiency. The `focus_*` colours are for the reporting country and the
#' `context_*` colours for a neighbour shown for context. The `contested*`
#' colours are for ground that carries no data.
#'
#' @return Named character vector of colours.
#'
#' @examples
#' pub_map_colours()[["not_measured"]]
#' @export
pub_map_colours <- function() {
  .pub_map_colours
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
