# entries ----------------------------------------------------------------------

#' List the Rows of the Contents Page
#'
#' Reads the sections and their runs of pages back off a built deck, so a
#' page number on the contents cannot drift from the page it names. A
#' section is read at the divider that opens it, so a deck built from only
#' some sections is still numbered correctly.
#'
#' @param deck An `rpptx` object holding the built pages.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return A tibble with one row per entry and the columns `kind`
#'   (`"section"` or `"run"`), `number`, `label` and `page`.
#'
#' @examples
#' \dontrun{
#' entries <- deck_contents_entries(deck)
#' }
#' @export
deck_contents_entries <- function(deck, config = deck_config_get()) {
  sections <- .sections(config)
  pages <- deck_page_labels(deck, config)

  top <- sections$label[is.na(sections$parent)]
  starts <- which(!is.na(pages$divider) & pages$divider %in% top)

  if (length(starts) == 0) {
    cli::cli_abort(c(
      "No section divider in the deck to build a contents from.",
      "i" = "Add the section pages first."
    ))
  }

  ends <- c(starts[-1] - 1L, nrow(pages))

  purrr::pmap(list(starts, ends, seq_along(starts)), \(from, to, number) {
    block <- dplyr::slice(pages, from:to)
    label <- block$divider[1]

    runs <- block |>
      dplyr::filter(is.na(.data$divider), !is.na(.data$foot)) |>
      dplyr::filter(
        .data$foot != label, !.data$foot %in% config$contents_skip
      ) |>
      dplyr::distinct(.data$foot, .keep_all = TRUE)

    tibble::tibble(
      kind = c("section", rep("run", nrow(runs))),
      # counted over the dividers the deck holds, matching the number each
      # one printed, so a section left out of a build leaves no gap
      number = c(number, rep(NA_integer_, nrow(runs))),
      label = c(label, runs$foot),
      page = c(block$page[1], runs$page)
    )
  }) |>
    purrr::list_rbind()
}

# layout -----------------------------------------------------------------------

# where one section block goes, given where the previous one ended
#' @noRd
.place_block <- function(state, block, height, box, columns, page, depth,
                         n_entries) {
  lead <- if (state$top > box$top) page$section_lead else 0
  filled <- state$top - box$top

  breaks <- state$column < page$n_cols && filled >= depth
  overflows <- state$top + lead + height > box$top + box$height

  if (breaks || overflows) {
    state$column <- state$column + 1L
    state$top <- box$top
    lead <- 0
  }

  if (state$column > page$n_cols || height > box$height) {
    cli::cli_abort(
      c(
        "The contents does not fit in {page$n_cols} column{?s}.",
        "i" = "{n_entries} entr{?y/ies} at {page$row_height} inches each."
      ),
      call = NULL
    )
  }

  rows <- seq_len(nrow(block))

  # built apart from the block, whose own page column would mask the settings
  position <- tibble::tibble(
    left = columns[[state$column]]$left,
    width = columns[[state$column]]$width,
    # the runs sit below the heading's band, which carries the pad
    top = state$top + lead + (rows - 1) * page$row_height +
      c(0, rep(page$section_pad, nrow(block) - 1))
  )

  placed <- dplyr::bind_cols(block, position)

  list(
    column = state$column,
    top = state$top + lead + height,
    rows = c(state$rows, list(placed))
  )
}

#' Lay Out the Rows of the Contents Page
#'
#' Works out where each contents row sits. A section keeps its runs with
#' it, and the columns are filled to about the same depth.
#'
#' @param entries A tibble of entries, as [deck_contents_entries()]
#'   returns.
#' @param box The box the contents fills, a list of `left`, `top`, `width`
#'   and `height` in inches.
#' @param page Contents page settings. Default `NULL`, the
#'   `contents_slide` setting of `config`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return `entries` with the added columns `left`, `width` and `top`, in
#'   inches.
#'
#' @examples
#' entries <- data.frame(
#'   kind = c("section", "run", "section"),
#'   number = c(1L, NA, 2L),
#'   label = c("Epidemiology", "Detections", "Campaigns"),
#'   page = c(3L, 4L, 9L)
#' )
#' deck_contents_layout(
#'   entries, box = list(left = 1, top = 1.5, width = 11, height = 5)
#' )
#' @export
deck_contents_layout <- function(entries, box, page = NULL,
                                 config = deck_config_get()) {
  page <- page %||% config$contents_slide
  columns <- deck_split_box(box, n = page$n_cols, gap = page$gap)

  blocks <- entries |>
    dplyr::mutate(block = cumsum(.data$kind == "section")) |>
    dplyr::group_split(.data$block, .keep = FALSE)

  heights <- purrr::map_dbl(
    blocks, \(block) nrow(block) * page$row_height + page$section_pad
  )

  depth <- (sum(heights) + page$section_lead * (length(blocks) - 1)) /
    page$n_cols

  placed <- purrr::reduce2(
    blocks, heights,
    \(state, block, height) {
      .place_block(
        state, block, height, box, columns, page, depth, nrow(entries)
      )
    },
    .init = list(column = 1L, top = box$top, rows = list())
  )

  purrr::list_rbind(placed$rows)
}

# drawing ----------------------------------------------------------------------

# removes the rows an earlier build drew, so a rebuild draws over a clean page
#' @noRd
.contents_clear <- function(deck) {
  drawn <- function(deck) {
    labels <- officer::slide_summary(deck)$ph_label
    labels <- labels[!is.na(labels)]
    labels[startsWith(labels, "contents_")]
  }

  while (length(drawn(deck)) > 0) {
    deck <- officer::ph_remove(deck, ph_label = drawn(deck)[1])
  }

  deck
}

# the filled band behind a section heading
#' @noRd
.contents_band <- function(width, height, config) {
  .filled_band(config$contents_section_band) |>
    flextable::fontsize(size = config$contents_section_pt, part = "body") |>
    .size_band(width = width, height = height)
}

# one run of text on the contents page
#' @noRd
.contents_chunk <- function(text, size, bold, colour, config) {
  officer::ftext(
    text,
    officer::fp_text(
      font.size = size, bold = bold, color = colour,
      font.family = config$font
    )
  )
}

# a text box that links to its page; a link belongs to a shape, so each
# line is its own box
#' @noRd
.contents_link <- function(deck, name, parts, row, left, width, align,
                           page) {
  paragraph <- do.call(
    officer::fpar,
    c(parts, list(fp_p = officer::fp_par(text.align = align, padding = 0)))
  )

  deck |>
    officer::ph_with(
      value = paragraph,
      location = officer::ph_location(
        left = left, top = row$top, width = width,
        height = page$row_height, newlabel = name
      )
    ) |>
    officer::ph_slidelink(ph_label = name, slide_index = row$page)
}

# one contents row: a band for a section, then the label and page number
#' @noRd
.contents_row <- function(deck, row, i, page, config) {
  is_section <- row$kind == "section"
  size <- if (is_section) config$contents_section_pt else config$contents_run_pt
  indent <- if (is_section) 0 else page$indent
  ink <- if (is_section) {
    config$contents_section_colour
  } else {
    config$contents_run_colour
  }

  lead <- if (is_section) {
    paste0(row$number, ". ")
  } else {
    paste0(config$contents_run_marker, "  ")
  }

  # the number and the marker take the accent, so the two levels are told
  # apart by colour as well as by indent
  label <- list(
    .contents_chunk(
      lead, size, is_section, config$contents_number_colour, config
    ),
    .contents_chunk(row$label, size, is_section, ink, config)
  )

  # drawn before the text, because officer stacks shapes in the order they
  # are placed and the band would otherwise cover the heading
  if (is_section) {
    band_height <- page$row_height + page$section_pad

    deck <- officer::ph_with(
      deck,
      value = .contents_band(row$width, band_height, config),
      location = officer::ph_location(
        left = row$left, top = row$top,
        width = row$width, height = band_height,
        newlabel = paste0("contents_band_", i)
      )
    )
  }

  # the page number takes the section's own weight, so the right edge
  # reads as a column
  deck |>
    .contents_link(
      paste0("contents_label_", i), label, row,
      left = row$left + indent,
      width = row$width - indent - page$number_width,
      align = "left",
      page = page
    ) |>
    .contents_link(
      paste0("contents_page_", i),
      list(.contents_chunk(
        as.character(row$page), size, is_section, ink, config
      )),
      row,
      left = row$left + row$width - page$number_width,
      width = page$number_width,
      align = "right",
      page = page
    )
}

#' Add and Fill the Contents Page
#'
#' `deck_contents_placeholder()` adds the page the contents will occupy.
#' Add it with the cover, so every later page counts it and the slide
#' numbers hold while the deck is built. `deck_contents_slide()` draws the
#' rows once the deck is complete, each linked to its page, and replaces
#' any rows an earlier build drew.
#'
#' @param deck An `rpptx` object.
#' @param title Title of the page. Default `NULL`, the `contents_title`
#'   setting of `config`.
#' @param entries A tibble of entries, as [deck_contents_entries()]
#'   returns.
#' @param index Slide the contents sits on. Default `NULL`, the
#'   `contents_page` setting of `config`.
#' @param page Contents page settings. Default `NULL`, the
#'   `contents_slide` setting of `config`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return The deck.
#'
#' @examples
#' \dontrun{
#' deck <- deck_open() |>
#'   deck_title_slide() |>
#'   deck_contents_placeholder()
#'
#' # after the sections are built
#' deck <- deck_contents_slide(deck, deck_contents_entries(deck))
#' }
#' @export
deck_contents_placeholder <- function(deck, title = NULL, page = NULL,
                                      config = deck_config_get()) {
  page <- page %||% config$contents_slide

  deck |>
    officer::add_slide(layout = page$layout, master = config$master) |>
    officer::ph_with(
      value = title %||% config$contents_title,
      location = .label_location("page_title", config)
    )
}

#' @rdname deck_contents_placeholder
#' @export
deck_contents_slide <- function(deck, entries, index = NULL, page = NULL,
                                config = deck_config_get()) {
  page <- page %||% config$contents_slide
  box <- deck_layout_box(page$layout, label = page$box, config = config)
  rows <- deck_contents_layout(entries, box, page, config)

  deck <- deck |>
    officer::on_slide(index = index %||% config$contents_page) |>
    .contents_clear()

  purrr::reduce(
    seq_len(nrow(rows)),
    \(deck, i) .contents_row(deck, dplyr::slice(rows, i), i, page, config),
    .init = deck
  )
}
