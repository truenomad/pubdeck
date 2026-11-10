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

#' List the Contents by Page Title
#'
#' Reads the contents back off a built deck, as [deck_contents_entries()]
#' does, with another reading of what a section is. Every divider opens a
#' section, whatever its level, and the pages under it are listed by
#' their page titles and not by their foot strips. Use it for a deck
#' whose pages are worth naming one by one.
#'
#' @param deck An `rpptx` object holding the built pages.
#' @param collapsed Titles of sections to list by their divider alone.
#'   Default `NULL`, the `contents_collapsed` setting of `config`.
#' @param dropped Titles of sections to leave out. A section left out
#'   keeps its number, so the numbers of the others do not move. Default
#'   `NULL`, the `contents_dropped` setting of `config`.
#' @param standalone Titles of pages to list as numbered sections of
#'   their own, though no divider opens them. Default `NULL`, the
#'   `contents_standalone` setting of `config`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return A tibble with one row per entry and the columns `kind`
#'   (`"section"` or `"run"`), `number`, `label` and `page`.
#'
#' @examples
#' \dontrun{
#' entries <- deck_contents_title_entries(deck, collapsed = "Annex")
#' }
#' @export
deck_contents_title_entries <- function(deck, collapsed = NULL,
                                        dropped = NULL, standalone = NULL,
                                        config = deck_config_get()) {
  collapsed <- collapsed %||% config$contents_collapsed
  dropped <- dropped %||% config$contents_dropped
  standalone <- standalone %||% config$contents_standalone

  pages <- deck_page_labels(deck, config)
  alone <- is.na(pages$divider) & pages$title %in% standalone
  starts <- which(!is.na(pages$divider) | alone)

  if (length(starts) == 0) {
    cli::cli_abort(c(
      "No section divider in the deck to build a contents from.",
      "i" = "Add the section pages first, or name pages in
             {.arg standalone}."
    ))
  }

  ends <- c(starts[-1] - 1L, nrow(pages))

  purrr::pmap(list(starts, ends, seq_along(starts)), \(from, to, number) {
    block <- dplyr::slice(pages, from:to)
    label <- dplyr::coalesce(block$divider[1], block$title[1])

    if (label %in% dropped) {
      return(NULL)
    }

    runs <- block |>
      dplyr::slice(-1) |>
      dplyr::filter(is.na(.data$divider), !is.na(.data$title))

    if (label %in% collapsed) {
      runs <- dplyr::slice(runs, 0)
    }

    tibble::tibble(
      kind = c("section", rep("run", nrow(runs))),
      number = c(number, rep(NA_integer_, nrow(runs))),
      label = c(label, runs$title),
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

# two balanced columns ---------------------------------------------------------

# the top of each row of one column, and where the column ends
#' @noRd
.fill_column <- function(kinds, start, page) {
  purrr::reduce(
    kinds,
    \(state, kind) {
      is_section <- kind == "section"

      # a section below the first row of its column takes a lead above it
      lead <- if (is_section && state$end > start) page$section_lead else 0
      top <- state$end + lead

      # a little more room under each section heading
      gap <- if (is_section) page$section_gap else 0

      list(tops = c(state$tops, top), end = top + page$row_height + gap)
    },
    .init = list(tops = numeric(0), end = start)
  )
}

# where each row sits in two columns, split at the section break that
# leaves the taller column shortest
#' @noRd
.contents_two_columns <- function(entries, box, page) {
  column_width <- (box$width - page$gap) / 2
  start <- box$top + page$top_offset

  # a section and the pages listed under it move as one block
  block <- cumsum(entries$kind == "section")
  rows_in_block <- dplyr::count(tibble::tibble(block = block), .data$block)$n

  heights <- rows_in_block * page$row_height + page$section_lead +
    page$section_gap

  tallest <- purrr::map_dbl(seq_along(heights), \(k) {
    max(sum(utils::head(heights, k)), sum(utils::tail(heights, -k)))
  })

  column <- dplyr::if_else(block <= which.min(tallest), 1L, 2L)

  filled <- purrr::map(
    1:2, \(k) .fill_column(entries$kind[column == k], start, page)
  )

  overflows <- purrr::map_lgl(filled, \(side) side$end > box$top + box$height)

  if (any(overflows)) {
    cli::cli_abort(c(
      "The contents does not fit in two columns.",
      "i" = "{nrow(entries)} entr{?y/ies} at {page$row_height} inches each."
    ))
  }

  # built apart from the entries, whose own page column would mask the
  # settings
  position <- tibble::tibble(
    left = box$left + (column - 1) * (column_width + page$gap),
    width = column_width,
    top = c(filled[[1]]$tops, filled[[2]]$tops)
  )

  dplyr::bind_cols(entries, position)
}

# one row of the filled contents: the label, then the page number, each a
# text box linked to its page
#' @noRd
.contents_fill_row <- function(deck, row, i, page, section_weight, config) {
  is_section <- row$kind == "section"
  size <- if (is_section) config$contents_section_pt else config$contents_run_pt
  indent <- if (is_section) 0 else page$indent
  weight <- if (is_section) section_weight else "regular"
  ink <- if (is_section) {
    config$contents_section_colour
  } else {
    config$contents_run_colour
  }

  text <- officer::ftext(
    row$label, deck_fp_text(size, ink, weight, config = config)
  )

  label <- if (is_section) {
    number <- officer::ftext(
      paste0(row$number, ". "),
      deck_fp_text(
        size, config$contents_number_colour, weight, config = config
      )
    )
    list(number, text)
  } else {
    list(text)
  }

  page_number <- officer::ftext(
    as.character(row$page), deck_fp_text(size, ink, weight, config = config)
  )

  deck |>
    .contents_link(
      paste0("contents_label_", i), label, row,
      left = row$left + indent,
      width = row$width - indent - page$number_width,
      align = "left",
      page = page
    ) |>
    .contents_link(
      paste0("contents_page_", i), list(page_number), row,
      left = row$left + row$width - page$number_width,
      width = page$number_width,
      align = "right",
      page = page
    )
}

#' Fill the Contents Page in Two Balanced Columns
#'
#' Draws the contents of a built deck as one linked line per entry, in
#' two columns. A section keeps its pages with it, and the columns break
#' at the section that leaves the taller column shortest. Section rows
#' are set in the emphasis weight with their number in the
#' `contents_number_colour`, and no band is drawn behind them. The rows
#' fill a box given in inches, so the page needs no placeholder for
#' them, and they replace any rows an earlier build drew.
#'
#' [deck_contents_slide()] is the contents for a deck listed by section
#' and foot strip, in any number of columns.
#'
#' @param deck An `rpptx` object holding the built pages.
#' @param entries A tibble of entries. Default `NULL`, the entries
#'   [deck_contents_title_entries()] reads off `deck`.
#' @param index Slide the contents sits on. Default `NULL`, the
#'   `contents_page` setting of `config`.
#' @param page Row spacing, a list with any of `gap`, `row_height`,
#'   `section_lead`, `section_gap`, `top_offset`, `indent` and
#'   `number_width`, in inches. Default `NULL`, the `contents_fill`
#'   setting of `config`.
#' @param box The box the contents fills, a list of `left`, `top`,
#'   `width` and `height` in inches. Default `NULL`, the `content_box`
#'   setting of `config`.
#' @param collapsed,dropped,standalone Passed to
#'   [deck_contents_title_entries()] when `entries` is `NULL`. Default
#'   `NULL` for each, the matching `contents_*` setting of `config`.
#' @param section_weight Weight of the section rows. Default
#'   `"semibold"`.
#' @param ... Settings of [deck_config()] to change for this call, as
#'   `name = value`, such as `contents_section_pt` or
#'   `contents_number_colour`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force. The spacing and the box default to those
#'   of the template the builder was first written for.
#'
#' @return The deck, with the contents drawn.
#'
#' @examples
#' \dontrun{
#' deck <- deck_contents_fill(deck, collapsed = "Annex")
#' }
#' @export
deck_contents_fill <- function(deck, entries = NULL, index = NULL,
                               page = NULL, box = NULL, collapsed = NULL,
                               dropped = NULL, standalone = NULL,
                               section_weight = "semibold", ...,
                               config = deck_config_get()) {
  config <- .call_config(config, ...)
  page <- .nested(page, "contents_fill", config)

  entries <- entries %||% deck_contents_title_entries(
    deck, collapsed, dropped, standalone, config
  )

  rows <- .contents_two_columns(entries, box %||% config$content_box, page)

  deck <- deck |>
    officer::on_slide(index = index %||% config$contents_page) |>
    .contents_clear()

  purrr::reduce(
    seq_len(nrow(rows)),
    \(deck, i) {
      .contents_fill_row(
        deck, dplyr::slice(rows, i), i, page, section_weight, config
      )
    },
    .init = deck
  )
}
