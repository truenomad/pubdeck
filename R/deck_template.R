# opening ----------------------------------------------------------------------

#' Open the Template as an Empty Deck
#'
#' Reads the PowerPoint template, ready for pages to be added.
#'
#' @param template Path to the `.pptx` template. Default `NULL`, the
#'   `template` setting of `config`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return An `rpptx` object from 'officer'.
#'
#' @examples
#' \dontrun{
#' deck_config_set(deck_config(template = "templates/slide_template.pptx"))
#' deck <- deck_open()
#' }
#' @export
deck_open <- function(template = NULL, config = deck_config_get()) {
  template <- template %||% .cfg_required("template", config)

  if (!fs::file_exists(template)) {
    cli::cli_abort("No template at {.path {template}}.")
  }

  officer::read_pptx(template)
}

#' Reopen a Deck That an Earlier Section Left
#'
#' Reads a deck that is built section by section, and drops the pages the
#' current section added in an earlier run, so a rerun replaces its pages
#' and does not append to them. A deck shorter than the slide count its
#' predecessor recorded is not the deck this section extends, so the
#' function stops.
#'
#' @param path Path to the deck.
#' @param manifest A list with `base_slides`, the number of slides the
#'   deck held when the previous section finished.
#' @param reread Logical. Write the deck and read it back after dropping
#'   pages. 'officer' can lose the next slide added in a session that also
#'   removed one, and rereading avoids that. Default `FALSE`.
#'
#' @return An `rpptx` object holding `manifest$base_slides` slides.
#'
#' @examples
#' \dontrun{
#' deck <- deck_reopen("outputs/weekly_deck.pptx", list(base_slides = 12))
#' }
#' @export
deck_reopen <- function(path, manifest, reread = FALSE) {
  deck <- officer::read_pptx(path)
  base <- manifest$base_slides

  if (length(deck) < base) {
    cli::cli_abort(c(
      "{.path {fs::path_file(path)}} holds {length(deck)} slide{?s},
       fewer than the {base} its predecessor recorded.",
      "i" = "Build the sections in order from the first."
    ))
  }

  extra <- length(deck) - base

  if (extra == 0) {
    return(deck)
  }

  cli::cli_alert_info("Dropping {extra} page{?s} from a previous run")

  deck <- purrr::reduce(
    seq_len(extra),
    \(deck, i) officer::remove_slide(deck, index = length(deck)),
    .init = deck
  )

  if (reread) {
    print(deck, target = path)
    deck <- officer::read_pptx(path)
  }

  deck
}

# template contract ------------------------------------------------------------

# the layouts and placeholders the page builders address
#' @noRd
.template_needs <- function(config) {
  layouts <- config$layouts
  labels <- config$labels
  contents <- config$contents_slide

  tibble::tribble(
    ~use, ~layout, ~placeholder,
    "cover", layouts$cover, NA_character_,
    "section", layouts$section, labels$section,
    "section", layouts$section, labels$section_title,
    "subsection", layouts$subsection, labels$section_title,
    "closing", layouts$closing, labels$section_title,
    "contents", contents$layout, labels$page_title,
    "contents", contents$layout, contents$box
  )
}

# one line per missing requirement, for the error message
#' @noRd
.template_gaps <- function(missing) {
  purrr::map2_chr(missing$layout, missing$placeholder, \(layout, ph) {
    if (is.na(ph)) {
      paste0("Layout '", layout, "'.")
    } else {
      paste0("Placeholder '", ph, "' on layout '", layout, "'.")
    }
  })
}

#' Check a Template Has What the Page Builders Need
#'
#' Compares a PowerPoint template with the layouts and placeholder labels
#' a configuration names: the cover, the section and subsection dividers,
#' the closing page and the contents page. Run it once when a template is
#' adopted or edited, because a renamed layout otherwise fails part way
#' through a build.
#'
#' @param template Path to the `.pptx` template. Default `NULL`, the
#'   `template` setting of `config`.
#' @param error Logical. Stop when something is missing. Default `TRUE`.
#'   With `FALSE` the result is returned for inspection.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return A tibble with one row per requirement and the columns `use`,
#'   `layout`, `placeholder` and `found`, invisibly.
#'
#' @examples
#' \dontrun{
#' deck_check_template("templates/slide_template.pptx")
#' }
#' @export
deck_check_template <- function(template = NULL, error = TRUE,
                                config = deck_config_get()) {
  template <- template %||% .cfg_required("template", config)

  if (!fs::file_exists(template)) {
    cli::cli_abort("No template at {.path {template}}.")
  }

  deck <- officer::read_pptx(template)
  wanted_master <- config$master

  layouts <- officer::layout_summary(deck) |>
    dplyr::filter(.data$master == wanted_master)

  placeholders <- officer::layout_properties(deck) |>
    dplyr::filter(.data$master_name == wanted_master)

  result <- .template_needs(config) |>
    dplyr::mutate(
      found = purrr::map2_lgl(.data$layout, .data$placeholder, \(layout, ph) {
        labelled <- placeholders$ph_label[placeholders$name == layout]
        layout %in% layouts$layout && (is.na(ph) || ph %in% labelled)
      })
    )

  missing <- dplyr::filter(result, !.data$found)

  if (nrow(missing) == 0) {
    cli::cli_alert_success(
      "{.path {fs::path_file(template)}} has every layout and placeholder
       the configuration names"
    )
    return(invisible(result))
  }

  if (error) {
    gaps <- .template_gaps(missing)

    cli::cli_abort(c(
      "{.path {fs::path_file(template)}} lacks {nrow(missing)}
       requirement{?s} under master {.val {wanted_master}}.",
      stats::setNames(gaps, rep("x", length(gaps))),
      "i" = "Rename them in the template, or set {.field layouts},
             {.field labels} and {.field contents_slide} in
             {.fn deck_config}."
    ))
  }

  invisible(result)
}

#' Read the Box a Layout Reserves
#'
#' Returns the position and size of a labelled placeholder on a layout, in
#' inches. The template is the authority on where an asset sits, so page
#' code reads the box and does not restate its geometry.
#'
#' @param layout Name of the layout.
#' @param label Label of the placeholder. Default `"plot"`.
#' @param master Name of the slide master. Default `NULL`, the `master`
#'   setting of `config`.
#' @param template Path to the `.pptx` template. Default `NULL`, the
#'   `template` setting of `config`.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force.
#'
#' @return A list of `left`, `top`, `width` and `height`.
#'
#' @examples
#' \dontrun{
#' deck_layout_box("one_plot", label = "plot")
#' }
#' @export
deck_layout_box <- function(layout, label = "plot", master = NULL,
                            template = NULL, config = deck_config_get()) {
  master <- master %||% config$master
  template <- template %||% .cfg_required("template", config)

  placeholders <- officer::layout_properties(
    officer::read_pptx(template), layout = layout, master = master
  )

  box <- dplyr::filter(placeholders, .data$ph_label == label)

  if (nrow(box) != 1) {
    cli::cli_abort(c(
      "Layout {.val {layout}} has {nrow(box)} placeholder{?s} labelled
       {.val {label}}.",
      "i" = "Labelled: {.val {sort(placeholders$ph_label)}}."
    ))
  }

  list(left = box$offx, top = box$offy, width = box$cx, height = box$cy)
}

# reading a built deck ---------------------------------------------------------

# the text of one labelled placeholder on a slide, or NA when it has none
#' @noRd
.shape_text <- function(shapes, label) {
  if (nrow(shapes) == 0) {
    return(NA_character_)
  }

  text <- shapes$text[!is.na(shapes$ph_label) & shapes$ph_label == label]
  text <- trimws(text[!is.na(text)])
  text <- text[nzchar(text)]

  if (length(text) == 0) NA_character_ else text[1]
}

#' Read What Each Page of a Deck Belongs To
#'
#' Reads a built deck back, one row per slide in presentation order: the
#' divider title where the page is a divider, the page title, the section
#' in the foot strip and the source line.
#'
#' @param deck An `rpptx` object.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force. Its `labels` setting names the
#'   placeholders that are read.
#'
#' @return A tibble with the columns `page`, `divider`, `title`, `foot`
#'   and `source`. A slide without one of them has `NA` there.
#'
#' @examples
#' \dontrun{
#' deck_page_labels(officer::read_pptx("outputs/weekly_deck.pptx"))
#' }
#' @export
deck_page_labels <- function(deck, config = deck_config_get()) {
  labels <- config$labels

  purrr::map(seq_len(length(deck)), \(page) {
    # slide_summary(index =) reads physical parts; on_slide() resolves the
    # presentation order, which can differ after move_slide()
    shapes <- officer::slide_summary(officer::on_slide(deck, index = page))

    tibble::tibble(
      page = page,
      divider = .shape_text(shapes, labels$section_title),
      title = .shape_text(shapes, labels$page_title),
      foot = .shape_text(shapes, labels$section),
      source = .shape_text(shapes, labels$source)
    )
  }) |>
    purrr::list_rbind()
}

#' List the Titles a Built Deck Carries
#'
#' Reads the page titles and divider titles of a saved deck. Only titles
#' are read, and never an empty one, because an empty string is a
#' substring of every page and would make any reference check pass.
#'
#' @param path Path to the deck.
#' @param config A configuration built with [deck_config()]. Default is
#'   the configuration in force. Its `labels` setting names the title
#'   placeholders.
#'
#' @return Character vector of distinct titles.
#'
#' @examples
#' \dontrun{
#' deck_page_titles("outputs/weekly_deck.pptx")
#' }
#' @export
deck_page_titles <- function(path, config = deck_config_get()) {
  deck <- officer::read_pptx(path)
  title_labels <- c(config$labels$page_title, config$labels$section_title)

  titles <- purrr::map(seq_len(length(deck)), \(page) {
    shapes <- tryCatch(
      officer::slide_summary(deck, index = page),
      error = \(e) NULL
    )

    if (is.null(shapes) || !"ph_label" %in% names(shapes)) {
      return(character())
    }

    shapes$text[
      !is.na(shapes$text) & !is.na(shapes$ph_label) &
        shapes$ph_label %in% title_labels
    ]
  })

  titles <- trimws(unlist(titles))

  unique(titles[nzchar(titles)])
}

#' Check That a Deck Carries a Page
#'
#' Tests whether a page title appears among the titles of a deck. The
#' match is a substring match in either direction and ignores case,
#' because a title can carry a computed figure that changes between
#' builds and can lead with a count written in lower case.
#'
#' @param page A single page title to look for.
#' @param titles Character vector of titles, as [deck_page_titles()]
#'   returns them.
#'
#' @return `TRUE` or `FALSE`. An empty or missing `page` gives `FALSE`.
#'
#' @examples
#' deck_carries_page("AFP cases", c("12 AFP cases awaiting classification"))
#' deck_carries_page("", c("Summary"))
#' @export
deck_carries_page <- function(page, titles) {
  page <- tolower(trimws(page))

  if (length(page) != 1 || is.na(page) || !nzchar(page)) {
    return(FALSE)
  }

  titles <- tolower(trimws(titles[!is.na(titles)]))
  titles <- titles[nzchar(titles)]

  if (length(titles) == 0) {
    return(FALSE)
  }

  any(grepl(page, titles, fixed = TRUE)) ||
    any(purrr::map_lgl(titles, \(title) grepl(title, page, fixed = TRUE)))
}
