# Builds template.pptx, the neutral template the tests run against, from the
# template officer ships. Layouts and placeholders are renamed to the names
# deck_config() expects by default. Run from the package root after an
# officer release changes its template:
#   Rscript tests/testthat/fixtures/make_template.R

renames <- list(
  "Title Slide" = list(layout = "main_title", labels = character(0)),
  "Section Header" = list(
    layout = "section_page_main",
    labels = c("Title 1" = "section_title", "Text Placeholder 2" = "section")
  ),
  "Title Only" = list(
    layout = "section_page",
    labels = c("Title 1" = "section_title")
  ),
  "Title and Content" = list(
    layout = "summary_text",
    labels = c(
      "Title 1" = "page_title",
      "Content Placeholder 2" = "Text Placeholder 5",
      "Footer Placeholder 4" = "section"
    )
  ),
  "Two Content" = list(
    layout = "two_plots",
    labels = c(
      "Title 1" = "page_title",
      "Content Placeholder 2" = "plot",
      "Content Placeholder 3" = "note",
      "Footer Placeholder 5" = "section"
    )
  )
)

rename_layout <- function(path) {
  xml <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  name <- sub(".*<p:cSld name=\"([^\"]*)\">.*", "\\1", xml)

  if (!name %in% names(renames)) {
    return(invisible(FALSE))
  }

  change <- renames[[name]]

  xml <- sub(
    paste0("<p:cSld name=\"", name, "\">"),
    paste0("<p:cSld name=\"", change$layout, "\">"),
    xml, fixed = TRUE
  )

  for (old in names(change$labels)) {
    xml <- gsub(
      paste0("name=\"", old, "\""),
      paste0("name=\"", change$labels[[old]], "\""),
      xml, fixed = TRUE
    )
  }

  writeLines(xml, path, useBytes = TRUE)

  invisible(TRUE)
}

work <- tempfile("template")

utils::unzip(
  system.file("template", "template.pptx", package = "officer"),
  exdir = work
)

layouts <- list.files(
  file.path(work, "ppt", "slideLayouts"), pattern = "\\.xml$",
  full.names = TRUE
)

invisible(lapply(layouts, rename_layout))

target <- file.path("tests", "testthat", "fixtures", "template.pptx")

if (file.exists(target)) {
  file.remove(target)
}

zip::zipr(
  target,
  files = list.files(work, full.names = TRUE, all.files = TRUE, no.. = TRUE)
)
