# pubdeck

A house figure theme and a PowerPoint deck builder for routine reports.

pubdeck holds the parts of a reporting pipeline that are the same from one
report to the next: the ggplot2 theme and colour palettes, the functions
that save figures and tables at a stated size, and the functions that add
a cover, section dividers, a contents page, page footers and explainer
boxes to a deck built with officer. Each report keeps its own template,
wording and section list, and passes them in as a configuration.

## Install

```r
# install.packages("pak")
pak::pak("truenomad/pubdeck")
```

## Theme and palettes

Start every chart from `theme_pub()`. `theme_pub_map()` is the same theme
for maps, `theme_pub_cols()` is for columns over rounds or dates, and
`theme_pub_panel()` is for a small panel that shares a page.

```r
ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point(colour = pubdeck::pub_palette(1)) +
  pubdeck::scale_y_pub() +
  pubdeck::theme_pub(grid = "y")
```

`pub_palette()` gives the categorical colours. Bind a colour to an entity
once and reuse the result, so each entity keeps its colour across a
report.

```r
source_colours <- pubdeck::pub_palette(named = c("Env", "AFP", "Contact"))
```

`pub_ramp()` gives the ordered ramps for coverage, harm, elapsed time and
diverging fills. `pub_alarm()` is reserved for flagged marks, and
`pub_map_colours()` holds the fills for ground that carries no series
colour.

## Save figures and tables

`ggsave_pub()` needs a width and a height, so the file that is checked is
the file that ships. It also keeps the drawn chart beside the PNG, which
lets `deck_figure()` place it on a slide as editable shapes.

```r
pubdeck::ggsave_pub(plot, "outputs/trend.png", width = 6, height = 3)
```

`gtsave_pub()` renders a gt table at the house row padding.
`style_deck_table()` gives a gt table the house look, and `bar_cell()`,
`dot_cell()`, `chip_cell()`, `spark_cell()` and `move_cell()` draw small
graphics inside table cells.

## Configure a deck

A configuration holds the template, the section list and the wording of
one report. Every setting has a default, so name only what differs.
`?deck_config` lists the settings.

```r
config <- pubdeck::deck_config(
  template = "templates/slide_template.pptx",
  title = "Weekly review",
  country = "Afghanistan",
  sections = data.frame(
    key = c("epi", "campaigns"),
    label = c("Epidemiology", "Campaigns"),
    parent = NA
  ),
  source_labels = c(polis = "POLIS", survey = "Post-campaign survey")
)

pubdeck::deck_config_set(config)
```

The default layout names, placeholder labels and boxes describe the
template the package was first written for. A report with a different
template sets its own in `layouts`, `labels` and the `*_box` settings.

## Check a template

Run the check once when a template is adopted or edited. It lists any
layout or placeholder the configuration names and the template lacks.

```r
pubdeck::deck_check_template()
```

## Build a deck

Slide positions are read from the template with `deck_layout_box()`, so
page code does not restate where a figure sits.

```r
deck <- pubdeck::deck_open() |>
  pubdeck::deck_title_slide(date = as.Date("2025-03-09")) |>
  pubdeck::deck_contents_placeholder() |>
  pubdeck::deck_section_slide("epi")

box <- pubdeck::deck_layout_box("one_plot", label = "plot")
fit <- pubdeck::deck_fit_image("outputs/trend.png", box$width, box$height)

deck <- deck |>
  officer::add_slide(layout = "one_plot", master = "Office Theme") |>
  officer::ph_with(
    value = pubdeck::deck_figure("outputs/trend.png", fit$width, fit$height),
    location = pubdeck::deck_place_in_box(fit, box)
  ) |>
  pubdeck::deck_page_footer("epi", sources = "polis")

deck <- pubdeck::deck_contents_slide(
  deck, pubdeck::deck_contents_entries(deck)
)

print(deck, target = "outputs/weekly_review.pptx")
```

A page that names no source stops the build, because a slide with no
source line reads as though it needs none.

## Change settings for one call or one report

Every builder takes a `config` argument, and most take the settings they
use most often as arguments of their own. Three ways to vary a setting,
from narrowest to widest:

```r
# one call
pubdeck::deck_subtitle("Cases by week", size = 12)

# one report built in the same session as another
annex <- pubdeck::deck_config(title = "Annex", base = config)
pubdeck::deck_title_slide(deck, config = annex)

# a block of code
pubdeck::with_deck_config(annex, {
  deck <- pubdeck::deck_thanks_slide(deck)
})
```
