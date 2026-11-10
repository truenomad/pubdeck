# pubdeck 0.1.0

* First version, built from the figure theme and deck helpers of the
  rise-afg weekly review.
* Theme and scales: `theme_pub()`, `theme_pub_map()`, `theme_pub_panel()`,
  `theme_pub_cols()`, `theme_pub_clear()`, `scale_x_pub()`, `scale_y_pub()`,
  `label_mag()`, `pub_expansion()` and `guide_legend_bottom()`.
* Colours: `pub_palette()`, `pub_ramp()`, `pub_alarm()`,
  `pub_map_colours()`, `tint()` and `contrast_ink()`.
* Saving: `ggsave_pub()`, `save_pub_grob()`, `gtsave_pub()`,
  `compress_pub()` and `trim_png()`.
* Tables: `style_deck_table()`, `bar_cell()`, `dot_cell()`, `chip_cell()`,
  `spark_cell()` and `move_cell()`.
* Configuration: `deck_config()` holds the template, wording and styling
  of one report. `deck_config_set()`, `deck_config_get()` and
  `with_deck_config()` manage the configuration in force, and every
  builder takes a `config` argument.
* Deck building: `deck_open()`, `deck_reopen()`, `deck_title_slide()`,
  `deck_section_slide()`, `deck_thanks_slide()`, `deck_page_footer()`,
  `deck_source_line()`, the contents page functions and the explainer box
  functions.
* `deck_check_template()` reports any layout or placeholder a
  configuration names and a template lacks.
