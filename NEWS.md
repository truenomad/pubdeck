# pubdeck 0.2.0

* New page builders, ported from the deck helpers of the measles-campaign
  review: `deck_cover_slide()`, `deck_text_page()`, `deck_figure_page()`,
  `deck_place_figure()` and `deck_place_vector()`.
* New blocks for a page built from boxes: `deck_bullets()`,
  `deck_hang_bullets()`, `deck_panel()`, `deck_callout()`,
  `deck_rule_note()`, `deck_message()`, `deck_kpi()`, `deck_table()`,
  `deck_text_box()`, `deck_hrule()` and `deck_ellipse()`.
* New text helpers: `deck_fp_text()`, `deck_flex_semibold()`,
  `deck_sized()`, `deck_marked()`, `deck_check_heading()` and
  `deck_title_pt()`. New box helpers: `deck_box_location()`,
  `deck_below_subtitle()` and `deck_columns()`.
* `deck_contents_title_entries()` lists a deck by divider and page title,
  and `deck_contents_fill()` draws the contents in two balanced columns.
  `deck_contents_entries()` and `deck_contents_slide()` are unchanged.
* The page builders take any setting of `deck_config()` by name for one
  call, so a setting can be changed on a call, in a `config`, or for the
  session with `deck_config_set()`.
* `theme_pub()` gains `frame`, `family`, `title_position`, `plot_margin`,
  `frame_colour` and `frame_linewidth`, and `theme_pub_panel()` gains
  `plot_margin`. The theme functions take a `config` argument and read
  the new `theme_*` settings.
* `ggsave_pub()` gains `grob`, with the `save_grob` setting, to save the
  picture alone. A `map_dpi` setting of `NULL` saves maps at `dpi`. The
  path in its message is written relative to the `path_root` setting.
* `deck_subtitle()` gains `weight`. `deck_section_slide()` gains
  `numbered` and `layout`. `deck_page_footer()` gains `framing`,
  `section_box` and `source_weight`. The new arguments follow `config`,
  so calls that pass `config` by position still work.
* The `sections` setting no longer needs a `parent` column. A table
  without one holds top-level sections only.
* `deck_config()` gains the settings these builders read. The defaults
  reproduce the behaviour of 0.1.0 for every function that existed then.

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
