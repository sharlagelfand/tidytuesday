theme_rspca <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title.position = "plot",
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
}

percent2 <- function(x) {
  percent(x, accuracy = 0.01)
}

plot_outcomes_over_time <- function(animal_type) {
  data <- outcomes_breakdown %>%
    filter(
      animal_type == !!animal_type,
      year %in% last_5_years
    )

  analysis_year_data <- data %>%
    filter(
      year %in% analysis_year
    )

  ggplot() +
    geom_line(data = data, aes(x = year, y = prop_outcome, colour = fct_reorder(outcome, prop_outcome)), show.legend = FALSE) +
    geom_point(data = analysis_year_data, aes(x = year, y = prop_outcome, colour = fct_reorder(outcome, prop_outcome)), show.legend = TRUE) +
    scale_x_continuous(limits = c(last_5_years[1], analysis_year + 0.25)) +
    ggrepel::geom_text_repel(data = analysis_year_data, aes(x = year, y = prop_outcome, colour = fct_reorder(outcome, prop_outcome), label = percent2(prop_outcome)), direction = "y", hjust = -0.25, segment.alpha = 0.5, show.legend = FALSE) +
    scale_y_continuous(labels = percent) +
    scale_colour_brewer(palette = "Dark2", guide = guide_legend(
      keywidth=0.1,
      keyheight=0.1,reverse = TRUE, nrow = 1)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL, title = glue("Outcomes for {tolower(animal_type)}, over time")) +
    theme(
      legend.position = "top",
      legend.justification = "left",
      legend.text = element_text(size = 10),
      legend.title = element_blank()
    )
}

plot_lrr <- function(animal_type) {
  lrr <- live_release_rate %>%
    filter(
      animal_type == !!animal_type,
      year %in% last_5_years
    )

  reclaim_rehome <- reclaimed_rehomed %>%
    filter(
      animal_type == !!animal_type,
      year %in% last_5_years
    )

  data <- lrr %>%
    bind_rows(reclaim_rehome)

  analysis_year_data <- data %>%
    filter(year == analysis_year)

  palette <- RColorBrewer::brewer.pal(3, 'Dark2')

  ggplot() +
    geom_line(data = data, aes(x = year, y = prop_outcome, colour = outcome)) +
    geom_point(data = analysis_year_data, aes(x = year, y = prop_outcome, colour = fct_reorder(outcome, prop_outcome)), show.legend = FALSE) +
    scale_x_continuous(limits = c(last_5_years[1], analysis_year + 0.5)) +
    ggrepel::geom_text_repel(data = analysis_year_data, aes(x = year, y = prop_outcome, colour = fct_reorder(outcome, prop_outcome), label = percent2(prop_outcome)), direction = "y", hjust = -0.25, segment.alpha = 0, show.legend = FALSE) +
    scale_y_continuous(labels = percent) +
    scale_colour_brewer(palette = "Dark2") +
    labs(x = NULL, y = NULL, title = glue("<span style='color:{palette[[1]]}'>Live release rate</span> and percentage of {tolower(animal_type)} <span style='color:{palette[[2]]}'>adopted/reclaimed</span>")) +
    theme(plot.title = element_markdown())
}
