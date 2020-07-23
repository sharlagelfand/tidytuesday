theme_rspca <- function() {
  theme_minimal(base_size = 14) +
    theme(plot.title.position = "plot",
          legend.position = "none",
          panel.grid.minor = element_blank())
}

percent2 <- function(x) {
  percent(x, accuracy = 0.01)
}

plot_outcomes_analysis_year <- function(animal_type) {

  data <- outcomes_breakdown %>%
    filter(year == analysis_year,
           animal_type == !!animal_type)

  ggplot(data, aes(x = prop_outcome, y = fct_reorder(outcome, prop_outcome))) +
    geom_col(aes(fill = fct_reorder(outcome, prop_outcome))) +
    geom_text(aes(label = percent2(prop_outcome)), hjust = -0.25) +
    scale_x_continuous(labels = percent, limits = c(0,max(data[["prop_outcome"]])*1.1)) +
    scale_fill_brewer(palette = "PuBu") +
    labs(x = NULL, y = NULL, title = glue("Outcomes for {tolower(animal_type)}, {analysis_year_text}"), subtitle = glue("Total {tolower(animal_type)} received: {comma(sum(data[['n']]))}"))
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
    geom_line(data = data, aes(x = year, y = prop_outcome, colour = fct_reorder(outcome, prop_outcome))) +
    geom_point(data = analysis_year_data, aes(x = year, y = prop_outcome, colour = fct_reorder(outcome, prop_outcome))) +
    # scale_x_continuous(limits = c(last_5_years[1], analysis_year + 0.5)) +
    # ggrepel::geom_text_repel(data = analysis_year_data, aes(x = year, y = prop_outcome, colour = fct_reorder(outcome, prop_outcome), label = percent2(prop_outcome)), vjust = -1) +
    scale_y_continuous(labels = percent) +
    scale_colour_brewer(palette = "Set2", guide = guide_legend(reverse = TRUE)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL, title = glue("Outcomes for {tolower(animal_type)}, over time")) +
    theme(legend.position = "right",
          legend.title = element_blank())
}

plot_lrr <- function(animal_type) {
  lrr <- live_release_rate %>%
    filter(animal_type == !!animal_type,
           year %in% last_5_years)

  reclaim_rehome <- reclaimed_rehomed %>%
    filter(animal_type == !!animal_type,
           year %in% last_5_years)

  data <- lrr %>%
    bind_rows(reclaim_rehome)

  ggplot(data, aes(x = year, y = prop_outcome, colour = outcome)) +
    geom_line() +
    scale_y_continuous(labels = percent) +
    scale_colour_brewer(palette = "Set2") +
    labs(x = NULL, y = NULL, title = glue("Live release rate and percentage of {tolower(animal_type)} adopted/reclaimed")) +
    theme(legend.position = "none")
}
