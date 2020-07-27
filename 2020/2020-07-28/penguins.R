library(palmerpenguins)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggridges)
library(patchwork)
library(prismatic)
library(forcats)
library(stringr)
library(extrafont)
library(magick)
library(ggtext)

penguin_colours <- tribble(
  ~species, ~colour,
  "Adelie", "darkorange",
  "Chinstrap", "#AD57E4",
  "Gentoo", "cyan4"
)

penguin_colours[["female"]] <- unclass(clr_darken(penguin_colours[["colour"]], shift = 0.15))
penguin_colours[["male"]] <- unclass(clr_lighten(penguin_colours[["colour"]], shift = 0.15))
penguin_colours[["unknown"]] <- unclass(clr_lighten(penguin_colours[["colour"]], shift = 0.5))

penguin_colours <- penguin_colours %>%
  pivot_longer(
    cols = c(female, male, unknown),
    names_to = "sex",
    values_to = "sex_colour"
  ) %>%
  mutate(sex = ifelse(sex == "unknown", NA_character_, sex))

penguins_with_colours <- penguins %>%
  left_join(penguin_colours, by = c("species", "sex")) %>%
  group_by(species) %>%
  fill(colour) %>%
  ungroup()

penguin_labels <- tribble(
  ~species, ~x, ~y, ~letter, ~number,
  "Adelie", 55, 21, "A", 1,
  "Chinstrap", 33.5, 14, "C", 2,
  "Gentoo", 35, 21, "G", 3
) %>%
  mutate(rest = str_remove(species, letter),
         rest = ifelse(species == "Adelie", NA_character_, rest))

plot_penguin_bill <- function(species) {
  species_data <- penguins_with_colours %>%
    filter(species == !!species)

  species_label <- penguin_labels %>%
    filter(species == !!species)

  ggplot() +
    geom_point(data = species_data, aes(x = bill_length_mm, y = bill_depth_mm, colour = sex_colour), alpha = 0.8) +
    geom_text(data = species_label, aes(x = x, y = y, label = letter), colour = unique(species_data[["colour"]]), family = "ZallmanCaps", size = 10) +
    geom_text(data = species_label, aes(x = x + 5, y = y, label = rest), colour = unique(species_data[["colour"]]), family = "Times New Roman", size = 6, hjust = 0) +
    geom_text(data = species_label, aes(x = 28.5, y = 22, label = number), size = 2, alpha = 0.9) +
    scale_colour_identity() +
    scale_x_continuous(limits = c(28, 62)) +
    scale_y_continuous(limits = c(13, 22))
}

theme_penguin <- function() {
  theme_void(base_family = "Times New Roman", base_size = 9) +
    theme(
      plot.background = element_rect(colour = "#FDFFEB", fill = "#FDFFEB"),
      panel.background = element_rect(colour = "#E8E4CD", fill = "#FDFFEB", size = 1),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      strip.text = element_blank(),
      plot.margin = margin(10, 10, 10, 10),
      plot.caption = element_markdown(hjust = 0.5)
    )
}

theme_set(theme_penguin())

adelie_start <- 21
diff <- 1.25

adelie <- plot_penguin_bill("Adelie") +
  annotate("text", x = 55, y = adelie_start - 1*diff, label = "d", family = "Times New Roman", size = 6, colour = "darkorange") +
  annotate("text", x = 55, y = adelie_start - 2*diff, label = "é", family = "Times New Roman", size = 6, colour = "darkorange") +
    annotate("text", x = 55, y = adelie_start - 3*diff, label = "l", family = "Times New Roman", size = 6, colour = "darkorange") +
    annotate("text", x = 55, y = adelie_start - 4*diff, label = "i", family = "Times New Roman", size = 6, colour = "darkorange") +
    annotate("text", x = 55, y = adelie_start - 5*diff + 0.25, label = "e", family = "Times New Roman", size = 6, colour = "darkorange")

p1 <- adelie + plot_penguin_bill("Chinstrap") + plot_penguin_bill("Gentoo")

penguin_labels_ridges <-
  tribble(
    ~species, ~x, ~y, ~colour,
    "Adélie", 220, 3, "darkorange",
    "Chinstrap", 169, 2, "#AD57E4",
    "Gentoo", 195, 1, "cyan4"
  )

p2 <- ggplot() +
  geom_density_ridges(data = penguins_with_colours, aes(x = flipper_length_mm, y = fct_rev(species), fill = colour, colour = colour), alpha = 0.8) +
  geom_text(data = penguin_labels_ridges, aes(x = x, y = y, label = species, colour = colour), vjust = -0.5, size = 3, family = "Times New Roman") +
  annotate("text", x = 212, y = 4, label = "F", family = "ZallmanCaps", size = 10, colour = clr_darken("#E8E4CD", 0.5), alpha = 0.75) +
  annotate("text", x = 215, y = 4, label = "lipper length", family = "Times New Roman", size = 6, hjust = 0, colour = clr_darken("#E8E4CD", 0.5), alpha = 0.75) +
  annotate("text", x = 165, y = 4, size = 2, alpha = 0.9, label = "4") +
  scale_fill_identity() +
  scale_colour_identity() +
  labs(caption = "**PENGUINS**: 1. Adélie, bill length versus bill depth, by sex; 2. Chinstrap, bill length versus<br>bill depth, by sex; 3. Gentoo, bill length versus bill depth, by sex; 4. Flipper length, all species.", 100)

p <- p1 / p2 + plot_annotation(title = "PENGUINS")

ggsave(here::here("2020", "2020-07-28", "penguins.png"), plot = p, width = 5, height = 6)

penguins_plot <- image_read(here::here("2020", "2020-07-28", "penguins.png"))

penguins_plot_altered <- image_modulate(penguins_plot, saturation = 70) %>%
  image_noise() %>%
  image_noise()

image_write(penguins_plot_altered, path = here::here("2020", "2020-07-28", "penguins_noise.png"))
