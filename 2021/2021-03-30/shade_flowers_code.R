library(readr)
library(dplyr)
library(ggplot2)
library(purrr)
library(ggforce)
library(stringr)
library(magick)
library(fs)

# Read data
all_shades <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv")

# Clean up product names for writing files
all_shades <- all_shades %>%
  mutate(
    product_clean = snakecase::to_snake_case(product),
    product_clean = str_replace_all(product_clean, "è|é", "e"),
    product_clean = str_replace_all(product_clean, "ō", "o")
  )

# Calculate the proportion of shades for a product that are deep (lightness < 0.5)
product_summary <- all_shades %>%
  group_by(product, product_clean) %>%
  summarise(prop_deep = mean(lightness < 0.5), .groups = "drop")

# Order shades from deepest to lightest for placing on plot
shade_summary <- all_shades %>%
  select(brand, product, description, hex, lightness) %>%
  left_join(product_summary, by = "product") %>%
  group_by(brand, product, product_clean) %>%
  arrange(lightness) %>%
  mutate(x = row_number()) %>%
  ungroup()

# Function for producing a single flower plot
plot_product_flower <- function(product_clean) {
  shades <- shade_summary %>%
    filter(product_clean == !!product_clean) %>%
    mutate(colour = "black")

  # Fake an empty shade if there is just one for a product line - otherwise the polar coordinates won't form a circle
  if (nrow(shades) == 1) {
    shades_sub <- shades %>%
      mutate(
        hex = NA_character_,
        colour = NA_character_,
        x = 2
      )

    shades <- shades %>%
      bind_rows(shades_sub)
  }

  p <- ggplot() +
    geom_ellipse(data = shades, aes(x0 = x, y0 = 0.5, a = 0.3, b = 1, angle = 0, fill = hex, colour = colour), size = 0.1) +
    # Size of circle is according to the proportion of shades that are deep
    geom_hline(data = product_summary %>% filter(product_clean == !!product_clean), aes(yintercept = prop_deep), size = 1, colour = "#5EC19E") +
    scale_fill_identity() +
    scale_colour_identity() +
    scale_x_continuous(limits = c(0.5, nrow(shades) + 0.5)) +
    scale_y_continuous(limits = c(-1, 1.5)) +
    coord_polar() +
    theme_void()

  ggsave(here::here("2021", "2021-03-30", "flowers", glue::glue("{product_clean}.png")), p, width = 2, height = 2, dpi = 300, bg = "transparent")

  p
}

# Generate plots
product_summary[["product_clean"]] %>%
  walk(plot_product_flower)

# Read plots back in for combining
flower_files <- dir_ls(here::here("2021", "2021-03-30", "flowers"))

flowers <- flower_files %>%
  map(image_read)

flower_files <- flower_files %>%
  basename() %>%
  path_ext_remove()

names(flowers) <- flower_files

# Create empty image
image_width <- 2000
image_height <- image_width * 0.75
flowers_image <- image_blank(width = image_width, height = image_height, color = "#5EC19E")

# Function for adding a flower to the image
add_flower <- function(product_clean, prop_deep) {
  flower <- flowers[[product_clean]]
  # Sized according to the proportion of shades that are deep
  flower <- image_scale(flower, image_info(flower)[["height"]] * (prop_deep + 0.25))

  # Set max offset so that a flower can only have half its size off the image
  x <- round(runif(1, min = 0, max = image_width - image_info(flower)[["width"]] / 2))
  y <- round(runif(1, min = 0, max = image_height - image_info(flower)[["height"]] / 2))

  flowers_image <<- image_composite(flowers_image, flower, offset = glue::glue("+{x}+{y}"))
}

set.seed(12345)

# Add flowers to image
product_summary %>%
  select(product_clean, prop_deep) %>%
  sample_n(nrow(.)) %>%
  pwalk(add_flower)

# Crop image since top left offset can only start at 0,0 and there's blank space otherwise
flowers_image <- image_crop(flowers_image, "1950x1450+50+100")

# Save image
image_write(flowers_image, path = here::here("2021", "2021-03-30", "flowers.png"), quality = 300)

# Save data
shade_summary <- shade_summary %>%
  select(brand, product, prop_deep_shades = prop_deep, shade_description = description, shade_hex = hex, shade_lightness = lightness, shade_placement = x) %>%
  arrange(brand, product, shade_placement)

write_csv(shade_summary, here::here("2021", "2021-03-30", "product_proportion_deep_with_shade_lightness.csv"))
