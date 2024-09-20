#importing r libraries we'll need
library(tidyverse)
library(giscoR)
library(janitor)
library(ggplot2)
library(sf)

country_districts <- gisco_get_nuts(
  country = 'Italy',
  nuts_level = 3,
  year = "2021",
  epsg = 3035
) |>
  as_tibble() |>
  janitor::clean_names()


country_states <- gisco_get_nuts(
  country = 'Italy',
  nuts_level = 1,
  year = "2021",
  epsg = 3035
) |>
  as_tibble() |>
  janitor::clean_names()


state_nmbrs <- map_dbl(
  country_districts$geometry,
  \(x) {
    map_lgl(
      country_states$geometry,
      \(y) {
        st_within(
          x,
          y,
        ) |> as.logical()
      }
    ) |> which()
  }
)
country_districts_w_state <- country_districts |>
  mutate(
    state = country_states$nuts_name[state_nmbrs]
  )


make_nice_label <- function(nuts_name, state) {
  nuts_name_label <- htmltools::span(
    nuts_name,
    style = htmltools::css(
      fontweight = 600,
      font_family = 'Brush Script MT, cursive',
      font_size = '20px'
    )
  )
  state_label <- htmltools::span(
    state,
    style = htmltools::css(
      font_family = 'Brush Script MT, cursive',
      font_size = '12px'
    )
  )
  glue::glue('{nuts_name_label}<br>{state_label}')
}

country_districts_w_state_and_labels <- country_districts_w_state  |> 
  mutate(
    nice_label = map2_chr(
      nuts_name,
      state,
      make_nice_label
    )
  )


library(ggiraph)
ggplt <- country_districts_w_state_and_labels |>
  ggplot(aes(geometry = geometry)) +
  geom_sf(
    data = country_states,
    aes(fill = nuts_name),
    color = 'black',
    linewidth = 0.5
  ) +
  geom_sf_interactive(
    fill = NA,
    aes(
      data_id = nuts_id,
      tooltip = nice_label
    ),
    linewidth = 0.1
  ) +
  geom_sf(
    data = country_states,
    aes(fill = nuts_name),
    color = 'black',
    linewidth = 0.5
  ) +
  geom_sf_interactive(
    fill = NA, 
    aes(
      data_id = nuts_id,
      tooltip = nice_label
    ),
    linewidth = 0.1
  ) +
  theme_void() +
  theme(
    legend.position = 'none'
  ) +
  scale_fill_manual(
    values = c("#A0CBE8FF", "#F28E2BFF", "#FFBE7DFF", "#59A14FFF", "#8CD17DFF", "#B6992DFF", "#F1CE63FF", "#499894FF", "#86BCB6FF", "#E15759FF", "#FF9D9AFF", "#79706EFF", "#BAB0ACFF", "#D37295FF", "#FABFD2FF", "#B07AA1FF", "#D4A6C8FF", "#9D7660FF", "#D7B5A6FF")
  )


girafe(
  ggobj = ggplt,
  options = list(
    opts_hover(
      css = girafe_css(
        css = '',
        area = 'stroke: black; fill: black;'
      )
    )
  )
)

