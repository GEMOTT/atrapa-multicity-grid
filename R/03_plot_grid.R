# ==============================================================================
# ATRAPA Multicity Grid
# 03_plot_grid.R
#
# Purpose:
# Create diagnostic maps of the harmonised JRC-ESTAT 100 m population grids
# using a common population scale and common cartographic scale across cities.
# ==============================================================================


# Packages ---------------------------------------------------------------------

library(sf)
library(dplyr)
library(purrr)
library(here)
library(ggplot2)
library(patchwork)


# Paths ------------------------------------------------------------------------

grid_dir <- here(
  "data",
  "processed",
  "grids"
)

figure_dir <- here(
  "figures",
  "grid_check"
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# Cities -----------------------------------------------------------------------

cities <- c(
  "barcelona",
  "paris",
  "milan",
  "warsaw",
  "utrecht",
  "malmo",
  "ljubljana"
)


# Read harmonised grids ---------------------------------------------------------

city_grids <- setNames(
  lapply(cities, function(city) {
    
    file <- file.path(
      grid_dir,
      paste0(city, "_grid_100m.gpkg")
    )
    
    if (!file.exists(file)) {
      stop("Grid file not found for ", city)
    }
    
    st_read(
      file,
      quiet = TRUE
    )
    
  }),
  cities
)


# Common population scale -------------------------------------------------------

pop_max <- max(
  sapply(
    city_grids,
    function(x) {
      max(
        x$population,
        na.rm = TRUE
      )
    }
  )
)


# Common cartographic scale -----------------------------------------------------

city_extents <- lapply(
  city_grids,
  st_bbox
)

x_ranges <- sapply(
  city_extents,
  function(x) {
    x["xmax"] - x["xmin"]
  }
)

y_ranges <- sapply(
  city_extents,
  function(x) {
    x["ymax"] - x["ymin"]
  }
)

common_width <- max(x_ranges)
common_height <- max(y_ranges)


# Create city maps --------------------------------------------------------------

city_plots <- lapply(cities, function(city) {
  
  grid <- city_grids[[city]]
  
  bbox <- st_bbox(grid)
  
  cx <- (
    bbox["xmin"] +
      bbox["xmax"]
  ) / 2
  
  cy <- (
    bbox["ymin"] +
      bbox["ymax"]
  ) / 2
  
  ggplot(grid) +
    geom_sf(
      aes(fill = population),
      colour = NA
    ) +
    scale_fill_viridis_c(
      trans = "sqrt",
      limits = c(
        0,
        pop_max
      ),
      name = "Population"
    ) +
    coord_sf(
      xlim = c(
        cx - common_width / 2,
        cx + common_width / 2
      ),
      ylim = c(
        cy - common_height / 2,
        cy + common_height / 2
      ),
      datum = NA,
      expand = FALSE
    ) +
    labs(
      title = tools::toTitleCase(city)
    ) +
    theme_void() +
    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      )
    )
  
})


# Combine city maps -------------------------------------------------------------

p_all <- wrap_plots(
  city_plots,
  ncol = 3,
  guides = "collect"
) +
  plot_annotation(
    title = "JRC-ESTAT Census Population Grid 2021",
    subtitle = "100 m population grid across ATRAPA study cities"
  ) &
  theme(
    legend.position = "right"
  )


# Display ----------------------------------------------------------------------

p_all


# Save -------------------------------------------------------------------------

ggsave(
  file.path(
    figure_dir,
    "all_cities_population_grid.png"
  ),
  p_all,
  width = 12,
  height = 9,
  dpi = 300
)