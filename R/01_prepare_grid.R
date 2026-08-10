# ==============================================================================
# ATRAPA Multicity Grid
# 01_prepare_grid.R
#
# Purpose:
# Prepare the harmonised 100 m spatial grid using the
# JRC-ESTAT Census Population Grid 2021.
# ==============================================================================


# Packages ---------------------------------------------------------------------

library(sf)
library(terra)
library(dplyr)
library(purrr)
library(here)
library(ggplot2)
library(patchwork)


# Paths ------------------------------------------------------------------------

perimeter_dir <- here("data", "perimeters")

jrc_dir <- here(
  "data",
  "population",
  "jrc_estat"
)

grid_output_dir <- here(
  "data",
  "processed",
  "grids"
)

figure_dir <- here(
  "figures",
  "grid_check"
)

dir.create(
  grid_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
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


# Read city perimeters ----------------------------------------------------------

perimeters <- setNames(
  lapply(cities, function(city) {
    
    st_read(
      file.path(
        perimeter_dir,
        paste0(city, "_perimeter.gpkg")
      ),
      quiet = TRUE
    )
    
  }),
  cities
)


# Check city perimeters ---------------------------------------------------------

perimeter_summary <- data.frame(
  city = cities,
  n_features = sapply(perimeters, nrow),
  crs = sapply(
    perimeters,
    function(x) st_crs(x)$input
  ),
  geometry = sapply(
    perimeters,
    function(x) {
      paste(
        unique(as.character(st_geometry_type(x))),
        collapse = ", "
      )
    }
  )
)

perimeter_summary


# Read JRC-ESTAT 100 m population grid -----------------------------------------

jrc_file <- list.files(
  jrc_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(jrc_file) != 1) {
  stop("Expected exactly one JRC-ESTAT TIFF file.")
}

jrc_pop <- rast(jrc_file)


# Check JRC-ESTAT grid ----------------------------------------------------------

jrc_pop
res(jrc_pop)
crs(jrc_pop)


# Extract JRC-ESTAT grid for each city -----------------------------------------

city_pop_rasters <- setNames(
  lapply(cities, function(city) {
    
    message("Extracting ", city)
    
    perimeter <- perimeters[[city]] |>
      st_make_valid() |>
      st_transform(3035)
    
    crop(
      jrc_pop,
      vect(perimeter)
    ) |>
      mask(
        vect(perimeter)
      )
    
  }),
  cities
)


# Convert rasters to 100 m vector grids ----------------------------------------

city_grids <- setNames(
  lapply(cities, function(city) {
    
    message("Vectorising ", city)
    
    grid <- as.polygons(
      city_pop_rasters[[city]],
      values = TRUE,
      na.rm = TRUE,
      aggregate = FALSE
    ) |>
      st_as_sf()
    
    names(grid)[1] <- "population"
    
    # Identify the corresponding cell in the original Europe-wide JRC raster
    xy <- st_coordinates(st_centroid(grid))
    
    grid$jrc_cell_id <- cellFromXY(
      jrc_pop,
      xy
    )
    
    grid <- grid |>
      mutate(
        city = city,
        grid_id = paste0("JRC100M_", jrc_cell_id)
      ) |>
      select(
        grid_id,
        jrc_cell_id,
        city,
        population,
        geometry
      )
    
    grid
    
  }),
  cities
)


# Summary ----------------------------------------------------------------------

grid_summary <- data.frame(
  city = cities,
  n_cells = sapply(city_grids, nrow),
  population = sapply(
    city_grids,
    function(x) sum(x$population, na.rm = TRUE)
  ),
  zero_pop_cells = sapply(
    city_grids,
    function(x) sum(x$population == 0, na.rm = TRUE)
  )
)

grid_summary


# Save harmonised grids ---------------------------------------------------------

walk(cities, function(city) {
  
  st_write(
    city_grids[[city]],
    file.path(
      grid_output_dir,
      paste0(city, "_grid_100m.gpkg")
    ),
    delete_dsn = TRUE,
    quiet = TRUE
  )
  
})

# Common population scale ------------------------------------------------------

pop_max <- max(
  sapply(
    city_grids,
    function(x) max(x$population, na.rm = TRUE)
  )
)

pop_max


# Common map scale -------------------------------------------------------------

city_extents <- lapply(
  city_grids,
  st_bbox
)

x_ranges <- sapply(
  city_extents,
  function(x) x["xmax"] - x["xmin"]
)

y_ranges <- sapply(
  city_extents,
  function(x) x["ymax"] - x["ymin"]
)

common_width <- max(x_ranges)
common_height <- max(y_ranges)


# Diagnostic maps ---------------------------------------------------------------

city_plots <- lapply(cities, function(city) {
  
  grid <- city_grids[[city]]
  
  bbox <- st_bbox(grid)
  
  cx <- (bbox["xmin"] + bbox["xmax"]) / 2
  cy <- (bbox["ymin"] + bbox["ymax"]) / 2
  
  ggplot(grid) +
    geom_sf(
      aes(fill = population),
      colour = NA
    ) +
    scale_fill_viridis_c(
      trans = "sqrt",
      limits = c(0, pop_max),
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


# Combined diagnostic figure ---------------------------------------------------

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

p_all


# Save diagnostic figure --------------------------------------------------------

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

