# ==============================================================================
# ATRAPA Multicity Grid
# 02_check_grid.R
#
# Purpose:
# Perform quality checks on the harmonised JRC-ESTAT 100 m population grids.
# ==============================================================================


# Packages ---------------------------------------------------------------------

library(sf)
library(dplyr)
library(purrr)
library(here)


# Paths ------------------------------------------------------------------------

grid_dir <- here(
  "data",
  "processed",
  "grids"
)

output_dir <- here(
  "outputs"
)

dir.create(
  output_dir,
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


# Read JRC grids ---------------------------------------------------------------

jrc_grids <- setNames(
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


# Grid quality checks ----------------------------------------------------------

grid_check <- data.frame(
  city = cities,
  
  n_cells = sapply(
    jrc_grids,
    nrow
  ),
  
  population = sapply(
    jrc_grids,
    function(x) {
      sum(
        x$population,
        na.rm = TRUE
      )
    }
  ),
  
  zero_pop_cells = sapply(
    jrc_grids,
    function(x) {
      sum(
        x$population == 0,
        na.rm = TRUE
      )
    }
  ),
  
  zero_pop_share = sapply(
    jrc_grids,
    function(x) {
      mean(
        x$population == 0,
        na.rm = TRUE
      )
    }
  ),
  
  duplicate_grid_ids = sapply(
    jrc_grids,
    function(x) {
      sum(
        duplicated(x$grid_id)
      )
    }
  ),
  
  duplicate_jrc_cell_ids = sapply(
    jrc_grids,
    function(x) {
      sum(
        duplicated(x$jrc_cell_id)
      )
    }
  ),
  
  invalid_geometries = sapply(
    jrc_grids,
    function(x) {
      sum(
        !st_is_valid(x)
      )
    }
  ),
  
  crs = sapply(
    jrc_grids,
    function(x) {
      st_crs(x)$epsg
    }
  )
)


# Add expected grid-cell area --------------------------------------------------

grid_check <- grid_check |>
  mutate(
    zero_pop_share = 100 * zero_pop_share,
    expected_cell_area_m2 = 10000
  )

grid_check


# Check actual cell areas ------------------------------------------------------

cell_area_check <- map_dfr(
  cities,
  function(city) {
    
    areas <- as.numeric(
      st_area(
        jrc_grids[[city]]
      )
    )
    
    data.frame(
      city = city,
      min_cell_area_m2 = min(areas),
      mean_cell_area_m2 = mean(areas),
      max_cell_area_m2 = max(areas)
    )
  }
)

cell_area_check


# Combine checks ---------------------------------------------------------------

grid_check <- grid_check |>
  left_join(
    cell_area_check,
    by = "city"
  )

grid_check


# Save QA summary --------------------------------------------------------------

write.csv(
  grid_check,
  file.path(
    output_dir,
    "grid_quality_check.csv"
  ),
  row.names = FALSE
)
