# ==============================================================================
# ATRAPA Multicity Grid
# 04_validate_population_utrecht_ljubljana.R
#
# Purpose:
# Compare the JRC-ESTAT Census Population Grid 2021 with official 2021
# 100 m population grids for Utrecht and Ljubljana.
#
# This is a diagnostic validation of the JRC population surface using the
# two ATRAPA cities for which official 2021 population data are available
# at the same nominal 100 m resolution.
# ==============================================================================


# Packages ---------------------------------------------------------------------

library(sf)
library(dplyr)
library(here)
library(ggplot2)
library(patchwork)


# Paths ------------------------------------------------------------------------

grid_dir <- here(
  "data",
  "processed",
  "grids"
)

perimeter_dir <- here(
  "data",
  "perimeters"
)

official_dir <- here(
  "data",
  "population",
  "official_2021"
)

netherlands_dir <- file.path(
  official_dir,
  "netherlands"
)

slovenia_dir <- file.path(
  official_dir,
  "slovenia"
)

output_dir <- here(
  "outputs",
  "population_validation"
)

figure_dir <- here(
  "figures",
  "population_validation"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# Cities -----------------------------------------------------------------------

validation_cities <- c(
  "utrecht",
  "ljubljana"
)


# Read JRC 100 m grids ----------------------------------------------------------

jrc_grids <- setNames(
  lapply(validation_cities, function(city) {
    
    file <- file.path(
      grid_dir,
      paste0(city, "_grid_100m.gpkg")
    )
    
    if (!file.exists(file)) {
      stop("JRC grid not found for ", city)
    }
    
    st_read(
      file,
      quiet = TRUE
    )
    
  }),
  validation_cities
)


# Read city perimeters ----------------------------------------------------------

perimeters <- setNames(
  lapply(validation_cities, function(city) {
    
    file <- file.path(
      perimeter_dir,
      paste0(city, "_perimeter.gpkg")
    )
    
    if (!file.exists(file)) {
      stop("Perimeter not found for ", city)
    }
    
    st_read(
      file,
      quiet = TRUE
    )
    
  }),
  validation_cities
)


# Read official 2021 100 m grids -----------------------------------------------

utrecht_official <- st_read(
  file.path(
    netherlands_dir,
    "2024-cbs_vk100_2021_vol",
    "cbs_vk100_2021_vol.gpkg"
  ),
  quiet = TRUE
)

ljubljana_official <- st_read(
  file.path(
    slovenia_dir,
    "STAGE_data",
    "v213_2021_01_01.gpkg"
  ),
  quiet = TRUE
)


# Standardise population variables ---------------------------------------------

utrecht_official <- utrecht_official |>
  transmute(
    population = as.numeric(aantal_inwoners)
  ) |>
  mutate(
    # CBS uses negative values as special/missing codes
    population = if_else(
      population < 0,
      NA_real_,
      population
    )
  )

ljubljana_official <- ljubljana_official |>
  transmute(
    population = as.numeric(tot_p)
  )


# Clip official grids to ATRAPA city perimeters --------------------------------

utrecht_perimeter <- perimeters[["utrecht"]] |>
  st_make_valid() |>
  st_transform(
    st_crs(utrecht_official)
  )

ljubljana_perimeter <- perimeters[["ljubljana"]] |>
  st_make_valid() |>
  st_transform(
    st_crs(ljubljana_official)
  )

utrecht_official_city <- utrecht_official[
  lengths(
    st_intersects(
      utrecht_official,
      utrecht_perimeter
    )
  ) > 0,
]

ljubljana_official_city <- ljubljana_official[
  lengths(
    st_intersects(
      ljubljana_official,
      ljubljana_perimeter
    )
  ) > 0,
]


# Population totals ------------------------------------------------------------

population_summary <- data.frame(
  city = validation_cities,
  
  jrc_population = c(
    sum(
      jrc_grids[["utrecht"]]$population,
      na.rm = TRUE
    ),
    sum(
      jrc_grids[["ljubljana"]]$population,
      na.rm = TRUE
    )
  ),
  
  official_population = c(
    sum(
      utrecht_official_city$population,
      na.rm = TRUE
    ),
    sum(
      ljubljana_official_city$population,
      na.rm = TRUE
    )
  )
) |>
  mutate(
    difference =
      jrc_population -
      official_population,
    
    percent_difference =
      100 *
      difference /
      official_population
  )

print(population_summary)


# Save population totals -------------------------------------------------------

write.csv(
  population_summary,
  file.path(
    output_dir,
    "population_total_comparison_2021.csv"
  ),
  row.names = FALSE
)


# Prepare grids for mapping -----------------------------------------------------

official_grids <- list(
  utrecht = utrecht_official_city,
  ljubljana = ljubljana_official_city
)

# Transform official grids to JRC CRS for consistent mapping
official_grids <- lapply(
  validation_cities,
  function(city) {
    
    st_transform(
      official_grids[[city]],
      st_crs(jrc_grids[[city]])
    )
    
  }
) |>
  setNames(validation_cities)


# Common population scale ------------------------------------------------------

pop_max <- max(
  c(
    unlist(
      lapply(
        jrc_grids,
        function(x) x$population
      )
    ),
    unlist(
      lapply(
        official_grids,
        function(x) x$population
      )
    )
  ),
  na.rm = TRUE
)

pop_max


# Side-by-side maps ------------------------------------------------------------

validation_plots <- lapply(
  validation_cities,
  function(city) {
    
    jrc_plot_grid <- jrc_grids[[city]] |>
      filter(population > 0)
    
    official_plot_grid <- official_grids[[city]] |>
      filter(
        !is.na(population),
        population > 0
      )
    
    p_jrc <- ggplot(
      jrc_plot_grid
    ) +
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
        datum = NA
      ) +
      labs(
        title = "JRC-ESTAT"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(
          face = "bold",
          hjust = 0.5
        )
      )
    
    p_official <- ggplot(
      official_plot_grid
    ) +
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
        datum = NA
      ) +
      labs(
        title = "Official"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(
          face = "bold",
          hjust = 0.5
        )
      )
    
    p_jrc + p_official +
      plot_layout(
        guides = "collect"
      ) +
      plot_annotation(
        title = tools::toTitleCase(city),
        subtitle = "Population 2021 — 100 m grids"
      ) &
      theme(
        legend.position = "right"
      )
  }
)

names(validation_plots) <- validation_cities


# Display maps -----------------------------------------------------------------

validation_plots$utrecht

validation_plots$ljubljana


# Save maps --------------------------------------------------------------------

ggsave(
  file.path(
    figure_dir,
    "utrecht_population_100m_comparison.png"
  ),
  validation_plots$utrecht,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(
    figure_dir,
    "ljubljana_population_100m_comparison.png"
  ),
  validation_plots$ljubljana,
  width = 10,
  height = 5,
  dpi = 300
)

