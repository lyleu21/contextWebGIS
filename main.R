library(sf)
library(dplyr)
library(tidyr)
sf_use_s2(FALSE)

urban_density <- 1500   # a 1 km square with at least this many residents counts as urban

# 1. "Kreise" from BKG

kreise_url <- paste0("https://sgx.geodatenzentrum.de/wfs_vg250?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature&TYPENAMES=vg250:vg250_krs&OUTPUTFORMAT=application/json&SRSNAME=EPSG:25832")
kreise <- st_read(kreise_url)
kreise <- filter(kreise, gf == 4)            # gf == 4 for land
kreise <- st_transform(kreise, 3035)

east_states <- c("12", "13", "14", "15", "16")

kreise <- kreise |> transmute(ags, name   = gen, region = case_when(sn_l %in% east_states ~ "East", sn_l == "11" ~ "Berlin", TRUE ~ "West"))

# 2. Z2022 data

# function to download census variable and return table w/ x, y and the value of interest
get_census <- function(feature, name) {
  d <- z22::z22_data(feature, categories = 0, year = 2022, res = "1km", as = "df")
  value_column <- setdiff(names(d), c("x", "y", "quality"))[1] 
  out <- tibble(x = as.numeric(d$x), y = as.numeric(d$y))
  out[[name]] <- as.numeric(d[[value_column]])
  out
}

pop     <- get_census("population", "pop")       # population
age     <- get_census("age_avg",    "age")       # average age
foreign <- get_census("foreigners", "foreign")   # % foreign citizens
rent    <- get_census("rent_avg",   "rent")      # average ren in EUR/m2

#bring it all together
cells <- pop |>
  left_join(age,     by = c("x", "y")) |>
  left_join(foreign, by = c("x", "y")) |>
  left_join(rent,    by = c("x", "y"))

# 3. Assign raster data to politcal boundaries and assign urban v rural 

cells_sf <- st_as_sf(cells, coords = c("x", "y"), crs = 3035)
cells_sf <- st_join(cells_sf, kreise["ags"], left = FALSE)   
cells    <- st_drop_geometry(cells_sf)

cells$lens <- ifelse(cells$pop >= urban_density, "u", "r")


# 4. find average value by Kreis for 1) for all residents, 2) urban squares only and 3) rural squares only 

all_cells <- mutate(cells, lens = "a")
stacked   <- bind_rows(all_cells, cells)

values <- stacked |>
  group_by(ags, lens) |>
  summarise(residents = sum(pop),
            age       = round(weighted.mean(age,     pop, na.rm = TRUE), 1), 
            foreign   = round(weighted.mean(foreign, pop, na.rm = TRUE), 1),
            rent      = round(weighted.mean(rent,    pop, na.rm = TRUE), 2),
            .groups = "drop") 

# pivot for arc gis online
values <- pivot_wider(values, id_cols = ags, names_from = lens, values_from = c(age, foreign, rent))


#5. Write to GeoJSON

output <- st_simplify(kreise, preserveTopology = TRUE, dTolerance = 200) # to reduce the file size a lot eliminating a little detail
output <- left_join(output, values, by = "ags")
output <- st_transform(output, 4326)
st_write(output, "germany_kreise.geojson", delete_dsn = TRUE)