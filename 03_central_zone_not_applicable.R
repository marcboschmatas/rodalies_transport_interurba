# determine central zone 

library(sf)
library(tidyverse)
library(tidytransit)
library(tmap)

cells <- st_read("./data/project_rodalies.gpkg", layer = "celles_mobilitat")

# fuse by municipality

cells_g <- cells %>%
  mutate("NOMBRE_CEL" = gsub(" \\(.*", "", NOMBRE_CEL)) %>%
  mutate("NOMBRE_CEL" = gsub("_", "'", NOMBRE_CEL)) %>%
  group_by(NOMBRE_CEL) %>% 
  summarise("POB_GRUPO" = sum(POB_GRUPO))

# central zone is determined as those municipalities w/ >10
# tmb or tram stops (+ Sant Cugat)

get_stops <- function(x){
  gtfs <- x %>% read_gtfs() %>% gtfs_as_sf()
  stops <- gtfs$stops
  stops <- select(stops, stop_id, geometry)
}

stops <- tibble(agency = c("./data/gtfs_tmb.zip", "./data/gtfs_trambaix.zip",
                         "./data/gtfs_trambesos.zip")) %>%
  mutate("stops" = map(agency, get_stops)) %>%
  unnest(cols = c(stops))

stops_per_cell <- stops %>%
  st_as_sf() %>%
  st_transform(st_crs(cells_g)) %>%
  st_join(cells_g) %>%
  as_tibble() %>%
  select(-geometry) %>%
  group_by(NOMBRE_CEL) %>%
  summarise("stations" = n()) %>%
  filter(stations > 2)

cells_g_2 <- cells_g %>%
  left_join(stops_per_cell, by = "NOMBRE_CEL") %>%
  mutate("NOMBRE_CEL" = case_when(!(is.na(stations)) ~ "Zona central",
                                  TRUE ~ NOMBRE_CEL)) %>%
  select(-stations) %>%
  group_by(NOMBRE_CEL) %>%
  summarise("POB_GRUPO" = sum(POB_GRUPO))


st_write(cells_g_2, "./data/project_rodalies.gpkg", layer = "celles_mobilitat_agrupades")