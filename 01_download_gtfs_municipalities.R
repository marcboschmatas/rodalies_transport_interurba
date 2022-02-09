# libraries

library(tidyverse)
library(sf)
library(rvest)
library(tmap)
library(tidytransit)
library(furrr)
future::multicore

# download gtfs files

# fgc
download.file("https://www.fgc.cat/google/google_transit.zip",
              destfile = paste0(getwd(),"/data/gtfs_fgc.zip"))


# rodalies (7/10/2021)
download.file("https://openmobilitydata.org/p/renfe/1016/20211007/download",
              destfile = paste0(getwd(),"/data/gtfs_rodalies.zip"))

# interurba
download.file("https://territori.gencat.cat/web/.content/home/01_departament/cartografia_i_toponimia/bases_cartografiques/infraestructures_mobilitat/autobusos_interurbans/autobusos-interurbans-GTFS.zip",
              destfile = paste0(getwd(),"/data/gtfs_interurba.zip"))

# tmb (bus+metro - 9/10/2021)
download.file("https://transitfeeds.com/p/transports-metropolitans-de-barcelona-tmb/995/20211009/download",
              destfile = paste0(getwd(),"/data/gtfs_tmb.zip"))

# bus urbà amb (7/10/2021)
download.file("https://transitfeeds.com/p/amb-mobilitat/994/20211007/download",
              destfile = paste0(getwd(),"/data/gtfs_amb.zip"))

# trambaix (23/6/2021)
download.file("https://transitfeeds.com/p/tram-trambaix/996/20210623/download",
              destfile = paste0(getwd(),"/data/gtfs_trambaix.zip"))

# trambesos (23/6/2021)
download.file("https://transitfeeds.com/p/tram-trambesos/997/20210623/download",
              destfile = paste0(getwd(),"/data/gtfs_trambesos.zip"))

# municipalities - shp

munis <- st_read("https://analisi.transparenciacatalunya.cat/api/geospatial/97qg-zvqd?method=export&format=GeoJSON")

# filter for municipalities in the integrated area
# parse ATM website
atm <- rvest::read_html("https://www.atm.cat/sistema-tarifari-integrat/sistema-de-transport/mapa-de-la-zonificacio")
# filter all elements containing municipality names
ps <- rvest::html_elements(atm, css = "p")

ps <- ps[7:58]

# extract text

ps <- mapply(rvest::html_text, ps)

# filter elements without text

ps <- ps[1:39]

ps <- ps[grepl(" ", ps)]

# remove zone codes & transform into list
munisatm <- ps %>% 
  lapply(\(x) strsplit(x, split = "\\n\\t\\t\\t")) %>%
  unlist() %>%
  sapply(\(x) str_remove(x, "\\**+\\s+[0-9]+[a-zA-Z]*"))


# filter for municipalities in list

munis_filtered <- munis %>%
  filter(nom_muni %in% munisatm)

# names are different, this will be done by hand

dif <- setdiff(munisatm, munis_filtered$nom_muni)

writeClipboard(dif)

# corrected names

names <- c("l'Ametlla del Vallès", "l'Arboç", "Bigues i Riells", "el Bruc", "el Brull", "les Cabanyes", "Cabrera d'Anoia", "Caldes d'Estrac", "Santa Maria de Corcó", "l'Estany", "les Franqueses del Vallès", "Garraf / les Botigues de Sitges", "la Garriga", "la Granada", "l'Hospitalet de Llobregat", "els Hostalets de Pierola", "la Llacuna", "la Llagosta", "Lliçà d'Amunt", "les Masies de Roda", "les Masies de Voltregà", "el Masnou", "la Palma de Cervelló", "el Papiol", "el Pla del Penedès", "la Pobla de Claramunt", "el Pont de Vilomara i Rocafort", "el Prat de Llobregat", "els Prats de Rei", "la Roca del Vallès", "Sant Llorenç d'Hortons", "Sant Martí d'Albars", "Sant Sadurní d'Anoia", "Sant Sadurní d'Osormort", "Santa Maria d'Oló", "la Torre de Claramunt", "Vallbona d'Anoia", "el Vendrell")


munis_filtered <- munis %>%
  filter((nom_muni %in% munisatm) | 
           (nom_muni %in% names))
# export shapefile

munis_filtered <- st_transform(munis_filtered, "EPSG:25831")

munis_filtered <- st_make_valid(munis_filtered)

st_write(munis_filtered, "./data/project_rodalies.gpkg", layer = "munis_integrats", append = FALSE)

# read & filter mobility cells (downloaded from here https://www.ine.es/experimental/movilidad/experimental_em4.htm)

cells <- st_read("./data/celdas_marzo_2020.shp")

cells <- cells %>% 
  st_transform("EPSG:25831") %>%
  st_make_valid()

cells_filtered <- cells[munis_filtered,]

# this gives cells that only share a border - need to eliminate those. 0.3 is the minimal overlap percentage to not exclude any municipality in the ATM

cells_pct <- cells_filtered %>%
  mutate("area_origin" = st_area(geometry)) %>% 
  select(ID_GRUPO, NOMBRE_CEL, area_origin, geometry) %>%
  st_intersection(select(munis_filtered, nom_muni, geometry)) %>% 
  mutate("area_end" = st_area(geometry),
         "cob_pct" = area_end/area_origin) %>%
  group_by(ID_GRUPO, NOMBRE_CEL) %>% 
  summarise("cob_pct" = sum(cob_pct)) %>%
  mutate("cob_pct" = case_when(as.numeric(cob_pct) > 1 ~ 1,
                               TRUE ~ as.numeric(cob_pct))) %>%
  filter(cob_pct >= 0.1)

# read & join cell population data

cellpop <- readxl::read_xlsx("./data/descripcion_areas_movilidad_poblacion/Descripción de las áreas de movilidad y su población/areas_de_movilidad_y_poblacion_a_1_ene_2020.xlsx")

cells_pct <- cells_pct %>%
  select(NOMBRE_CEL, ID_GRUPO, geometry) %>% 
  left_join(select(cellpop, ID_GRUPO, POB_GRUPO), by = "ID_GRUPO")

# delete repeated features

cells_pct <- distinct(cells_pct)

# save

st_write(cells_pct, "./data/project_rodalies.gpkg", layer = "celles_mobilitat", append = FALSE)




# plots


bm <- tmaptools::read_osm(x = munis_filtered, type = "stamen-toner")


# read gtfs files


gtfss <- list.files("./data", pattern = "gtfs", full.names = TRUE)

gtfss <- as_tibble(gtfss)

colnames(gtfss) <- "file"


get_stops <- function(x){
  files <- read_gtfs(x)
  sfs <- gtfs_as_sf(files)
  stops <- sfs$stops
  stops <- select(stops, stop_name, geometry)
}


gtfss <- gtfss %>%
  mutate("list" = future_map(file, get_stops))

stops_map <- unnest(gtfss) %>%
  st_sf()

stops_map <- stops_map %>%
  mutate("agència" = str_remove(file, "./data/gtfs_"),
         "agència" = str_remove(`agència`, ".zip")) %>%
  select(-file) %>%
  mutate("agència" = case_when(`agència` == "amb" ~ "Bus urbà TMB/AMB i metro",
                               `agència` == "tmb" ~ "Bus urbà TMB/AMB i metro",
                               `agència` == "trambaix" ~ "Tram",
                               `agència` == "trambesos" ~ "Tram",
                               `agència` == "fgc" ~ "Ferrocarrils de la Generalitat de Catalunya",
                               `agència` == "interurba" ~ "Bus Interurbà Generalitat",
                               TRUE ~ "Rodalies RENFE")) %>%
  st_transform(., crs = st_crs(cells_pct)) %>%
  st_intersection(cells_pct) %>%
  select(`agència`, stop_name, geometry)





tm_shape(bm) + 
  tm_rgb() + 
  tm_shape(st_union(cells_pct)) + 
  tm_borders(lwd = 2) + 
  tm_shape(stops_map) +
  tm_dots(col = "purple") + 
  tm_facets(by = "agència",
            free.coords = FALSE) + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_credits("Fonts: Col·laboradors d'OpenStreetMap, \nGeneralitat de Catalunya, AMB, TMB, Renfe",
             size = 1.2) +
  tm_layout(main.title = "Xarxa de Transport Públic a la Zona Tarifària Integrada",
            attr.outside = TRUE)

