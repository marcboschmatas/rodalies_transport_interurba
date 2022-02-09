library(sf)
library(tidyverse)
library(tidytransit)
library(sfnetworks)
library(stplanr)
library(tmap)

# POSAR POBLACIÓ EXISTENT

# read cells
cells_g <- st_read("./data/project_rodalies.gpkg", layer = "celles_mobilitat_agrupades")
cells <- st_read("./data/project_rodalies.gpkg", layer = "celles_mobilitat")

# read mobility patterns

mob <- readxl::read_xlsx("./data/mobilitat_20211020.xlsx")

# filter movements within the studied area

mob <- mob %>%
  filter((`Código área de residencia` %in% cells$ID_GRUPO) & 
           (`Código área de destino` %in% cells$ID_GRUPO))


# group by municipalities & central zone - taken from script n. 3


czone <- c("Badalona", "Barcelona", "Cerdanyola del Vallès", "Cornellà de Llobregat",
           "Esplugues de Llobregat", "Hospitalet de LLobregat, L'",
           "Montcada i Reixac", "Prat de Llobregat, El", "Sant Adrià de Besòs",
           "Sant Cugat del Vallès", "Sant Joan Despí", "Sant Just Desvern",
           "Santa Coloma de Gramenet")


mob <- mob %>%
  select(-c(`Comunidad Autónoma de destino`, `Provincia de destino`, 
            `Comunidad Autónoma de residencia`, `Provincia de residencia`)) %>%
  mutate("Nombre área de destino" = gsub(" \\(.*", "", `Nombre área de destino`),
         "Nombre área de residencia" = gsub(" \\(.*", "", `Nombre área de residencia`)) %>%
  mutate("Nombre área de destino" = gsub("´", "'", `Nombre área de destino`),
         "Nombre área de residencia" = gsub("´", "'", `Nombre área de residencia`)) %>%
  mutate("Nombre área de destino" = case_when(`Nombre área de destino` %in% czone ~ "Zona central",
                                              TRUE ~ `Nombre área de destino`),
         "Nombre área de residencia" = case_when(`Nombre área de residencia` %in% czone ~ "Zona central",
                                                 TRUE ~ `Nombre área de residencia`)) %>%
  group_by(`Nombre área de residencia`, `Nombre área de destino`) %>%
  summarise("flux_persones" = sum(`Flujo origen-destino (nº de personas)`)) %>%
  ungroup() %>%
  rename("cella_residencia" = "Nombre área de residencia",
         "cella_desti" = "Nombre área de destino")



write.csv(mob, "./data/fluxos_agrupats.csv")


cells_g <- cells_g %>%
  mutate("NOMBRE_CEL" = case_when(NOMBRE_CEL == "Sant Hipòlit de Voltregà y Masies de Voltregà," ~ "Sant Hipòlit de Voltregà y Masies de Voltregà, Les",
                                  TRUE ~ NOMBRE_CEL))


# plots


# how many people stay in their area


not_stay <- mob %>%
  filter(cella_residencia != cella_desti) %>%
  group_by(cella_residencia) %>%
  summarise("flux_persones" = sum(flux_persones))

# refer canviar breaks

b <- tmaptools::read_osm(cells_g)

prop_leave <- cells_g %>%
  left_join(not_stay, by = c("NOMBRE_CEL" = "cella_residencia")) %>%
  mutate(prop_not_staying = (flux_persones)/POB_GRUPO)

tm_shape(b) +
  tm_rgb() + 
  tm_shape(prop_leave) +
  tm_polygons(col = "prop_not_staying", 
              title = "",
              palette = "viridis",
              style = "quantile",
              textNA = "<15 persones",
              lwd = 1.5,
              border.col = "black",
              colorNA = NULL) + 
  tm_shape(stops) + 
  tm_dots(col = "Xarxa", palette = "Set1", size = 0.15) +
  tm_compass() + 
  tm_scale_bar() + 
  tm_credits("Font: INE, Col·laboradors d'OpenStreetMap") + 
  tm_layout(main.title = "Persones que surten de la seva àrea\nde residència a l'Àrea Tarifària Integrada")

# how many people go to central zone

go_czone <- mob %>%
  filter(cella_desti == "Zona central")

# posar un mapa base


stops_rodalies <- "./data/gtfs_rodalies.zip" %>% read_gtfs() 

stops_rodalies <- stops_rodalies$stops %>% 
  stops_as_sf(crs = "EPSG:4326") %>% 
  st_transform(st_crs(cells_g)) %>%
  st_intersection(cells_g)

stops_fgc <- "./data/gtfs_fgc.zip" %>% read_gtfs() 

stops_fgc <- stops_fgc$stops %>% 
  stops_as_sf(crs = "EPSG:4326") %>% 
  st_transform(st_crs(cells_g)) %>%
  st_intersection(cells_g)


stops <- rbind(select(stops_rodalies, stop_name, geometry), select(stops_fgc, stop_name, geometry)) %>%
  mutate("Xarxa" = "Rodalies i FGC")

prop_czone <- cells_g %>%
  filter(NOMBRE_CEL != "Zona central") %>%
  left_join(go_czone, by = c("NOMBRE_CEL" = "cella_residencia")) %>%
  rename("go_centre" = "flux_persones") %>%
  left_join(not_stay, by = c("NOMBRE_CEL" = "cella_residencia")) %>%
  mutate(prop_czone = (go_centre)/flux_persones)

tm_shape(b) +
  tm_rgb() +
  tm_shape(prop_czone) +
  tm_polygons(col = "prop_czone", 
              title = "",
              palette = "viridis",
              style = "quantile",
              textNA = "0",
              lwd = 1.5,
              border.col = "black") + 
  tm_shape(stops) + 
  tm_dots(col = "Xarxa", palette = "Set1", size = 0.15) + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_credits("Font: INE, col·laboradors d'OpenStreetMap") + 
  tm_layout(main.title = "Proporció de persones que es desplacen\na la zona central")
  

# create network object

filt_mob <- mob %>%
  filter(cella_residencia != cella_desti)

desire_lines <- od2line(filt_mob, cells_g)


tm_shape(b) + 
  tm_rgb() + 
  tm_shape(cells_g) + 
  tm_borders(lwd = 1.5) + 
  tm_shape(filter(desire_lines, flux_persones > 150)) + 
  tm_lines(col = "flux_persones",
           style = "quantile",
           palette = "viridis",
           lwd = 2) + 
  tm_shape(stops) + 
  tm_dots(col = "Xarxa", palette = "Set1", size = 0.15) +
  tm_compass() + 
  tm_scale_bar() +
  tm_credits("Font: INE, col·laboradors d'OpenStreetMap") + 
  tm_layout(main.title = "Fluxos diaris de més de 150 persones")



recepcio <- cells_g %>%
  left_join({filt_mob %>%
      group_by(cella_desti) %>%
      summarise("flux_desti" = sum(flux_persones))},
      by = c("NOMBRE_CEL" = "cella_desti"))

tm_shape(b) +
  tm_rgb() +
  tm_shape(recepcio) +
  tm_polygons(col = "flux_desti", 
              title = "",
              palette = "viridis",
              style = "quantile",
              textNA = "0",
              colorNA = NULL,
              lwd = 1.5,
              border.col = "black") + 
  tm_shape(stops) + 
  tm_dots(col = "Xarxa", palette = "Set1", size = 0.15) + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_credits("Font: INE, col·laboradors d'OpenStreetMap") + 
  tm_layout(main.title = "Persones que es desplacen\ndiàriament a cada cel·la")


