library(tidyverse)
library(tidytransit)
library(sf)
library(furrr)
library(tmap)
library(lubridate)
future::multicore

# PENDENT: encodings, gràfics


# read gtfs

gtfss <- list.files("./data", pattern = "gtfs", full.names = TRUE)

gtfss <- as_tibble(gtfss)

colnames(gtfss) <- "file"

# functions

get_stops <- function(x){
  files <- read_gtfs(x)
  sfs <- gtfs_as_sf(files)
  stops <- sfs$stops
}

get_stop_times <- function(x){
  files <- read_gtfs(x)
  stops <- files$stop_times
}

get_trips <- function(x){
  files <- read_gtfs(x)
  trips <- files$trips
}


get_calendar <- function(x){
  files <- read_gtfs(x)
  calendar <- files$calendar
}

get_calendar_dates <- function(x){
  files <- read_gtfs(x)
  calendar <- files$calendar_dates
}


get_routes <- function(x){
  files <- read_gtfs(x)
  routes <- files$routes
}

# read stops & stop_times

stops_map <- gtfss %>%
  mutate("list" = future_map(file, get_stops)) %>%
  unnest() %>%
  mutate("agència" = str_remove(file, "./data/gtfs_"),
         "agència" = str_remove(`agència`, ".zip")) %>%
  select(-file) %>%
  mutate("agència" = case_when(`agència` == "amb" ~ "Bus urbà TMB/AMB i metro",
                               `agència` == "tmb" ~ "Bus urbà TMB/AMB i metro",
                               `agència` == "trambaix" ~ "Tram",
                               `agència` == "trambesos" ~ "Tram",
                               `agència` == "fgc" ~ "Ferrocarrils de la Generalitat de Catalunya",
                               `agència` == "interurba" ~ "Bus Interurbà Generalitat",
                               TRUE ~ "Rodalies RENFE"))



stop_times <- gtfss %>%
  mutate("list" = future_map(file, get_stop_times)) %>% 
  unnest() %>%
  mutate("agència" = str_remove(file, "./data/gtfs_"),
         "agència" = str_remove(`agència`, ".zip")) %>%
  mutate("agència" = case_when(`agència` == "amb" ~ "Bus urbà TMB/AMB i metro",
                               `agència` == "tmb" ~ "Bus urbà TMB/AMB i metro",
                               `agència` == "trambaix" ~ "Tram",
                               `agència` == "trambesos" ~ "Tram",
                               `agència` == "fgc" ~ "Ferrocarrils de la Generalitat de Catalunya",
                               `agència` == "interurba" ~ "Bus Interurbà Generalitat",
                               TRUE ~ "Rodalies RENFE")) %>%
  select(-file)


trips <- gtfss %>%
  mutate("list" = future_map(file, get_trips)) %>% 
  unnest() %>%
  mutate("agència" = str_remove(file, "./data/gtfs_"),
         "agència" = str_remove(`agència`, ".zip")) %>%
  mutate("agència" = case_when(`agència` == "amb" ~ "Bus urbà TMB/AMB i metro",
                               `agència` == "tmb" ~ "Bus urbà TMB/AMB i metro",
                               `agència` == "trambaix" ~ "Tram",
                               `agència` == "trambesos" ~ "Tram",
                               `agència` == "fgc" ~ "Ferrocarrils de la Generalitat de Catalunya",
                               `agència` == "interurba" ~ "Bus Interurbà Generalitat",
                               TRUE ~ "Rodalies RENFE")) %>%
  select(-file)


# select interurban bus stops at less than 500 metres of a commuter rail stop


intbus <- stops_map %>%
  filter(`agència` == "Bus Interurbà Generalitat") %>%
  st_sf()

fgc <- stops_map %>%
  filter(`agència` == "Ferrocarrils de la Generalitat de Catalunya") %>%
  st_sf()

st_crs(fgc) <- "EPSG:4326"
st_crs(intbus) <- "EPSG:4326"

# read cells

cells <- st_read("./data/project_rodalies.gpkg", layer = "celles_mobilitat_agrupades")

# get stops only in studied cells

fgc <- fgc %>%
  st_transform(st_crs(cells)) %>% 
  st_intersection(cells)

intbus <- intbus %>%
  st_transform(st_crs(cells)) %>% 
  st_intersection(cells)


# define buffer


fgc_buffer <- fgc %>% 
  st_buffer(500)

# get stops at 500 metres of a train station with train info

intbus_filt <- intbus %>%
  st_join({fgc_buffer %>% 
      rename("fgc_stop" = "stop_name") %>%
      select(fgc_stop, geometry)}, left = FALSE) %>% 
  select(stop_id, stop_name, fgc_stop, geometry) %>%
  distinct()


# get distance & travel time between each stop and its nearest neighbour

dist_matrix <- as.data.frame(st_distance(intbus_filt, fgc))

colnames(dist_matrix) <- fgc$stop_name

dist_matrix <- cbind(dist_matrix, as.data.frame(intbus_filt$stop_name))



dist_matrix_long <- dist_matrix %>%
  pivot_longer(cols = (-`intbus_filt$stop_name`), names_to = "fgc_stop", values_to = "distance") %>%
  rename("stop_name" = "intbus_filt$stop_name")

intbus_filt <- intbus_filt %>%
  left_join(dist_matrix_long, by = c("stop_name", "fgc_stop")) %>%
  mutate("time" = lubridate::period(second = ((3600/3500)*as.numeric(distance)))) %>% # assumming 3.5 kmh
  distinct()


# train stops without nearby bus stops

fgc_nobus <- fgc %>%
  filter(!(stop_name %in% intbus_filt$fgc_stop)) %>%
  distinct()


# get stop_times


fgc_stoptimes <- stop_times %>%
  filter(stop_id %in% fgc$stop_id) %>%
  left_join(select(fgc, stop_id, stop_name), by = "stop_id")

intbus_stoptimes <- stop_times %>%
  filter(stop_id %in% intbus_filt$stop_id) %>%
  left_join(select(intbus_filt, stop_id, stop_name), by = "stop_id")


# filter stop_times for any Thursday in October - NOTE need to do this ad-hoc for each agency

intbus_calendar <- gtfss[3,1] %>%
  mutate("list" = map(file, get_calendar)) %>%
  select(-file) %>%
  mutate("agència" = "Bus Interurbà Generalitat") %>%
  unnest()

intbus_calendar_dates <- gtfss[3,1] %>%
  mutate("list" = map(file, get_calendar_dates)) %>%
  select(-file) %>%
  mutate("agència" = "Bus Interurbà Generalitat") %>%
  unnest()

intbus_calendar_dates_filt <- intbus_calendar_dates %>%
  filter(`date` == "2021-10-21")


fgc_calendar <- gtfss[2,1] %>%
  mutate("list" = map(file, get_calendar)) %>%
  select(-file) %>%
  mutate("agència" = "Ferrocarrils de la Generalitat de Catalunya") %>%
  unnest()

fgc_calendar_dates <- gtfss[2,1] %>%
  mutate("list" = map(file, get_calendar_dates)) %>%
  select(-file) %>%
  mutate("agència" = "Ferrocarrils de la Generalitat de Catalunya") %>%
  unnest()

fgc_filtered_dates <- fgc_calendar %>% 
  filter(date == "2022-10-06") %>%
  select(service_id)


fgc_trips_filtered <- trips %>%
  filter((`agència` == "Ferrocarrils de la Generalitat de Catalunya") &
           (service_id %in% fgc_filtered_dates$service_id)) #2021 only have Monestir de Montserrat


fgc_stoptimes_filtered <- fgc_stoptimes %>%
  filter(trip_id %in% fgc_trips_filtered$trip_id)



intbus_trips_filtered <- trips %>%
  filter((`agència` == "Bus Interurbà Generalitat") & 
           (service_id %in% intbus_calendar_dates_filt$service_id))

intbus_stoptimes_filtered <- intbus_stoptimes %>%
  filter(trip_id %in% intbus_trips_filtered$trip_id) %>%
  left_join(select(as_tibble(intbus_filt), stop_id, fgc_stop, time, -geometry), by = "stop_id")

# get earliest possible train departure time to ensure 

intbus_stoptimes_filtered <- intbus_stoptimes_filtered %>%
  mutate("minimal_deptime" = hms::hms(as.period(arrival_time)+time))


# join route names to intbus & train

intbus_stoptimes_filtered <- intbus_stoptimes_filtered %>%
  left_join(select(intbus_trips_filtered, trip_id, route_id), by = "trip_id")



fgc_stoptimes_filtered <- fgc_stoptimes_filtered %>%
  left_join(select(fgc_trips_filtered, trip_id, route_id), by = "trip_id")

fgc_routes <- gtfss[2,1] %>%
  mutate("list" = map(file, get_routes)) %>%
  select(-file) %>%
  mutate("agència" = "Ferrocarrils de la Generalitat de Catalunya") %>%
  unnest()

fgc_stoptimes_filtered <- fgc_stoptimes_filtered %>%
  left_join(select(fgc_routes, route_id, route_long_name), by = "route_id")

# get nearest train departure time for each route & headsign after each assignation

get_next_stop <- function(time, train_stop){
  mintrips <- fgc_stoptimes_filtered %>%
    filter((stop_name == train_stop) &
             (departure_time >= time)) %>% 
    rename("fgc_route_name" = "route_long_name") %>%
    group_by(fgc_route_name) %>%
    summarise("first_stop" = hms::hms(as.period(min(departure_time)))) %>%
    ungroup()
}


connections <- intbus_stoptimes_filtered %>%
  mutate("stop_times" = future_map2(minimal_deptime, fgc_stop, get_next_stop)) %>%
  as_tibble() %>% 
  select(-geometry) %>%
  unnest(cols = everything(), keep_empty = TRUE)


write.csv(connections, "./data/connexions_bus_fgc.csv")


cells <- st_read("./data/project_rodalies.gpkg", layer = "celles_mobilitat_agrupades")

connections <- read.csv("./data/connexions_bus_fgc.csv")

connections <- connections %>% 
  filter((arrival_time >= "07:00:00") & 
           (arrival_time <= "10:00:00")) %>%
  arrange(arrival_time)



# get time lapse between bus arrival and first departure

connections$waiting_time <- lubridate::hms(connections$first_stop)-lubridate::hms(connections$minimal_deptime)


connections$waiting_time <- as.duration(connections$waiting_time)

# filter for stops outside of the central zone

fgc_nocentre <- fgc %>%
  st_intersection(filter(cells, NOMBRE_CEL != "Zona central")) %>%
  select(colnames(fgc))


connections <- connections %>%
  filter(fgc_stop %in% fgc_nocentre$stop_name)






connections_bystop <- connections %>%
  group_by(fgc_stop) %>%
  summarise("min_waitingtime" = duration(seconds = min(as.numeric(waiting_time), na.rm = TRUE)),
            "mean_waitingtime" = duration(seconds = mean(as.numeric(waiting_time), na.rm = TRUE)),
            "median_waitingtime" = duration(seconds = median(as.numeric(waiting_time), na.rm = TRUE)),
            "max_waitingtime" = duration(seconds = max(as.numeric(waiting_time), na.rm = TRUE)))

connections_byroute <- connections %>%
  group_by(fgc_route_name) %>%
  summarise("min_waitingtime" = duration(seconds = min(as.numeric(waiting_time), na.rm = TRUE)),
            "mean_waitingtime" = duration(seconds = mean(as.numeric(waiting_time), na.rm = TRUE)),
            "median_waitingtime" = duration(seconds = median(as.numeric(waiting_time), na.rm = TRUE)),
            "max_waitingtime" = duration(seconds = max(as.numeric(waiting_time), na.rm = TRUE)))


write.csv(connections_byroute, "./data/connexions_ruta_horapunta_fgc.csv")
write.csv(connections_bystop, "./data/connexions_parada_horapunta_fgc.csv")