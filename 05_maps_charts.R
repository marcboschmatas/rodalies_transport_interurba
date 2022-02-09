library(tidyverse)
library(tidytransit)
library(sf)
library(furrr)
library(tmap)


gtfss <- list.files("./data", pattern = "gtfs", full.names = TRUE)

gtfss <- as_tibble(gtfss)

colnames(gtfss) <- "file"

# functions

get_stops <- function(x){
  files <- read_gtfs(x)
  sfs <- gtfs_as_sf(files)
  stops <- sfs$stops
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


routes <- gtfss %>%
  mutate("list" = future_map(file, get_routes)) %>%
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


trips <- gtfss %>%
  mutate("list" = future_map(file, get_trips)) %>%
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


cells <- st_read("./data/project_rodalies.gpkg", layer = "celles_mobilitat_agrupades")



stops_filt <- stops_map %>%
  st_sf() %>%
  filter(`agència` %in% c("Rodalies RENFE", "Ferrocarrils de la Generalitat de Catalunya"))


st_crs(stops_filt) <- "EPSG:4326"


stops_filt <- stops_filt %>%
  st_transform(st_crs(cells)) %>% 
  st_intersection(cells)


get_stop_times <- function(x){
  files <- read_gtfs(x)
  stops <- files$stop_times
}

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


fgc_calendar <- gtfss[2,1] %>%
  mutate("list" = map(file, get_calendar)) %>%
  select(-file) %>%
  mutate("agència" = "Ferrocarrils de la Generalitat de Catalunya") %>%
  unnest()

stop_times <- stop_times %>%
  filter(stop_id %in% stops_filt$stop_id)

trips <- trips %>%
  filter(trip_id %in% stop_times$trip_id & 
           service_id %in% c("5048J", "684bdae302747664", "624ddae302746810", "625cdae3027410", "6c4bdab14472690fbb55c60d53"))

ggplot(stop_times) + 
  geom_histogram(aes(x = arrival_time)) + 
  theme_minimal() + 
  labs(title = "Distribució d'hores d'arribada",
       caption = "Font: Elaboració pròpia amb dades de Renfe i FGC",
       x = "Hora d'arribada",
       y = "Arribades") + 
  xlab("Hora d'arribada") + 
  ylab("Arribades")









get_freqs <- function(gtfs, start, finish){
  files <- read_gtfs(gtfs)
  fs <- get_route_frequency(files, start_time = start, end_time = finish)
}
  
freqs <- gtfss %>%
  filter(file %in% c("./data/gtfs_fgc.zip", "./data/gtfs_rodalies.zip")) %>%
  mutate("list" = future_map(file, ~get_freqs(gtfs = .x, start = "06:00:00", finish = "22:00:00"))) %>%
  unnest() %>%
  mutate("agència" = str_remove(file, "./data/gtfs_"),
         "agència" = str_remove(`agència`, ".zip")) %>%
  select(-file) %>%
  mutate("agència" = case_when(`agència` == "fgc" ~ "Ferrocarrils de la Generalitat de Catalunya",
                               TRUE ~ "Rodalies RENFE"))  


get_stop_frequency(read_gtfs("./data/gtfs_fgc.zip"), start_time = "06:00:00", end_time = "22:00:00")

bm <- tmaptools::read_osm(x = cells, type = "osm")

tm_shape(bm) + 
  tm_rgb() + 
  tm_shape(stops_filt) +
  tm_dots(col = "agència", palette = "viridis", size = 0.25) + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_credits("Fonts: Col·laboradors d'OpenStreetMap, \nGeneralitat de Catalunya, Renfe") +
  tm_layout(main.title = "Xarxa de Rodalies i FGC",
            attr.outside = FALSE,
            legend.outside = TRUE,
            legend.width = 10)

hhmmss_to_seconds <- function(hhmmss_str) {
  as.numeric(substr(hhmmss_str, 0, 2)) * 3600 +
    as.numeric(substr(hhmmss_str, 4, 5)) * 60 +
    as.numeric(substr(hhmmss_str, 7, 8))
}
#' Get Stop Frequency
#' 
#' Calculate the number of departures and mean headways for all stops within a
#' given timespan and for given service_ids. 
#' 
#' @note Some GTFS feeds contain a frequency data frame already. 
#' Consider using this instead, as it will be more accurate than what 
#' tidytransit calculates.
#' 
#' @param gtfs_obj a list of gtfs dataframes as read by [read_gtfs()].
#' @param start_time analysis start time, can be given as "HH:MM:SS", 
#'                   hms object or numeric value in seconds.
#' @param end_time analysis perdiod end time, can be given as "HH:MM:SS", 
#'                 hms object or numeric value in seconds.
#' @param service_ids A set of service_ids from the calendar dataframe 
#'                    identifying a particular service id. If not provided, the service_id 
#'                    with the most departures is used.
#' @param by_route Default TRUE, if FALSE then calculate headway for any line coming 
#'                 through the stop in the same direction on the same schedule. 
#' @return dataframe of stops with the number of departures and the headway 
#'         (departures divided by timespan) in seconds as columns
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data !! quo enquo
#' @importFrom stats median sd
#' @export
#' @examples 
#' data(gtfs_duke)
#' stop_frequency <- get_stop_frequency(gtfs_duke)
#' x <- order(stop_frequency$mean_headway)
#' head(stop_frequency[x,])
get_stop_frequency_2 <- function(gtfs_obj,
                               start_time = "06:00:00",
                               end_time = "22:00:00",
                               service_ids = NULL,
                               by_route = TRUE) {
  n_deps <- direction_id <- NULL
  
  if(is.character(start_time)) start_time <- hhmmss_to_seconds(start_time)
  if(is.character(end_time)) end_time <- hhmmss_to_seconds(end_time)
  
  # get service id with most departures
  if(is.null(service_ids)) {
    dep_per_trip = gtfs_obj$stop_times %>% 
      dplyr::group_by(trip_id) %>% dplyr::count(name = "n_deps") %>% 
      dplyr::ungroup()
    dep_per_service_id = left_join(gtfs_obj$trips, dep_per_trip, "trip_id") %>% 
      dplyr::group_by(service_id) %>% 
      dplyr::summarise(n_deps = sum(n_deps)) %>% 
      dplyr::arrange(dplyr::desc(n_deps))
    service_ids = dep_per_service_id$service_id[1]  
  }
  
  # filter stop_times to service_ids and start/end_time
  trips = gtfs_obj$trips %>% filter(service_id %in% service_ids)
  
  stop_times = gtfs_obj$stop_times %>%
    filter(trip_id %in% trips$trip_id) %>% 
    filter(departure_time >= start_time & arrival_time <= end_time) %>% 
    left_join(trips[c("trip_id", "route_id", "service_id")], "trip_id") 
  
  # find number of departure per stop_id (route_id, direction_id, service_id)
  if(by_route) {
    freq = stop_times %>% 
      dplyr::group_by(stop_id, route_id, service_id) %>% 
      dplyr::count(name = "n_departures") %>% dplyr::ungroup()
  } else {
    freq = stop_times %>% 
      dplyr::group_by(stop_id, service_id) %>% 
      dplyr::count(name = "n_departures") %>% dplyr::ungroup()
  }
  
  # calculate average headway
  duration = as.numeric(end_time-start_time)
  freq$mean_headway <- round(duration / freq$n_departures)
  
  freq
}

#' Get Route Frequency
#' 
#' Calculate the number of departures and mean headways for routes within a given timespan
#' and for given service_ids.
#' 
#' @note Some GTFS feeds contain a frequency data frame already. 
#' Consider using this instead, as it will be more accurate than what 
#' tidytransit calculates. 
#' 
#' @param gtfs_obj a list of gtfs dataframes as read by the trread package.
#' @param start_time analysis start time, can be given as "HH:MM:SS", 
#'                   hms object or numeric value in seconds.
#' @param end_time analysis perdiod end time, can be given as "HH:MM:SS", 
#'                 hms object or numeric value in seconds.
#' @param service_ids A set of service_ids from the calendar dataframe 
#'                    identifying a particular service id. If not provided, the service_id 
#'                    with the most departures is used.
#' @return a dataframe of routes with variables or headway/frequency in seconds for a route 
#'         within a given time frame
#' @export
#' @examples 
#' data(gtfs_duke)
#' routes_frequency <- get_route_frequency(gtfs_duke)
#' x <- order(routes_frequency$median_headways)
#' head(routes_frequency[x,])
get_route_frequency_2 <- function(gtfs_obj,
                                start_time = "06:00:00",
                                end_time = "22:00:00",
                                service_ids = NULL) {
  total_departures <- median_headways <- mean_headways <- NULL
  n_departures <- mean_headway <- st_dev_headways <- stop_count <- NULL
  departures_per_stop = get_stop_frequency_2(gtfs_obj, start_time, end_time, 
                                           service_ids, by_route = TRUE)
  
  if(dim(departures_per_stop)[[1]] != 0) {
    routes_frequency = departures_per_stop %>% 
      group_by(route_id) %>%
      summarise(total_departures = sum(n_departures),
                median_headways = round(median(mean_headway)),
                mean_headways = round(mean(mean_headway)),
                st_dev_headways = round(sd(mean_headway), 2),
                stop_count = dplyr::n())
  } else {
    warning("Failed to calculate frequency, try passing a service_id from calendar_df.")
  }
  
  return(routes_frequency)
}

freqs_fgc <- get_route_frequency_2(read_gtfs("./data/gtfs_fgc.zip"), 
                                   start_time = "07:00:00",
                                   end_time = "10:00:00",
                                   service_ids = c('6c4bdab14472690fbb55c60d53',
                                                   '625cdae3027410',
                                                   '684bdae302747664'))

freqs_fgc <- freqs_fgc %>%
  mutate("median_headways" = median_headways/60,
         "mean_headways" = mean_headways/60,
         "st_dev_headways" = st_dev_headways/60) %>%
  left_join(select(routes, route_id, route_long_name), by = "route_id")

write.csv(freqs_fgc, "./data/frequencies_fgc_horapunta.csv")


freqs_rodalies <- get_route_frequency_2(read_gtfs("./data/gtfs_rodalies.zip"), 
                                        start_time = "07:00:00",
                                        end_time = "10:00:00",
                                        service_ids = c('5048J'))

freqs_rodalies <- freqs_rodalies %>%
  filter(route_id %in% trips$route_id) %>%
  mutate("median_headways" = median_headways/60,
         "mean_headways" = mean_headways/60,
         "st_dev_headways" = st_dev_headways/60) %>%
  left_join(select(routes, route_id, route_long_name), by = "route_id")

write.csv(freqs_rodalies, ".data/frequencies_rodalies_horapunta.csv")


freqs_parades_fgc <- get_stop_frequency_2(read_gtfs("./data/gtfs_fgc.zip"),
                                          start_time = '07:00:00',
                                          end_time = "10:00:00",
                                          service_ids = c('6c4bdab14472690fbb55c60d53',
                                                          '625cdae3027410',
                                                          '684bdae302747664'))

freqs_parades_rodalies <- get_stop_frequency_2(read_gtfs("./data/gtfs_rodalies.zip"),
                                          start_time = '07:00:00',
                                          end_time = "10:00:00",
                                          service_ids = c('5048J'))


freqs_parades_fgc <- freqs_parades_fgc %>%
  mutate("mean_headway" = mean_headway/30) %>%
  group_by(stop_id) %>%
  summarise("mean_headway" = mean(mean_headway))

freqs_parades_rodalies <- freqs_parades_rodalies %>%
  mutate("mean_headway" = mean_headway/60) %>%
  group_by(stop_id) %>%
  summarise("mean_headway" = mean(mean_headway))


stops_freqs <- stops_filt %>%
  select(stop_id, stop_name, geometry) %>%
  left_join(rbind(freqs_parades_fgc, freqs_parades_rodalies), by = "stop_id")


cells_waiting <- stops_freqs %>%
  st_join(cells) %>%
  as_tibble() %>%
  select(-geometry) %>%
  group_by(NOMBRE_CEL) %>%
  summarise("median_headway" = mean(median_headway, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(select(cells, NOMBRE_CEL, POB_GRUPO)) %>%
  as_tibble() %>%
  select(-geom)

cells_waiting_60 <- cells_waiting %>%
  filter(mean_headway >= 60)


tm_shape(bm) + 
  tm_rgb() + 
  tm_shape(stops_freqs) +
  tm_dots(col = "mean_headway", title = "minuts", palette = "viridis", size = 0.25,
          textNA = "Sense dades") + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_credits("Fonts: Col·laboradors d'OpenStreetMap, \nGeneralitat de Catalunya, Renfe") +
  tm_layout(main.title = "Temps mitjà d'espera a les estacions de\nRodalies i FGC de 7 a 10 del matí",
            attr.outside = FALSE,
            legend.outside = TRUE)


stops_per_cell <- stops_filt %>%
  group_by(NOMBRE_CEL) %>%
  summarise("stops" = n())

cells_notrain <- cells %>%
  filter(!(NOMBRE_CEL %in% stops_per_cell$NOMBRE_CEL))


stops_bus <- stops_map %>%
  st_sf() %>%
  filter(`agència` %in% c("Bus Interurbà Generalitat"))


st_crs(stops_bus) <- "EPSG:4326"


stops_bus <- stops_bus %>%
  st_transform(st_crs(cells)) %>% 
  st_intersection(cells)



get_stop_times <- function(x){
  files <- read_gtfs(x)
  stops <- files$stop_times
}

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




intbus_stoptimes <- stop_times %>%
  filter(stop_id %in% stops_bus$stop_id)

intbus_calendar_dates <- gtfss[3,1] %>%
  mutate("list" = map(file, get_calendar_dates)) %>%
  select(-file) %>%
  mutate("agència" = "Bus Interurbà Generalitat") %>%
  unnest()

intbus_calendar_dates_filt <- intbus_calendar_dates %>%
  filter(`date` == "2021-10-21")

intbus_trips_filtered <- trips %>%
  filter((`agència` == "Bus Interurbà Generalitat") & 
           (service_id %in% intbus_calendar_dates_filt$service_id))

intbus_routes <- routes %>%
  filter(route_id %in% intbus_trips_filtered$route_id)

# note we do the native function because there's direction_id. hooray for la Gene!

freqs_parades_intbus <- get_stop_frequency(read_gtfs("./data/gtfs_interurba.zip"),
                                               start_time = '07:00:00',
                                               end_time = "10:00:00",
                                               service_ids = as.vector(unique(intbus_trips_filtered$service_id)))


freqs_parades_intbus <- freqs_parades_intbus %>%
  mutate("mean_headway" = mean_headway/60) %>%
  group_by(stop_id) %>%
  summarise("mean_headway" = mean(mean_headway),
            "n_departures" = sum(n_departures))


stops_bus <- stops_bus %>%
  select(stop_id, stop_name) %>%
  left_join(freqs_parades_intbus, by = "stop_id")

tm_shape(bm) + 
  tm_rgb() + 
  tm_shape(filter(stops_bus, !is.na(mean_headway))) +
  tm_dots(col = "mean_headway", title = "minuts", palette = "viridis", size = 0.15,
          textNA = "Sense dades") + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_credits("Fonts: Col·laboradors d'OpenStreetMap, \nGeneralitat de Catalunya") +
  tm_layout(main.title = "Temps mitjà d'espera a les estacions de\nBus Interurbà de 7 a 10 del matí",
            attr.outside = FALSE,
            legend.outside = TRUE)

