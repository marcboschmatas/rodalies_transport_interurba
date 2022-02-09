library(rtweet)
library(tidyverse)

twitter_token <- create_token(app = "app",
                              consumer_key = "key",
                              consumer_secret = "secret",
                              access_token = "token",
                              access_secret = "secret",
                              set_renv = TRUE)


rodalies <- get_timeline(user = "@rodalies", n = 3200)
fgc <- get_timeline(user = "@fgc", n = 3200)

get_avisos <- function(x){
  av <- x %>%
    filter(((str_detect(`text`, "retard") |
              str_detect(`text`, "incidència") | 
              str_detect(`text`, "ATENCIÓ"))) &
             !(str_sub(`text`, start=1, end = 1) == "@"))
}

avisos_rodalies <- get_avisos(rodalies)

avisos_fgc <- get_avisos(fgc)

dies_avisos_fgc <- avisos_fgc %>%
  mutate("data" = lubridate::date(created_at)) %>%
  group_by(data) %>%
  summarise("n" = n()) %>%
  ungroup() %>%
  filter(data >= "2021-11-24")

dies_avisos_rodalies <- avisos_rodalies %>%
  mutate("data" = lubridate::date(created_at)) %>%
  group_by(data) %>%
  summarise("n" = n())
