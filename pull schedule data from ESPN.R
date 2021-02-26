library(DBI)
library(dbplyr)

# pull in config file to hide passwords
dw <- config::get("datawarehouse_ncaa")

con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = dw$dbname,
                      host = dw$networkhost,
                      port    = dw$port,
                      user    = dw$user,
                      password   = dw$password
)


# load libraries and functions
library(tidyverse)
source("get_game_ids.R")

# basic data gathering
ids <- read.csv("ids.csv", as.is = T) 
team_list = ids$team # all D1
#sched = get_season_schedule(team_list, 2020)





### MANUAL ####

season = 2017

rm(sched)

# get complete season schedule for list of teams
sched = get_season_schedule(team_list, season)

# convert year to written form for better SQL play
english::as.english(season) -> x
x = stringr::str_replace_all(x, fixed(" "), "")


con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = dw$dbname,
                      host = dw$networkhost,
                      port    = dw$port,
                      user    = dw$user,
                      password   = dw$password
)

# write schedule to SQL db
dbWriteTable(con, SQL(paste0('schedules.', x)), sched, overwrite=T)



