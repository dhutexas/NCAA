library(tidyverse)
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

# read in schedules for a particular season from SQL db
schedule = dbReadTable(con, SQL('schedules.twothousandtwenty'))

# bring in function
source("get_game_data.R")

#gameIds = schedule %>%
#  filter(team == 'Kansas') %>%
#  select(game_ids) %>%
#  pull() 

# get unique gameIds from the schedule (~6,000 per typical season)
gameIds = schedule %>%
  select(game_ids) %>%
  pull() %>%
  unique()

### Function to grab all games in a list, write to large dataframe, then write to SQL ####

dfList <- list()  ## create empty list to hold data for each team

for (i in gameIds){

  # get complete season schedule for list of teams
  game_pbp = get_game_data(i)
  
  Sys.sleep(2)
  
  dfList[[i]] = game_pbp ## add schedule data to running list for each season

}

# if use dplyr::bind_rows() you can join dfs where columns do not match correctly (as is case here)
pbp = dplyr::bind_rows(dfList)

# convert year of season to written form for better SQL play
english::as.english(unique(pbp$season)) -> x
x = stringr::str_replace_all(x, fixed(" "), "")
x = stringr::str_replace_all(x, "-", "")

# establish connection with SQL db
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = dw$dbname,
                      host = dw$networkhost,
                      port = dw$port,
                      user = dw$user,
                      password = dw$password)

# write game_data to SQL db
dbWriteTable(con, SQL(paste0('pbp.', x)), pbp, append=T, row.names=F)






### Double-check if worked

# likely need to grab all, rbind before sending to SQL (to avoid numerous errors)
pbp_sql = dbReadTable(con, SQL('pbp.twothousandtwenty'))

pbp_sql %>%
  select(game_id) %>%
  unique() %>%
  count()

