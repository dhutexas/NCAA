# load rvest package
library(rvest)
library(tidyverse)
library(magrittr)
#library(ncaahoopR)

source("get_pbp_game_new.R")
source("ncaa_helpers.R")
load("dict.rda")
load("ids.rda")


##### The Process ###### 
# 1 - get_game_ids_new(team, season)
# using team name and season, grab ESPN season overview page
# https://www.espn.com/mens-college-basketball/team/schedule/_/id/2305/season/2019
# and pull gameIds from html (hidden behind score in a link to the game summary)

# 2 - get_pbp_game_new(team, season)
# use these gameIds to pull data about each game, such as play by play
# https://www.espn.com/mens-college-basketball/playbyplay?gameId=401082470

# 2a - within get_pbp_game_new() parse the data, pulling out shot location data, etc.
# and doing general cleaning

# 3 - get_pbp()
# pull all pbp data for a given team in a given season, saving it as a df


# 4 - collect data across historical time
# perhaps make lists of all teams in conferences
# then run for all teams in conference for all time
# would give some time between scrapes


## TODO ##
# need to combine dataframes (without rbind since columns may not match)

























## fixed function
# team as string
# season as year tournament held for the season (so, latter year)


get_game_ids_new <- function(team, season) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  
    library(rvest)
    library(tidyverse)
    base_url <- "https://www.espn.com/mens-college-basketball/team/schedule/_/id/"
    url <- paste0(base_url, ids$id[ids$team == team], "/season/", as.numeric(substring(season, 1, 4)) )
    html_data = read_html(url)
    game_ids <- html_data %>% 
      html_nodes("a") %>% # get the a classes 
      html_attr("href") %>% # get only the hyperlinks on the page
      as_tibble() %>%
      filter(grepl('gameId', value)) %>% # retain only links for games
      mutate(value = gsub("http://www.espn.com/mens-college-basketball/game\\?gameId=", "", value)) %>%
      filter(!str_detect(value, 'recap')) %>% # remove the extra 'recap' games that show up
      filter(!str_detect(value, 'preview')) %>%
      pull()
  
  return(game_ids)
}


#get_game_ids_new('Kansas',season = '2021')







get_pbp <- function(team, season, extra_parse = T) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  if(!team %in% ids$team) {
    stop("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
  }
  
  message(paste("Getting Game IDs: ", team, sep = ""))
  
  ### Get Game IDs
  game_ids <- get_game_ids_new(team, season)
  
  ### Get PBP Data
  pbp_season <- get_pbp_game_new(game_ids, extra_parse)
  
  return(pbp_season)
}






# can't do rbind because columns don't all match across seasons
team = 'Kansas'

# run for loop creating dataframe of games and opponents for each team
for (i in 2013:2021) {
  
  assign(paste0('team_df_', i), get_pbp(team, i))
  
  #team_df %<>%
  #  mutate(team_id = team,
  #         season = toString(i))
  
  # add these to a running list of dataframes
  # dfList[[i]] = team_df
  
}



#write.csv(team_df_2010, 'kansas_2010.csv')
