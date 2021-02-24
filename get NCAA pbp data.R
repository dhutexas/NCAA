# get pbp data from ESPN using ncaahoopR from github

# analytics from these data
# https://kentuckysportsradio.com/basketball-2/analytics-show-kentuckys-shooters-take-the-worst-shots-in-college-basketball/

# old espn pbp url
# https://www.espn.com/mens-college-basketball/playbyplay?gameId=401083442


# get old schedule
# https://www.espn.com/mens-college-basketball/team/schedule/_/id/2305/season/2020
# https://www.espn.com/mens-college-basketball/team/schedule/_/id/2305/season/2018


#devtools::install_github("lbenz730/ncaahoopR")

library(tidyverse)
library(magrittr)
library(ncaahoopR)

# only pulls current season right now
kansas = get_pbp(team = 'Duke', season = '2018')

# works
get_roster('Kansas', season = '2018-19')

# this is the problem with get_pbp for seasons before this one
# needs to be fixed (urls likely different)
get_game_ids('Kansas', season = '2018-19')


library(readxl)
team_crosswalk <- read_excel("team_crosswalk.xlsx")

team_crosswalk %>%
  filter(conference == 'Big 12') -> big12

team_crosswalk %>%
  select(ESPN_PBP) %>%
  pull() -> teams




# function to gather all pbp data for a list of teams, then save as giant df (per season)

dfList <- list()  ## create empty list
season_pull = "2020-21"


for (i in teams) {
  
  #### create dataframe of games and opponents for each team ####
  get_pbp(toString(i), season = season_pull) -> team_df
  
  team_df %<>%
    mutate(season = season_pull)
  
  # add these to a running list of dataframes
  dfList[[i]] = team_df
  
}

# combine this list of dataframes into one dataframe (more efficient than doing it inside for loop in R)
pbp = do.call(rbind,dfList)

# drop index/place teams as first column
pbp <- cbind(team = rownames(pbp), pbp)
rownames(pbp) <- NULL






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
  if(season == current_season) {
    game_ids <- get_schedule(team) %>%
      dplyr::filter(date < Sys.Date()) %>%
      pull(game_id)
  } else {
    game_ids <- get_game_ids(team, season)
  }
  
  
  ### Get PBP Data
  pbp_season <- get_pbp_game(game_ids, extra_parse)
  
  return(pbp_season)
}

get_pbp('Kansas', current_season= '2018-19')

