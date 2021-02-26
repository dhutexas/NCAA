################################### Get Game Ids from ESPN ####################
#' Get Team gameIds By Season
#'
#' Scrapes the season schedule for desired team. Team
#' is assumed to be the ESPN team name, which can be looked up in the ids
#' dataframe.
#'
#' Requires use of ESPN oriented team names, found in locally saved file (ids.csv)
#'
#' @param team (str) Team to get Play-by-Play data for
#' @param season (str) or (int) Season for which to get schedule. In form "2019-20" or "2020". Single number is season of tournament.
#' @return A data frame of the team's gameIds data for the specified season.

get_game_ids_new <- function(team, season) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  
  library(rvest)
  library(tidyverse)
  
  # create list of ids to use in searches
  ids <- read.csv("ids.csv", as.is = T) 
  
  rm(url)
  base_url <- "https://www.espn.com/mens-college-basketball/team/schedule/_/id/"
  url <- paste0(base_url, ids$id[ids$team == team], "/season/", as.numeric(substring(season, 1, 4)) )
  tryCatch(
    game_ids <- read_html(url) %>% 
      html_nodes("a") %>% # get the a classes 
      html_attr("href") %>% # get only the hyperlinks on the page
      as_tibble() %>%
      filter(grepl('gameId', value)) %>% # retain only links for games
      mutate(value = gsub("http://www.espn.com/mens-college-basketball/game\\?gameId=", "", value)) %>%
      filter(!str_detect(value, 'recap')) %>% # remove the extra 'recap' games that show up
      filter(!str_detect(value, 'preview')) %>%
      pull(),
    error = function(e){NA}) # moves on to next team if error, printing NA for gameIds
  
  return(game_ids)
  
}


################################### Get Game Ids from ESPN ####################
#' Get a list of all teams in a conference
#' 
#' @param pull_conference (str) Conference to get list of teams for
#' 

get_conf_teams <- function(pull_conference) {
  
  library(tidyverse)
  # pull in list of ids
  teams <- read.csv("team_crosswalk.csv") %>% 
    filter(conference == pull_conference) %>%
    pull(ESPN)
  
  return(teams)

}

team_list = get_conf_teams('Big 12')

################################### Get Full Season gameIds ####################
#' Get a full master schedule of all games in a season for a list of teams
#' 
#' Can use a single team, a list of teams in a conference, or full D1 list
#' Teams must be in ESPN format, found in ids file (ids.csv)
#' 
#' @param team (str) Team to get Play-by-Play data for
#' @param season (str) or (int) Season for which to get schedule. In form "2019-20" or "2020". Single number is season of tournament.
#' @return A data frame of the team, gameIds, and season

get_season_schedule <- function(team, season) {
 
  dfList <- list()  ## create empty list to hold data for each team
  
  library(tidyverse)
  
   for (i in team){
     ### Error Testing - move on to next team instead of breaking the loop
     if(is.na(team)) {
       next("team is missing with no default")
     }
     
     get_game_ids_new(i, season) %>% 
       enframe(name = NULL, value = 'game_ids') %>%
       mutate(team = toString(i),
              season = toString(season)) -> team_df 
     
     Sys.sleep(1) ## add a small delay to reduce load on ESPN servers
     
     dfList[[i]] = team_df ## add team data to running list for each team
     
   }

  schedule = do.call(rbind, c(dfList, make.row.names = FALSE)) ## combine teams into large df of the data
  return(schedule)
  
}

