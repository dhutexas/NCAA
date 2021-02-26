################################### Get Game Data from ESPN ####################
#' Get game data (play-by-play, odds, and game details) for a given gameId
#'
#'
#' Requires use of ESPN oriented team names, found in locally saved file (ids.csv)
#'
#' @param espn_game_id (str) The eight digit gameId for the desired game
#' 

get_game_data <- function(espn_game_id) {

    library(tidyverse)
    library(DBI)
    library(dbplyr)
    
    dw <- config::get("datawarehouse_ncaa")
    
    # Pull the JSon
    game_json <- httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event={espn_game_id}")) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)
    
    #### extract the data from the json ####
    
    # odds data
    odds = game_json$pickcenter %>%
      select(-links) %>%
      mutate(game_id = game_json$header$id,
             season = game_json$header$season$year,
             season_type = game_json$header$season$type) %>%
      select(game_id, season, season_type, everything())
    
    # game info for the teams
    comps = game_json$header$competitions$competitors[[1]] %>%
      select(id, homeAway, winner, score, rank, 
             team.location, team.name)
    
    # create new columns with home team data
    comps %>%
      filter(homeAway == 'home') %>%
      select(-homeAway) %>%
      setNames(paste0('home_', names(.))) -> home
    
    # create new columns with away team data
    comps %>%
      filter(homeAway == 'away') %>%
      select(-homeAway) %>%
      setNames(paste0('away_', names(.))) -> away
    
    cbind(home, away) %>%
      mutate(game_id = game_json$header$id,
             season = game_json$header$season$year,
             season_type = game_json$header$season$type) %>%
      select(game_id, season, season_type, everything()) -> comps
    
    rm(away, home)
    
    # pull a df of all the plays and shots
    #library(stringr)
    game_plays = game_json$plays %>%
      mutate(game_id = game_json$header$id,
             season = game_json$header$season$year,
             season_type = game_json$header$season$type) %>%
      separate(participants, into=c('player1','player2'), sep = ',') %>%
      mutate(player1 = as.numeric(str_extract_all(player1, "\\d{7}")),
             player2 = as.numeric(str_extract_all(player2, "\\d{7}")))
    
    game_plays %>%
      left_join(comps, by = c('game_id','season','season_type')) -> game_data
    
    # return as a list of dataframes, OR write each to SQL
    return(game_data)
    
    rm(game_json)

}


# get_game_data(401260001)

# pull out pbp and odds if returning dfs as list of dfs
#do.call(rbind.data.frame, df[1]) -> pbp
#do.call(rbind.data.frame, df[2]) -> odds_gm

