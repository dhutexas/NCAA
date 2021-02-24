### ESPN API ###



# secret api for game data (summary page)
# http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event=323142305


# ideas from football
# https://www.opensourcefootball.com/posts/2020-08-29-adding-espn-and-538-game-predictions-to-nflfastr-data/


# here can enter in game_id if add http address
# Pull the JSon
game_json <- httr::GET(url = glue::glue("http...{espn_game_id}")) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON(flatten = TRUE)

# Pull the JSon
game_json <- httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event=401260001")) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON(flatten = TRUE)


game_json$plays$participants

library(tidyjson)
game_json %>% enter_object(plays) %>% gather_array

# game id
game_json$header$id
# season
game_json$header$season$year
# reg, post, etc.
game_json$header$season$type





# pull a df of all the plays and shots
library(stringr)
game_plays = game_json$plays %>%
  mutate(game_id = game_json$header$id,
         season = game_json$header$season$year,
         season_type = game_json$header$season$type) %>%
  separate(participants, into=c('player1','player2'), sep = ',') %>%
  mutate(player1 = str_extract_all(player1, "\\d{7}"),
         player2 = str_extract_all(player2, "\\d{7}"))


# odds data
odds = game_json$pickcenter %>%
  select(-links) %>%
  mutate(game_id = game_json$header$id,
         season = game_json$header$season$year,
         season_type = game_json$header$season$type)


# game info for the teams
comps = game_json$header$competitions$competitors[[1]] %>%
  select(id, homeAway, winner, score, rank, team.id, 
         team.location, team.name, team.nickname) %>%
  mutate(game_id = game_json$header$id,
         season = game_json$header$season$year,
         season_type = game_json$header$season$type)






