# NCAA
stats and analysis of NCAA mens basketball data

### Two main functions:
- get_game_ids_new(team, season)
- get_season_schedule(team, season)
- get_game_data(espn_game_id)

GameIds obtains all gameIds from ESPN for a particular team in a particular season

Season schedule returns a dataframe with the gameIds, team, and season. It relies upon get_game_ids_new() to pull the game ids, but can then build a large dataframe of many teams and gameIds, such as all teams in a conference, or all teams in the ESPN archive.

Game data pulls the play-by-play and general game info (teams, rankings, winner, score, etc.) and returns these in a single dataframe based upon a single ESPN gameId.

### Two files to collect data:
- pull schedule data from ESPN
- pull pbp data from ESPN

The first (schedule data) can be used to gather the complete schedule for a season, returning it as a dataframe (such as all D1 games in a single season).

The second (pbp data) can be used to gather pbp data for a series of games, such as all games for a particular team in a particular season, or all pbp data for every game for every team in a season. A basic function is included in the file to gather, then bind together, the pbp data into a very large dataframe at the end.

### TODO
The data by gameId includes odds data from websites (moneyline, over/under, spread) but is not being gathered by the pbp function at this time.

Also, the gathering of schedules per year was not automated. This process suffered from errors and needs adequate error handling when a schedule comes back blank. (add tryCatch)
