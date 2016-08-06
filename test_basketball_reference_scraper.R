# Test Functions ----------------------------------------------------------

#download the schedule and results for one season
schedules <- getSchedules(2015)

#download the schedule and results for mutliple seasons
schedules <- getSchedules(c(2015, 2016))

#download games from a single date
box_score <- getSeasonPlayerGameStats(2016, dates = as.Date("2015-11-24"))

#download games from multiple dates
dates <- c("2015-11-24", "2015-11-23")
dates <- as.Date(dates)
box_score <- getSeasonPlayerGameStats(2016, dates = dates)

#download games from multiple dates and a single team
dates <- c("2015-11-24", "2015-11-01")
dates <- as.Date(dates)
teams <- "BOS"
box_score <- getSeasonPlayerGameStats(2016, dates = dates, teams = teams)

#download games from multiple teams and multiple dates
dates <- c("2015-11-24", "2015-11-19")
dates <- as.Date(dates)
teams <- c("BOS", "LAC")
box_score <- getSeasonPlayerGameStats(2016, dates = dates, teams = teams)

#download all games in a season
box_score <- getSeasonPlayerGameStats(2016)

#download all games in multiple seasons
box_score <- getSeasonPlayerGameStats(c(2016, 2015))

