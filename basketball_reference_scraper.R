#These functions allow for the downloading of player stats from single games by 
#scraping basketball-referece. Functions are intended to be used to scrape a 
#single game season, or multiple season's of player data. Scraping this data 
#should provide users with the data required to build up to game summary stats, 
# player summary stats, etc.
#functionality is likely broken prior to the 2001 season

require(data.table)
require(lubridate)
require(rvest)
require(RCurl)
require(XML)
require(htmltab)
require(sqldf)
require(stringr)
require(dplyr)

# UDF ---------------------------------------------------------------------
getRefs <- function(pg){
  pg <- read_html(pg)
  refs <- pg %>%
    rvest::html_nodes(".small_text tr+ tr a") %>%
    rvest::html_text()
  refs
}

getSchedules <- function(seasons){
  #team_abbv is a dput from the team_translations.csv
  #update the team_translations file if new team translations are necessary
  team_abbv <- structure(list(team = c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", 
                                       "Charlotte Bobcats", "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", 
                                       "Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors", 
                                       "Houston Rockets", "Indiana Pacers", "Los Angeles Clippers", 
                                       "Los Angeles Lakers", "Memphis Grizzlies", "Miami Heat", "Milwaukee Bucks", 
                                       "Minnesota Timberwolves", "New Jersey Nets", "New Orleans Hornets", 
                                       "New Orleans Pelicans", "New Orleans/Oklahoma City Hornets", 
                                       "New York Knicks", "Oklahoma City Thunder", "Orlando Magic", 
                                       "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers", 
                                       "Sacramento Kings", "San Antonio Spurs", "Seattle SuperSonics", 
                                       "Toronto Raptors", "Utah Jazz", "Washington Wizards"), abbv = c("ATL", 
                                                                                                       "BOS", "BRK", "CHA", "CHO", "CHI", "CLE", "DAL", "DEN", "DET", 
                                                                                                       "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", 
                                                                                                       "NJN", "NOH", "NOP", "NOK", "NYK", "OKC", "ORL", "PHI", "PHO", 
                                                                                                       "POR", "SAC", "SAS", "SEA", "TOR", "UTA", "WAS")), .Names = c("team", 
                                                                                                                                                                     "abbv"), class = "data.frame", row.names = c(NA, -35L))
  
  schedules <- data.frame()
  for(i in seasons){
    season <- i
    url_base <- paste0("http://www.basketball-reference.com/leagues/NBA_", season, "_games.html")
    pg <- getURL(url_base)
    tbl <- readHTMLTable(pg, stringsAsFactors = F)
    regular_season <- tbl$games
    playoffs <- tbl$games_playoffs
    regular_season$regular_season <- T
    playoffs$regular_season <- F
    schedule <- rbind(regular_season, playoffs)
    schedules <- rbind(schedules, schedule)
  }
  colnames(schedules) <- c("date", "start", "boxscore", "visitor", "vpts", "home", "hpts", "ot", "notes", "regular_season")
  
  schedules <- sqldf("select
                     schedules.*,
                     team_abbv.abbv as home_abbv
                     from schedules 
                     join team_abbv on schedules.home = team_abbv.team")
  
  schedules <- sqldf("select
                     schedules.*,
                     team_abbv.abbv as visitor_abbv
                     from schedules 
                     join team_abbv on schedules.visitor = team_abbv.team")
  #fix dates and box scores
  schedules$date <- str_trim(str_sub(schedules$date, 5), side = "both")
  schedules$date <- as.Date(schedules$date, "%b %d, %Y")
  
  month <- as.character(month(schedules$date))
  day <- as.character(day(schedules$date))
  month <- ifelse(str_length(month) == 1, paste0("0", month), month)
  day <- ifelse(str_length(day) == 1, paste0("0", day), day)
  
  
  schedules$boxscore <- paste0("http://www.basketball-reference.com/boxscores/", 
                               year(schedules$date),
                               month,
                               day,
                               "0",
                               schedules$home_abbv,
                               ".html")
  schedules
}

getPlayerGameStats <- function(url, home, visitor, date){
  pg <- getURL(url)
  # pg <- readLines(tc <- textConnection(pg)); close(tc)
  tbl <- readHTMLTable(pg, stringsAsFactors = F)
  # ff <- tbl$four_factors
  visitor_basic <- tbl[[paste0(visitor, "_basic")]]
  visitor_basic$start <- NA
  visitor_basic$start[1:5] <- T
  visitor_basic$start[6:nrow(visitor_basic)] <- F
  visitor_advanced <- tbl[[paste0(visitor, "_advanced")]]
  home_basic <- tbl[[paste0(home, "_basic")]]
  home_basic$start <- NA
  home_basic$start[1:5] <- T
  home_basic$start[6:nrow(visitor_basic)] <- F
  home_advanced <- tbl[[paste0(home, "_advanced")]]
  
  #bind data.frames
  visitor_stats <- cbind(visitor_basic, visitor_advanced[3:ncol(visitor_advanced)])
  home_stats <- cbind(home_basic, home_advanced[3:ncol(home_advanced)])
  
  
  #mark whether home or away
  visitor_stats$opp <- home
  visitor_stats$home <- F
  visitor_stats$team <- visitor
  home_stats$opp <- visitor
  home_stats$home <- T
  home_stats$team <- home
  
  df_out <- rbind(visitor_stats, home_stats)
  
  #filter out basd values 
  df_out <- df_out %>%
    mutate(
      dnp = ifelse(MP == "Did Not Play", T, F),
      suspended = ifelse(MP == "Player Suspended", T, F),
      MP = str_trim(MP, side = "both"),
      MP = ifelse(MP == "Did Not Play", "00:00", MP),
      MP = ifelse(MP == "Player Suspended", "00:00", MP)
    ) %>%
    filter(Starters != "Reserves")
  
  #fix minutes played
  mp <- str_split_fixed(df_out$MP, ":", 2)
  mp[,2] <- as.numeric(mp[,2])/60
  df_out$MP <- as.numeric(mp[,1]) + as.numeric(mp[,2])
  df_out[is.na(df_out)] <- 0
  df_out[df_out == ""] <- 0
  df_out$date <- date
  
  if(ncol(df_out) == 41){
    #basketball reference doesn't always supply plus minus for some reason
    #set column names
    cnames <- c("player", "mp", "fg", "fga", "fg_perc", "tp", "tpa", "tp_perc", "ft",
                "fta", "ft_perc", "orb", "drb", "trb", "ast", "stl", "blk", "tov",
                "pf", "pts", "plus_minus", "ts_perc", "efg_perc", "tpar", "ftar",
                "orb_perc", "drb_perc", "trb_perc", "ast_perc", "stl_perc", "blk_perc",
                "tov_perc", "usg_perc", "ortg", "drtg", "opp", "home", "team","dnp", 
                "suspended", "date")
    colnames(df_out) <- cnames
    #convert numeric fields to numeric
    numerics <- c("mp", "fg", "fga", "fg_perc", "tp", "tpa", "tp_perc", "ft",
                  "fta", "ft_perc", "orb", "drb", "trb", "ast", "stl", "blk", "tov",
                  "pf", "pts", "plus_minus", "ts_perc", "efg_perc", "tpar", "ftar",
                  "orb_perc", "drb_perc", "trb_perc", "ast_perc", "stl_perc", "blk_perc",
                  "tov_perc", "usg_perc", "ortg", "drtg")
    df_out$plus_minus_found <- T
  } else {
    #set column names
    cnames <- c("player", "mp", "fg", "fga", "fg_perc", "tp", "tpa", "tp_perc", "ft",
                "fta", "ft_perc", "orb", "drb", "trb", "ast", "stl", "blk", "tov",
                "pf", "pts", "ts_perc", "efg_perc", "tpar", "ftar",
                "orb_perc", "drb_perc", "trb_perc", "ast_perc", "stl_perc", "blk_perc",
                "tov_perc", "usg_perc", "ortg", "drtg", "opp", "home", "team","dnp", 
                "suspended", "date")
    colnames(df_out) <- cnames
    #convert numeric fields to numeric
    numerics <- c("mp", "fg", "fga", "fg_perc", "tp", "tpa", "tp_perc", "ft",
                  "fta", "ft_perc", "orb", "drb", "trb", "ast", "stl", "blk", "tov",
                  "pf", "pts", "ts_perc", "efg_perc", "tpar", "ftar",
                  "orb_perc", "drb_perc", "trb_perc", "ast_perc", "stl_perc", "blk_perc",
                  "tov_perc", "usg_perc", "ortg", "drtg")
    df_out$plus_minus_found <- F
  }
  
  for(i in numerics) {
    df_out[,i] <- as.numeric(df_out[,i])
  }
  refs <- getRefs(pg)
  df_out$ref_1 <- refs[1]
  df_out$ref_2 <- refs[2]
  df_out$ref_3 <- refs[3]
  
  df_out
}

getSeasonPlayerGameStats <- function(seasons, verbose = T, dates = F, teams = F, sleep = 1){
  schedules <- getSchedules(seasons)
  
  #filter schedules to pick up games on a specific date
  if(dates[1] != F){
    schedules <- schedules[schedules$date %in% dates, ]
  }
  
  if(teams[1] != F){
    schedules <- schedules[schedules$home_abbv %in% teams | schedules$visitor_abbv %in% teams, ]
  }
  
  
  df_out <- data.table()
  for(i in 1:nrow(schedules)){
    #print games being downloaded as progress check
    if(verbose == T){
      flush.console()
      print(paste0(i, " of ", nrow(schedules), ": ", schedules$home_abbv[i], " vs. ",
                   schedules$visitor_abbv[i], " on ", schedules$date[i]))
      #print(schedules$boxscore[i])
    }
    nrow_start <- nrow(df_out)
    try({
      df_sub <- getPlayerGameStats(schedules$boxscore[i],
                                   schedules$home_abbv[i],
                                   schedules$visitor_abbv[i],
                                   schedules$date[i])
      df_out <- rbind(df_sub, df_out, fill = T)
    })
    
    #indicate if download failed (should wrap in trycatch)
    if(nrow(df_out) == nrow_start){
      flush.console()
      print(paste0("Download of: ", schedules$home_abbv[i], " vs. ",
            schedules$visitor_abbv[i], " on ", schedules$date[i], " failed!!!!!"))
    }
    Sys.sleep(sleep)
  }
  
  df_out
}
