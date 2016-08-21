  require(data.table)
  require(lubridate)
  require(rvest)
  require(RCurl)
  require(XML)
  require(htmltab)
  require(sqldf)
  require(caret)
  require(quantreg)
  require(stringr)
  require(dplyr)
  
  
  # UDV ---------------------------------------------------------------------
  season <- 2017
  
  
  # Main --------------------------------------------------------------------
  
  setwd("~/Desktop/Github/basketball_reference_scraper")
  source("basketball_reference_scraper.r")
  
  
  setwd("~/Desktop/Betting/Betting Data")
  df <- readRDS("basketball_data.rds")
  df_start <- nrow(df)
  
  yday <- Sys.Date() - 1
  try({
    df_sub <- getSeasonPlayerGameStats(seasons = season,
                                       verbose = T,
                                       dates = yday)
  })
  
  if(exists("df_sub")){
    df <- rbind(df, df_sub)
    if(nrow(df) > df_start){
      saveRDS(df, file = "basketball_data.rds")
    }
  }
