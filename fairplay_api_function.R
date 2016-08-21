require(data.table)
require(lubridate)
require(rvest)
require(RCurl)
require(XML)
require(htmltab)
require(sqldf)
require(stringr)
require(dplyr)
require(jsonlite)
require(tidyjson)

setwd("~/Desktop")
url <- "http://31.172.83.181:8080/free/markets/%7B%22Cat%22:14,%20%22NoZombie%22:true%7D"
# url <- "http://31.172.83.181:8080/free/markets/%7B%22OnlyActive%22:true,%20%22NoZombie%22:true,%20%22ToID%22:10000%7D"
i <- 26

getSpreads <- function(url){
  pg <- read_html(url)
  pg <- as.character(pg)
  start <- str_locate(pg, "<html><body><p>")
  end <- str_locate(pg, "</p></body></html>\\n")
  pg <- str_sub(pg, 1, end[1]-1)
  pg <- str_sub(pg, start[2]+1)
  tbl <- fromJSON(pg)
  tbl$name_away <- NA
  tbl$name_home <- NA
  tbl$vol_matched_away <- NA
  tbl$vol_matched_home <- NA
  tbl$on <- NA
  tbl$against <- NA
  tbl$on_vol <- NA
  tbl$against_vol <- NA
  for(i in 1:nrow(tbl)){
    df_sub <- as.data.frame(tbl$Ru[i])
    tbl$name_away[i] <- df_sub$Name[1]
    tbl$name_home[i] <- df_sub$Name[2]
    tbl$vol_matched_away[i] <- df_sub$VolMatched[1]
    tbl$vol_matched_home[i] <- df_sub$VolMatched[2]
    start <- as.data.frame(str_locate_all(tbl$OrdBStr[i], "\\:\\[\\[")[[1]])
    end <- as.data.frame(str_locate_all(tbl$OrdBStr[i], "\\]\\]")[[1]])
    away <- str_sub(tbl$OrdBStr[i], start[1,2]+1, end[1,1]-1) %>%
      str_split_fixed(pattern = ",", n = 2)
    if(!is.na(str_locate(away[1,2], "\\]")[1])){
      n <- str_locate(away[1,2], "\\]")[1,1] -1
      away[1,2] <- str_sub(away[1,2], 1, n)
    }
    
    home <- str_sub(tbl$OrdBStr[i], start[2,2]+1, end[2,1]-1)%>%
      str_split_fixed(pattern = ",", n = 2)
    if(!is.na(str_locate(home[1,2], "\\]")[1])){
      n <- str_locate(home[1,2], "\\]")[1,1] -1
      home[1,2] <- str_sub(home[1,2], 1, n)
    }
    
    tbl$on[i] <- away[1,1]
    tbl$on_vol[i] <- away[1,2]
    tbl$against[i] <- home[1,1]
    tbl$against_vol[i] <- home[1,2]
  }
  tbl$on_vol <- as.numeric(tbl$on_vol)
  tbl$on <- as.numeric(tbl$on)
  tbl$against <- as.numeric(tbl$against)
  tbl$against_vol <- as.numeric(tbl$against_vol)
  tbl
}

tbl <- getSpreads(url)



1 / (tbl$on[1] + 1)
1 / (tbl$against[1] + 1)

