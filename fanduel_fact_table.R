require(TTR)
require(data.table)
require(dtplyr)


setwd("~/Desktop/Betting/Betting Data")
df <- fread("seasons_2005_2016.csv")
df$date <- as.Date(df$date)

movAvg <- function(x, n){
  out <- c()
  for(i in 1:length(x)){
    if(i < n){
      out <- c(out, mean(x[1:i], na.rm = T))
    } else {
      out <- c(out, mean(x[(i-n):i], na.rm = T))
    }
  }
  out
}

shift<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

df <- df %>%
  arrange(player, team, date) %>%
  group_by(player, team) %>%
  mutate(
    fd_fp_pts = pts + 1.2 * trb + 1.5 * ast + 2 * blk + 2 * stl - tov,
    fd_fp_pts_1 = shift(fd_fp_pts, 1),
    fd_pts = pts,
    fd_trb = 1.2 * trb,
    fd_orb = 1.2 * orb,
    fd_drb = 1.2 * drb,
    fd_ast = 1.5 * ast,
    fd_blk = 2 * blk,
    fd_stl = 2 * stl,
    fd_tov = -tov,
    #last 3 games
    mp_3 = movAvg(mp, 3),
    fd_pts_3 = movAvg(fd_pts, 3),
    fd_trb_3 = movAvg(fd_trb, 3),
    fd_orb_3 = movAvg(fd_orb, 3),
    fd_drb_3 = movAvg(fd_drb, 3),
    fd_ast_3 = movAvg(fd_ast, 3),
    fd_blk_3 = movAvg(fd_blk, 3),
    fd_stl_3 = movAvg(fd_stl, 3),
    fd_tov_3 = movAvg(fd_tov, 3),
    #last 5 games
    mp_5 = movAvg(mp, 5),
    fd_pts_5 = movAvg(fd_pts, 5),
    fd_trb_5 = movAvg(fd_trb, 5),
    fd_orb_5 = movAvg(fd_orb, 5),
    fd_drb_5 = movAvg(fd_drb, 5),
    fd_ast_5 = movAvg(fd_ast, 5),
    fd_blk_5 = movAvg(fd_blk, 5),
    fd_stl_5 = movAvg(fd_stl, 5),
    fd_tov_5 = movAvg(fd_tov, 5),
    #last 10 games
    mp_10 = movAvg(mp, 10),
    fd_pts_10 = movAvg(fd_pts, 10),
    fd_trb_10 = movAvg(fd_trb, 10),
    fd_orb_10 = movAvg(fd_orb, 10),
    fd_drb_10 = movAvg(fd_drb, 10),
    fd_ast_10 = movAvg(fd_ast, 10),
    fd_blk_10 = movAvg(fd_blk, 10),
    fd_stl_10 = movAvg(fd_stl, 10),
    fd_tov_10 = movAvg(fd_tov, 10),
    #last 20 games
    mp_20 = movAvg(mp, 20),
    fd_pts_20 = movAvg(fd_pts, 20),
    fd_trb_20 = movAvg(fd_trb, 20),
    fd_orb_20 = movAvg(fd_orb, 20),
    fd_drb_20 = movAvg(fd_drb, 20),
    fd_ast_20 = movAvg(fd_ast, 20),
    fd_blk_20 = movAvg(fd_blk, 20),
    fd_stl_20 = movAvg(fd_stl, 20),
    fd_tov_20 = movAvg(fd_tov, 20)
  )

