require(TTR)
require(data.table)
require(dtplyr)


setwd("~/Desktop/Betting/Betting Data")
df <- fread("seasons_2005_2016.csv")

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

#calculate doubles
df$doubles <- ifelse(df$pts > 9, 1, 0) + ifelse(df$trb > 9, 1, 0) +
  ifelse(df$stl > 9, 1, 0) + ifelse(df$blk > 9, 1, 0) + ifelse(df$ast > 9 , 1, 0)


df$dk_fp <- df$pts + 0.5 * df$tp + 1.25 * df$trb + 1.5 * df$ast + dft$stl * 2 +
  df$blk * 2 - df$tov * .5 + ifelse(df$doubles > 1, 1.5, 
                                    ifelse(df$doubles > 2, 4.5, 0))
  


df <- df %>%
  arrange(player, team, date) %>%
  group_by(player, team) %>%
  mutate(
    dk_fp_pts = pts + 1.2 * trb + 1.5 * ast + 2 * blk + 2 * stl - tov,
    dk_fp_pts_1 = shift(dk_fp_pts, 1),
    dk_pts = pts + tp * .5,
    dk_trb = 1.25 * trb,
    dk_orb = 1.25 * orb,
    dk_drb = 1.25 * drb,
    dk_ast = 1.5 * ast,
    dk_blk = 2 * blk,
    dk_stl = 2 * stl,
    dk_tov = -0.5 * tov,
    dk_dbl = ifelse(doubles > 1, 1.5, 
                    ifelse(doubles > 2, 4.5, 0)),
    #last 3 games
    mp_3 = movAvg(mp, 3),
    dk_pts_3 = movAvg(dk_pts, 3),
    dk_trb_3 = movAvg(dk_trb, 3),
    dk_orb_3 = movAvg(dk_orb, 3),
    dk_drb_3 = movAvg(dk_drb, 3),
    dk_ast_3 = movAvg(dk_ast, 3),
    dk_blk_3 = movAvg(dk_blk, 3),
    dk_stl_3 = movAvg(dk_stl, 3),
    dk_tov_3 = movAvg(dk_tov, 3),
    dk_dbl_3 = movAvg(dk_dbl, 3),
    #last 5 games
    mp_5 = movAvg(mp, 5),
    dk_pts_5 = movAvg(dk_pts, 5),
    dk_trb_5 = movAvg(dk_trb, 5),
    dk_orb_5 = movAvg(dk_orb, 5),
    dk_drb_5 = movAvg(dk_drb, 5),
    dk_ast_5 = movAvg(dk_ast, 5),
    dk_blk_5 = movAvg(dk_blk, 5),
    dk_stl_5 = movAvg(dk_stl, 5),
    dk_tov_5 = movAvg(dk_tov, 5),
    dk_dbl_5 = movAvg(dk_dbl, 5),
    #last 10 games
    mp_10 = movAvg(mp, 10),
    dk_pts_10 = movAvg(dk_pts, 10),
    dk_trb_10 = movAvg(dk_trb, 10),
    dk_orb_10 = movAvg(dk_orb, 10),
    dk_drb_10 = movAvg(dk_drb, 10),
    dk_ast_10 = movAvg(dk_ast, 10),
    dk_blk_10 = movAvg(dk_blk, 10),
    dk_stl_10 = movAvg(dk_stl, 10),
    dk_tov_10 = movAvg(dk_tov, 10),
    dk_dbl_10 = movAvg(dk_dbl, 10),
    #last 20 games
    mp_20 = movAvg(mp, 20),
    dk_pts_20 = movAvg(dk_pts, 20),
    dk_trb_20 = movAvg(dk_trb, 20),
    dk_orb_20 = movAvg(dk_orb, 20),
    dk_drb_20 = movAvg(dk_drb, 20),
    dk_ast_20 = movAvg(dk_ast, 20),
    dk_blk_20 = movAvg(dk_blk, 20),
    dk_stl_20 = movAvg(dk_stl, 20),
    dk_tov_20 = movAvg(dk_tov, 20),
    dk_dbl_20 = movAvg(dk_dbl, 20)
  )
