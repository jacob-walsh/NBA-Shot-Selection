# NBA-Shot-Selection
It might seem like a foregone conclusion that if a defender is nowhere near the offensive
player, then the offensive player has a better chance of making the shot. We wanted to see if
the data backs this up and what else we could find out. The data we have includes a great deal
of shots, in fact, all shots taken from the 2014-2015 regular NBA season, 121,691 shots in total.
Our primary question of interest was to discover what factors have a large effect on whether or
not a shot will be made.
Shot logs data set was gathered from https://www.kaggle.com/erikbabb/d/dansbecker/nba-shot-logs/itm-6285-shot-prediction
Players data set was built from https://www.basketball-reference.com/leagues/NBA_2015_per_game.html and used to add additional variables
such as position played, age of player, games played, and minutes per game of player.

## Read in and Clean up
```
shots<-read.csv("shot_logs.csv", head=TRUE)
library(stringr)
shots$player_name<-str_replace_all(shots$player_name, "jimmer dredette", "jimmer fredette")#fixed misspellings
shots$player_name<-str_replace_all(shots$player_name, "mnta ellis", "monta ellis")
shots$player_name<-str_replace_all(shots$player_name, "nerles noel", "nerlens noel")
shots$player_name<-str_replace_all(shots$player_name, "beno urdih", "beno udrih")
shots$player_name<-str_replace_all(shots$player_name, "steve adams", "steven adams")
shots$player_name<-str_replace_all(shots$player_name, "dwayne wade", "dwyane wade")
shots$player_name<-str_replace_all(shots$player_name, "time hardaway jr", "tim hardaway")
shots$player_name<-str_replace_all(shots$player_name, "dirk nowtizski", "dirk nowitzki")
shots$player_name<-str_replace_all(shots$player_name, "jon ingles", "joe ingles")
shots$player_name<-str_replace_all(shots$player_name, "al farouq aminu", "al-farouq aminu")
shots$player_name<-str_replace_all(shots$player_name, "danilo gallinai", "danilo gallinari")

players<-read.csv("players.csv", na.strings=".", head=TRUE)
players<-as.data.frame(players)


shots$pos<-players$Pos[match(shots$player_name,players$Player)] #adding columns to shot_logs
shots$age<-players$Age[match(shots$player_name,players$Player)]
shots$GP<-players$G[match(shots$player_name,players$Player)]
shots$MPG<-players$MP[match(shots$player_name,players$Player)]
shots<-na.omit(shots)
shots2<-within(shots, rm(GAME_ID,MATCHUP,CLOSEST_DEFENDER, W,FINAL_MARGIN,CLOSEST_DEFENDER_PLAYER_ID,PTS,player_id, FGM))
shots2$GAME_CLOCK<-as.character(shots2$GAME_CLOCK)
shots2$GAME_CLOCK<-sapply(strsplit(shots2$GAME_CLOCK,":"),
                          function(x) {
                            x <- as.numeric(x)
                            x[1]+x[2]/60
                          }
                          )

write.csv(shots2,file="shot_logs2.csv")
```
