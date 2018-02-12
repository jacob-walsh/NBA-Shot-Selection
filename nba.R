
library(epitools)

shots<-read.csv("shot_logs2.csv", head=TRUE)

full.model<-glm(SHOT_RESULT~LOCATION+SHOT_NUMBER+PERIOD+SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+PTS_TYPE+CLOSE_DEF_DIST+pos+age+GP+MPG, family=binomial(link=logit), data=shots)
summary(full.model)  #  based on p-values, position, shot number, period, and games played don't seem as important, and we some some very large G^2.
anova(full.model)

##Exploring fewer predictors

#Reduced Logistic Model
reduced.model<-glm(SHOT_RESULT~LOCATION+SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+PTS_TYPE+CLOSE_DEF_DIST+age+GP+MPG, family=binomial(link=logit), data=shots)
summary(reduced.model)
anova(reduced.model)

#further reduced removing location and games played.
reduced.model2<-glm(SHOT_RESULT~SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+CLOSE_DEF_DIST+age+MPG, family=binomial(link=logit), data=shots)
summary(reduced.model2)
anova(reduced.model2)

#further reduced by the massive G^2 only, this model is almost as good as the previous without nearly as many predictors.
reduced.model3<-glm(SHOT_RESULT~SHOT_CLOCK+SHOT_DIST+CLOSE_DEF_DIST, family=binomial(link=logit), data=shots)
summary(reduced.model3)
anova(reduced.model3)

#shot distance seems to be of two categories, close shots and far shots. Splitting it at the free through line (15ft).
hist(shots$SHOT_DIST)
hist(SHOT_DIST[SHOT_DIST>=15])
hist(SHOT_DIST[SHOT_DIST<15])
shots$dist<-ifelse(SHOT_DIST<15, "<15ft",">=15ft")
tabdist<-table(shots$SHOT_RESULT, shots$dist)
epitab(tabdist)#odds of making a shot when further than average distance is about half of that when compared with shots of less than average distance.


#Just a thought about looking at subsets of shots based on the bimodal distribution of distances.
shots.near<-subset(shots,SHOT_DIST<15)
ggplot(data=shots.near, aes(x=SHOT_DIST, y=CLOSE_DEF_DIST)) + geom_point(aes(color=factor(SHOT_RESULT)))
ggplot(data=shots.near, aes(x=SHOT_CLOCK, y=CLOSE_DEF_DIST)) + geom_point(aes(color=factor(SHOT_RESULT)))
ggplot(data=shots, aes(x=SHOT_DIST, y=SHOT_CLOCK, color=SHOT_RESULT))+ geom_point()

ggplot(data=shots.near[shots.near$SHOT_CLOCK>18,], aes(x=SHOT_DIST, y=CLOSE_DEF_DIST, colour=SHOT_RESULT)) +geom_point() +ggtitle("Fast Break Shot Results") + labs(x="Distance to Basket", y="Distance to Closest Defender")
ggplot(data=shots.near[shots.near$SHOT_CLOCK<18,], aes(x=SHOT_DIST, y=CLOSE_DEF_DIST, colour=SHOT_RESULT)) +geom_point() +ggtitle("Fast Break Shot Results") + labs(x="Distance to Basket", y="Distance to Closest Defender")

ggplot(data=shots.near, aes(x=SHOT_CLOCK, y=CLOSE_DEF_DIST, color=SHOT_RESULT)) + geom_point() +geom_vline(xintercept=18) + geom_hline(yintercept=5)+ labs(x="Shot Clock", y="Distance to Closest Defender", title="Defender Distance vs Shot Clock")

ggplot(data=shots.near, aes(x=SHOT_CLOCK, y=SHOT_DIST, color=SHOT_RESULT)) + geom_point() +geom_vline(xintercept=18)+ labs(x="Shot Clock", y="Distance to Closest Defender", title="Defender Distance vs Shot Clock")

#The full model for the shots less than 15 feet is similar to what we have above with the whole data set except that number of dribbles and touch time and defender distance are much more significant and shot distance(understandably as that was the reason for the split) and shot clock are less significant.
near.model<-glm(SHOT_RESULT~LOCATION+SHOT_NUMBER+PERIOD+SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+PTS_TYPE+CLOSE_DEF_DIST+pos+age+GP+MPG, family=binomial(link=logit), data=shots.near)
summary(near.model)
anova(near.model)

#Reduced
near.model1<-glm(SHOT_RESULT~SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+CLOSE_DEF_DIST+age+MPG, family=binomial(link=logit), data=shots.near)
summary(near.model1)
anova(near.model1)

#Further Reduced
near.model2<-glm(SHOT_RESULT~SHOT_CLOCK+DRIBBLES+SHOT_DIST+CLOSE_DEF_DIST, family=binomial(link=logit), data=shots.near)
summary(near.model2)
anova(near.model2)

#exploring dribbles on near shots
hist(shots.near$DRIBBLES, breaks=25)
shots.near$drib<-ifelse(shots.near$DRIBBLES==0, "Assisted", "Not Assisted")
tab.near.drib<-table(shots.near$SHOT_RESULT, shots.near$drib)
tab.near.drib
epitab(tab.near.drib)  #odds of a shot being made based on distance when 0 dribbles are taken compared to 1+ dribbles.

hist(shots.near$CLOSE_DEF_DIST, breaks=20) #not sure what to do with defender distance yet.


shots.far<-subset(shots,SHOT_DIST>=15)
ggplot(data=shots.far, aes(x=SHOT_DIST, y=CLOSE_DEF_DIST)) + geom_point(aes(color=factor(SHOT_RESULT))) + geom_vline(xintercept=22)#22ft is the closest 3 point shot.
#The full model for shots that are 15 feet or further reveal a different set of predictors based on delta G^2 (p-value<0.01).
far.model<-glm(SHOT_RESULT~LOCATION+SHOT_NUMBER+PERIOD+SHOT_CLOCK+DRIBBLES+TOUCH_TIME+SHOT_DIST+PTS_TYPE+CLOSE_DEF_DIST+pos+age+GP+MPG, family=binomial(link=logit), data=shots.far)
summary(far.model)
anova(far.model)
#Reduced
far.model1<-glm(SHOT_RESULT~PERIOD+SHOT_CLOCK+SHOT_DIST+CLOSE_DEF_DIST+age, family=binomial(link=logit), data=shots.far)
summary(far.model1)
anova(far.model1)

#Dribbles wasn't significant in the full model, but it was in the near model.  Compare shots taken with 0 dribbles, compared with 1 or more dribbles you see a drastic difference.
#so I thought maybe we should include this, the odds of making a shot when 0 dribbles (getting the pass and shooting) are much higher than when dribbling and then shooting.

hist(shots$DRIBBLES)
shots.0drib<-subset(shots,shots$DRIBBLES==0)

hist(shots.0drib$SHOT_DIST) #distribution by distance seems to be approximately the same as the complete data set
tab.0drib<-table(shots.0drib$SHOT_RESULT, shots.0drib$dist)
epitab(tab.0drib)
#When considering the the odds of making a shot with 0 dribbles only there is a drastic improvement in the odds of a shot being a make (from 1.77 to 2.58 times as likely to make) - likely due to passes close to the basket like alley oops.

shots.1drib<-subset(shots,shots$DRIBBLES>=1)

tab.1drib<-table(shots.1drib$SHOT_RESULT, shots.1drib$dist)
tab.1drib
epitab(tab.1drib)
#A player taking 1 or more dribbles still has a higher odds of making a closer shot but the odds are now lower than the overall group.  It is clearly the 0 dribble group that impacts the odds of a shot make versus distance.

#investigating the association between position and shot result which showed some significance in the far model.

#I was curious about splitting by position, maybe it comes in handy. I don't have much evidence of it being worthwile so far though, other than the obvious that center's shoot less three point shots - see odds ratios.
type.pos<-table(shots$pos, shots$PTS_TYPE)
epitab(type.pos)


x<-split(shots.far, shots$pos)
shots.c<-x$C
shots.pf<-x$PF
shots.sg<-x$SG
shots.pg<-x$PG
shots.sf<-x$SF

# For far shots
hist(shots.c$SHOT_DIST)
hist(shots.pf$SHOT_DIST)

hist(shots.sf$SHOT_DIST)
hist(shots.sg$SHOT_DIST)
hist(shots.pg$SHOT_DIST)

#Not a very big difference. The positions have a similar odds increase over center at making the shot.

tabpos.make<-table(shots.far$pos, shots.far$SHOT_RESULT)
tabpos.make
oddsbypos<-epitab(tabpos.make)
oddsbypos


#Reference: https://www.kaggle.com/erikbabb/d/dansbecker/nba-shot-logs/itm-6285-shot-prediction

####################################################################################################




  Sys.setenv(MAKE = 'make -j 8')
  library(MASS)
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(magrittr)
  library(data.table)
  library(lubridate)
  library(RPostgreSQL)
  library(plotly)
  #library(rbokeh)
  library(jsonlite)
  library(htmltools)
  library(glmnet)
  library(epitools)
  library(broom)
  library(lme4)
  library(sjPlot)
  library(parallel)
  library(car)

################################################################################################

# Shot Dataset
d.shots <- 
  fread( "shot_logs.csv") %>% 
  .[ , Scored := case_when( SHOT_RESULT == 'made' ~ TRUE , 
                            SHOT_RESULT == 'missed' ~ FALSE ) ]

# Update Game clock to represent number of seconds (numeric)
# bug with using some lubridate functions inside data.table - using tmp var as workaround
tmp <- d.shots$GAME_CLOCK %>% ms %>% as.numeric()
d.shots[ , GAME_CLOCK := { tmp } ]
rm(tmp)

# Player Dataset : 
#   create player_name column to join with shot data
#   manually update player_name column for mismatches between two sources

d.players <- 
  fread('players.csv')%>%
  .[ , player_name := Player ] %>% 
  .[ Player == 'allen crabbe' , player_name := 'alan crabbe' ] %>% 
  .[ Player == 'steven adams' , player_name := 'steve adams' ] %>% 
  .[ Player == 'dwyane wade' , player_name := 'dwayne wade' ] %>% 
  .[ Player == 'danilo gallinari' , player_name := 'danilo gallinai' ] %>% 
  .[ Player == 'dirk nowitzki' , player_name := 'dirk nowtizski' ] %>% 
  .[ Player == 'tim hardaway' , player_name := 'time hardaway jr' ] %>% 
  .[ Player == 'beno udrih' , player_name := 'beno urdih' ] %>% 
  .[ Player == 'al-farouq aminu' , player_name := 'al farouq aminu' ] %>% 
  .[ Player == 'jj barea' , player_name := 'jose juan barea' ] %>% 
  .[ Player == 'monta ellis' , player_name := 'mnta ellis' ] %>% 
  .[ Player == 'nerlens noel' , player_name := 'nerles noel' ] %>% 
  .[ Player == 'jimmer fredette' , player_name := 'jimmer dredette' ] %>% 
  .[ Player == 'joe ingles' , player_name := 'jon ingles' ] %>% 
  .[ Player == 'j.j. hickson' , player_name := 'jj hickson' ]

# Confirm no players from the shots data set are missing from the player data
# setdiff( { d[ , unique( player_name ) ] } , { d.players[ , unique( player_name )]})

# Create Working data set
d <- 
  d.shots[ d.players , , on = c( 'player_name' ) , nomatch = 0 ]

tmp.2pt.dist.clusters <- 
  d[ PTS_TYPE == 2  , { kmeans( SHOT_DIST , 10 , iter.max = 20 ) } ] %$%
  centers[ cluster , 1] %>% 
  unname %>% 
  factor

levels(tmp.2pt.dist.clusters) <- 
  tmp.2pt.dist.clusters %>% 
  levels %>% 
  as.numeric %>% 
  signif( digits = 3 ) %>% 
  { paste0( '2 pt: ' , . ) }

tmp.3pt.dist.clusters <- 
  d[ PTS_TYPE == 3  , { kmeans( SHOT_DIST , 10 , iter.max = 20 ) } ] %$%
  centers[ cluster , 1] %>% 
  unname %>% 
  factor

levels(tmp.3pt.dist.clusters) <- 
  tmp.3pt.dist.clusters %>% 
  levels %>% 
  as.numeric %>% 
  signif( digits = 3 ) %>% 
  { paste0( '3 pt: ' , . ) }

tmp.dist.clusters <- 
  d[ , { kmeans( SHOT_DIST , 10 , iter.max = 20 ) } ] %$%
  centers[ cluster , 1] %>% 
  unname %>% 
  factor

levels(tmp.dist.clusters) <- 
  tmp.dist.clusters %>% 
  levels %>% 
  as.numeric %>% 
  signif( digits = 3 )

d %<>% 
  .[ , .( 
    Points = PTS , 
    Scored , 
    Game = as.factor(GAME_ID) , 
    Player = as.factor( player_name) , 
    Position = as.factor(Pos) , 
    ShotDistance = SHOT_DIST , 
    ClosestDefenderDistance = CLOSE_DEF_DIST , 
    ShotClock = SHOT_CLOCK , 
    Dribbles = DRIBBLES , 
    TouchTime = TOUCH_TIME , 
    GameClock = GAME_CLOCK ,
    ShotType = { as.factor( PTS_TYPE ) }
  ) ] %>% 
  .[ ShotType == "2" , ShotTypeDistFactor := tmp.2pt.dist.clusters ] %>% 
  .[ ShotType == "3" , ShotTypeDistFactor := tmp.3pt.dist.clusters ] %>% 
  .[ , ShotDistFactor := tmp.dist.clusters ] %>% 
  .[ ShotDistance < 13 , ShotDistanceClass := factor( 'Close (<13ft)')] %>% 
  .[ ShotDistance >= 13 , ShotDistanceClass := factor( 'Far (>=13ft)')] %>% 
  .[ , PositionDistClass := factor( paste0( Position , ' - ' , ShotDistanceClass ) ) ]
#Cleanup
rm( d.players , d.shots , tmp.dist.clusters , tmp.2pt.dist.clusters , tmp.3pt.dist.clusters )
## EDA Visuals ########################################################################

# Points per Shot vs Points
d %>%
  select( Player , Game , Position , Points , Scored ) %>% 
  group_by( Player , Game , Position ) %>% 
  summarise( PointsPerGame = sum( Points ) , PointsPerShot = mean( Points )  ) %>% {
    ggplot( . , aes( PointsPerGame , PointsPerShot , colour = Position ) ) + 
      geom_point() + 
      geom_density_2d( ) + 
      ggtitle( 'Points Per Shot vs Points Per Game' , subtitle = 'Per Game - By Position')  } %>% 
  ggplotly

# Baskets per Shot vs Baskets
d %>% 
  select( Player , Game , Position , Points , Scored ) %>% 
  group_by( Player , Game , Position ) %>% 
  summarise( BasketsPerGame = sum( Scored ) , BasketsPerShot = mean( Scored )  ) %>% {
    ggplot( . , aes( BasketsPerGame , BasketsPerShot , colour = Position ) ) + 
      geom_point() + 
      geom_density_2d( ) + 
      ggtitle( 'Baskets Per Shot vs Baskets Per Game' , subtitle = 'Per Game - By Position')  } %>% 
  ggplotly

# Baskets vs Shots
d %>% 
  select( Player , Game , Position , Points , Scored ) %>% 
  group_by( Player , Game , Position ) %>% 
  summarise( Baskets = sum( Scored ) , Shots = n()  ) %>% {
    ggplot( . , aes( Shots , Baskets , colour = Position ) ) + 
      geom_point() + 
      geom_smooth( method = 'loess' ) +
      ggtitle( 'Baskets vs Shots' , subtitle = 'Per Game - By Position')  } %>% 
  ggplotly

## Histograms
d %>% 
  .[ , .( Scored , ShotType , ShotDistance , ClosestDefenderDistance , ShotClock , Dribbles = as.numeric( Dribbles ) , TouchTime , GameClock ) ] %>% 
  .[ !is.na( ShotClock ) ] %>% 
  .[ TouchTime >= 0  ] %>% 
  melt( id.vars = c('Scored' , 'ShotType' ) ) %>% 
  .[ , variable := factor( variable ) ] %>% 
  { ggplot( . , aes( value , colour = ShotType , group = ShotType ) ) + facet_wrap(  ~ variable , scales = 'free' ) + geom_histogram( bins = 30 ) } %>% 
  ggplotly

d %>% 
  .[ , .( Scored , ShotType , ShotDistance , ClosestDefenderDistance , ShotClock , Dribbles = as.numeric( Dribbles ) , TouchTime , GameClock ) ] %>% 
  .[ !is.na( ShotClock ) ] %>% 
  .[ TouchTime >= 0  ] %>% 
  melt( id.vars = c('Scored' , 'ShotType' ) ) %>% 
  .[ , variable := factor( variable ) ] %>% 
  { ggplot( . , aes( value , colour = Scored , group = Scored ) ) + facet_wrap(  ~ variable , scales = 'free' ) + geom_histogram( bins = 30 ) } %>% 
  ggplotly

# Model ############################################################################################
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf

# glm 

cl <- makeCluster(getOption("cl.cores", { min( detectCores() , 10 ) } ) )
l.m.glm <- 
  parSapply( 
    cl , 
    d[ , unique( PositionDistClass ) ] , 
    function(x) {
      # prep each environment
      load( 'working.RData')
      eval( config )
      
      d[ PositionDistClass == x , ] %>%  
        .[ !is.na( ShotClock ) , ] %$%
        glm(
          Scored ~ ShotDistance * ClosestDefenderDistance ,
          family = binomial( link = 'logit' )
        )
    } , simplify = F , USE.NAMES = T )
names( l.m.glm ) <- { d[ , unique( PositionDistClass ) ] }
save.image( file =  'working.RData' )  
stopCluster( cl )
rm( cl )

# glm with random effects 
