######
## Stage 12: Preparing objects for the testing of forecasting performance: 
##    - Getting the adjusted shots/corners,
##    to be later fed to "Stage12p1_JQAS_Forecasting_Performance_Actual_Testing.R"
##
##  The adjusted values get saved into .Robj files as follows:
##    save(final.df, file="Corners_final.df.Robj")
##
## NOTE: If you want to switch from one offensive statistic to another, 
##       use "Ctrl+F" with "Match Case" turned out in order 
##       to replace all occurrences of "Corner" for "Shot" (), or vice versa.
######


library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we use models where "Minute.clean" was included
include.minute <- FALSE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

# load(file="gam_nb_obj_Corners.Robj")

load(file=paste0("gam_nb_obj",  
                 ifelse(remove.extra, "_NO_EXTRA_TIME", ""), 
                 "_Corners.Robj"))

final.df <- NULL


for (league in league.name){
  
  print(league)
  
  cat("\n")
  
  #######
  ### LOADING, SETTING UP DATA
  #######
  
  ## To be used for prediction (including everything, so that totals correspond to full data)
  our.df.pred <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))

  #######
  ### GETTING ADJUSTMENTS
  #######
  
  ### 1. SCORE DIFFERENTIAL
  
  # Getting the DIFFERENTIALS (log-scale "linear" effects)
  all.score.diffs <- c(min(our.df.pred$Score.Diff):max(our.df.pred$Score.Diff))
  log.score.diff.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                               newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                                                    Score.Diff = all.score.diffs))) -
    as.numeric(predict(gam.nb.obj[[league]],
                       newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                            Score.Diff = rep(0, length(all.score.diffs)))))
  
  names(log.score.diff.effects) <- all.score.diffs
  log.score.diff.effects
  
  # Multiplicative effects
  exp(log.score.diff.effects)
  
  # Doing the multiplication
  our.df.pred$Adj.Coef.ScoreDiff <-
    exp(-log.score.diff.effects)[sapply(our.df.pred$Score.Diff, function(x) which(names(log.score.diff.effects) == x))]
  
  
  
  
  
  ### 2. RED CARD DIFF
  
  all.redcard.diffs <- c(min(our.df.pred$RedCard.Diff):max(our.df.pred$RedCard.Diff))
  log.redcard.diff.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                                 newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                                                      RedCard.Diff = all.redcard.diffs))) -
    as.numeric(predict(gam.nb.obj[[league]],
                       newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                            RedCard.Diff = rep(0, length(all.redcard.diffs)))))
  
  names(log.redcard.diff.effects) <- all.redcard.diffs
  log.redcard.diff.effects
  
  
  
  # Multiplicative effects
  exp(log.redcard.diff.effects)
  
  # Doing the multiplication
  our.df.pred$Adj.Coef.RedCardDiff <-
    exp(-log.redcard.diff.effects)[sapply(our.df.pred$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]
  
  
  
  ### 3. HOME/AWAY FACTOR
  
  all.homeaway <- unique(our.df.pred$HomeAway)
  log.homeaway.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                             newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                                                  HomeAway = all.homeaway))) -
    as.numeric(predict(gam.nb.obj[[league]],
                       newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                            HomeAway = rep("Home", length(all.homeaway)))))
  
  names(log.homeaway.effects) <- all.homeaway
  log.homeaway.effects
  
  
  
  
  # Multiplicative effects
  exp(log.homeaway.effects)
  
  # Doing the multiplication
  our.df.pred$Adj.Coef.HomeAway <-
    exp(-log.homeaway.effects)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]
  
  our.df.pred <- our.df.pred %>%
    mutate(Corners.Adj = Corners*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway
           #,Corners.Adj.No.Min = Corners*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway
    )
  
  
  
  ### MAKING SURE TEAM NAMES ARE CORRECT (there's still some other leagues getting mistakenly mixed in here)
  team.names <- read.csv(paste0("Odds_Data/Final_matched_teams_", league, ".csv"))$Team_ESPN
  our.df.pred <- our.df.pred %>% filter(Team %in% team.names)
  
  
  
  final.df <- rbind(final.df,
    data.frame(League = league,
    our.df.pred %>%
    group_by(gameId, gamedDate, Team, HomeAway, season) %>%
    summarise(Corners = sum(Corners),
              Corners.Adj= sum(Corners.Adj),
              Score.Diff = head(Score.Diff, 1)
              # Corners.Adj.No.Min = sum(Corners.Adj.No.Min),
              # Corners.Diff.No.Min = Corners.Adj.No.Min - Corners
    )))
              
              
  
  save(final.df, file="Corners_final.df.Robj")
  

  cat("\n")
  cat("\n")
}