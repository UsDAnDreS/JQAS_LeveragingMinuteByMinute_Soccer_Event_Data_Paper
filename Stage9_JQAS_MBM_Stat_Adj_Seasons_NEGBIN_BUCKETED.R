#####
## Stage 9: Helping yield Tables 3 & 4 of the main manuscript
##          (games with some of the strongest SEASON-LONG positive/negative shifts in each respective leagues,
##          first table is for shots, second - for corner kicks).
##    
##  This script allows to calculate Top-10 (for positive) and Bottom-10 (for negative)
##  season-long shifts in each league. That is controlled by "top_or_bottom" variable.
##
## NOTE: If you want to switch from one offensive statistic to another, 
##       use "Ctrl+F" with "Match Case" turned out in order 
##       to replace all occurrences of "Corner" for "Shot" (), or vice versa.
####


library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)

# Whether to show Top-10 or Bottom-10
top.or.bottom <- "Top"

## We're bucketing the extreme categories here
bucketed <- TRUE

## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we use models where "Minute.clean" was included
include.minute <- FALSE

## If we want to still keep 1st half extra time
keep.1st.half.extra <- TRUE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

# load(file="gam_nb_obj_Shots.Robj")

# load(file=paste0("gam_nb_obj",  
#                  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), 
#                  "_Shots.Robj"))


for (league in league.name){
  
  print(league)
  
  cat("\n")
  
  
  hey <- load(file=paste0("Fitted_Objects/GAM_W_BUCKETED_EXTREME_CATEGORIES_gam_nb_obj_Team_AND_Season_Mixed_Eff_",
                   ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                   ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                   "_", league,
                   "_Corners.Robj"))
 # print(summary(ns.obj[[league]]))
  
  if (hey == "gam.nb.obj") ns.obj <- gam.nb.obj

  
  
  #######
  ### LOADING, SETTING UP DATA
  #######
  
  # Including ALL THE MINUTES (so that the actual game totals are correct)
  our.df.pred <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  
  
  ## REMOVING NA betting data
  our.df.pred <- our.df.pred %>% filter(!is.na(Weighted.Win.Prob))
  
  ## Remove extra time data from first half (so anything with half_id=1, minute>45)
  ## => TO AVOID "POINT MASS"/"OUT-OF-ORDER MINUTE" ISSUES...
  ##
  ## !!! BUUUT... the >90 IS ALSO AN ISSUE... data is way too sporadic over there !!!
  # our.df.pred <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
  # print(dim(our.df.pred))
  
  
  
  ############
  ############
  
  our.df.pred  <- our.df.pred  %>%
    mutate(abs.Score.Diff = abs(Score.Diff),
           abs.RedCard.Diff = abs(RedCard.Diff))
  
  our.df.pred  <- our.df.pred  %>%
    mutate(Period.ID = group_indices(our.df.pred, .dots=c("ID", "Minute.clean", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
    arrange(Period.ID) %>%
    mutate(Period.ID = factor(Period.ID))
  
  ## Creating "TeamSeason" combo indicator, and in case we use Half_ID => converting Minutes into "minutes in the half" 
  ## (so always starting from 0 or 1 when the half starts, rahter than starting from 46 for 2nd half)
  
  if (keep.1st.half.extra){
    
    our.df.pred <- our.df.pred %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID), Team=factor(Team), half_id = factor(half_id),
                                          season = factor(season),
                                          TeamSeason = factor(paste0(Team, season)),
                                          Minute.clean = ifelse(half_id == 2, 
                                                                Minute.clean - 45,
                                                                Minute.clean))
  } else {
    
    our.df.pred <- our.df.pred %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID), Team=factor(Team), half_id = factor(half_id),
                                          season=factor(season),
                                          TeamSeason = factor(paste0(Team, season)))
    
  }
  
  
  
  if (bucketed){
    
    print("BUCKETED")
    
    #########
    ## BUCKETING:
    ##    * Putting 3+ scores as "3"
    ##    * Putting 2+ red card diffs as "2"
    ##    * Putting 45+ minutes as "45"
    ########
    
    our.df.pred <- our.df.pred %>%
      mutate(Score.Diff = ifelse(abs(Score.Diff) >= 3, sign(Score.Diff)*3, Score.Diff),
             RedCard.Diff = ifelse(abs(RedCard.Diff) >= 2, sign(RedCard.Diff)*2, RedCard.Diff),
             Minute.clean = ifelse(Minute.clean >= 45, 45, Minute.clean))
  }
  
  
  
  
  #######
  ### GETTING ADJUSTMENTS
  #######
  
  
  

  
  #########
  ## CHECK THE CODE EXAMPLES for "predict.gam"{mgcv}
  #########
  
  # Getting the DIFFERENTIALS (log-scale "linear" effects)
  all.score.diffs <- c(min(our.df.pred$Score.Diff):max(our.df.pred$Score.Diff))
  
  ## Removing "Team" and "season" from coefficients
  ## [!!! ACTUALLY, THAT PART NOT REALLY NEEDED !!!]
  incl.ind <- which(!grepl("\\(Team\\)", names(coef(ns.obj[[league]]))) & 
                      !grepl("\\(season\\)", names(coef(ns.obj[[league]]))))
  
  log.score.diff.effects <- predict(ns.obj[[league]],
                                    newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", half_id=1, RedCard.Diff =0,
                                                         Team = ns.obj[[league]]$model$Team[1], season = ns.obj[[league]]$model$season[1],
                                                         Score.Diff = all.score.diffs)) -
    predict(ns.obj[[league]],
            newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", half_id=1, RedCard.Diff =0,
                                 Team = ns.obj[[league]]$model$Team[1], season = ns.obj[[league]]$model$season[1],
                                 Score.Diff = rep(0, length(all.score.diffs))))
  
  names(log.score.diff.effects) <- all.score.diffs
  log.score.diff.effects
  
  
  ## Calculating std. errors of all differences between predictions
  all.diff.SE <- c()
  for (score.diff in all.score.diffs){
    
    Xp <-  predict(ns.obj[[league]],
                   newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", half_id = 1, RedCard.Diff =0,
                                        Team = ns.obj[[league]]$model$Team[1], season = ns.obj[[league]]$model$season[1],
                                        Score.Diff = c(score.diff,0)),
                   type="lpmatrix")
    
    a <- c(1,-1)
    Xs <- t(a) %*% Xp
    all.diff.SE <- c(all.diff.SE,
                     sqrt(Xs %*% ns.obj[[league]]$Vp %*% t(Xs)))
  }
  
  all.diff.SE
  
  
  
  ### Getting the min-max of the difference via +-1.96xSE approach
  log.score.diff.effects.min <- log.score.diff.effects - qnorm(0.975)*all.diff.SE
  log.score.diff.effects.max <- log.score.diff.effects + qnorm(0.975)*all.diff.SE
  
  
  # Multiplicative effects
  exp(log.score.diff.effects)
  # Their 95% CIs:
  cbind(exp(log.score.diff.effects.min), 
        exp(log.score.diff.effects.max))
  
  # Doing the multiplication
  our.df.pred$Adj.Coef.ScoreDiff <- 
    exp(-log.score.diff.effects)[sapply(our.df.pred$Score.Diff, function(x) which(names(log.score.diff.effects) == x))]
  
  our.df.pred$Adj.Coef.ScoreDiff.Max <- 
    exp(-log.score.diff.effects.min)[sapply(our.df.pred$Score.Diff, function(x) which(names(log.score.diff.effects) == x))]
  
  our.df.pred$Adj.Coef.ScoreDiff.Min <- 
    exp(-log.score.diff.effects.max)[sapply(our.df.pred$Score.Diff, function(x) which(names(log.score.diff.effects) == x))]
  
  
  
  ### 2. RED CARD DIFF
  
  all.redcard.diffs <- c(min(our.df.pred$RedCard.Diff):max(our.df.pred$RedCard.Diff))
  
  
  log.redcard.diff.effects <- predict(ns.obj[[league]],
                                      newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", half_id=1, Score.Diff =0,
                                                           Team = ns.obj[[league]]$model$Team[1], season = ns.obj[[league]]$model$season[1],
                                                           RedCard.Diff = all.redcard.diffs)) -
    predict(ns.obj[[league]],
            newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", half_id=1, Score.Diff =0,
                                 Team = ns.obj[[league]]$model$Team[1], season = ns.obj[[league]]$model$season[1],
                                 RedCard.Diff = rep(0, length(all.redcard.diffs))))
  
  names(log.redcard.diff.effects) <- all.redcard.diffs
  log.redcard.diff.effects
  
  ## Calculating std. errors of all differences between predictions
  all.diff.SE <- c()
  for (redcard.diff in all.redcard.diffs){
    
    Xp <-  predict(ns.obj[[league]],
                   newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, half_id = 1, HomeAway = "Home", Score.Diff =0,
                                        Team = ns.obj[[league]]$model$Team[1], season = ns.obj[[league]]$model$season[1],
                                        RedCard.Diff = c(redcard.diff,0)),
                   type="lpmatrix")
    
    a <- c(1,-1)
    Xs <- t(a) %*% Xp
    all.diff.SE <- c(all.diff.SE,
                     sqrt(Xs %*% ns.obj[[league]]$Vp %*% t(Xs)))
  }
  
  
  ### Getting the min-max of the difference via +-1.96xSE approach
  log.redcard.diff.effects.min <- log.redcard.diff.effects - qnorm(0.975)*all.diff.SE
  log.redcard.diff.effects.max <- log.redcard.diff.effects + qnorm(0.975)*all.diff.SE
  
  
  
  
  
  # Multiplicative effects
  exp(log.redcard.diff.effects)
  # Their 95% CIs:
  cbind(exp(log.redcard.diff.effects.min), 
        exp(log.redcard.diff.effects.max))
  
  # Doing the multiplication
  our.df.pred$Adj.Coef.RedCardDiff <- 
    exp(-log.redcard.diff.effects)[sapply(our.df.pred$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]
  
  our.df.pred$Adj.Coef.RedCardDiff.Max <- 
    exp(-log.redcard.diff.effects.min)[sapply(our.df.pred$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]
  
  our.df.pred$Adj.Coef.RedCardDiff.Min <- 
    exp(-log.redcard.diff.effects.max)[sapply(our.df.pred$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]
  

  ### 3. HOME/AWAY FACTOR
  
  all.homeaway <- unique(our.df.pred$HomeAway)
  log.homeaway.effects <- 
    predict(ns.obj[[league]],
            newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0,  half_id=1, Score.Diff = 0, RedCard.Diff =0,
                                 Team = ns.obj[[league]]$model$Team[1], season = ns.obj[[league]]$model$season[1],
                                 HomeAway = all.homeaway)) -
    predict(ns.obj[[league]],
            newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, half_id=1, Score.Diff = 0, RedCard.Diff =0,
                                 Team = ns.obj[[league]]$model$Team[1], season = ns.obj[[league]]$model$season[1],
                                 HomeAway = rep("Home", length(all.homeaway))))
  
  
  
  names(log.homeaway.effects) <- all.homeaway
  log.homeaway.effects
  
  
  ## Calculating std. errors of all differences between predictions
  all.diff.SE <- c()
  for (homeaway in all.homeaway){
    
    Xp <-  predict(ns.obj[[league]],
                   newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, half_id=1, Score.Diff = 0, RedCard.Diff =0,
                                        Team = ns.obj[[league]]$model$Team[1], season = ns.obj[[league]]$model$season[1],
                                        HomeAway = c(homeaway,"Home")),
                   type="lpmatrix")
    
    a <- c(1,-1)
    Xs <- t(a) %*% Xp
    all.diff.SE <- c(all.diff.SE,
                     sqrt(Xs %*% ns.obj[[league]]$Vp %*% t(Xs)))
  }
  
  
  ### Getting the min-max of the difference via +-1.96xSE approach
  log.homeaway.effects.min <- log.homeaway.effects - qnorm(0.975)*all.diff.SE
  log.homeaway.effects.max <- log.homeaway.effects + qnorm(0.975)*all.diff.SE
  
  
  
  
  
  # Multiplicative effects
  exp(log.homeaway.effects)
  # Their 95% CIs:
  cbind(exp(log.homeaway.effects.min), 
        exp(log.homeaway.effects.max))
  
  # Doing the multiplication
  our.df.pred$Adj.Coef.HomeAway <- 
    exp(-log.homeaway.effects)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]
  
  our.df.pred$Adj.Coef.HomeAway.Max <- 
    exp(-log.homeaway.effects.min)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]
  
  our.df.pred$Adj.Coef.HomeAway.Min <- 
    exp(-log.homeaway.effects.max)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]
  

  
  #######
  ### 4. EVERYTHING TOGETHER (getting one CI for the entire adjustment... not the same as multiplying all the other adjustments together)
  ######### 
  
  # !!!!!!!!!!!!!!
  
  all.diffs <- expand.grid(
    Score.Diff = all.score.diffs,
    RedCard.Diff = all.redcard.diffs,
    HomeAway = all.homeaway)
  all.diffs
  
  log.all.effects <- 
    predict(ns.obj[[league]],
            newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0,  half_id=1, # Score.Diff = 0, RedCard.Diff =0,
                                 Team = ns.obj[[league]]$model$Team[1], season = ns.obj[[league]]$model$season[1],
                                 all.diffs)) -
    predict(ns.obj[[league]],
            newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, half_id=1, # Score.Diff = 0, RedCard.Diff =0,
                                 Team = ns.obj[[league]]$model$Team[1], season = ns.obj[[league]]$model$season[1],
                                 all.diffs %>% mutate(Score.Diff = 0, RedCard.Diff = 0, HomeAway = "Home")))
  
  
  
  # names(log.all.effects) <- all.homeaway
  #  log.homeaway.effects
  
  
  ## Calculating std. errors of all differences between predictions
  all.diff.SE <- c()
  
  for (j in 1:nrow(all.diffs)){
    
    Xp <-  predict(ns.obj[[league]],
                   newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, half_id=1, # Score.Diff = 0, RedCard.Diff =0,
                                        Team = ns.obj[[league]]$model$Team[1], season = ns.obj[[league]]$model$season[1],
                                        rbind(all.diffs[j,],
                                              data.frame(Score.Diff = 0, RedCard.Diff = 0, HomeAway = "Home"))),
                   type="lpmatrix")
    
    a <- c(1,-1)
    Xs <- t(a) %*% Xp
    all.diff.SE <- c(all.diff.SE,
                     sqrt(Xs %*% ns.obj[[league]]$Vp %*% t(Xs)))
  }
  
  
  ### Getting the min-max of the difference via +-1.96xSE approach
  log.all.effects.min <- log.all.effects - qnorm(0.975)*all.diff.SE
  log.all.effects.max <- log.all.effects + qnorm(0.975)*all.diff.SE
  
  
  
  
  
  # Multiplicative effects
  exp(log.all.effects)
  # Their 95% CIs:
  cbind(exp(log.all.effects.min), 
        exp(log.all.effects.max))
  
  # Doing the multiplication
  
  
  
  # Indices of which triplet does each row from original data correspond to
  all.diffs$Index <-c(1:nrow(all.diffs))
  our.ind <- our.df.pred %>% left_join(all.diffs) %>% .[["Index"]]
  
  our.df.pred$Adj.Coef.All <- 
    exp(-log.all.effects)[our.ind]
  
  our.df.pred$Adj.Coef.All.Max <- 
    exp(-log.all.effects.min)[our.ind]
  
  our.df.pred$Adj.Coef.All.Min <- 
    exp(-log.all.effects.max)[our.ind]
  
 
  
  
  our.df.pred <- our.df.pred %>%
    mutate(Corners.Adj = Corners*Adj.Coef.All,
           Corners.Adj.Min = Corners*Adj.Coef.All.Min,
           Corners.Adj.Max = Corners*Adj.Coef.All.Max
    )
  
  
  
  
  
  ### MAKING SURE TEAM NAMES ARE CORRECT (there's still some other leagues getting mistakenly mixed in here)
  team.names <- read.csv(paste0("Odds_Data/Final_matched_teams_", league, ".csv"))$Team_ESPN
  our.df.pred <- our.df.pred %>% filter(Team %in% team.names)
  
  
  # ### 
  # cat("\n")
  # print(paste0("Top-10 single game shifts, ", ifelse(include.minute, "WITH MINUTE:", "NO MINUTE:")))
  # 
  
  if (top.or.bottom == "Top"){
    
  final.df <- our.df.pred %>%
    group_by(gameId, Team, season) %>%
    summarise(Corners = sum(Corners),
              Corners.Adj= sum(Corners.Adj),
              Corners.Adj.Min = sum(Corners.Adj.Min),
              Corners.Adj.Max = sum(Corners.Adj.Max),
              Corners.Diff = Corners.Adj - Corners,
              # Corners.Adj.No.Min = sum(Corners.Adj.No.Min),
              # Corners.Diff.No.Min = Corners.Adj.No.Min - Corners
    ) %>%
    ungroup() %>%
    group_by(Team, season) %>%
    summarise(Corners.Per.Game = round(mean(Corners),1),
              Corners.Adj.Per.Game = round(mean(Corners.Adj),1),
              Corners.Adj.Min.Per.Game = round(mean(Corners.Adj.Min),1),
              Corners.Adj.Max.Per.Game = round(mean(Corners.Adj.Max),1),
              Corners.Diff.Per.Game = round(Corners.Adj.Per.Game - Corners.Per.Game,1),
              n.games = length(unique(gameId))) %>%
    ungroup() %>%
    group_by(season) %>%
    mutate(Corners.Per.Game.Full = paste0(round(Corners.Per.Game,1), " (\\#", rank(-Corners.Per.Game, ties="first"), ")"),
           Corners.Adj.Per.Game.Full = paste0(round(Corners.Adj.Per.Game,1), ", [", Corners.Adj.Min.Per.Game, ", ", Corners.Adj.Max.Per.Game,  "]", " (\\#", rank(-Corners.Adj.Per.Game, ties="first"), ")")) %>%
    dplyr::select(Team, season, n.games, 
                  Corners.Per.Game.Full, Corners.Adj.Per.Game.Full, Corners.Diff.Per.Game) %>%
    arrange(desc(Corners.Diff.Per.Game))
  
  } else {
    
    final.df <- our.df.pred %>%
      group_by(gameId, Team, season) %>%
      summarise(Corners = sum(Corners),
                Corners.Adj= sum(Corners.Adj),
                Corners.Adj.Min = sum(Corners.Adj.Min),
                Corners.Adj.Max = sum(Corners.Adj.Max),
                Corners.Diff = Corners.Adj - Corners,
                # Corners.Adj.No.Min = sum(Corners.Adj.No.Min),
                # Corners.Diff.No.Min = Corners.Adj.No.Min - Corners
      ) %>%
      ungroup() %>%
      group_by(Team, season) %>%
      summarise(Corners.Per.Game = round(mean(Corners),1),
                Corners.Adj.Per.Game = round(mean(Corners.Adj),1),
                Corners.Adj.Min.Per.Game = round(mean(Corners.Adj.Min),1),
                Corners.Adj.Max.Per.Game = round(mean(Corners.Adj.Max),1),
                Corners.Diff.Per.Game = round(Corners.Adj.Per.Game - Corners.Per.Game,1),
                n.games = length(unique(gameId))) %>%
      ungroup() %>%
      group_by(season) %>%
      mutate(Corners.Per.Game.Full = paste0(round(Corners.Per.Game,1), " (\\#", rank(-Corners.Per.Game, ties="first"), ")"),
             Corners.Adj.Per.Game.Full = paste0(round(Corners.Adj.Per.Game,1), ", [", Corners.Adj.Min.Per.Game, ", ", Corners.Adj.Max.Per.Game,  "]", " (\\#", rank(-Corners.Adj.Per.Game, ties="first"), ")")) %>%
      dplyr::select(Team, season, n.games, 
                    Corners.Per.Game.Full, Corners.Adj.Per.Game.Full, Corners.Diff.Per.Game) %>%
      arrange(Corners.Diff.Per.Game)
    
  }
  
  
  ## Getting Corners in the lead, Corners shorthanded
  
  scorediff.diff.ranks <- our.df.pred %>%
    # filter(Score.Diff > 0) %>%
    group_by(gameId, Team, season) %>%
    summarise(Corners.Lead = sum(Corners[Score.Diff > 0]),
              Minutes.Lead = sum(Score.Diff > 0),
              Corners.Trail = sum(Corners[Score.Diff < 0]),
              Minutes.Trail = sum(Score.Diff < 0)) %>%
    ungroup() %>%
    group_by(Team, season) %>%
    summarise(Corners.Lead.Per.Game = mean(Corners.Lead),
              Minutes.Lead.Per.Game = mean(Minutes.Lead),
              Corners.Trail.Per.Game = mean(Corners.Trail),
              Minutes.Trail.Per.Game = mean(Minutes.Trail),
              n.games = length(unique(gameId))) %>%
    ungroup() %>%
    #filter(season==2020)
    group_by(season) %>%
    mutate(Corners.Lead.Per.Game.Full = paste0(round(Corners.Lead.Per.Game,1), " (\\#", rank(-Corners.Lead.Per.Game, ties="first"), ")"),
           Minutes.Lead.Per.Game.Full = paste0(round(Minutes.Lead.Per.Game,1), " (\\#", rank(-Minutes.Lead.Per.Game, ties="first"), ")"),
           Corners.Trail.Per.Game.Full = paste0(round(Corners.Trail.Per.Game,1), " (\\#", rank(-Corners.Trail.Per.Game, ties="first"), ")"),
           Minutes.Trail.Per.Game.Full = paste0(round(Minutes.Trail.Per.Game,1), " (\\#", rank(-Minutes.Trail.Per.Game, ties="first"), ")")) %>%
    dplyr::select(Team, season, n.games, 
                  Corners.Lead.Per.Game.Full, Minutes.Lead.Per.Game.Full,
                  Corners.Trail.Per.Game.Full, Minutes.Trail.Per.Game.Full)
  # arrange(desc(Corners.Lead.Per.Game))
  
  
  redcard.diff.ranks <- our.df.pred %>%
    #  filter(RedCard.Diff > 0) %>%
    group_by(gameId, Team, season) %>%
    summarise(Corners.UpMen = sum(Corners[RedCard.Diff < 0]),
              Minutes.UpMen = sum(RedCard.Diff < 0),
              Corners.DownMen = sum(Corners[RedCard.Diff > 0]),
              Minutes.DownMen = sum(RedCard.Diff > 0)) %>%
    ungroup() %>%
    group_by(Team, season) %>%
    summarise(Corners.UpMen.Per.Game = mean(Corners.UpMen),
              Minutes.UpMen.Per.Game = mean(Minutes.UpMen),
              Corners.DownMen.Per.Game = mean(Corners.DownMen),
              Minutes.DownMen.Per.Game = mean(Minutes.DownMen),
              n.games = length(unique(gameId))) %>%
    ungroup() %>%
    #filter(season==2020)
    group_by(season) %>%
    mutate(Corners.UpMen.Per.Game.Full = paste0(round(Corners.UpMen.Per.Game,1), " (\\#", rank(-Corners.UpMen.Per.Game, ties="first"), ")"),
           Minutes.UpMen.Per.Game.Full = paste0(round(Minutes.UpMen.Per.Game,1), " (\\#", rank(-Minutes.UpMen.Per.Game, ties="first"), ")"),
           Corners.DownMen.Per.Game.Full = paste0(round(Corners.DownMen.Per.Game,1), " (\\#", rank(-Corners.DownMen.Per.Game, ties="first"), ")"),
           Minutes.DownMen.Per.Game.Full = paste0(round(Minutes.DownMen.Per.Game,1), " (\\#", rank(-Minutes.DownMen.Per.Game, ties="first"), ")")
           ) %>%
    dplyr::select(Team, season, n.games, 
                  Corners.UpMen.Per.Game.Full, Minutes.UpMen.Per.Game.Full,
                  Corners.DownMen.Per.Game.Full, Minutes.DownMen.Per.Game.Full)
  #  arrange(desc(Corners.DownMen.Per.Game))
  
  
  options(tibble.width=Inf)
  print(head(final.df %>% left_join(scorediff.diff.ranks) %>% left_join(redcard.diff.ranks) %>%
               filter(n.games > 30) 
            # %>% filter(Corners.Diff.Per.Game > 0)
             , 10))
  
  print(final.df %>%
          filter(n.games > 30) %>%
          left_join(scorediff.diff.ranks) %>% left_join(redcard.diff.ranks) %>%
         #  filter(Corners.Diff.Per.Game > 0) %>%
          mutate(latex.entry = paste(league, Team, season, Corners.Per.Game.Full, Corners.Adj.Per.Game.Full,
                                     Corners.Lead.Per.Game.Full, Corners.Trail.Per.Game.Full,
                                     Corners.UpMen.Per.Game.Full, Corners.DownMen.Per.Game.Full, sep = " & "))%>%
          .[["latex.entry"]] %>%
          head(10))
  
  # print(head(final.df, 50))
  
  # our.df %>%
  #   filter(gameId == head(final.df$gameId, 1), Team == head(final.df$Team, 1))
  
  
  
  
  cat("\n")
  cat("\n")
}



######
### SHOTS
###   TOP-10
######

# [1] "Bundesliga"
# 
# [1] "Bundesliga & Bayern Munich & 2023 & 18.4 (\\#1) & 21.1, [20.7, 21.4] (\\#1) & 9.6 (\\#1) & 2.2 (\\#17) & 0.4 (\\#2) & 0 (\\#16)"    
# [2] "Bundesliga & Bayern Munich & 2015 & 17.1 (\\#1) & 19.6, [19.3, 19.9] (\\#1) & 8.8 (\\#1) & 1.5 (\\#17) & 0.5 (\\#2) & 0.3 (\\#7)"   
# [3] "Bundesliga & Bayern Munich & 2020 & 18 (\\#1) & 20.5, [20.1, 20.8] (\\#1) & 10.2 (\\#1) & 1.8 (\\#18) & 0.3 (\\#6) & 0.6 (\\#7)"    
# [4] "Bundesliga & Bayern Munich & 2022 & 19.5 (\\#1) & 21.9, [21.6, 22.3] (\\#1) & 9.2 (\\#1) & 2.4 (\\#17) & 0.1 (\\#6) & 0 (\\#12)"    
# [5] "Bundesliga & Bayern Munich & 2021 & 17 (\\#1) & 19.3, [19, 19.7] (\\#1) & 8.3 (\\#1) & 3.8 (\\#7) & 0.6 (\\#1) & 0 (\\#14)"         
# [6] "Bundesliga & Bayern Munich & 2014 & 18.5 (\\#1) & 20.6, [20.3, 20.9] (\\#1) & 9.3 (\\#1) & 1.8 (\\#18) & 0 (\\#16) & 0.2 (\\#12)"   
# [7] "Bundesliga & Borussia Dortmund & 2013 & 15.9 (\\#3) & 18, [17.7, 18.3] (\\#2) & 7.7 (\\#2) & 2.8 (\\#13) & 0.9 (\\#1) & 0.4 (\\#10)"
# [8] "Bundesliga & Bayern Munich & 2012 & 15.7 (\\#2) & 17.7, [17.4, 18] (\\#2) & 6.8 (\\#2) & 2.5 (\\#16) & 0.5 (\\#3) & 0.4 (\\#10)"    
# [9] "Bundesliga & Bayern Munich & 2013 & 17 (\\#1) & 18.9, [18.6, 19.2] (\\#1) & 8.8 (\\#1) & 1.6 (\\#18) & 0 (\\#15) & 0.6 (\\#5)"      
# [10] "Bundesliga & Bayern Munich & 2019 & 18.5 (\\#1) & 20.4, [20.1, 20.7] (\\#1) & 9.5 (\\#1) & 2 (\\#18) & 0.1 (\\#12) & 0.9 (\\#3)"    
# 
# 
# 
# [1] "SerieA"
#
# [1] "SerieA & Napoli & 2017 & 17.6 (\\#2) & 20.3, [20, 20.6] (\\#1) & 8.1 (\\#1) & 2.5 (\\#18) & 0.2 (\\#7) & 0.7 (\\#5)"           
# [2] "SerieA & AC Milan & 2012 & 15.8 (\\#3) & 18.1, [17.8, 18.3] (\\#3) & 6.4 (\\#2) & 2 (\\#19) & 0.5 (\\#4) & 0.2 (\\#15)"        
# [3] "SerieA & Atalanta & 2021 & 16.1 (\\#2) & 18.3, [18.1, 18.5] (\\#1) & 7.6 (\\#1) & 2.3 (\\#20) & 0.2 (\\#11) & 0.3 (\\#12)"     
# [4] "SerieA & Napoli & 2018 & 17.1 (\\#1) & 19.3, [19.1, 19.6] (\\#1) & 6.8 (\\#1) & 2.4 (\\#18) & 0.2 (\\#12) & 0 (\\#20)"         
# [5] "SerieA & AS Roma & 2017 & 17.6 (\\#1) & 19.7, [19.5, 19.9] (\\#2) & 7.8 (\\#2) & 2.6 (\\#17) & 0 (\\#17) & 0.1 (\\#20)"        
# [6] "SerieA & Internazionale & 2009 & 14.7 (\\#7) & 16.7, [16.5, 16.9] (\\#3) & 6.1 (\\#1) & 0.6 (\\#20) & 0.5 (\\#8) & 0.5 (\\#13)"
# [7] "SerieA & Internazionale & 2022 & 17.5 (\\#1) & 19.5, [19.3, 19.8] (\\#1) & 7.5 (\\#1) & 2.8 (\\#18) & 0 (\\#19) & 0.5 (\\#8)"  
# [8] "SerieA & Internazionale & 2020 & 16.1 (\\#6) & 18, [17.8, 18.3] (\\#5) & 6.9 (\\#1) & 2.5 (\\#19) & 0.2 (\\#13) & 0.6 (\\#8)"  
# [9] "SerieA & Juventus & 2014 & 15.5 (\\#3) & 17.4, [17.2, 17.6] (\\#2) & 6.5 (\\#1) & 0.9 (\\#19) & 0.2 (\\#12) & 0.7 (\\#11)"     
# [10] "SerieA & Juventus & 2017 & 15.2 (\\#6) & 17.1, [16.9, 17.4] (\\#4) & 6.6 (\\#3) & 1.8 (\\#20) & 0 (\\#20) & 0.6 (\\#8)"        
# 
# 
# 
# [1] "LaLiga"
# 
# [1] "LaLiga & Real Madrid & 2011 & 19.9 (\\#2) & 24.5, [24.1, 24.9] (\\#2) & 10.2 (\\#2) & 1.9 (\\#19) & 0.7 (\\#2) & 0.4 (\\#15)"
# [2] "LaLiga & Real Madrid & 2015 & 17.7 (\\#1) & 21.5, [21.2, 21.9] (\\#1) & 8.9 (\\#1) & 2 (\\#18) & 0.3 (\\#9) & 0 (\\#20)"     
# [3] "LaLiga & Barcelona & 2010 & 15.9 (\\#3) & 19.5, [19.1, 19.8] (\\#2) & 8.6 (\\#2) & 0.7 (\\#20) & 0.6 (\\#3) & 0.4 (\\#17)"   
# [4] "LaLiga & Real Madrid & 2010 & 21.6 (\\#1) & 25, [24.6, 25.4] (\\#1) & 9.9 (\\#1) & 2.9 (\\#14) & 0.6 (\\#4) & 1.4 (\\#4)"    
# [5] "LaLiga & Barcelona & 2009 & 18.3 (\\#1) & 21.6, [21.2, 22] (\\#1) & 8.8 (\\#1) & 2.6 (\\#17) & 0.4 (\\#12) & 1.2 (\\#8)"     
# [6] "LaLiga & Real Madrid & 2014 & 19.3 (\\#1) & 22.6, [22.3, 23] (\\#1) & 9.1 (\\#1) & 2.8 (\\#15) & 0.1 (\\#16) & 0.5 (\\#15)"  
# [7] "LaLiga & Barcelona & 2015 & 16.1 (\\#2) & 19.3, [19, 19.7] (\\#2) & 8.2 (\\#2) & 1.5 (\\#19) & 0.3 (\\#11) & 1 (\\#3)"       
# [8] "LaLiga & Real Madrid & 2013 & 18.1 (\\#1) & 21.3, [20.9, 21.6] (\\#1) & 8.9 (\\#1) & 2.6 (\\#17) & 0.8 (\\#3) & 0.7 (\\#12)" 
# [9] "LaLiga & Real Madrid & 2017 & 17.3 (\\#1) & 20.5, [20.2, 20.8] (\\#1) & 8.2 (\\#1) & 1.8 (\\#18) & 0.5 (\\#3) & 0.4 (\\#11)" 
# [10] "LaLiga & Barcelona & 2014 & 16.6 (\\#2) & 19.5, [19.2, 19.8] (\\#2) & 7.8 (\\#2) & 2.3 (\\#17) & 0.1 (\\#18) & 0.4 (\\#17)"  
# 
# 
# 
# [1] "Ligue1"
#
# 
# [1] "Ligue1 & Paris Saint-Germain & 2018 & 16.1 (\\#2) & 19.3, [18.9, 19.6] (\\#1) & 8.2 (\\#1) & 1.7 (\\#20) & 0.8 (\\#1) & 0.5 (\\#10)" 
# [2] "Ligue1 & Paris Saint-Germain & 2016 & 15.1 (\\#1) & 17.5, [17.2, 17.8] (\\#1) & 7.7 (\\#1) & 0.7 (\\#20) & 0 (\\#19) & 0.6 (\\#10)"  
# [3] "Ligue1 & AS Monaco & 2017 & 14.5 (\\#3) & 16.8, [16.6, 17.1] (\\#2) & 7.7 (\\#1) & 1.5 (\\#19) & 0.1 (\\#18) & 0.7 (\\#10)"          
# [4] "Ligue1 & Paris Saint-Germain & 2017 & 15.1 (\\#2) & 17.4, [17.1, 17.6] (\\#1) & 6.9 (\\#2) & 1.3 (\\#20) & 0.2 (\\#16) & 0 (\\#20)"  
# [5] "Ligue1 & Paris Saint-Germain & 2019 & 14.5 (\\#2) & 16.6, [16.3, 16.8] (\\#2) & 6.7 (\\#1) & 1.7 (\\#20) & 0.1 (\\#16) & 0.6 (\\#13)"
# [6] "Ligue1 & Paris Saint-Germain & 2023 & 14.9 (\\#1) & 17, [16.8, 17.3] (\\#1) & 7.3 (\\#1) & 2.2 (\\#19) & 0.7 (\\#2) & 0.9 (\\#8)"    
# [7] "Ligue1 & Stade Rennais & 2022 & 14.8 (\\#1) & 16.7, [16.5, 17] (\\#1) & 6.4 (\\#1) & 2.4 (\\#19) & 0.2 (\\#13) & 0.9 (\\#9)"         
# [8] "Ligue1 & Paris Saint-Germain & 2022 & 14.7 (\\#2) & 16.5, [16.2, 16.7] (\\#2) & 5.9 (\\#2) & 2.4 (\\#18) & 0.2 (\\#15) & 0.8 (\\#11)"
# [9] "Ligue1 & Lens & 2023 & 13.6 (\\#5) & 15.3, [15.1, 15.5] (\\#3) & 4.5 (\\#5) & 2 (\\#20) & 0.7 (\\#1) & 0.2 (\\#16)"                  
# [10] "Ligue1 & Lyon & 2011 & 14.6 (\\#3) & 16.3, [16.1, 16.5] (\\#1) & 5.1 (\\#1) & 2.1 (\\#18) & 0.5 (\\#3) & 0.1 (\\#18)"                
# 
# 
# 
# [1] "PremierLeague"
# 
# [1] "PremierLeague & Manchester City & 2019 & 17.9 (\\#1) & 20.7, [20.4, 21] (\\#1) & 11.2 (\\#1) & 0.8 (\\#20) & 0 (\\#17) & 0.6 (\\#7)"    
# [2] "PremierLeague & Manchester City & 2020 & 19.4 (\\#1) & 22.2, [21.8, 22.5] (\\#1) & 8.8 (\\#1) & 3.7 (\\#8) & 0.3 (\\#4) & 0.3 (\\#11)"  
# [3] "PremierLeague & Manchester City & 2018 & 17.4 (\\#1) & 20.1, [19.8, 20.4] (\\#1) & 8.7 (\\#1) & 1 (\\#20) & 0.3 (\\#5) & 0.2 (\\#12)"   
# [4] "PremierLeague & Manchester City & 2022 & 18.7 (\\#2) & 21.1, [20.8, 21.5] (\\#1) & 9.6 (\\#1) & 2.3 (\\#17) & 0.2 (\\#6) & 1.4 (\\#1)"  
# [5] "PremierLeague & Tottenham Hotspur & 2017 & 17.3 (\\#1) & 19.6, [19.3, 19.9] (\\#1) & 7.6 (\\#1) & 1.7 (\\#20) & 0 (\\#19) & 0.3 (\\#10)"
# [6] "PremierLeague & Manchester City & 2023 & 15.7 (\\#3) & 17.9, [17.6, 18.1] (\\#1) & 7.1 (\\#1) & 1.5 (\\#20) & 0.3 (\\#3) & 0.3 (\\#8)"  
# [7] "PremierLeague & Liverpool & 2019 & 14.9 (\\#3) & 17, [16.7, 17.2] (\\#3) & 7.6 (\\#2) & 0.8 (\\#19) & 0.1 (\\#10) & 0.2 (\\#13)"        
# [8] "PremierLeague & Manchester City & 2014 & 17.4 (\\#2) & 19.5, [19.2, 19.8] (\\#1) & 8.9 (\\#1) & 2.1 (\\#19) & 0.2 (\\#9) & 1.1 (\\#3)"  
# [9] "PremierLeague & Liverpool & 2022 & 18.9 (\\#1) & 20.9, [20.6, 21.1] (\\#2) & 9.6 (\\#2) & 1.8 (\\#19) & 0 (\\#17) & 0.9 (\\#4)"         
# [10] "PremierLeague & Liverpool & 2014 & 16.8 (\\#3) & 18.7, [18.5, 19] (\\#3) & 8.3 (\\#2) & 2.4 (\\#16) & 0 (\\#16) & 1 (\\#5)"  




######
### SHOTS
###   BOTTOM-10
######

# [1] "Bundesliga"
# 
# 
# [1] "Bundesliga & Hannover 96 & 2016 & 11.2 (\\#13) & 10.9, [10.8, 11.1] (\\#16) & 0.9 (\\#18) & 6 (\\#1) & 0 (\\#15) & 0.4 (\\#6)"               
# [2] "Bundesliga & FC Cologne & 2018 & 11.4 (\\#12) & 11.2, [11, 11.3] (\\#15) & 1.2 (\\#18) & 6 (\\#1) & 0 (\\#15) & 0.6 (\\#8)"                  
# [3] "Bundesliga & Fortuna Dsseldorf & 2020 & 12.6 (\\#11) & 12.4, [12.2, 12.5] (\\#15) & 1.7 (\\#17) & 4.5 (\\#7) & 0 (\\#18) & 1.3 (\\#2)"       
# [4] "Bundesliga & Kaiserslautern & 2012 & 12.1 (\\#14) & 11.9, [11.8, 12] (\\#15) & 1.2 (\\#18) & 5 (\\#2) & 0.1 (\\#12) & 0.8 (\\#4)"            
# [5] "Bundesliga & Schalke 04 & 2021 & 8.9 (\\#18) & 8.7, [8.6, 8.8] (\\#18) & 0.7 (\\#18) & 4.6 (\\#3) & 0.1 (\\#8) & 0.3 (\\#8)"                 
# [6] "Bundesliga & SpVgg Greuther Furth & 2022 & 9.2 (\\#18) & 9, [8.9, 9.1] (\\#18) & 0.9 (\\#18) & 3.9 (\\#11) & 0 (\\#17) & 0.4 (\\#4)"         
# [7] "Bundesliga & TSV Eintracht Braunschweig & 2014 & 12.1 (\\#13) & 11.9, [11.8, 12] (\\#14) & 1.3 (\\#18) & 5.1 (\\#2) & 0 (\\#18) & 0.6 (\\#6)"
# [8] "Bundesliga & Fortuna Dsseldorf & 2013 & 9.2 (\\#18) & 9.1, [9, 9.3] (\\#18) & 1.7 (\\#17) & 3.4 (\\#10) & 0 (\\#16) & 0.9 (\\#4)"            
# [9] "Bundesliga & Hertha Berlin & 2023 & 11.3 (\\#15) & 11.2, [11.1, 11.4] (\\#17) & 1.6 (\\#17) & 5 (\\#3) & 0.1 (\\#12) & 0.2 (\\#13)"          
# [10] "Bundesliga & Schalke 04 & 2023 & 12.4 (\\#6) & 12.3, [12.2, 12.5] (\\#11) & 1.8 (\\#16) & 5.2 (\\#2) & 0.1 (\\#16) & 0.6 (\\#8)"             
# 
# 
# [1] "SerieA"
# 
# 
# [1] "SerieA & US Pescara & 2017 & 11.5 (\\#12) & 10.3, [10.2, 10.5] (\\#17) & 0.9 (\\#20) & 6.4 (\\#1) & 0.2 (\\#10) & 1.2 (\\#1)"  
# [2] "SerieA & Benevento & 2021 & 11.1 (\\#13) & 10, [9.8, 10.1] (\\#16) & 1.7 (\\#15) & 6.4 (\\#1) & 0.2 (\\#10) & 1.7 (\\#1)"      
# [3] "SerieA & US Pescara & 2013 & 10.5 (\\#18) & 9.4, [9.3, 9.5] (\\#19) & 0.4 (\\#20) & 5.7 (\\#1) & 0.3 (\\#15) & 0.8 (\\#7)"     
# [4] "SerieA & Livorno & 2014 & 12.6 (\\#15) & 11.6, [11.5, 11.8] (\\#17) & 1.4 (\\#18) & 7 (\\#1) & 0 (\\#20) & 0.3 (\\#17)"        
# [5] "SerieA & Novara & 2012 & 12.1 (\\#13) & 11.1, [10.9, 11.2] (\\#18) & 1 (\\#19) & 5.9 (\\#1) & 0.1 (\\#15) & 0.9 (\\#2)"        
# [6] "SerieA & Bologna & 2011 & 11.7 (\\#19) & 10.9, [10.8, 11.1] (\\#20) & 1.7 (\\#17) & 4.8 (\\#4) & 0 (\\#19) & 0.7 (\\#5)"       
# [7] "SerieA & Bologna & 2014 & 13 (\\#9) & 12.2, [12.1, 12.4] (\\#15) & 1.2 (\\#19) & 5.9 (\\#2) & 0.2 (\\#14) & 0.8 (\\#9)"        
# [8] "SerieA & Frosinone & 2019 & 11.9 (\\#17) & 11.1, [11, 11.2] (\\#18) & 0.9 (\\#19) & 6.2 (\\#1) & 0.2 (\\#9) & 0.4 (\\#11)"     
# [9] "SerieA & Salernitana & 2022 & 11.3 (\\#15) & 10.5, [10.4, 10.6] (\\#15) & 1.2 (\\#19) & 5.4 (\\#1) & 0.1 (\\#10) & 0.4 (\\#10)"
# [10] "SerieA & Sampdoria & 2020 & 14.6 (\\#8) & 13.8, [13.7, 14] (\\#11) & 1.9 (\\#18) & 6.6 (\\#1) & 0.3 (\\#7) & 1.7 (\\#3)"       
# 
# 
# [1] "LaLiga"

# 
# [1] "LaLiga & Las Palmas & 2018 & 10.7 (\\#15) & 10.4, [10.3, 10.5] (\\#20) & 1.1 (\\#17) & 4.6 (\\#3) & 0.1 (\\#17) & 0.7 (\\#6)"     
# [2] "LaLiga & Elche & 2023 & 10.6 (\\#17) & 10.4, [10.3, 10.6] (\\#18) & 1.1 (\\#19) & 4.6 (\\#3) & 0.2 (\\#9) & 1.4 (\\#2)"           
# [3] "LaLiga & Osasuna & 2017 & 10.2 (\\#16) & 10, [9.9, 10.1] (\\#17) & 0.9 (\\#20) & 4.9 (\\#2) & 0.1 (\\#17) & 0.1 (\\#16)"          
# [4] "LaLiga & Granada & 2017 & 9.8 (\\#18) & 9.7, [9.5, 9.8] (\\#19) & 1.1 (\\#19) & 4.7 (\\#4) & 0.2 (\\#12) & 0.4 (\\#10)"           
# [5] "LaLiga & Mlaga & 2017 & 12.8 (\\#4) & 12.7, [12.5, 12.9] (\\#9) & 2 (\\#12) & 5.6 (\\#1) & 0 (\\#20) & 1 (\\#4)"                  
# [6] "LaLiga & Rayo Vallecano & 2012 & 12.7 (\\#9) & 12.6, [12.4, 12.8] (\\#11) & 2.4 (\\#12) & 5.5 (\\#1) & 0.1 (\\#16) & 1.6 (\\#3)"  
# [7] "LaLiga & Real Betis & 2019 & 11.6 (\\#12) & 11.5, [11.4, 11.6] (\\#15) & 1.2 (\\#19) & 3.9 (\\#9) & 0 (\\#18) & 1 (\\#1)"         
# [8] "LaLiga & Real Valladolid & 2019 & 10.8 (\\#17) & 10.7, [10.6, 10.8] (\\#19) & 1.2 (\\#20) & 4.7 (\\#4) & 0 (\\#20) & 0.5 (\\#6)"  
# [9] "LaLiga & Recreativo Huelva & 2009 & 10.7 (\\#20) & 10.6, [10.4, 10.7] (\\#20) & 1.5 (\\#20) & 4.1 (\\#7) & 0 (\\#20) & 1.4 (\\#4)"
# [10] "LaLiga & Almera & 2015 & 10.2 (\\#18) & 10.2, [10.1, 10.4] (\\#18) & 1.3 (\\#18) & 4.2 (\\#5) & 0.3 (\\#10) & 0.7 (\\#7)"         
# 
# 
# 
# [1] "Ligue1"
# 
# [1] "Ligue1 & Angers & 2023 & 9.7 (\\#19) & 9.1, [9, 9.3] (\\#19) & 0.6 (\\#19) & 5.2 (\\#1) & 0.3 (\\#6) & 0.8 (\\#10)"        
# [2] "Ligue1 & Lorient & 2022 & 11.7 (\\#10) & 11.1, [10.9, 11.2] (\\#15) & 1.1 (\\#20) & 4.3 (\\#5) & 0.2 (\\#16) & 1.6 (\\#1)" 
# [3] "Ligue1 & Nimes & 2021 & 10.1 (\\#17) & 9.5, [9.3, 9.6] (\\#19) & 1 (\\#19) & 5 (\\#1) & 0.1 (\\#14) & 1.5 (\\#2)"          
# [4] "Ligue1 & Bordeaux & 2022 & 11.8 (\\#9) & 11.3, [11.1, 11.5] (\\#11) & 1.6 (\\#16) & 4.7 (\\#2) & 0.3 (\\#11) & 1.3 (\\#6)" 
# [5] "Ligue1 & Caen & 2017 & 11.9 (\\#8) & 11.4, [11.3, 11.6] (\\#13) & 1.7 (\\#19) & 5.3 (\\#2) & 0.2 (\\#11) & 1.7 (\\#1)"     
# [6] "Ligue1 & Brest & 2013 & 10.9 (\\#15) & 10.5, [10.4, 10.7] (\\#17) & 1.3 (\\#20) & 5.6 (\\#1) & 0.4 (\\#8) & 0.5 (\\#14)"   
# [7] "Ligue1 & Toulouse & 2014 & 11.4 (\\#12) & 11, [10.9, 11.2] (\\#14) & 1.6 (\\#19) & 4.2 (\\#3) & 0.1 (\\#15) & 1.5 (\\#1)"  
# [8] "Ligue1 & Toulouse & 2018 & 11.2 (\\#17) & 10.8, [10.6, 10.9] (\\#18) & 1.6 (\\#17) & 4.2 (\\#7) & 0.2 (\\#16) & 1.9 (\\#2)"
# [9] "Ligue1 & AC Ajaccio & 2023 & 8.2 (\\#20) & 7.9, [7.8, 8] (\\#20) & 0.4 (\\#20) & 4.4 (\\#4) & 0.2 (\\#13) & 0.2 (\\#15)"   
# [10] "Ligue1 & Dijon FCO & 2019 & 11.9 (\\#11) & 11.6, [11.5, 11.8] (\\#12) & 1.2 (\\#18) & 5.3 (\\#1) & 0.5 (\\#2) & 0.9 (\\#6)"
# 
# 
# 
# [1] "PremierLeague"
# 
# [1] "PremierLeague & AFC Bournemouth & 2019 & 11.7 (\\#12) & 11.3, [11.2, 11.5] (\\#16) & 2.3 (\\#13) & 4.5 (\\#5) & 0 (\\#18) & 1.2 (\\#2)"    
# [2] "PremierLeague & Southampton & 2023 & 10.8 (\\#17) & 10.4, [10.3, 10.5] (\\#18) & 0.9 (\\#20) & 5.3 (\\#1) & 0 (\\#19) & 0.6 (\\#3)"        
# [3] "PremierLeague & Cardiff City & 2019 & 10.8 (\\#17) & 10.5, [10.4, 10.6] (\\#17) & 1.3 (\\#18) & 4.6 (\\#4) & 0 (\\#15) & 0.8 (\\#3)"       
# [4] "PremierLeague & Norwich City & 2014 & 12.3 (\\#13) & 12, [11.8, 12.1] (\\#13) & 1.3 (\\#19) & 5 (\\#4) & 0 (\\#17) & 0.4 (\\#9)"           
# [5] "PremierLeague & Norwich City & 2020 & 10.6 (\\#14) & 10.3, [10.2, 10.5] (\\#17) & 1.3 (\\#20) & 5 (\\#1) & 0.1 (\\#15) & 0.4 (\\#9)"       
# [6] "PremierLeague & Portsmouth & 2010 & 13.9 (\\#11) & 13.6, [13.5, 13.8] (\\#13) & 1.4 (\\#19) & 5.6 (\\#1) & 0.2 (\\#12) & 0.9 (\\#5)"       
# [7] "PremierLeague & West Bromwich Albion & 2016 & 10.2 (\\#19) & 9.9, [9.7, 10] (\\#19) & 1.4 (\\#18) & 3.5 (\\#11) & 0.2 (\\#12) & 1.1 (\\#2)"
# [8] "PremierLeague & Aston Villa & 2016 & 9.9 (\\#20) & 9.7, [9.6, 9.8] (\\#20) & 0.5 (\\#20) & 5.5 (\\#1) & 0.1 (\\#13) & 0 (\\#18)"           
# [9] "PremierLeague & Aston Villa & 2020 & 12.1 (\\#8) & 11.9, [11.7, 12.1] (\\#10) & 2 (\\#13) & 4.4 (\\#2) & 0.1 (\\#7) & 1.4 (\\#1)"          
# [10] "PremierLeague & Blackpool & 2011 & 13.9 (\\#12) & 13.7, [13.5, 13.9] (\\#14) & 2.9 (\\#10) & 5 (\\#6) & 0.1 (\\#11) & 0.9 (\\#5)"



##########
### APRIL 9th RUN, for CORNERS
##########

###### TOP 10

# [1] "Bundesliga & Bayern Munich & 2020 & 7.1 (\\#1) & 8.4, [8.1, 8.6] (\\#1) & 3.6 (\\#1) & 0.6 (\\#18) & 0.1 (\\#3) & 0.2 (\\#6)"  
# [2] "Bundesliga & Bayern Munich & 2015 & 6.5 (\\#1) & 7.7, [7.4, 7.9] (\\#1) & 3.1 (\\#1) & 0.5 (\\#18) & 0.2 (\\#3) & 0.1 (\\#7)"  
# [3] "Bundesliga & Bayern Munich & 2019 & 8.5 (\\#1) & 9.7, [9.4, 10] (\\#1) & 4.4 (\\#1) & 0.7 (\\#17) & 0 (\\#10) & 0.4 (\\#1)"    
# [4] "Bundesliga & Bayern Munich & 2014 & 7.1 (\\#1) & 8.2, [7.9, 8.4] (\\#1) & 3.4 (\\#1) & 0.8 (\\#18) & 0 (\\#14) & 0.1 (\\#10)"  
# [5] "Bundesliga & Bayern Munich & 2023 & 6.7 (\\#1) & 7.8, [7.6, 8.1] (\\#1) & 3.4 (\\#1) & 0.8 (\\#17) & 0 (\\#12) & 0 (\\#15)"    
# [6] "Bundesliga & Bayern Munich & 2012 & 6.4 (\\#1) & 7.4, [7.2, 7.7] (\\#1) & 2.6 (\\#1) & 0.8 (\\#16) & 0.2 (\\#2) & 0.1 (\\#11)" 
# [7] "Bundesliga & Bayern Munich & 2013 & 7.6 (\\#1) & 8.6, [8.4, 8.9] (\\#1) & 3.9 (\\#1) & 0.7 (\\#18) & 0 (\\#13) & 0.4 (\\#2)"   
# [8] "Bundesliga & Bayern Munich & 2021 & 7 (\\#1) & 8, [7.7, 8.3] (\\#1) & 3.4 (\\#1) & 1.5 (\\#5) & 0.1 (\\#2) & 0 (\\#13)"        
# [9] "Bundesliga & Bayern Munich & 2017 & 7.2 (\\#1) & 8, [7.8, 8.2] (\\#1) & 3.1 (\\#1) & 1.1 (\\#12) & 0 (\\#12) & 0.2 (\\#6)"     
# [10] "Bundesliga & Borussia Dortmund & 2013 & 5.5 (\\#4) & 6.3, [6.1, 6.5] (\\#3) & 2.4 (\\#2) & 1 (\\#17) & 0.3 (\\#1) & 0.1 (\\#8)"


# [1] "SerieA & AC Milan & 2012 & 6.7 (\\#2) & 8.4, [8.2, 8.7] (\\#2) & 2.8 (\\#1) & 0.5 (\\#20) & 0.2 (\\#4) & 0 (\\#18)"     
# [2] "SerieA & Internazionale & 2022 & 6.5 (\\#1) & 8, [7.8, 8.2] (\\#1) & 2.8 (\\#1) & 0.7 (\\#20) & 0 (\\#18) & 0.2 (\\#6)" 
# [3] "SerieA & Napoli & 2017 & 7 (\\#2) & 8.5, [8.2, 8.8] (\\#2) & 2.8 (\\#1) & 1.3 (\\#13) & 0.2 (\\#6) & 0.4 (\\#3)"        
# [4] "SerieA & Juventus & 2014 & 5.7 (\\#4) & 7.1, [6.9, 7.3] (\\#1) & 2.5 (\\#1) & 0.4 (\\#19) & 0.1 (\\#10) & 0.2 (\\#12)"  
# [5] "SerieA & Juventus & 2017 & 5.8 (\\#8) & 7.2, [7, 7.4] (\\#4) & 2.6 (\\#2) & 0.8 (\\#20) & 0 (\\#20) & 0.1 (\\#11)"      
# [6] "SerieA & Juventus & 2019 & 6.4 (\\#4) & 7.8, [7.6, 8] (\\#2) & 2.9 (\\#1) & 0.7 (\\#20) & 0.1 (\\#12) & 0.1 (\\#16)"    
# [7] "SerieA & Atalanta & 2021 & 5.4 (\\#6) & 6.7, [6.5, 6.9] (\\#1) & 2.4 (\\#1) & 0.6 (\\#20) & 0 (\\#15) & 0.1 (\\#8)"     
# [8] "SerieA & Juventus & 2012 & 7.4 (\\#1) & 8.7, [8.5, 8.9] (\\#1) & 2.4 (\\#2) & 0.7 (\\#19) & 0.3 (\\#2) & 0.1 (\\#11)"   
# [9] "SerieA & Juventus & 2015 & 5.8 (\\#7) & 7.1, [6.9, 7.3] (\\#1) & 2.3 (\\#1) & 0.2 (\\#20) & 0.1 (\\#12) & 0.1 (\\#18)"  
# [10] "SerieA & Internazionale & 2009 & 5.8 (\\#5) & 7, [6.8, 7.2] (\\#2) & 2.2 (\\#2) & 0.2 (\\#20) & 0.2 (\\#6) & 0.3 (\\#8)"


# [1] "LaLiga & Real Madrid & 2011 & 7.2 (\\#3) & 9.4, [9.1, 9.7] (\\#3) & 3.2 (\\#3) & 0.5 (\\#19) & 0.3 (\\#4) & 0.1 (\\#17)"
# [2] "LaLiga & Barcelona & 2011 & 7.3 (\\#2) & 9.4, [9.1, 9.7] (\\#2) & 3.7 (\\#2) & 0.5 (\\#20) & 0 (\\#19) & 0.4 (\\#6)"    
# [3] "LaLiga & Barcelona & 2010 & 6.7 (\\#2) & 8.6, [8.4, 8.9] (\\#1) & 3.2 (\\#1) & 0.3 (\\#20) & 0.1 (\\#10) & 0.1 (\\#17)" 
# [4] "LaLiga & Barcelona & 2013 & 5.9 (\\#7) & 7.8, [7.5, 8.1] (\\#3) & 3.3 (\\#1) & 0.5 (\\#19) & 0 (\\#18) & 0.3 (\\#9)"    
# [5] "LaLiga & Barcelona & 2014 & 6.9 (\\#1) & 8.7, [8.4, 8.9] (\\#1) & 3.1 (\\#1) & 0.9 (\\#16) & 0 (\\#18) & 0.1 (\\#18)"   
# [6] "LaLiga & Real Madrid & 2013 & 6.4 (\\#3) & 8.2, [7.9, 8.4] (\\#1) & 3.1 (\\#2) & 0.9 (\\#18) & 0.2 (\\#6) & 0.3 (\\#11)"
# [7] "LaLiga & Real Madrid & 2014 & 6.7 (\\#3) & 8.5, [8.2, 8.7] (\\#2) & 3 (\\#2) & 0.8 (\\#19) & 0 (\\#17) & 0.1 (\\#17)"   
# [8] "LaLiga & Barcelona & 2009 & 6.7 (\\#1) & 8.4, [8.1, 8.7] (\\#1) & 3 (\\#1) & 1.1 (\\#18) & 0.1 (\\#15) & 0.3 (\\#9)"    
# [9] "LaLiga & Real Madrid & 2015 & 6.4 (\\#1) & 8, [7.7, 8.3] (\\#1) & 2.7 (\\#1) & 1.1 (\\#17) & 0.1 (\\#12) & 0 (\\#19)"   
# [10] "LaLiga & Real Madrid & 2017 & 5.9 (\\#2) & 7.5, [7.2, 7.7] (\\#2) & 2.7 (\\#2) & 0.7 (\\#19) & 0.1 (\\#4) & 0.2 (\\#11)"


# [1] "Ligue1 & Paris Saint-Germain & 2018 & 6.7 (\\#1) & 8.7, [8.4, 9] (\\#1) & 3 (\\#1) & 0.6 (\\#20) & 0.3 (\\#1) & 0.1 (\\#11)"   
# [2] "Ligue1 & Paris Saint-Germain & 2016 & 5.6 (\\#2) & 7.1, [6.9, 7.3] (\\#1) & 2.8 (\\#1) & 0.3 (\\#20) & 0 (\\#19) & 0.2 (\\#12)"
# [3] "Ligue1 & Paris Saint-Germain & 2017 & 7 (\\#1) & 8.5, [8.3, 8.7] (\\#1) & 2.8 (\\#1) & 0.7 (\\#19) & 0.1 (\\#14) & 0 (\\#20)"  
# [4] "Ligue1 & AS Monaco & 2017 & 6.1 (\\#3) & 7.5, [7.3, 7.8] (\\#2) & 2.7 (\\#2) & 0.7 (\\#18) & 0.1 (\\#11) & 0.2 (\\#12)"        
# [5] "Ligue1 & Paris Saint-Germain & 2019 & 6.4 (\\#1) & 7.8, [7.5, 8] (\\#1) & 2.7 (\\#1) & 0.8 (\\#19) & 0.1 (\\#16) & 0.3 (\\#6)" 
# [6] "Ligue1 & Paris Saint-Germain & 2023 & 5.4 (\\#3) & 6.8, [6.5, 7] (\\#1) & 2.6 (\\#1) & 0.8 (\\#19) & 0.2 (\\#4) & 0.2 (\\#10)" 
# [7] "Ligue1 & Marseille & 2018 & 6.4 (\\#2) & 7.5, [7.3, 7.8] (\\#2) & 2.2 (\\#2) & 1.3 (\\#16) & 0.2 (\\#3) & 0.1 (\\#10)"         
# [8] "Ligue1 & Paris Saint-Germain & 2015 & 4.5 (\\#12) & 5.6, [5.4, 5.7] (\\#4) & 2.1 (\\#1) & 0.4 (\\#20) & 0 (\\#19) & 0 (\\#19)" 
# [9] "Ligue1 & AS Monaco & 2021 & 5.7 (\\#3) & 6.7, [6.5, 6.9] (\\#3) & 2.2 (\\#2) & 1.2 (\\#17) & 0.1 (\\#11) & 0.5 (\\#4)"         
# [10] "Ligue1 & Lyon & 2011 & 5.7 (\\#2) & 6.7, [6.5, 6.9] (\\#2) & 2 (\\#1) & 1.1 (\\#13) & 0.2 (\\#3) & 0.1 (\\#16)"  

# [1] "PremierLeague & Manchester City & 2019 & 7.9 (\\#1) & 9.7, [9.4, 10] (\\#1) & 4.5 (\\#1) & 0.5 (\\#19) & 0 (\\#19) & 0.3 (\\#4)"    
# [2] "PremierLeague & Liverpool & 2019 & 6.6 (\\#2) & 8.2, [7.9, 8.4] (\\#2) & 3.7 (\\#2) & 0.2 (\\#20) & 0 (\\#18) & 0.1 (\\#14)"        
# [3] "PremierLeague & Manchester City & 2018 & 7.5 (\\#1) & 9.1, [8.9, 9.4] (\\#1) & 3.6 (\\#1) & 0.4 (\\#20) & 0.1 (\\#7) & 0.1 (\\#9)"  
# [4] "PremierLeague & Manchester City & 2022 & 8.2 (\\#1) & 9.8, [9.5, 10.1] (\\#1) & 4.2 (\\#1) & 1.2 (\\#14) & 0.1 (\\#5) & 0.6 (\\#1)" 
# [5] "PremierLeague & Manchester City & 2014 & 7.3 (\\#1) & 8.7, [8.4, 8.9] (\\#1) & 3.5 (\\#1) & 1 (\\#16) & 0.1 (\\#5) & 0.5 (\\#2)"    
# [6] "PremierLeague & Manchester City & 2023 & 6.3 (\\#2) & 7.7, [7.5, 8] (\\#2) & 3.1 (\\#1) & 0.5 (\\#20) & 0.2 (\\#3) & 0.2 (\\#7)"    
# [7] "PremierLeague & Liverpool & 2020 & 6.8 (\\#2) & 8.1, [7.9, 8.4] (\\#2) & 3.3 (\\#1) & 0.9 (\\#19) & 0 (\\#18) & 0 (\\#18)"          
# [8] "PremierLeague & Liverpool & 2022 & 7.5 (\\#2) & 8.8, [8.6, 9.1] (\\#2) & 3.6 (\\#2) & 0.6 (\\#20) & 0 (\\#16) & 0.4 (\\#2)"         
# [9] "PremierLeague & Manchester City & 2021 & 6.4 (\\#2) & 7.6, [7.4, 7.9] (\\#1) & 2.9 (\\#1) & 0.8 (\\#20) & 0.1 (\\#6) & 0.2 (\\#10)" 
# [10] "PremierLeague & Tottenham Hotspur & 2017 & 7.2 (\\#2) & 8.3, [8.1, 8.6] (\\#2) & 2.7 (\\#1) & 0.7 (\\#20) & 0 (\\#19) & 0.1 (\\#11)"


###### BOTTOM 10


# [1] "Bundesliga & FC Cologne & 2018 & 5.1 (\\#6) & 4.8, [4.7, 5] (\\#11) & 0.5 (\\#18) & 2.7 (\\#1) & 0 (\\#15) & 0.2 (\\#8)"                   
# [2] "Bundesliga & Hannover 96 & 2016 & 4.6 (\\#8) & 4.3, [4.2, 4.4] (\\#13) & 0.3 (\\#18) & 2.7 (\\#2) & 0 (\\#15) & 0.2 (\\#5)"                
# [3] "Bundesliga & SC Paderborn 07 & 2020 & 5.2 (\\#7) & 4.9, [4.7, 5] (\\#11) & 0.4 (\\#18) & 3.1 (\\#1) & 0.1 (\\#11) & 0.2 (\\#9)"            
# [4] "Bundesliga & Arminia Bielefeld & 2009 & 4.4 (\\#17) & 4.2, [4.2, 4.3] (\\#17) & 0.5 (\\#18) & 1.6 (\\#7) & 0 (\\#11) & 0.3 (\\#3)"         
# [5] "Bundesliga & FC Augsburg & 2022 & 4.3 (\\#13) & 4.1, [4, 4.2] (\\#15) & 0.6 (\\#16) & 2.2 (\\#1) & 0 (\\#12) & 0 (\\#14)"                  
# [6] "Bundesliga & Fortuna Dsseldorf & 2020 & 4.4 (\\#14) & 4.2, [4.1, 4.3] (\\#15) & 0.5 (\\#17) & 1.6 (\\#6) & 0 (\\#15) & 0.5 (\\#1)"         
# [7] "Bundesliga & Kaiserslautern & 2012 & 4.3 (\\#12) & 4.1, [4, 4.2] (\\#16) & 0.3 (\\#18) & 1.9 (\\#2) & 0.1 (\\#10) & 0.3 (\\#5)"            
# [8] "Bundesliga & Schalke 04 & 2023 & 5 (\\#5) & 4.8, [4.7, 4.9] (\\#9) & 0.5 (\\#18) & 2.3 (\\#1) & 0 (\\#17) & 0.3 (\\#5)"                    
# [9] "Bundesliga & SpVgg Greuther Furth & 2022 & 4.1 (\\#17) & 3.9, [3.8, 4] (\\#17) & 0.4 (\\#17) & 1.9 (\\#3) & 0 (\\#16) & 0.1 (\\#4)"        
# [10] "Bundesliga & TSV Eintracht Braunschweig & 2014 & 4.4 (\\#15) & 4.2, [4.1, 4.3] (\\#17) & 0.4 (\\#18) & 1.8 (\\#4) & 0 (\\#18) & 0.3 (\\#4)"


# [1] "SerieA & Benevento & 2021 & 4.6 (\\#12) & 4.1, [4, 4.2] (\\#16) & 0.6 (\\#16) & 3.1 (\\#1) & 0.1 (\\#4) & 0.7 (\\#1)"     
# [2] "SerieA & Livorno & 2014 & 4.8 (\\#15) & 4.3, [4.2, 4.5] (\\#19) & 0.3 (\\#20) & 2.9 (\\#1) & 0 (\\#20) & 0.1 (\\#17)"     
# [3] "SerieA & Novara & 2012 & 5.3 (\\#9) & 4.8, [4.7, 4.9] (\\#15) & 0.3 (\\#18) & 2.9 (\\#1) & 0.1 (\\#9) & 0.3 (\\#3)"       
# [4] "SerieA & US Pescara & 2013 & 3.8 (\\#20) & 3.3, [3.2, 3.4] (\\#20) & 0 (\\#20) & 2.4 (\\#3) & 0.1 (\\#10) & 0.4 (\\#6)"   
# [5] "SerieA & Bologna & 2014 & 4.6 (\\#18) & 4.2, [4.1, 4.3] (\\#20) & 0.4 (\\#19) & 2.3 (\\#4) & 0.1 (\\#13) & 0.3 (\\#4)"    
# [6] "SerieA & Siena & 2010 & 5.7 (\\#6) & 5.3, [5.2, 5.4] (\\#14) & 0.3 (\\#20) & 2.8 (\\#1) & 0.1 (\\#14) & 0.4 (\\#2)"       
# [7] "SerieA & SPAL & 2020 & 4.2 (\\#19) & 3.8, [3.7, 3.9] (\\#20) & 0.2 (\\#20) & 2.2 (\\#3) & 0 (\\#20) & 0 (\\#18)"          
# [8] "SerieA & US Pescara & 2017 & 4.8 (\\#13) & 4.4, [4.2, 4.6] (\\#15) & 0.5 (\\#19) & 2.7 (\\#1) & 0.1 (\\#8) & 0.7 (\\#1)"  
# [9] "SerieA & Chievo Verona & 2009 & 5.2 (\\#11) & 4.9, [4.8, 5] (\\#17) & 0.3 (\\#20) & 1.9 (\\#5) & 0.1 (\\#14) & 0.5 (\\#4)"
# [10] "SerieA & Frosinone & 2019 & 4.2 (\\#18) & 3.9, [3.8, 4] (\\#19) & 0.2 (\\#19) & 2.1 (\\#3) & 0.1 (\\#11) & 0.2 (\\#9)" 


# [1] "LaLiga & Elche & 2023 & 5.4 (\\#6) & 5.2, [5.1, 5.4] (\\#8) & 0.4 (\\#20) & 2.8 (\\#1) & 0.2 (\\#6) & 0.5 (\\#2)"          
# [2] "LaLiga & Osasuna & 2017 & 3.8 (\\#17) & 3.6, [3.5, 3.7] (\\#20) & 0.2 (\\#20) & 2 (\\#3) & 0.1 (\\#10) & 0.1 (\\#15)"      
# [3] "LaLiga & Recreativo Huelva & 2009 & 5.3 (\\#9) & 5.1, [5, 5.2] (\\#16) & 0.4 (\\#20) & 2.3 (\\#1) & 0 (\\#19) & 0.6 (\\#3)"
# [4] "LaLiga & Granada & 2017 & 3.9 (\\#15) & 3.8, [3.7, 3.9] (\\#18) & 0.5 (\\#17) & 1.9 (\\#4) & 0 (\\#12) & 0.2 (\\#6)"       
# [5] "LaLiga & Mlaga & 2017 & 5.4 (\\#5) & 5.3, [5.1, 5.4] (\\#8) & 0.6 (\\#16) & 2.7 (\\#1) & 0 (\\#20) & 0.6 (\\#1)"           
# [6] "LaLiga & Almera & 2011 & 5.3 (\\#10) & 5.3, [5.2, 5.5] (\\#13) & 0.8 (\\#18) & 2.4 (\\#2) & 0.1 (\\#17) & 0.6 (\\#1)"      
# [7] "LaLiga & Crdoba & 2015 & 4.1 (\\#19) & 4.1, [4, 4.3] (\\#20) & 0.4 (\\#20) & 2 (\\#5) & 0.2 (\\#4) & 0.1 (\\#11)"          
# [8] "LaLiga & Espanyol & 2020 & 4.1 (\\#17) & 4.1, [4, 4.3] (\\#18) & 0.3 (\\#20) & 2.2 (\\#1) & 0.1 (\\#6) & 0.2 (\\#5)"       
# [9] "LaLiga & Espanyol & 2023 & 4.7 (\\#9) & 4.7, [4.6, 4.8] (\\#13) & 0.4 (\\#18) & 2.2 (\\#2) & 0.1 (\\#15) & 0.1 (\\#16)"    
# [10] "LaLiga & Getafe & 2015 & 4.8 (\\#13) & 4.8, [4.7, 5] (\\#17) & 0.5 (\\#19) & 2 (\\#4) & 0 (\\#17) & 0.1 (\\#14)"  

# [1] "Ligue1 & Angers & 2023 & 4.3 (\\#14) & 4.1, [4, 4.2] (\\#15) & 0.2 (\\#20) & 2.2 (\\#1) & 0 (\\#17) & 0.2 (\\#9)"        
# [2] "Ligue1 & GFC Ajaccio & 2016 & 4 (\\#18) & 3.8, [3.7, 3.9] (\\#20) & 0.2 (\\#19) & 1.9 (\\#4) & 0.1 (\\#15) & 0.2 (\\#13)"
# [3] "Ligue1 & Lorient & 2022 & 4.3 (\\#17) & 4.1, [4, 4.2] (\\#18) & 0.3 (\\#20) & 1.5 (\\#11) & 0.1 (\\#12) & 0.4 (\\#6)"    
# [4] "Ligue1 & Nimes & 2021 & 4.2 (\\#18) & 4, [3.9, 4.1] (\\#18) & 0.4 (\\#18) & 2 (\\#2) & 0.1 (\\#10) & 0.6 (\\#2)"         
# [5] "Ligue1 & Toulouse & 2014 & 4.5 (\\#15) & 4.3, [4.2, 4.4] (\\#17) & 0.5 (\\#19) & 1.8 (\\#5) & 0 (\\#20) & 0.6 (\\#1)"    
# [6] "Ligue1 & Troyes & 2016 & 5.4 (\\#5) & 5.2, [5.1, 5.4] (\\#10) & 0.2 (\\#20) & 2.9 (\\#1) & 0.5 (\\#1) & 0.4 (\\#7)"      
# [7] "Ligue1 & Valenciennes & 2014 & 6.2 (\\#1) & 6, [5.9, 6.2] (\\#5) & 0.9 (\\#9) & 2.9 (\\#1) & 0 (\\#11) & 0.4 (\\#3)"     
# [8] "Ligue1 & AC Ajaccio & 2023 & 3.3 (\\#20) & 3.2, [3.1, 3.3] (\\#20) & 0.2 (\\#19) & 1.8 (\\#3) & 0.1 (\\#9) & 0.1 (\\#16)"
# [9] "Ligue1 & Bordeaux & 2022 & 3.9 (\\#19) & 3.8, [3.7, 4] (\\#20) & 0.5 (\\#16) & 1.7 (\\#7) & 0.1 (\\#15) & 0.4 (\\#7)"    
# [10] "Ligue1 & Brest & 2013 & 3.9 (\\#19) & 3.8, [3.7, 3.9] (\\#20) & 0.4 (\\#20) & 1.8 (\\#4) & 0.1 (\\#9) & 0.2 (\\#10)" 
# 
# 
# [1] "PremierLeague & Aston Villa & 2016 & 4.4 (\\#15) & 4.2, [4.1, 4.3] (\\#19) & 0.3 (\\#20) & 2.3 (\\#2) & 0.1 (\\#12) & 0 (\\#16)"       
# [2] "PremierLeague & Brighton & Hove Albion & 2019 & 4.2 (\\#19) & 4.1, [4, 4.2] (\\#19) & 0.4 (\\#20) & 2 (\\#3) & 0.1 (\\#8) & 0.2 (\\#8)"
# [3] "PremierLeague & Fulham & 2019 & 4.3 (\\#17) & 4.2, [4.1, 4.3] (\\#18) & 0.5 (\\#18) & 2.1 (\\#2) & 0 (\\#13) & 0 (\\#19)"              
# [4] "PremierLeague & Norwich City & 2014 & 5.2 (\\#11) & 5.1, [5, 5.2] (\\#14) & 0.5 (\\#19) & 2.1 (\\#4) & 0 (\\#17) & 0.2 (\\#10)"        
# [5] "PremierLeague & Norwich City & 2022 & 4.4 (\\#15) & 4.3, [4.2, 4.4] (\\#19) & 0.5 (\\#18) & 2.1 (\\#3) & 0 (\\#18) & 0 (\\#14)"        
# [6] "PremierLeague & Portsmouth & 2010 & 4.9 (\\#13) & 4.8, [4.7, 5] (\\#19) & 0.6 (\\#20) & 1.9 (\\#4) & 0.1 (\\#13) & 0.4 (\\#2)"         
# [7] "PremierLeague & Southampton & 2023 & 4.1 (\\#17) & 4, [3.9, 4.1] (\\#17) & 0.3 (\\#20) & 2 (\\#3) & 0 (\\#18) & 0.2 (\\#4)"            
# [8] "PremierLeague & Stoke City & 2018 & 3.6 (\\#20) & 3.5, [3.4, 3.6] (\\#20) & 0.2 (\\#20) & 1.5 (\\#8) & 0 (\\#18) & 0 (\\#19)"          
# [9] "PremierLeague & Sunderland & 2016 & 4 (\\#20) & 3.9, [3.9, 4] (\\#20) & 0.5 (\\#18) & 1.4 (\\#11) & 0 (\\#19) & 0.3 (\\#5)"            
# [10] "PremierLeague & Sunderland & 2017 & 4.2 (\\#17) & 4.1, [4, 4.2] (\\#19) & 0.1 (\\#20) & 1.8 (\\#4) & 0.1 (\\#11) & 0 (\\#20)" 