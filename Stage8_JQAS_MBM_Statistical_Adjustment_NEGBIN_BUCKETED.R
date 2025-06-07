#####
## Stage 8: Yielding Tables 1 & 2 of the main manuscript
##          (games with strongest positive/negative SINGLE-GAME shifts in each respective leagues,
##          first table is for shots, second - for corner kicks).
##          Also, calculating the multiplicative adjustment coefficients, 
##          saving those into .Robj file as follows:
##
##         save(my.obj.shots, file="my.obj.adj.coefs.shots.Robj")
##
##        That is to be later fed to "Stage8p2_JQAS_MBM_Plotting_Adjustment_Coefficients.R"
##
## NOTE: If you want to switch from one offensive statistic to another, 
##       use "Ctrl+F" with "Match Case" turned out in order 
##       to replace all occurrences of "Corner" for "Shot" (), or vice versa.
####


######
######

library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)

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

adj.coefs.scorediff <- list()
adj.coefs.redcarddiff <- list()
adj.coefs.homeaway <- list()



for (league in league.name){
  
  print(league)
  
  cat("\n")
  
  
  hey <- load(file=paste0("Fitted_Objects/GAM_W_BUCKETED_EXTREME_CATEGORIES_gam_nb_obj_Team_AND_Season_Mixed_Eff_",
                   ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                   ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                   "_", league,
                   "_Shots.Robj"))
  print(hey)
 
  ## For some reason it's "ns.obj" for Shots, while "gam.nb.obj" for Shots, so I move it all to same nomenclature 
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
  
  
  adj.coefs.scorediff[[league]] <- data.frame(Adj = exp(-log.score.diff.effects),
                                              Adj.Min = exp(-log.score.diff.effects.min),
                                              Adj.Max=exp(-log.score.diff.effects.max))
  
  
  
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
  
  
  adj.coefs.redcarddiff[[league]] <- data.frame(Adj = exp(-log.redcard.diff.effects),
                                              Adj.Min = exp(-log.redcard.diff.effects.min),
                                              Adj.Max=exp(-log.redcard.diff.effects.max))
  
  
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
  
  
  adj.coefs.homeaway[[league]] <- data.frame(Adj = exp(-log.homeaway.effects),
                                                Adj.Min = exp(-log.homeaway.effects.min),
                                                Adj.Max=exp(-log.homeaway.effects.max))
  
  
  
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
  
  
  
  
  
  
  # summary(our.df.pred$Adj.Coef.All)
  # summary(our.df.pred$Adj.Coef.ScoreDiff*our.df.pred$Adj.Coef.RedCardDiff*our.df.pred$Adj.Coef.HomeAway)
  # 
  # summary(our.df.pred$Adj.Coef.All.Max)
  # summary(our.df.pred$Adj.Coef.ScoreDiff.Max*our.df.pred$Adj.Coef.RedCardDiff.Max*our.df.pred$Adj.Coef.HomeAway.Max)
  # 
  # summary(our.df.pred$Adj.Coef.All.Min)
  # summary(our.df.pred$Adj.Coef.ScoreDiff.Min*our.df.pred$Adj.Coef.RedCardDiff.Min*our.df.pred$Adj.Coef.HomeAway.Min)
  
  
  
  
  
  
  
  our.df.pred <- our.df.pred %>%
    mutate(Shots.Adj = Shots*Adj.Coef.All,
           Shots.Adj.Min = Shots*Adj.Coef.All.Min,
           Shots.Adj.Max = Shots*Adj.Coef.All.Max
           #,Shots.Adj.No.Min = Shots*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway
    )
  
  
  
  
  
  
  # ### 
  # cat("\n")
  # print(paste0("Top-10 single game shifts, ", ifelse(include.minute, "WITH MINUTE:", "NO MINUTE:")))
  # 
  final.df <- our.df.pred %>%
    group_by(gameId, Team) %>%
    summarise(Shots = sum(Shots),
              Shots.Adj= sum(Shots.Adj),
              Shots.Adj.Min = sum(Shots.Adj.Min),
              Shots.Adj.Max = sum(Shots.Adj.Max),
              Shots.Diff = Shots.Adj - Shots,
              # Shots.Adj.No.Min = sum(Shots.Adj.No.Min),
              # Shots.Diff.No.Min = Shots.Adj.No.Min - Shots
    ) %>%
    arrange(desc(abs(Shots.Diff)))
  
  # print(head(final.df, 50))
  
  # our.df %>%
  #   filter(gameId == head(final.df$gameId, 1), Team == head(final.df$Team, 1))
  
  
  
  
  #######
  ## IF WE WANT THE TABLE ENTRIES FOR TOP MOVEMENTS
  #######
  
  biggest.jump <- final.df %>% filter(Shots.Diff > 0) %>% dplyr::select(gameId, Team) %>% head(.,1)
  biggest.fall <- final.df %>% filter(Shots.Diff < 0) %>% dplyr::select(gameId, Team) %>% head(.,1)
  
  cat("\n")
  print("Biggest jump and fall, breakdown:")
  biggest.jump.fall <- our.df.pred %>%
    filter(gameId %in% c(biggest.jump$gameId, biggest.fall$gameId),
           Team %in% c(biggest.jump$Team, biggest.fall$Team)) %>%
    group_by(gameId, Team, gamedDate, HomeAway, Score.Diff, RedCard.Diff) %>%
    summarise(Shots = sum(Shots),
              Shots.Adj = sum(Shots.Adj),
              Shots.Adj.Min = sum(Shots.Adj.Min),
              Shots.Adj.Max = sum(Shots.Adj.Max),
              # minutes.spent = sum(minutes.spent)
              minutes.spent=n()
    ) %>%
    mutate(Score.Diff.Categ = ifelse(Score.Diff < 0,
                                     "Trail",
                                     ifelse(Score.Diff > 0,
                                            "Lead",
                                            "0")),
           Score.2.plus.Categ = ifelse(Score.Diff < -1,
                                       "Trail.2.plus",
                                       ifelse(Score.Diff > 1,
                                              "Lead.2.plus",
                                              "0")),
           RedCard.Diff.Categ = ifelse(RedCard.Diff < 0,
                                       "UpMen",
                                       ifelse(RedCard.Diff > 0,
                                              "DownMen",
                                              "0")),
           RedCard.2.plus.Categ = ifelse(RedCard.Diff < -1,
                                         "UpMen.2.plus",
                                         ifelse(RedCard.Diff > 1,
                                                "DownMen.2.plus",
                                                "0"))) %>%
    ungroup()
  # print(biggest.jump.fall)
  
  score.diffs.lead.trail <- biggest.jump.fall %>%
    dplyr::select(-RedCard.Diff) %>%
    group_by(gameId, Team, gamedDate, Score.Diff.Categ) %>%
    summarise(Shots = sum(Shots),
              minutes = sum(minutes.spent),
    ) %>%
    pivot_wider(names_from = c(Score.Diff.Categ),
                values_from = c(Shots, minutes)) %>%
    replace(is.na(.), 0 ) %>%
    mutate(Shots.Total.Actual = Shots_0 + 
             ifelse("Shots_Lead" %in% colnames(.), Shots_Lead, 0)
           + ifelse("Shots_Trail" %in% colnames(.), Shots_Trail, 0),
           #Shots.Total.Adj = sum(Shots.Adj),
           Shots.Min.Lead = paste0(ifelse("Shots_Lead" %in% colnames(.), Shots_Lead, 0), 
                                     " (", 
                                     ifelse("minutes_Lead" %in% colnames(.), minutes_Lead, 0), 
                                     ")"),
           Shots.Min.Trail = paste0(ifelse("Shots_Trail" %in% colnames(.), Shots_Trail, 0), 
                                      " (", 
                                      ifelse("minutes_Trail" %in% colnames(.), minutes_Trail, 0), 
                                      ")")) %>%
    dplyr::select(Shots.Total.Actual,
                  #Shots.Total.Adj,
                  Shots.Min.Lead,# Minutes.Lead,
                  Shots.Min.Trail #, Minutes.Trail
    )
  
  
  score.diffs.2.plus <- biggest.jump.fall %>%
    dplyr::select(-RedCard.Diff) %>%
    group_by(gameId, Team, gamedDate, Score.2.plus.Categ) %>%
    summarise(Shots = sum(Shots),
              minutes = sum(minutes.spent),
    ) %>%
    pivot_wider(names_from = c(Score.2.plus.Categ),
                values_from = c(Shots, minutes)) %>%
    replace(is.na(.), 0 ) %>%
    mutate(Shots.Total.Actual = Shots_0 + 
             ifelse("Shots_Lead.2.plus" %in% colnames(.), Shots_Lead.2.plus, 0)
           + ifelse("Shots_Trail.2.plus" %in% colnames(.), Shots_Trail.2.plus, 0),
           #Shots.Total.Adj = sum(Shots.Adj),
           Shots.Min.Lead.2.plus = paste0(ifelse("Shots_Lead.2.plus" %in% colnames(.), Shots_Lead.2.plus, 0), 
                                            " (", 
                                            ifelse("minutes_Lead.2.plus" %in% colnames(.), minutes_Lead.2.plus, 0), 
                                            ")"),
           Shots.Min.Trail.2.plus = paste0(ifelse("Shots_Trail.2.plus" %in% colnames(.), Shots_Trail.2.plus, 0), 
                                             " (", 
                                             ifelse("minutes_Trail.2.plus" %in% colnames(.), minutes_Trail.2.plus, 0), 
                                             ")")) %>%
    dplyr::select(Shots.Total.Actual,
                  #Shots.Total.Adj,
                  Shots.Min.Lead.2.plus,# Minutes.Lead,
                  Shots.Min.Trail.2.plus #, Minutes.Trail
    )
  
  
  
  
  
  redcard.diffs.lead.trail  <- biggest.jump.fall %>%
    dplyr::select(-Score.Diff) %>%
    group_by(gameId, Team, gamedDate, RedCard.Diff.Categ) %>%
    summarise(Shots = sum(Shots),
              # Shots.Adj = sum(Shots.Adj),
              # minutes.spent = sum(minutes.spent)
              minutes = sum(minutes.spent)
    ) %>%
    pivot_wider(names_from = c(RedCard.Diff.Categ),
                values_from = c(Shots, minutes)) %>%
    replace(is.na(.), 0 ) %>%
    mutate(Shots.Total.Actual = Shots_0 + 
             ifelse("Shots_UpMen" %in% colnames(.), Shots_UpMen, 0)
           + ifelse("Shots_DownMen" %in% colnames(.), Shots_DownMen, 0),
           #Shots.Total.Adj = sum(Shots.Adj),
           Shots.Min.UpMen = paste0(ifelse("Shots_UpMen" %in% colnames(.), Shots_UpMen, 0), 
                                      " (", 
                                      ifelse("minutes_UpMen" %in% colnames(.), minutes_UpMen, 0), 
                                      ")"),
           Shots.Min.DownMen = paste0(ifelse("Shots_DownMen" %in% colnames(.), Shots_DownMen, 0), 
                                        " (", 
                                        ifelse("minutes_DownMen" %in% colnames(.), minutes_DownMen, 0), 
                                        ")")) %>%
    dplyr::select(Shots.Total.Actual,
                  #Shots.Total.Adj,
                  Shots.Min.UpMen,# Minutes.Lead,
                  Shots.Min.DownMen #, Minutes.Trail
    )
  
  
  redcard.diffs.2.plus  <- biggest.jump.fall %>%
    dplyr::select(-Score.Diff) %>%
    group_by(gameId, Team, gamedDate, RedCard.2.plus.Categ) %>%
    summarise(Shots = sum(Shots),
              # Shots.Adj = sum(Shots.Adj),
              # minutes.spent = sum(minutes.spent)
              minutes = sum(minutes.spent)
    ) %>%
    pivot_wider(names_from = c(RedCard.2.plus.Categ),
                values_from = c(Shots, minutes)) %>%
    replace(is.na(.), 0 ) %>%
    mutate(Shots.Total.Actual = Shots_0 + 
             ifelse("Shots_UpMen.2.plus" %in% colnames(.), Shots_UpMen.2.plus, 0)
           + ifelse("Shots_DownMen.2.plus" %in% colnames(.), Shots_DownMen.2.plus, 0),
           #Shots.Total.Adj = sum(Shots.Adj),
           Shots.Min.UpMen.2.plus = paste0(ifelse("Shots_UpMen.2.plus" %in% colnames(.), Shots_UpMen.2.plus, 0), 
                                             " (", 
                                             ifelse("minutes_UpMen.2.plus" %in% colnames(.), minutes_UpMen.2.plus, 0), 
                                             ")"),
           Shots.Min.DownMen.2.plus = paste0(ifelse("Shots_DownMen.2.plus" %in% colnames(.), Shots_DownMen.2.plus, 0), 
                                               " (", 
                                               ifelse("minutes_DownMen.2.plus" %in% colnames(.), minutes_DownMen.2.plus, 0), 
                                               ")")) %>%
    dplyr::select(Shots.Total.Actual,
                  #Shots.Total.Adj,
                  Shots.Min.UpMen.2.plus,# Minutes.Lead,
                  Shots.Min.DownMen.2.plus #, Minutes.Trail
    )
  
  
  
  
  cat("\n")
  # print(score.diffs.lead.trail %>% left_join(redcard.diffs.lead.trail))
  
  
  all.stuff <- score.diffs.lead.trail %>% left_join(redcard.diffs.lead.trail)
  
  all.stuff.lead.trail <- all.stuff %>%
    left_join(biggest.jump.fall %>% group_by(Team, gameId) %>% 
                summarise(Shots.Adj = round(sum(Shots.Adj), 1),
                          Shots.Adj.Min = round(sum(Shots.Adj.Min), 1),
                          Shots.Adj.Max = round(sum(Shots.Adj.Max), 1),
                          Shots.Adj.w.CI = paste0(Shots.Adj, ", ", "[", Shots.Adj.Min, ", ", Shots.Adj.Max, "]")),
              by=c("Team", "gameId"))
  
  
  
  all.stuff.lead.trail <- all.stuff.lead.trail[, c("gameId", "Team", "gamedDate",
                                                   "Shots.Total.Actual", 
                                                   "Shots.Adj.w.CI",
                                                   # "Shots.Adj", "Shots.Adj.Min", "Shots.Adj.Max",
                                                   "Shots.Min.Lead", "Shots.Min.Trail",
                                                   "Shots.Min.UpMen", "Shots.Min.DownMen")]
  
  
  # print(colnames(all.stuff.lead.trail))
  all.stuff.lead.trail$Team <- as.character(all.stuff.lead.trail$Team)
  for (j in 1:nrow(all.stuff)){
    print(paste0(all.stuff.lead.trail[j,-1], collapse=" & "))
  }
  
  
  
  ####
  ## If we also want to add the 2+ scenarios
  ####
  
  all.stuff.lead.trail.plus.2 <- all.stuff.lead.trail %>% left_join(score.diffs.2.plus %>% left_join(redcard.diffs.2.plus))
  all.stuff.lead.trail.plus.2 <- all.stuff.lead.trail.plus.2[, c("gameId", "Team", "gamedDate",
                                                                 "Shots.Total.Actual",
                                                                 "Shots.Adj.w.CI",
                                                                 # "Shots.Adj", "Shots.Adj.Min", "Shots.Adj.Max",
                                                                 "Shots.Min.Lead", "Shots.Min.Trail",
                                                                 "Shots.Min.UpMen", "Shots.Min.DownMen",
                                                                 "Shots.Min.Lead.2.plus", "Shots.Min.Trail.2.plus",
                                                                 "Shots.Min.UpMen.2.plus", "Shots.Min.DownMen.2.plus")]
  
  # print(colnames(all.stuff.lead.trail.plus.2))
  for (j in 1:nrow(all.stuff)){
    print(paste0(all.stuff.lead.trail.plus.2[j,-1], collapse=" & "))
  }
  

  
  
  cat("\n")
  cat("\n")
}


print("Adjustment coefficients for score diff")
print(adj.coefs.scorediff)

print("Adjustment coefficients for red card diff")
print(adj.coefs.redcarddiff)

print("Adjustment coefficients for home/away:")
print(adj.coefs.homeaway)

my.obj.shots <- list(
  adj.coefs.scorediff,
  adj.coefs.redcarddiff,
  adj.coefs.homeaway
)

save(my.obj.shots, file="my.obj.adj.coefs.shots.Robj")


#########
### RESULTS
##########

## SHOTS

# [1] "Bundesliga"
# [1] "Eintracht Frankfurt & 2011-02-27 & 30 & 22.1, [21.5, 22.8] & 0 (0) & 12 (26) & 26 (76) & 0 (0) & 0 (0) & 12 (22) & 0 (0) & 0 (0)"
# [1] "Bayern Munich & 2021-03-20 & 15 & 31.9, [30.1, 33.7] & 12 (76) & 0 (0) & 0 (0) & 14 (82) & 11 (71) & 0 (0) & 0 (0) & 0 (0)"
# 
# 
# [1] "SerieA"
# [1] "Internazionale & 2017-12-03 & 40 & 54.6, [53.6, 55.7] & 28 (71) & 0 (0) & 0 (0) & 0 (0) & 26 (56) & 0 (0) & 0 (0) & 0 (0)"
# [1] "AS Roma & 2022-05-14 & 43 & 28.7, [28, 29.4] & 0 (0) & 29 (79) & 38 (70) & 0 (0) & 0 (0) & 0 (0) & 0 (0) & 0 (0)"
# 
# 
# [1] "LaLiga"
# [1] "Getafe & 2009-11-07 & 22 & 12.8, [12.1, 13.6] & 0 (0) & 22 (79) & 19 (66) & 0 (0) & 0 (0) & 14 (32) & 9 (19) & 0 (0)"
# [1] "Real Madrid & 2011-08-28 & 40 & 60.7, [59.1, 62.4] & 33 (67) & 0 (0) & 0 (0) & 0 (0) & 31 (63) & 0 (0) & 0 (0) & 0 (0)"
# 
# [1] "Ligue1"
# [1] "Caen & 2015-09-12 & 15 & 31.7, [30.4, 33.1] & 13 (78) & 0 (0) & 0 (0) & 9 (38) & 11 (61) & 0 (0) & 0 (0) & 0 (0)"
# [1] "Bordeaux & 2022-03-20 & 31 & 13.8, [12.5, 15.1] & 0 (0) & 31 (94) & 31 (66) & 0 (0) & 0 (0) & 31 (89) & 27 (54) & 0 (0)"
# 
# [1] "PremierLeague"
# [1] "Blackpool & 2010-11-01 & 26 & 13, [11.1, 15.2] & 25 (79) & 0 (0) & 26 (81) & 0 (0) & 6 (23) & 0 (0) & 25 (62) & 0 (0)"
# [1] "Manchester City & 2012-10-20 & 24 & 41.7, [39.7, 43.9] & 1 (2) & 2 (13) & 0 (0) & 20 (78) & 0 (0) & 0 (0) & 0 (0) & 0 (0)"


## CORNERS

# [1] "Eintracht Frankfurt & 2011-02-27 & 13 & 9.1, [8.5, 9.6] & 0 (0) & 5 (26) & 13 (76) & 0 (0) & 0 (0) & 5 (22) & 0 (0) & 0 (0)"
# [1] "Eintracht Frankfurt & 2013-05-18 & 6 & 4, [3.8, 4.2] & 0 (0) & 6 (87) & 5 (62) & 0 (0) & 0 (0) & 1 (18) & 0 (0) & 0 (0)"
# [1] "VfL Wolfsburg & 2013-05-18 & 9 & 17.9, [16.5, 19.4] & 8 (87) & 0 (0) & 0 (0) & 7 (62) & 1 (18) & 0 (0) & 0 (0) & 0 (0)"
# 
# [1] "AS Roma & 2012-11-04 & 15 & 25.6, [24.3, 26.9] & 13 (85) & 0 (0) & 0 (0) & 2 (14) & 13 (65) & 0 (0) & 0 (0) & 0 (0)"
# [1] "AS Roma & 2022-05-14 & 20 & 13.1, [12.6, 13.8] & 0 (0) & 14 (79) & 18 (70) & 0 (0) & 0 (0) & 0 (0) & 0 (0) & 0 (0)"
# 
# [1] "Getafe & 2009-11-07 & 14 & 8.7, [7.8, 9.6] & 0 (0) & 13 (79) & 11 (66) & 0 (0) & 0 (0) & 8 (32) & 7 (19) & 0 (0)"
# [1] "Barcelona & 2020-10-01 & 8 & 22.3, [20.7, 24] & 8 (91) & 0 (0) & 0 (0) & 7 (60) & 6 (45) & 0 (0) & 0 (0) & 0 (0)"
# 
# [1] "Paris Saint-Germain & 2012-11-17 & 18 & 9.5, [8.6, 10.5] & 0 (0) & 17 (71) & 16 (72) & 0 (0) & 0 (0) & 0 (0) & 11 (41) & 0 (0)"
# [1] "Marseille & 2018-05-11 & 11 & 18.9, [17.9, 20] & 2 (53) & 0 (11) & 0 (0) & 8 (31) & 1 (28) & 0 (0) & 0 (0) & 0 (0)"
# 
# [1] "Manchester City & 2012-12-29 & 9 & 17.2, [16.1, 18.4] & 9 (101) & 0 (0) & 0 (0) & 4 (59) & 4 (30) & 0 (0) & 0 (0) & 0 (0)"
# [1] "AFC Bournemouth & 2019-05-04 & 10 & 6, [5.2, 6.9] & 0 (5) & 0 (0) & 9 (58) & 0 (0) & 0 (0) & 0 (0) & 7 (49) & 0 (0)"



### GOALS

# [1] "Mainz & 2019-11-24 & 5 & 8.2, [7.2, 9.3] & 4 (67) & 0 (0) & 0 (0) & 4 (54) & 3 (42) & 0 (0) & 0 (0) & 0 (0)"
# [1] "Eintracht Frankfurt & 2019-11-02 & 5 & 3, [2.7, 3.3] & 4 (69) & 0 (0) & 5 (86) & 0 (0) & 2 (46) & 0 (0) & 0 (0) & 0 (0)"
# 
# [1] "Lazio & 2018-03-31 & 6 & 4, [3.7, 4.4] & 3 (31) & 1 (9) & 6 (90) & 0 (0) & 2 (25) & 0 (0) & 0 (0) & 0 (0)"
# [1] "Cagliari & 2018-12-08 & 3 & 7.6, [5.3, 11.2] & 0 (1) & 2 (84) & 0 (0) & 2 (6) & 0 (0) & 1 (44) & 0 (0) & 2 (5)"
# 
# [1] "Rayo Vallecano & 2022-10-22 & 5 & 2.9, [2.6, 3.4] & 4 (55) & 0 (0) & 5 (58) & 0 (0) & 3 (52) & 0 (0) & 2 (27) & 0 (0)"
# [1] "Barcelona & 2022-11-08 & 2 & 5.9, [4.2, 8.3] & 0 (9) & 1 (48) & 0 (0) & 2 (70) & 0 (0) & 0 (0) & 0 (0) & 2 (50)"
# 
# [1] "Valenciennes & 2012-10-20 & 6 & 4, [3.6, 4.4] & 4 (79) & 1 (4) & 4 (76) & 0 (0) & 3 (39) & 0 (0) & 3 (15) & 0 (0)"
# [1] "Paris Saint-Germain & 2017-08-20 & 6 & 9.8, [8.8, 10.9] & 4 (61) & 1 (13) & 0 (0) & 4 (26) & 2 (15) & 0 (0) & 0 (0) & 0 (0)"
# 
# [1] "Newcastle United & 2016-05-15 & 5 & 7.9, [7.1, 8.8] & 4 (80) & 0 (0) & 0 (0) & 3 (29) & 2 (46) & 0 (0) & 0 (0) & 0 (0)"
# [1] "Manchester United & 2021-02-02 & 9 & 4.8, [4.2, 5.5] & 8 (80) & 0 (0) & 9 (97) & 0 (0) & 7 (73) & 0 (0) & 3 (9) & 0 (0)"
