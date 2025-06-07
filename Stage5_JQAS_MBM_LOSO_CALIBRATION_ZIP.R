########
##  Stage 5: Running the Leave-One-Season-Out Cross Validation (LOSOCV) 
##           for Zero-Inflated Poisson model
##           to save the out-of-sample predictions in .Robj files as follows:
##
## save(file=paste0(ifelse(bucketed, "BUCKETED_", ""),
##                 "LEAVE_SEASON_OUT_PREDICTIONS_NEW_TEAM_AND_SEASON_gam_zip_obj", 
##                 "_", league,
##                 #"_", string_add, 
##                 "_Shots.Robj"),
##     pred.list.zip)
##
##  Those files are to be used later for reporting LOSOCV errors and calibration plots.
##
## NOTE: If you want to switch from one offensive statistic to another, 
##       use "Ctrl+F" with "Match Case" turned out in order 
##       to replace all occurrences of "Corner" for "Shot" (), or vice versa.
####

require(mgcv)
# require(tidyverse)
require(dplyr)


# library(splines)
# library(MASS)
# library(DHARMa)


###
## Whether to bucket extreme scores (3+), red card diffs (2+) and minutes (45+)
###
bucketed <- TRUE

## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we want to still keep 1st half extra time
keep.1st.half.extra <- TRUE


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")


pred.list <- pred.list.nb <- pred.list.zip <- list()

# load(paste0("LEAVE_SEASON_OUT_PREDICTIONS_gam_nb_obj", "_", string_add, "_Shots.Robj"))


for (league in league.name){
  
  print(league)
  print("Shots")
  
  #our.df <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  our.df.cut <- read.csv(paste0(league, 
                                ifelse(remove.extra, "_NO_EXTRA_TIME", ""), 
                                ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                                "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  ## REMOVING NA betting data
  our.df.cut <- our.df.cut %>% filter(!is.na(Weighted.Win.Prob))
  
  
  ## Remove extra time data from first half (so anything with half_id=1, minute>45)
  ## => TO AVOID "POINT MASS"/"OUT-OF-ORDER MINUTE" ISSUES...
  ##
  ## !!! BUUUT... the >90 IS ALSO AN ISSUE... data is way too sporadic over there !!!
  # our.df <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
  # print(dim(our.df))
  
  
  
  ############
  ############
  
  our.df.cut  <- our.df.cut  %>%
    mutate(abs.Score.Diff = abs(Score.Diff),
           abs.RedCard.Diff = abs(RedCard.Diff))
  
  our.df.cut  <- our.df.cut  %>%
    mutate(Period.ID = group_indices(our.df.cut, .dots=c("ID", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
    arrange(Period.ID) %>%
    mutate(Period.ID = factor(Period.ID))
  
  our.df.cut <- our.df.cut %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID))
  
  
  ## Creating "TeamSeason" combo indicator, and in case we use Half_ID => converting Minutes into "minutes in the half" 
  ## (so always starting from 0 or 1 when the half starts, rahter than starting from 46 for 2nd half)
  
  if (keep.1st.half.extra){
    
    our.df.cut <- our.df.cut %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID), Team=factor(Team), half_id = factor(half_id),
                                        season = factor(season),
                                        TeamSeason = factor(paste0(Team, season)),
                                        Minute.clean = ifelse(half_id == 2, 
                                                              Minute.clean - 45,
                                                              Minute.clean))
  } else {
    
    our.df.cut <- our.df.cut %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID), Team=factor(Team), half_id = factor(half_id),
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
    
    our.df.cut <- our.df.cut %>%
      mutate(Score.Diff = ifelse(abs(Score.Diff) >= 3, sign(Score.Diff)*3, Score.Diff),
             RedCard.Diff = ifelse(abs(RedCard.Diff) >= 2, sign(RedCard.Diff)*2, RedCard.Diff),
             Minute.clean = ifelse(Minute.clean >= 45, 45, Minute.clean))
  }
  
  
  ######
  ## Fitting SPLINE models
  ######
  
  for (year in 2009:2023){
    if (league == "LaLiga" & year %in% c(2016, 2022)) next;
    if (league %in% c("SerieA") & year == 2016) next;
    if (league == "Ligue1" & year == 2022) next;
    if (league == "PremierLeague" & year == 2009) next;
    
    
    print(year)

    
    score.df <- ifelse(length(unique(our.df.cut[our.df.cut$season != year, ]$Score.Diff)) < 10,
                       length(unique(our.df.cut[our.df.cut$season != year, ]$Score.Diff)),
                       10)
    redcard.df <- length(unique(our.df.cut$RedCard.Diff))
    
    
    
    ######
    ## ZIP
    ######
    
    print("ZIP")
    
    gam.zip.obj <-  gam(list(Shots ~
                               s(Score.Diff,k=score.df) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
                             + s(Team, bs="re") + s(season, bs="re"),
                             ~ s(Score.Diff,k=score.df) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
                             + s(Team, bs="re") + s(season, bs="re")),
                        family=ziplss(), data = our.df.cut %>% mutate(Team = factor(as.character(Team)),
                                                                      season = factor(as.character(season))),
                        subset = (season != year))
    
    
    our.true <- our.df.cut$Shots[our.df.cut$season == year]
    incl.ind <- which(!grepl("\\(Team\\)", names(coef(gam.zip.obj))) & !grepl("\\(season\\)", names(coef(gam.zip.obj))))
    n.included.vars <- length(incl.ind)
    
    
    reverse_cloglog <- function(cloglog_value) {
      return(1 - exp(-exp(cloglog_value)))
    }
    
    
    b <- coef(gam.zip.obj)[incl.ind]
    
    Xp <- predict(gam.zip.obj,
                  our.df.cut[our.df.cut$season == year, ], type="lpmatrix")[, incl.ind]
    
    Pois.ind <- 1:(which(names(b) == "(Intercept).1")-1)
    LogLog.ind <- c(1:length(b))[-Pois.ind]
    pr.Pois <- exp(Xp[, Pois.ind] %*% b[Pois.ind]) ## replicate predictions
    pr.LogLog <- reverse_cloglog(Xp[, LogLog.ind] %*% b[LogLog.ind])
    our.pred <- pr.Pois*pr.LogLog
    
    
    print(length(our.true))
    print(length(our.pred))
    
    if (year == 2009){
      pred.list.zip[[league]] <- data.frame(Year = year, 
                                            gameId = our.df.cut[our.df.cut$season == year, ]$gameId,
                                            Team = our.df.cut[our.df.cut$season == year, ]$Team,
                                            True = our.true,
                                            Pred = our.pred)
    } else {
      pred.list.zip[[league]] <- rbind(pred.list.zip[[league]],
                                       data.frame(Year = year, 
                                                  gameId = our.df.cut[our.df.cut$season == year, ]$gameId,
                                                  Team = our.df.cut[our.df.cut$season == year, ]$Team,
                                                  True = our.true,
                                                  Pred = our.pred))
    }
    
    save(file=paste0(ifelse(bucketed, "BUCKETED_", ""),
                     "LEAVE_SEASON_OUT_PREDICTIONS_NEW_TEAM_AND_SEASON_gam_zip_obj", 
                     "_", league,
                     #"_", string_add, 
                     "_Shots.Robj"),
         pred.list.zip)
    rm(gam.zip.obj)
    
    
    
    
  }
  
  
  cat("\n")
  cat("\n")
}



