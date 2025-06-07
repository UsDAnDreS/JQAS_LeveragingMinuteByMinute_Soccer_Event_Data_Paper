########
##  Stage 5: Running the Leave-One-Season-Out Cross Validation (LOSOCV) 
##           for the simpler natural cubic splines models, where we fix the knots at most typical values,
##            e.g., -2:2 for score diff, -1:1 for red card diff, making df for win prob and minute's effects =5 (lower number than picked by leave-one-out CV)
##          [no bucketed extreme categories - leaving values as is]
##
##           To save the out-of-sample predictions in .Robj files as follows:
##
## save(file=paste0("LEAVE_SEASON_OUT_PREDICTIONS_NAT_SPLINE_TEAM_AND_SEASON_gam_nb_obj", 
##                  "_", league,
##                  #"_", string_add, 
##                  "_Corners.Robj"),
##      pred.list.nb)
##
##  Those files are to be used later for reporting LOSOCV errors and calibration plots.
##
## NOTE: If you want to switch from one offensive statistic to another, 
##       use "Ctrl+F" with "Match Case" turned out in order 
##       to replace all occurrences of "Corner" for "Shot" (), or vice versa.
####



library(mgcv)
library(tidyverse)
library(splines)
library(lme4)
library(MASS)
# library(DHARMa)

## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we want to still keep 1st half extra time
keep.1st.half.extra <- TRUE

## Using the reduced version of ZIP?
reduced.ver <- FALSE

dir.create("Fitted_Objects/")

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

# league <- league.name[1]

pred.list <- pred.list.nb <- pred.list.zip <- list()


## Start of main loop

for (league in league.name[5]){
  
  ns.obj <- ns.nb.obj <- ns.ziP.obj <- list()
  
  
  # load(file=paste0("Fitted_Objects/gam_nb_obj_Team_Season_Additive_Mixed_Eff_",  
  #                  ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
  #                  ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
  #                  "_", league,
  #                  "_Corners.Robj"))
  
  print("Corners")
  
  print(league)
  
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
  # our.df.cut <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
  # print(dim(our.df.cut))
  
  
  
  ############
  ############
  
  our.df.cut  <- our.df.cut  %>%
    mutate(abs.Score.Diff = abs(Score.Diff),
           abs.RedCard.Diff = abs(RedCard.Diff))
  
  our.df.cut  <- our.df.cut  %>%
    mutate(Period.ID = group_indices(our.df.cut, .dots=c("ID", "Minute.clean", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
    arrange(Period.ID) %>%
    mutate(Period.ID = factor(Period.ID))
  
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
  
  
  
  ######
  ## Fitting SPLINE models
  ######
  
  for (year in 2009:2023){
    
    if (league == "LaLiga" & year %in% c(2016, 2022)) next;
    if (league %in% c("SerieA") & year == 2016) next;
    if (league == "Ligue1" & year == 2022) next;
    if (league == "PremierLeague" & year == 2009) next;
    
    print(year)
    
  
  if (keep.1st.half.extra){
    
    # print("Poisson")
    
    redcard.df <- length(unique(our.df.cut$RedCard.Diff))
    
    # knots.scorediff <- sort(unique(our.df.cut$Score.Diff))
    knots.scorediff <- c(-2:2)
    knots.redcarddiff <- sort(unique(our.df.cut$RedCard.Diff))
 
    ## Based on the GAM Neg Bin fits, we have:
    ##    DF for minute: 8
    ##    DF for weighted win prob: 5.5 (let's make it 5)
    
    # summary(gam.nb.obj[[league]])
    
    df.minute <- 5
    df.winprob <- 5
    

    print("Neg Bin")
    
    #### Use gam(), with SMOOTHS for RANDOM EFFECTS
    ###  but then "ns()" and linear terms for other stuff
    
    gam.nb.obj <- gam(Corners ~
                              ns(Score.Diff, knots = c(-2:2)) +
                              ns(RedCard.Diff, knots = c(-1:1)) +
                              half_id*ns(Minute.clean, df=5) + 
                              Weighted.Win.Prob + HomeAway
                            + s(Team, bs="re") + s(season, bs="re"),
                            family = "nb", 
                            # verbose=TRUE,
                            data= our.df.cut,
                            subset = (season != year)
    )
    
    our.true <- our.df.cut$Corners[our.df.cut$season == year]
    
    our.true <- our.df.cut$Corners[our.df.cut$season == year]
    incl.ind <- which(!grepl("\\(Team\\)", names(coef(gam.nb.obj))) & !grepl("\\(season\\)", names(coef(gam.nb.obj))))
    n.included.vars <- length(incl.ind)
    
    b <- coef(gam.nb.obj)[incl.ind]
    Xp <- predict(gam.nb.obj,
                  our.df.cut[our.df.cut$season == year, ], type="lpmatrix")[, incl.ind]
    our.pred <- exp(Xp %*% b)
    
    print(length(our.true))
    print(length(our.pred))
    
    if (year == 2009){
      pred.list.nb[[league]] <- data.frame(Year = year, 
                                           gameId = our.df.cut[our.df.cut$season == year, ]$gameId,
                                           Team = our.df.cut[our.df.cut$season == year, ]$Team,
                                           True = our.true,
                                           Pred = our.pred)
    } else {
      pred.list.nb[[league]] <- rbind(pred.list.nb[[league]],
                                      data.frame(Year = year, 
                                                 gameId = our.df.cut[our.df.cut$season == year, ]$gameId,
                                                 Team = our.df.cut[our.df.cut$season == year, ]$Team,
                                                 True = our.true,
                                                 Pred = our.pred))
    }
    
    save(file=paste0("LEAVE_SEASON_OUT_PREDICTIONS_NAT_SPLINE_TEAM_AND_SEASON_gam_nb_obj", 
                     "_", league,
                     #"_", string_add, 
                     "_Corners.Robj"),
         pred.list.nb)
    rm(gam.nb.obj)
    
  
    
    
    
  } else {
    
  }

  cat("\n")
  
  ## End of main loop
}

}