#####
## Stage 4p05:  Fitting simpler natural cubic splines, where we fix the knots at most typical values,
##            e.g., -2:2 for score diff, -1:1 for red card diff,
##                  making df for win prob and minute's effects =5 (lower number than picked by leave-one-out CV)
## 
## (Only doing it for Negative Binomial, which was shown to be the most robus modeling class)
##
##  Saving the resulting objects into:
## save(ns.obj, file=paste0("Fitted_Objects/NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_gam_nb_obj_Team_AND_Season_Mixed_Eff_",
##                          ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
##                          ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
##                          "_", league,
##                          "_Shots.Robj"))
##
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


## Start of main loop

for (league in league.name){
  
  ns.obj <- ns.nb.obj <- ns.ziP.obj <- list()
  
  
  # load(file=paste0("Fitted_Objects/gam_nb_obj_Team_Season_Additive_Mixed_Eff_",  
  #                  ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
  #                  ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
  #                  "_", league,
  #                  "_Shots.Robj"))
  
  print("Shots")
  
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
  
  
  #########
  ## BUCKETING:
  ##    * Putting 3+ scores as "3"
  ##    * Putting 45+ minutes as "45"
  ########
  
  our.df.cut <- our.df.cut %>%
    mutate(Score.Diff = ifelse(abs(Score.Diff) >= 3, sign(Score.Diff)*3, Score.Diff),
           RedCard.Diff = ifelse(abs(RedCard.Diff) >= 2, sign(RedCard.Diff)*2, RedCard.Diff),
           Minute.clean = ifelse(Minute.clean >= 45, 45, Minute.clean))
  
  
  ######
  ## Fitting SPLINE models
  ######
  
  
  if (keep.1st.half.extra){
    
    # print("Poisson")
    
    redcard.df <- length(unique(our.df.cut$RedCard.Diff))
    
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
    
    ns.obj[[league]] <- gam(Shots ~
                              ns(Score.Diff, knots = c(-2:2)) +
                              ns(RedCard.Diff, knots = c(-1:1)) +
                              half_id*ns(Minute.clean, df=5) + 
                              Weighted.Win.Prob + HomeAway
                            + s(Team, bs="re") + s(season, bs="re")
                            ,
                            family = "nb", 
                            # verbose=TRUE,
                            data= our.df.cut 
                           # %>% filter(season %in% c(2009))
    )
    


    save(ns.obj, file=paste0("Fitted_Objects/NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_gam_nb_obj_Team_AND_Season_Mixed_Eff_",
                             ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                             ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                             "_", league,
                             "_Shots.Robj"))
    
    rm(ns.obj)
    
    
    
    
    
    
  } else {
    

  }
  

  cat("\n")
  
  ## End of main loop
}
