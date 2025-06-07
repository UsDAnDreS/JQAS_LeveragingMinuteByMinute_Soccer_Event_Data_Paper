#####
## Stage 4p05: Fitting simpler natural cubic splines, where we fix the knots at most typical values,
##            e.g., -2:2 for score diff, -1:1 for red card diff,
##                  making df for win prob and minute's effects =5 (lower number than picked by leave-one-out CV)
## 
## (Only doing it for Negative Binomial, which was shown to be the most robus modeling class)
##
##  Saving the resulting objects into:
## save(ns.obj, file=paste0("Fitted_Objects/NAT_SPLINE_gam_nb_obj_Team_AND_Season_Mixed_Eff_",
##                          ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
##                          ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
##                          "_", league,
##                          "_Shots.Robj"))
##
## NOTE: If you want to switch from one offensive statistic to another, 
##       use "Ctrl+F" with "Match Case" turned out in order 
##       to replace all occurrences of "Corner" for "Shot" (), or vice versa.
####


######
## "LeagueName_ifelse(remove.extra, "_NO_EXTRA_TIME", "")_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"
######

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

for (league in league.name[1:2]){
  
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
  
  
  
  ######
  ## Fitting SPLINE models
  ######
  
  
  if (keep.1st.half.extra){
    
   # print("Poisson")
    
    redcard.df <- length(unique(our.df.cut$RedCard.Diff))
    
    # knots.scorediff <- sort(unique(our.df.cut$Score.Diff))
    knots.scorediff <- c(-2:2)
    knots.redcarddiff <- c(-1:1)
    
    # table(our.df.cut$Score.Diff)
    
    
    # knots.scorediff.scaled <- sort(unique(scale(our.df.cut$Score.Diff)))
    # knots.redcarddiff.scaled <- sort(unique(scale(our.df.cut$RedCard.Diff)))
    
    ## Based on the GAM Neg Bin fits, we have:
    ##    DF for minute: 8
    ##    DF for weighted win prob: 5.5 (let's make it 5)
    
   # summary(gam.nb.obj[[league]])
    
    df.minute <- 5
    df.winprob <- 5
    
    
    # Create spline terms
   # Score.Diff.spline <- ns(our.df.cut$Score.Diff, knots = knots.scorediff[-c(1, length(knots.scorediff))])
  #  RedCard.Diff.spline <- ns(our.df.cut$RedCard.Diff, knots = knots.redcarddiff[-c(1, length(knots.redcarddiff))])
      

    print("Neg Bin")
    # library(parallel)
    # cl <- makeCluster(detectCores() - 1)
    # 
    # data <- our.df.cut %>% filter(season %in% c(2013))
    # clusterExport(cl, list("data", "glmer.nb"))
    # result <- parLapply(cl, 1:10, function(i) {
    #   glmer.nb(Shots ~
    #              ns(Score.Diff, knots = c(-3:3)) + (1|Team) , data = data, control = control)
    # })
    # stopCluster(cl)
    
    
    #### Use gam(), with SMOOTHS for RANDOM EFFECTS
    ###  but then "ns()" and linear terms for other stuff
    
    ns.obj[[league]] <- gam(Shots ~
                                 ns(Score.Diff, knots = c(-2:2)) +
                                 ns(RedCard.Diff, knots = c(-1:1)) +
                                 half_id*ns(Minute.clean, df=5) + 
                              Weighted.Win.Prob + HomeAway
                            + s(Team, bs="re") + s(season, bs="re"),
                                 family = "nb", 
                               # verbose=TRUE,
                               data= our.df.cut 
                            #%>% filter(season %in% c(2009))
    )
    
    # plot(ns.obj[[league]], all.terms=TRUE)
    
    # install.packages("visreg")
    # library(visreg)
    # visreg(ns.obj[[league]], "Score.Diff", type = "contrast", 
    #        scale = "response", main = "Effect of Score.Diff")
    # 
    # install.packages("effects")
    # library(effects)
    # # Extract the effect of Score.Diff using ns()
    # score_diff_effect <- effect("Score.Diff", ns.obj[[league]])
    # 
    # # Plot the effect of Score.Diff
    # plot(score_diff_effect, main = "Effect of Score.Diff (Natural Spline)")
    
    
    
    
   #  summary(ns.obj[[league]])
    
    
    
    # ns.obj[[league]] <- glm.nb(Shots ~
    #                                ns(Score.Diff, knots = c(-3:3)) +
    #                                 ns(RedCard.Diff, knots = knots.redcarddiff[-c(1, length(knots.redcarddiff))]) +
    #                                  half_id + ns(Minute.clean, df = df.minute) + half_id:ns(Minute.clean, df=df.minute) +
    #                                   ns(Weighted.Win.Prob, df=df.winprob)  + HomeAway
    #                               # + (1|Team) 
    #                              
    #                              # + (1|season)
    #                              ,
    #                              #  family = "nb", 
    #                              # verbose=TRUE,
    #                              data= our.df.cut %>% filter(season %in% c(2013))
    # )
    # 
    # 
    # 
    # ns.obj[[league]] <- glmer.nb(Shots ~
    #                               ns(Score.Diff, knots = c(-3:3)) +
    #                             #  ns(RedCard.Diff, knots = knots.redcarddiff[-c(1, length(knots.redcarddiff))]) +
    #                            #   half_id + ns(Minute.clean, df = df.minute) + half_id:ns(Minute.clean, df=df.minute) +
    #                           #    ns(Weighted.Win.Prob, df=df.winprob)  + HomeAway 
    #                             + (1|Team) 
    #                             
    #                           # + (1|season)
    #                             ,
    #                            #  family = "nb", 
    #                            verbose=TRUE,
    #                           control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
    #                             data= our.df.cut %>% filter(season %in% c(2013))
    #                            # %>% 
    #                            #   mutate(Score.Diff = scale(Score.Diff),
    #                            #          RedCard.Diff = scale(RedCard.Diff),
    #                            #          Minute.clean = scale(Minute.clean),
    #                            #          Weighted.Win.Prob = scale(Weighted.Win.Prob))
    #                          #  %>% filter(season %in% c(2009, 2013))
    #                              )

    save(ns.obj, file=paste0("Fitted_Objects/NAT_SPLINE_gam_nb_obj_Team_AND_Season_Mixed_Eff_",
                                 ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                                 ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                                 "_", league,
                                 "_Shots.Robj"))

    rm(ns.obj)


    
    
  
    
  } else {
    
    
    # print("ZIP")
    # gam.ziP.obj[[league]] <- gam(Shots ~
    #                                s(Score.Diff) + s(Minute.clean) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5)
    #                              + s(TeamSeason, bs="re"),
    #                              family="ziP", data= our.df.cut)
  }
  
  
  
  #print(league)
  # print(BIC(#glm.obj[[league]], glm.nb.obj[[league]], glm.ziP.obj[[league]],
  #   gam.obj[[league]], gam.nb.obj[[league]]
  #   , gam.ziP.obj[[league]]
  # ))
  
  cat("\n")
  
  ## End of main loop
}
