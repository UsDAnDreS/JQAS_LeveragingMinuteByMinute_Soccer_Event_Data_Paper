#####
## Stage 4: Fitting the smoothing splines for the main modeling classes of:
##    * regular Poisson, 
##    * Negative Binomial,
##    * Zero-Inflated Poisson
## to the entire 2008-2023 dataset for each respective league. 
##
## Including mixed effects for team and season.
##
## [Disregard the "bucketed" part here. Just run it with "bucketed=FALSE", because
##  the bucketing will be done in a different file: "Stage4p05_JQAS_GAM_w_BUCKETED_EXTREME_CATEGORIES_POIS_NEGBIN_ZIP.R"]
##
##  Saving the resulting objects into (on the example of Negative Binomial):
##
## save(gam.nb.obj, file=paste0("Fitted_Objects/gam_nb_obj_Team_Season_Additive_Mixed_Eff_",
##                              ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
##                              ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
##                              "_", league,
##                              "_Shots.Robj"))
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
library(MASS)
# library(DHARMa)


bucketed <- FALSE

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


######
## COMPARING:
##    Regular Poisson,
##    Negative binomial,
##    Zero-Inflated Poisson
######


# load(file=paste0("gam_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Shots.Robj"))
# load(file=paste0("gam_nb_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Shots.Robj"))
# load(file=paste0("gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Shots.Robj"))


## Start of main loop

for (league in league.name[3:5]){
  
  if (reduced.ver) print("REDUCED")
  
  glm.obj <- glm.nb.obj <- glm.ziP.obj <- list()
  gam.obj <- gam.nb.obj <- gam.ziP.obj <- list()
  
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
  
  
  
  
  # gam.obj <- gam(Shots ~ 
  #       s(Score.Diff) + s(Minute.clean) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=3)
  #     + s(TeamSeason, bs="re") +  s(ID, bs="re"),
  #     family = "poisson", data= our.df.cut[1:10000,] %>% mutate(const = 0))
  
  
  ##  Too tough to understand what's going on here
  ##  A bunch of "sub-categories" of objects ("lme", "gam")
  
  # gamm.obj <- gamm(Shots ~ 
  #        s(Score.Diff) + s(Minute.clean) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=3),
  #      #+ s(season, const, by=Team, bs="gp") +  s(ID, bs="re"),
  #      family = "poisson",
  #      random = list(TeamSeason = ~1, ID=~1),  # Random intercepts for GroupID
  #      data= our.df.cut[1:10000,])
  
  
  
  # ## Takes WAY too long, even for smaller subset of data (just 10K)
  # gamm(Shots ~ 
  #        s(Score.Diff) + s(Minute.clean) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=3),
  #      #+ s(season, const, by=Team, bs="gp") +  s(ID, bs="re"),
  #      family = "poisson",
  #      random = list(Team = ~1),  # Random intercepts for GroupID
  #      correlation = corExp(form = ~ season_jitt | Team),
  #      data= our.df.cut[1:10000,] %>% mutate(season_jitt = season + runif(n(), -0.01, 0.01)))
  
  
  summary(gam.obj)
  
  ######
  ## Fitting SPLINE models
  ######
  
  
  if (keep.1st.half.extra){
    
    print("Poisson")
    
    redcard.df <- length(unique(our.df.cut$RedCard.Diff))
    
    gam.obj[[league]] <- gam(Shots ~
                               s(Score.Diff) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
                             + s(Team, bs="re") + s(season, bs="re"),
                             family = "poisson", data= our.df.cut)

    save(gam.obj, file=paste0("Fitted_Objects/gam_obj_Team_Season_Additive_Mixed_Eff_",
                              ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                              ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                              "_", league,
                              "_Shots.Robj"))

    rm(gam.obj)


    
    print("Neg Bin")
    gam.nb.obj[[league]] <- gam(Shots ~
                                  s(Score.Diff) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
                                + s(Team, bs="re") + s(season, bs="re"),
                                family = "nb", data= our.df.cut)

    save(gam.nb.obj, file=paste0("Fitted_Objects/gam_nb_obj_Team_Season_Additive_Mixed_Eff_",
                                 ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                                 ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                                 "_", league,
                                 "_Shots.Robj"))

    rm(gam.nb.obj)
    
    
    print("ZIPLSS")

    if (!reduced.ver){
      gam.ziP.obj[[league]] <- gam(list(Shots ~
                                          s(Score.Diff) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
                                        + s(Team, bs="re") + s(season, bs="re"),
                                        ~ s(Score.Diff) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
                                        + s(Team, bs="re") + s(season, bs="re")),
                                   family=ziplss(), data= our.df.cut)
    } else {
      gam.ziP.obj[[league]] <- gam(list(Shots ~
                                          s(Score.Diff) + half_id + Weighted.Win.Prob + RedCard.Diff,
                                        ~ s(Score.Diff) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
                                        + s(Team, bs="re") + s(season, bs="re")),
                                   family=ziplss(), data= our.df.cut)
    }

    save(gam.ziP.obj, file=paste0("Fitted_Objects/gam_ziplss_obj_Team_Season_Additive_Mixed_Eff_",
                                  ifelse(reduced.ver, "REDUCED", ""),
                                  ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                                  ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                                  "_", league,
                                  "_Shots.Robj"))

    rm(gam.ziP.obj)
    
    
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
