######
##    Stage 4p1: Testing the GameID random effect significance, for the most robust modeling class (Negative Binomial)
##                We also did bucketing, as it was shown to give the best performance.
##
##    If running for all seasons at once - it wouldn't run (neither locally, nor on the server). Too much memory required.
##    Hence we ended up doing just a single season at a time.
##
##    For each season, we fit a model with GameID random effect, a model without it, and ran an ANOVA test comparing them.
##    It ended up being insignificant in vast majority of cases.
######

library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
# library(DHARMa)


bucketed <- TRUE


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we want to still keep 1st half extra time
keep.1st.half.extra <- TRUE



league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")


######
## COMPARING:
##    Regular Poisson,
##    Negative binomial,
##    Zero-Inflated Poisson
######

glm.obj <- glm.nb.obj <- glm.ziP.obj <- list()
gam.obj <- gam.nb.obj <- gam.ziP.obj <- list()


# load(file=paste0("gam_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))
# load(file=paste0("gam_nb_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))
# load(file=paste0("gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))




print("Corners")


for (league in league.name){
  
  print(league)
  
  if (bucketed) print("BUCKETED")
  
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
                                        season = factor(season),
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
  
  
  if (keep.1st.half.extra){
    
  print("Neg Bin")
  
  for (year in sort(unique(our.df.cut$season))){
    print(year)
    
    redcard.df <- length(unique(our.df.cut[our.df.cut$season == year,]$RedCard.Diff))
    score.df <- ifelse(length(unique(our.df.cut[our.df.cut$season == year,]$Score.Diff)) < 10,
                       length(unique(our.df.cut[our.df.cut$season == year,]$Score.Diff)),
                       10)
    
  gam.nb.obj.w.ID <- gam(Corners ~ 
                           s(Score.Diff, k=score.df) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
                              + s(Team, bs="re") +  s(ID, bs="re"),
                              family = "nb", data= our.df.cut %>% filter(season == year))
  
  gam.nb.obj.no.ID <- gam(Corners ~ 
                            s(Score.Diff, k=score.df) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
                              + s(Team, bs="re"),
                              family = "nb", data= our.df.cut %>% filter(season == year))
  
  
  anova.obj <- anova(gam.nb.obj.no.ID, gam.nb.obj.w.ID, test = "Chisq")
  print(anova.obj$`Pr(>Chi)`)
  
  rm(gam.nb.obj.w.ID)
  rm(gam.nb.obj.no.ID)
  
  }
  
  } else {
    
  }
  
  
  
  cat("\n")
}


