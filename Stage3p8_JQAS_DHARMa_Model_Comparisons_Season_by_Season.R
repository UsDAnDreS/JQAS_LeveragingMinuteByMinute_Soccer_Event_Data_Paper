####
## Stage 3p8: We conduct the DHARMa diagnostics for various model classes under consideration,
##            such as: "Poisson", "NegBin", "Normal", "LogNormal".
##            The fits are done for each season individually, 
##            which helps with keeping the p-values meaningful (due to moderate sample sizes).
##
##     All results are saved in a large pdf:
##      pdf(file="DHARMa_ZeroInflation_Overdispersion_Test_Results_Corners.pdf")
##      , pages of which also contain plots that were included in Figures 2 & 3 of Supplementary materials.
##
##     Moreover, p-values for all goodness-of-fit significance tests
##    (uniformity, overdispersion, outliers, zero-inflation)
##    are saved into an .Robj file as follows:
##    save(all.pvals, file = paste0("Fitted_Objects/",
##         ifelse(bucketed, "BUCKETED_", ""),
##         "all_pvals_DHARMa_BY_SEASON_Corners.Robj"))
##
## NOTE: If you want to switch from one offensive statistic to another, 
##       use "Ctrl+F" with "Match Case" turned out in order 
##       to replace all occurrences of "Corner" for "Shot" (), or vice versa.
####


## https://stats.stackexchange.com/questions/583279/compare-glm-aics-with-different-likelihoods

library(mgcv)
library(tidyverse)
library(splines)
library(MASS)

#install.packages("DHARMa")
# install.packages("mgcViz")
library(DHARMa)


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



pdf(file="DHARMa_ZeroInflation_Overdispersion_Test_Results_Corners.pdf")

all.pvals <- list()

for (league in league.name){
  
  all.pvals[[league]] <- list()
  all.pvals[[league]][["Poisson"]] <- list()
  all.pvals[[league]][["NegBin"]] <- list()
  all.pvals[[league]][["Normal"]] <- list()
  all.pvals[[league]][["LogNormal"]] <- list()
  
  
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
  
  
  
  for (year in levels(our.df.cut$season)){
  
    print(year)
  
    gam.obj <- gam.nb.obj <- gam.ziP.obj <- gam.ziplss.obj <- list()
    gam.normal.obj <- gam.lognormal.obj  <- list()
    
  
  redcard.df <- length(unique(our.df.cut[our.df.cut$season == year,]$RedCard.Diff))
  score.df <- ifelse(length(unique(our.df.cut[our.df.cut$season == year,]$Score.Diff)) < 10,
                     length(unique(our.df.cut[our.df.cut$season == year,]$Score.Diff)),
                     10)
  
  
  gam.obj[[league]] <- gam(Corners ~
                             s(Score.Diff, k=score.df) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
                           + s(Team, bs="re"),
                           family = "poisson", data= our.df.cut %>% filter(season == year))

  sim.res <- simulateResiduals(gam.obj[[league]])
  
  plot(sim.res, title=paste0(league, ", Poisson"))
  
  test.res.obj <- testResiduals(sim.res, plot=F)
  print(testDispersion(sim.res, plot=TRUE))
  
  zeroinfl.test.obj <- testZeroInflation(sim.res)
  
  all.pvals[[league]][["Poisson"]][[as.character(year)]] <- c(Uniformity = test.res.obj$uniformity$p.value,
                           Dispersion = test.res.obj$dispersion$p.value,
                           Outliers = test.res.obj$outliers$p.value,
                           ZeroInfl = zeroinfl.test.obj$p.value)
  

  # print(testZeroInflation(sim.res))
  # print(testDispersion(sim.res))
  print(test.res.obj)
  print(zeroinfl.test.obj)
  
  rm(gam.obj)
  rm(sim.res)


  print("Neg Bin")
  gam.nb.obj[[league]] <- gam(Corners ~
                                s(Score.Diff, k=score.df) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
                              + s(Team, bs="re"),
                              family = "nb", data= our.df.cut %>% filter(season == year))

  sim.res <- simulateResiduals(gam.nb.obj[[league]])
  
  plot(sim.res, title=paste0(league, ", Negative Binomial"))  
  
  test.res.obj <- testResiduals(sim.res, plot=F)
  print(testDispersion(sim.res, plot=TRUE))
  
  zeroinfl.test.obj <- testZeroInflation(sim.res)
  
  all.pvals[[league]][["NegBin"]][[as.character(year)]] <- c(Uniformity = test.res.obj$uniformity$p.value,
                                        Dispersion = test.res.obj$dispersion$p.value,
                                        Outliers = test.res.obj$outliers$p.value,
                                        ZeroInfl = zeroinfl.test.obj$p.value)
  
  
  
  # print(testZeroInflation(sim.res))
  # print(testDispersion(sim.res))
  print(test.res.obj)
  print(zeroinfl.test.obj)
  
  rm(gam.nb.obj)
  rm(sim.res)
  
  

  

  
  
  
  # print("ZIPLSS")
  # 
  # if (!reduced.ver){
  #   gam.ziP.obj[[league]] <- gam(list(Corners ~
  #                                       s(Score.Diff, k=score.df) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
  #                                     + s(Team, bs="re"),
  #                                     ~ s(Score.Diff, k=score.df) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
  #                                     + s(Team, bs="re")),
  #                                family=ziplss(), data= our.df.cut %>% filter(season == year))
  # } else {
  #   gam.ziP.obj[[league]] <- gam(list(Corners ~
  #                                       s(Score.Diff, k=score.df) + half_id + Weighted.Win.Prob + RedCard.Diff,
  #                                     ~ s(Score.Diff, k=score.df) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
  #                                     + s(Team, bs="re")),
  #                                family=ziplss(), data= our.df.cut %>% filter(season == year))
  # }
  # 
  # 
  # sim.res <- simulateResiduals(gam.ziP.obj[[league]])
  # 
  # plot(sim.res, title=paste0(league, ", Zero-Inflated Poisson"))
  # 
  # test.res.obj <- testResiduals(sim.res, plot=F)
  # testUniformity(sim.res)
  # 
  # plot(sim.res)
  # testZeroInflation(sim.res)
  # testDispersion(sim.res)
  # 
  # # hist(resid(gam.ziP.obj[[league]]),
  # #      main="Neg Bin, resid() function")

  
  
  
  
  print("NORMAL")
  gam.normal.obj[[league]] <- gam(Corners ~
                                    s(Score.Diff, k=score.df) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
                              + s(Team, bs="re"),
                              family = gaussian(), data= our.df.cut %>% filter(season == year))
  
  sim.res <- simulateResiduals(gam.normal.obj[[league]])
  
  plot(sim.res, title=paste0(league, ", Gaussian GLM"))
  
  test.res.obj <- testResiduals(sim.res, plot=F)
  # print(testDispersion(sim.res, plot=TRUE))
  
  
  #test.res.obj <- testResiduals(sim.res)
  #zeroinfl.test.obj <- testZeroInflation(sim.res)
  
  all.pvals[[league]][["Normal"]][[as.character(year)]] <- c(Uniformity = test.res.obj$uniformity$p.value,
                                       Dispersion = test.res.obj$dispersion$p.value,
                                       Outliers = test.res.obj$outliers$p.value,
                                       ZeroInfl = NA)
  
  
  
  # print(testZeroInflation(sim.res))
  # print(testDispersion(sim.res))
  print(test.res.obj)
  print(zeroinfl.test.obj)
  
  rm(gam.normal.obj)
  rm(sim.res)
  
  
  
  
  
  print("LOG-NORMAL")
  gam.lognormal.obj[[league]] <- gam(Corners ~
                                       s(Score.Diff, k=score.df) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=redcard.df)
                              + s(Team, bs="re"),
                              family = gaussian(link="log"), data= our.df.cut %>% filter(season == year))
  
  sim.res <- simulateResiduals(gam.lognormal.obj[[league]])
  
  
  plot(sim.res, title=paste0(league, ", Log-Link Gaussian GLM"))
  # testZeroInflation(sim.res)
  # testDispersion(sim.res)
  
  test.res.obj <- testResiduals(sim.res, plot=F)
  # print(testDispersion(sim.res, plot=TRUE))
  
  all.pvals[[league]][["LogNormal"]][[as.character(year)]] <- c(Uniformity = test.res.obj$uniformity$p.value,
                                                             Dispersion = test.res.obj$dispersion$p.value,
                                                             Outliers = test.res.obj$outliers$p.value,
                                                             ZeroInfl = NA)
  
  # print(testZeroInflation(sim.res))
  # print(testDispersion(sim.res))
  print(test.res.obj)
  print(zeroinfl.test.obj)
  
  rm(gam.lognormal.obj)
  rm(sim.res)
  
  
  save(all.pvals, file = paste0("Fitted_Objects/",
                                ifelse(bucketed, "BUCKETED_", ""),
                                "all_pvals_DHARMa_BY_SEASON_Corners.Robj"))
  
  }
  
}

dev.off()
