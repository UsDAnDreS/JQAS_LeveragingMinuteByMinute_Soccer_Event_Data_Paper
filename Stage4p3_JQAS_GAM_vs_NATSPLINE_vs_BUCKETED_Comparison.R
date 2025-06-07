#######
## Stage 4p3: Doing the AIC/BIC comparison between the four approaches considered:
##            * Smoothing splines, non-bucketed extremes
##            * Smoothing splines, bucketed extremes
##            * Natural splines, non-bucketed extremes
##            * Natural splines, bucketed extremes 
##
##            To be used in Supplement, Tables 2 & 3.
##
## NOTE: If you want to switch from one offensive statistic to another, 
##       use "Ctrl+F" with "Match Case" turned out in order 
##       to replace all occurrences of "Corner" for "Shot" (), or vice versa.
#########



library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
# library(DHARMa)

## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we want to still keep 1st half extra time
keep.1st.half.extra <- TRUE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")


for (league in league.name){
  
  gam.nb.obj.main <- gam.nb.obj.bucketed.main <- list()
  ns.nb.obj.main <- ns.nb.obj.bucketed.main <- list()
  
  print(league)
  


    load(file=paste0("Fitted_Objects/gam_nb_obj_Team_Season_Additive_Mixed_Eff_",  
                     ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                     ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                     "_", league,
                     "_Corners.Robj"))
  
  
  gam.nb.obj.main[[league]] <- gam.nb.obj[[league]]
  rm(gam.nb.obj)
  
  

  hey <- load(file=paste0("Fitted_Objects/GAM_W_BUCKETED_EXTREME_CATEGORIES_gam_nb_obj_Team_AND_Season_Mixed_Eff_",  
                                                ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                                                ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                                                "_", league,
                                                "_Corners.Robj"))
  if (hey == "ns.obj"){
    gam.nb.obj.bucketed.main[[league]] <- ns.obj[[league]]
    rm(ns.obj)
  } else {
    gam.nb.obj.bucketed.main[[league]] <- gam.nb.obj[[league]]
    rm(gam.nb.obj)
  }
  
  
  load(file=paste0("Fitted_Objects/NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_gam_nb_obj_Team_AND_Season_Mixed_Eff_",  
                   ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                   ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                   "_", league,
                   "_Corners.Robj"))
  ns.nb.obj.bucketed.main[[league]] <- ns.obj[[league]]
  rm(ns.obj)
  
  
  load(file=paste0("Fitted_Objects/NAT_SPLINE_gam_nb_obj_Team_AND_Season_Mixed_Eff_",  
                   ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                   ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                   "_", league,
                   "_Corners.Robj"))
  ns.nb.obj.main[[league]] <- ns.obj[[league]]
  rm(ns.obj)
  
  
  
  print(AIC(gam.nb.obj.main[[league]],
            gam.nb.obj.bucketed.main[[league]],
            ns.nb.obj.bucketed.main[[league]],
            ns.nb.obj.main[[league]]
            
  ))
  
  print(BIC(gam.nb.obj.main[[league]],
            gam.nb.obj.bucketed.main[[league]],
            ns.nb.obj.bucketed.main[[league]],
            ns.nb.obj.main[[league]]
  ))
  
  
}


#########
### MAIN TAKEAWAYS:
###   * Nat Splines, while simpler, are not doing as well.
###   * The BUCKETED GAMS though actually DO outperform the regular approach in 4/5 leagues !!!
###       (according to BOTH the AIC & BIC.. although in several cases it's relatively SLIGHT)
#########




#########
### SHOTS
#########


# [1] "Bundesliga"
# df      AIC
# gam.nb.obj.main[[league]]          74.40680 693475.7
# gam.nb.obj.bucketed.main[[league]] 71.14394 693432.3
# ns.nb.obj.bucketed.main[[league]]  63.06023 693910.5
# ns.nb.obj.main[[league]]           63.01746 693887.9
# df      BIC
# gam.nb.obj.main[[league]]          74.40680 694343.2
# gam.nb.obj.bucketed.main[[league]] 71.14394 694261.8
# ns.nb.obj.bucketed.main[[league]]  63.06023 694645.7
# ns.nb.obj.main[[league]]           63.01746 694622.6
# [1] "SerieA"
# df      AIC
# gam.nb.obj.main[[league]]          77.41399 812796.5
# gam.nb.obj.bucketed.main[[league]] 76.10625 812576.1
# ns.nb.obj.bucketed.main[[league]]  66.99297 813079.8
# ns.nb.obj.main[[league]]           66.97127 813058.8
# df      BIC
# gam.nb.obj.main[[league]]          77.41399 813711.2
# gam.nb.obj.bucketed.main[[league]] 76.10625 813475.3
# ns.nb.obj.bucketed.main[[league]]  66.99297 813871.3
# ns.nb.obj.main[[league]]           66.97127 813850.0
# [1] "LaLiga"
# df      AIC
# gam.nb.obj.main[[league]]          84.57322 702611.1
# gam.nb.obj.bucketed.main[[league]] 78.01018 702608.5
# ns.nb.obj.bucketed.main[[league]]  69.90713 703010.3
# ns.nb.obj.main[[league]]           69.90124 703016.7
# df      BIC
# gam.nb.obj.main[[league]]          84.57322 703602.5
# gam.nb.obj.bucketed.main[[league]] 78.01018 703522.9
# ns.nb.obj.bucketed.main[[league]]  69.90713 703829.8
# ns.nb.obj.main[[league]]           69.90124 703836.1
# [1] "Ligue1"
# df      AIC
# gam.nb.obj.main[[league]]          82.31324 788717.4
# gam.nb.obj.bucketed.main[[league]] 75.75851 788846.0
# ns.nb.obj.bucketed.main[[league]]  66.97743 789257.5
# ns.nb.obj.main[[league]]           66.89757 789238.3
# df      BIC
# gam.nb.obj.main[[league]]          82.31324 789692.6
# gam.nb.obj.bucketed.main[[league]] 75.75851 789743.6
# ns.nb.obj.bucketed.main[[league]]  66.97743 790051.0
# ns.nb.obj.main[[league]]           66.89757 790030.9
# [1] "PremierLeague"
# df      AIC
# gam.nb.obj.main[[league]]          84.12987 720369.5
# gam.nb.obj.bucketed.main[[league]] 81.88005 720360.4
# ns.nb.obj.bucketed.main[[league]]  70.74164 720770.3
# ns.nb.obj.main[[league]]           70.73303 720681.8
# df      BIC
# gam.nb.obj.main[[league]]          84.12987 721355.5
# gam.nb.obj.bucketed.main[[league]] 81.88005 721320.0
# ns.nb.obj.bucketed.main[[league]]  70.74164 721599.4
# ns.nb.obj.main[[league]]           70.73303 721510.8





########
### CORNERS
########

### ALSO BEATS 4/5 (Ligue 1 is the only one where it's behind regular GAM)

# 
# [1] "Bundesliga"
# df      AIC
# gam.nb.obj.main[[league]]          68.38996 343880.7
# gam.nb.obj.bucketed.main[[league]] 67.65371 343844.3
# ns.nb.obj.bucketed.main[[league]]  58.62775 344070.1
# ns.nb.obj.main[[league]]           58.53382 344056.5
# df      BIC
# gam.nb.obj.main[[league]]          68.38996 344678.1
# gam.nb.obj.bucketed.main[[league]] 67.65371 344633.0
# ns.nb.obj.bucketed.main[[league]]  58.62775 344753.6
# ns.nb.obj.main[[league]]           58.53382 344738.9
# [1] "SerieA"
# df      AIC
# gam.nb.obj.main[[league]]          68.29995 413634.0
# gam.nb.obj.bucketed.main[[league]] 68.38054 413629.8
# ns.nb.obj.bucketed.main[[league]]  56.06845 413887.7
# ns.nb.obj.main[[league]]           56.04376 413865.6
# df      BIC
# gam.nb.obj.main[[league]]          68.29995 414440.9
# gam.nb.obj.bucketed.main[[league]] 68.38054 414437.7
# ns.nb.obj.bucketed.main[[league]]  56.06845 414550.2
# ns.nb.obj.main[[league]]           56.04376 414527.8
# [1] "LaLiga"
# df      AIC
# gam.nb.obj.main[[league]]          73.41875 372841.5
# gam.nb.obj.bucketed.main[[league]] 73.33610 372732.8
# ns.nb.obj.bucketed.main[[league]]  63.74279 373025.3
# ns.nb.obj.main[[league]]           63.72858 373033.7
# df      BIC
# gam.nb.obj.main[[league]]          73.41875 373702.1
# gam.nb.obj.bucketed.main[[league]] 73.33610 373592.5
# ns.nb.obj.bucketed.main[[league]]  63.74279 373772.5
# ns.nb.obj.main[[league]]           63.72858 373780.8
# [1] "Ligue1"
# df      AIC
# gam.nb.obj.main[[league]]          66.94146 405624.2
# gam.nb.obj.bucketed.main[[league]] 66.16139 405661.2
# ns.nb.obj.bucketed.main[[league]]  55.08019 405913.8
# ns.nb.obj.main[[league]]           54.68308 405865.4
# df      BIC
# gam.nb.obj.main[[league]]          66.94146 406417.3
# gam.nb.obj.bucketed.main[[league]] 66.16139 406445.1
# ns.nb.obj.bucketed.main[[league]]  55.08019 406566.3
# ns.nb.obj.main[[league]]           54.68308 406513.2
# [1] "PremierLeague"
# df      AIC
# gam.nb.obj.main[[league]]          69.77619 380185.8
# gam.nb.obj.bucketed.main[[league]] 74.75481 380089.1
# ns.nb.obj.bucketed.main[[league]]  59.09770 380360.3
# ns.nb.obj.main[[league]]           58.88712 380311.6
# df      BIC
# gam.nb.obj.main[[league]]          69.77619 381003.6
# gam.nb.obj.bucketed.main[[league]] 74.75481 380965.2
# ns.nb.obj.bucketed.main[[league]]  59.09770 381053.0
# ns.nb.obj.main[[league]]           58.88712 381001.7
