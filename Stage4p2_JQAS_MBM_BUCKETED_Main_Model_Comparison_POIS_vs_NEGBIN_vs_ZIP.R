#######
## Stage 4p2: Doing the AIC/BIC comparison between the main smoothing spline models
##            with bucketed extreme categories
##            for regular Poisson, Negative Binomial, Zero-Inflated Poisson
##
##            To be used in Appendix of the main manuscript, Tables 6 & 7.
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



for (league in league.name[5]){
  
  gam.obj.main <- gam.nb.obj.main <- gam.ziP.obj.main <- gam.ziplss.obj.main <- list()
  gam.normal.obj.main <- gam.log.normal.obj.main  <- list()
  
  
  print(league)
  
  # load(file=paste0("Fitted_Objects/gam_obj_Team_Season_Additive_Mixed_Eff_",  
  #                  ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
  #                  ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
  #                  "_", league,
  #                  "_Corners.Robj"))
  load(file=paste0("Fitted_Objects/GAM_W_BUCKETED_EXTREME_CATEGORIES_gam_obj_Team_AND_Season_Mixed_Eff_",  
                   ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                   ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                   "_", league,
                   "_Corners.Robj"))
  gam.obj.main[[league]] <- gam.obj[[league]]
  rm(gam.obj)
  
  
  # gam.nb.obj.main[[league]] <- load(file=paste0("Fitted_Objects/gam_nb_obj_Team_Season_Additive_Mixed_Eff_",  
  #                                               ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
  #                                               ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
  #                                               "_", league,
  #                                               "_Corners.Robj"))
  # gam.nb.obj.main[[league]] <- gam.nb.obj[[league]]
  # rm(gam.nb.obj)
  gam.nb.obj.main[[league]] <- load(file=paste0("Fitted_Objects/GAM_W_BUCKETED_EXTREME_CATEGORIES_gam_nb_obj_Team_AND_Season_Mixed_Eff_",  
                                                ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                                                ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                                                "_", league,
                                                "_Corners.Robj"))
  if (gam.nb.obj.main[[league]] == "ns.obj"){
    gam.nb.obj.main[[league]] <- ns.obj[[league]]
    rm(ns.obj)
  } else {
    gam.nb.obj.main[[league]] <- gam.nb.obj[[league]]
    rm(gam.nb.obj)
  }
  
  
  gam.ziplss.obj.main[[league]] <- load(file=paste0("Fitted_Objects/GAM_W_BUCKETED_EXTREME_CATEGORIES_gam_ziP_obj_Team_AND_Season_Mixed_Eff_",  
                                                    ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                                                    ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                                                    "_", league,
                                                    "_Corners.Robj"))
  gam.ziplss.obj.main[[league]] <- gam.ziP.obj[[league]]
  rm(gam.ziP.obj)
  

  
  print(AIC(gam.obj.main[[league]],
            gam.nb.obj.main[[league]],
            gam.ziplss.obj.main[[league]]
  ))
  
  print(BIC(gam.obj.main[[league]],
            gam.nb.obj.main[[league]],
            gam.ziplss.obj.main[[league]]
  ))
  
  
}


############
### MAIN TAKEAWAYS:
#############

## ZIP model might win on AIC, but not on BIC - NB beats it there.
## ZIP is a VERY complex model (35-40+ more degrees of freedom: ~105-125 over Pois/Negbin's 70-85)


#######
## SHOTS
#######

# [1] "Bundesliga"
# df      AIC
# gam.obj.main[[league]]         73.81003 695012.5
# gam.nb.obj.main[[league]]      71.14394 693432.3
# gam.ziplss.obj.main[[league]] 106.24797 693113.7
# df      BIC
# gam.obj.main[[league]]         73.81003 695873.0
# gam.nb.obj.main[[league]]      71.14394 694261.8
# gam.ziplss.obj.main[[league]] 106.24797 694352.4
# [1] "SerieA"
# df      AIC
# gam.obj.main[[league]]         80.44518 813704.8
# gam.nb.obj.main[[league]]      76.10625 812576.1
# gam.ziplss.obj.main[[league]] 117.85551 811871.9
# df      BIC
# gam.obj.main[[league]]         80.44518 814655.2
# gam.nb.obj.main[[league]]      76.10625 813475.3
# gam.ziplss.obj.main[[league]] 117.85551 813264.4
# [1] "LaLiga"
# df      AIC
# gam.obj.main[[league]]         81.37377 703460.4
# gam.nb.obj.main[[league]]      78.01018 702608.5
# gam.ziplss.obj.main[[league]] 125.86666 702148.5
# df      BIC
# gam.obj.main[[league]]         81.37377 704414.3
# gam.nb.obj.main[[league]]      78.01018 703522.9
# gam.ziplss.obj.main[[league]] 125.86666 703623.9
# [1] "Ligue1"
# df      AIC
# gam.obj.main[[league]]         80.61320 790313.9
# gam.nb.obj.main[[league]]      75.75851 788846.0
# gam.ziplss.obj.main[[league]] 123.54672 788451.6
# df      BIC
# gam.obj.main[[league]]         80.61320 791269.0
# gam.nb.obj.main[[league]]      75.75851 789743.6
# gam.ziplss.obj.main[[league]] 123.54672 789915.3
# [1] "PremierLeague"
# df      AIC
# gam.obj.main[[league]]         86.94710 722807.5
# gam.nb.obj.main[[league]]      81.88005 720360.4
# gam.ziplss.obj.main[[league]] 123.91591 719835.4
# df      BIC
# gam.obj.main[[league]]         86.94710 723826.5
# gam.nb.obj.main[[league]]      81.88005 721320.0
# gam.ziplss.obj.main[[league]] 123.91591 721287.6




#######
## CORNERS
########

# [1] "Bundesliga"
# df      AIC
# gam.obj.main[[league]]         68.94785 344286.1
# gam.nb.obj.main[[league]]      67.65371 343844.3
# gam.ziplss.obj.main[[league]] 104.09278 343792.6
# df      BIC
# gam.obj.main[[league]]         68.94785 345090.0
# gam.nb.obj.main[[league]]      67.65371 344633.0
# gam.ziplss.obj.main[[league]] 104.09278 345006.2
# [1] "SerieA"
# df      AIC
# gam.obj.main[[league]]        70.25939 414359.2
# gam.nb.obj.main[[league]]     68.38054 413629.8
# gam.ziplss.obj.main[[league]] 98.58529 413501.0
# df      BIC
# gam.obj.main[[league]]        70.25939 415189.4
# gam.nb.obj.main[[league]]     68.38054 414437.7
# gam.ziplss.obj.main[[league]] 98.58529 414665.8
# [1] "LaLiga"
# df      AIC
# gam.obj.main[[league]]        70.83994 373223.2
# gam.nb.obj.main[[league]]     73.33610 372732.8
# gam.ziplss.obj.main[[league]] 98.67180 372711.5
# df      BIC
# gam.obj.main[[league]]        70.83994 374053.6
# gam.nb.obj.main[[league]]     73.33610 373592.5
# gam.ziplss.obj.main[[league]] 98.67180 373868.1
# [1] "Ligue1"
# df      AIC
# gam.obj.main[[league]]        65.90280 406397.3
# gam.nb.obj.main[[league]]     66.16139 405661.2
# gam.ziplss.obj.main[[league]] 94.01352 405623.6
# df      BIC
# gam.obj.main[[league]]        65.90280 407178.0
# gam.nb.obj.main[[league]]     66.16139 406445.1
# gam.ziplss.obj.main[[league]] 94.01352 406737.4