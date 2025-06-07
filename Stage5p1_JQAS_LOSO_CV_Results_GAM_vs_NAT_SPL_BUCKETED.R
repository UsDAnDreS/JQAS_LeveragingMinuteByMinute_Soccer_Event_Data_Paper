#######
## Stage 5p1: After having carried out LOSO CV for Negative Binomial modeling class and
##            * Smoothing splines
##            * Smoothing splines with bucketed extremes,
##            * Simpler natural cubic splines,
##            * Simpler natural cubic splines with bucketed extremes
##  we use the .Robj out-of-sample prediction files as follows:
## 
## load(paste0("LEAVE_SEASON_OUT_PREDICTIONS_",
##             model,
##             "TEAM_AND_SEASON_gam_nb_obj_", 
##             league,
##             "_Corners.Robj"))
##
##   to come up with LOSOCV MAE & RMSE.
##
##  Those are used in Table 1 of Supplement (only carried out for shot attempts)
##
############


library(tidyverse)


## Whether to only check the performance on non-zero counts
only.nonzeros <- FALSE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")
completed.models <- c("NEW_",    # the regular gam, no bucketing
                        "GAM_W_BUCKETED_EXTREME_CATEGORIES_", # gam with bucketing (promising)
                       "NAT_SPLINE_",   # Simpler, natural splines, no bucketing
                      "NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_"  # Simpler, natural splines, WITH bucketing
                      ) 

# LEAVE_SEASON_OUT_PREDICTIONS_NEW_TEAM_AND_SEASON_gam_nb_obj_Bundesliga_Corners.Robj
# LEAVE_SEASON_OUT_PREDICTIONS_GAM_W_BUCKETED_EXTREME_CATEGORIES_TEAM_AND_SEASON_gam_nb_obj_Bundesliga_Corners.Robj
# LEAVE_SEASON_OUT_PREDICTIONS_NAT_SPLINE_TEAM_AND_SEASON_gam_nb_obj_SerieA_Corners.
# LEAVE_SEASON_OUT_PREDICTIONS_NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_TEAM_AND_SEASON_gam_nb_obj_PremierLeague_Corners.Robj

errors.mat <- list()

for (j in 1:5){
  
  cat("\n")
  print(j)
  league <- league.name[j]
  print(league)
  
  errors.mat[[j]] <- list()
  
  
  for (model in completed.models){
    
    print(model)
    load(paste0("LEAVE_SEASON_OUT_PREDICTIONS_",
                model,
                "TEAM_AND_SEASON_gam_nb_obj_", 
                league,
                "_Corners.Robj"))
    

    # load(paste0("LEAVE_SEASON_OUT_PREDICTIONS_NEW_gam_obj_Corners.Robj"))
    # load(paste0("LEAVE_SEASON_OUT_PREDICTIONS_NEW_gam_zip_obj_Corners.Robj"))
    
    error.vec <- c()
    
   pred.list <- pred.list.nb
    
    if (only.nonzeros){
      pred.list[[league.name[j]]] <- pred.list[[league.name[j]]] %>% filter(True != 0)
    }
    
    error.vec <-  pred.list[[league.name[j]]] %>%
      mutate(Err = True-Pred,
             log.Err = log(True+1) - log(Pred+1),
             unit.dev = 2*(ifelse(True !=0, 
                                  True*log(True/Pred) - True + Pred,
                                  - True + Pred))) %>%
      group_by(Year) %>%
      summarise(MAE = mean(abs(Err)),
                RMSE = sqrt(mean(Err^2)),
                # ERR.se = sd(abs(Err))/sqrt(length(Err)),
                MAE.log = mean(abs(log.Err)),
                RMSE.log = sqrt(mean(log.Err^2)),
                # LOG_ERR.log.se = sd(abs(log.Err))/sqrt(length(Err)),
                MAE.dev = mean(abs(unit.dev)),
                RMSE.dev = sqrt(mean(unit.dev^2))
                # , UNIT.se = sd(abs(unit.dev))/sqrt(length(Err))
      ) %>%
      select(MAE, RMSE, # ERR.se,
             MAE.log, RMSE.log, # LOG_ERR.log.se,
             MAE.dev, RMSE.dev
             # , UNIT.se
      )
    
    cat("\n")
    print(model)
     # print(error.vec)
    
    print("Mean:")
    print(round(apply(error.vec, 2, function(x) mean(x)), 4))
    
    print("SD:")
    print(round(apply(error.vec, 2, function(x) sd(x)/sqrt(length(x))), 4))
    
    # if (model == completed.models[1]){
    #   for (i in colnames(error.vec)){
    #     errors.mat[[j]][[i]] <- data.frame(error.vec[, i])
    #     colnames(errors.mat[[j]][[i]])[ncol(errors.mat[[j]][[i]])] <- model
    #   }
    # 
    # } else {
    #   for (i in colnames(error.vec)){
    #     errors.mat[[j]][[i]] <- cbind(errors.mat[[j]][[i]], error.vec[, i])
    #     colnames(errors.mat[[j]][[i]])[ncol(errors.mat[[j]][[i]])] <- model
    #   }
    # }
    
    
  }
}


######
## Which metric do we want?
#####

metric <- c("MAE", "RMSE", "MAE.log", "RMSE.log", "MAE.dev", "RMSE.dev")[1]



final.error.mat <- final.error.mat.mse <- NULL
final.error.mat.sd <- final.error.mat.mse.sd <- NULL


for (j in 1:5){
  final.error.mat <- rbind(final.error.mat,
                           data.frame(League=league.name[j], t(apply(errors.mat[[j]][[metric]], 2, mean))))
  final.error.mat.sd <- rbind(final.error.mat.sd,
                              data.frame(League=league.name[j], t(apply(errors.mat[[j]][[metric]], 2, sd))))
  
  # apply(errors.mat.mse[[j]], 2, mean)
  # apply(errors.mat.mse[[j]], 2, sd)
}


t(final.error.mat)
t(final.error.mat.sd)


##### MAIN TAKEAWAYS:
##    - Nat Spline BUCKETED is always THE WORST;
##    - GAM BUCKETED & REGULAR NAT SPLINE are on par with each other (almost identical)
##    - Regular GAM is mostly on par with best, BUT IS NOTABLY WORSE FOR PREMIER LEAGUE & LIGUE1


###########
### SHOTS
##########



# [1] 1
# [1] "Bundesliga"
# [1] "NEW_"
# 
# [1] "NEW_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2327   0.3786   0.1814   0.2456   0.5651   1.1636 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0010   0.0017   0.0006   0.0009   0.0029   0.0086 
# [1] "GAM_W_BUCKETED_EXTREME_CATEGORIES_"
# 
# [1] "GAM_W_BUCKETED_EXTREME_CATEGORIES_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2328   0.3786   0.1815   0.2456   0.5651   1.1627 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0009   0.0017   0.0005   0.0009   0.0029   0.0084 
# [1] "NAT_SPLINE_"
# 
# [1] "NAT_SPLINE_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2328   0.3786   0.1816   0.2457   0.5656   1.1643 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0010   0.0017   0.0005   0.0009   0.0029   0.0085 
# [1] "NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_"
# 
# [1] "NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2349   0.3787   0.1835   0.2460   0.5680   1.2220 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0010   0.0017   0.0005   0.0009   0.0036   0.0366 
# 
# [1] 2
# [1] "SerieA"
# [1] "NEW_"
# 
# [1] "NEW_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2328   0.3775   0.1814   0.2456   0.5635   1.1511 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0009   0.0016   0.0005   0.0008   0.0029   0.0077 
# [1] "GAM_W_BUCKETED_EXTREME_CATEGORIES_"
# 
# [1] "GAM_W_BUCKETED_EXTREME_CATEGORIES_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2329   0.3775   0.1815   0.2457   0.5637   1.1508 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0009   0.0016   0.0005   0.0008   0.0029   0.0076 
# [1] "NAT_SPLINE_"
# 
# [1] "NAT_SPLINE_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2329   0.3776   0.1815   0.2457   0.5641   1.1524 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0009   0.0016   0.0005   0.0008   0.0029   0.0078 
# [1] "NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_"
# 
# [1] "NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2372   0.3779   0.1854   0.2466   0.5659   1.1293 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0009   0.0015   0.0005   0.0008   0.0027   0.0075 
# 
# [1] 3
# [1] "LaLiga"
# [1] "NEW_"
# 
# [1] "NEW_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2175   0.3634   0.1704   0.2372   0.5408   1.1478 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0016   0.0028   0.0009   0.0016   0.0054   0.0106 
# [1] "GAM_W_BUCKETED_EXTREME_CATEGORIES_"
# 
# [1] "GAM_W_BUCKETED_EXTREME_CATEGORIES_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2176   0.3635   0.1704   0.2372   0.5409   1.1475 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0015   0.0028   0.0008   0.0016   0.0053   0.0108 
# [1] "NAT_SPLINE_"
# 
# [1] "NAT_SPLINE_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2176   0.3635   0.1704   0.2372   0.5414   1.1490 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0015   0.0028   0.0008   0.0016   0.0054   0.0107 
# [1] "NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_"
# 
# [1] "NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2260   0.3636   0.1778   0.2389   0.5420   1.0988 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0016   0.0028   0.0009   0.0015   0.0049   0.0090 
# 
# [1] 4
# [1] "Ligue1"
# [1] "NEW_"
# 
# [1] "NEW_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2190   0.8730   0.1689   0.2363   0.5445   2.1406 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0038   0.5120   0.0005   0.0010   0.0083   0.9959 
# [1] "GAM_W_BUCKETED_EXTREME_CATEGORIES_"
# 
# [1] "GAM_W_BUCKETED_EXTREME_CATEGORIES_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2153   0.3615   0.1689   0.2359   0.5372   1.1485 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0008   0.0016   0.0005   0.0008   0.0029   0.0082 
# [1] "NAT_SPLINE_"
# 
# [1] "NAT_SPLINE_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2153   0.3615   0.1689   0.2358   0.5376   1.1504 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0008   0.0016   0.0005   0.0008   0.0028   0.0082 
# [1] "NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_"
# 
# [1] "NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2200   0.3615   0.1731   0.2367   0.5379   1.1204 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0008   0.0016   0.0005   0.0008   0.0027   0.0081 
# 
# [1] 5
# [1] "PremierLeague"
# [1] "NEW_"
# 
# [1] "NEW_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2345   0.3881   0.1813   0.2495   0.5790   1.2201 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0020   0.0033   0.0011   0.0018   0.0063   0.0134 
# [1] "GAM_W_BUCKETED_EXTREME_CATEGORIES_"
# 
# [1] "GAM_W_BUCKETED_EXTREME_CATEGORIES_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2284   0.3782   0.1778   0.2439   0.5598   1.1875 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0017   0.0028   0.0009   0.0016   0.0054   0.0104 
# [1] "NAT_SPLINE_"
# 
# [1] "NAT_SPLINE_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2283   0.3782   0.1777   0.2439   0.5601   1.1890 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0017   0.0029   0.0010   0.0016   0.0054   0.0101 
# [1] "NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_"
# 
# [1] "NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_"
# [1] "Mean:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.2402   0.3786   0.1883   0.2463   0.5623   1.1176 
# [1] "SD:"
# MAE     RMSE  MAE.log RMSE.log  MAE.dev RMSE.dev 
# 0.0016   0.0027   0.0009   0.0014   0.0047   0.0094 
