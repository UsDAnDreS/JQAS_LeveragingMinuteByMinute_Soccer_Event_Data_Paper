######
## Stage 5p2: Using the LOSO CV errors to put together out-of-sample errors for game totals
##            E.g., by how much did we miss the shot and corner totals per game? (Instead of per minute of a game)
##      
##          Some of these results are used in the discussion at the end of Section 3.1 of the main paper.
##         ("the average errors across leagues ranged from 3.24 to 3.53 shots per
## game, which corresponds to about 27-28% of the actual per-game shot attempt averages. For corner kick
## predictions, the average errors across leagues were between 1.9 and 2.1 corners per game, about 40% of
## the actual per-game corner kick averages. The out-of-sample 𝑅2 values for shot attempts ranged from 21%
## to 31%, while for corner kicks, they ranged from 17% to 22%, depending on the league)
##
######

library(tidyverse)


## We're bucketing the extreme categories here
bucketed <- TRUE

## Whether to only check the performance on non-zero counts
only.nonzeros <- FALSE


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we want to still keep 1st half extra time
keep.1st.half.extra <- TRUE




league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")
completed.models <- c("gam_nb"
                     #  , 
                       # "gam_zip"
)

errors.mat <- list()

for (j in 1:5){
  
  cat("\n")
  print(j)
  league <- league.name[j]
  print(league)
  
  errors.mat[[j]] <- list()
  
  
  for (model in completed.models){
    
    print(model)
    load(paste0(ifelse(bucketed, "BUCKETED_", ""),
                "LEAVE_SEASON_OUT_PREDICTIONS_NEW_TEAM_AND_SEASON_",
                model,
                "_obj_", league,
                "_Shots.Robj"))
    
    
    # load(paste0("LEAVE_SEASON_OUT_PREDICTIONS_NEW_gam_obj_Shots.Robj"))
    # load(paste0("LEAVE_SEASON_OUT_PREDICTIONS_NEW_gam_zip_obj_Shots.Robj"))
    
    error.vec <- c()
    
    if (model == "gam_nb"){
      pred.list <- pred.list.nb
    } else if (model == "gam_zip") {
      pred.list <- pred.list.zip
    } else if (model == "gam_normal"){
      pred.list <- pred.list.normal
    } else if (model == "gam_lognormal"){
      pred.list <- pred.list.lognormal
    }
    
    if (only.nonzeros){
      pred.list[[league.name[j]]] <- pred.list[[league.name[j]]] %>% filter(True != 0)
    }
    
    
    
    
    ##########
    ### CALIBRATION PLOTS
    ##########
    
    
    ## Average error per minute
    print(pred.list[[league]] %>%
      summarise(mean(abs(True - Pred))) %>% .[[1]])
    
    
    ## Average error for a game
    print(pred.list[[league]] %>%
      group_by(gameId, Team) %>%
      summarise(Actual = sum(True),
                Predicted = sum(Pred)) %>%
      ungroup() %>%
      summarise(Error=mean(abs(Actual - Predicted)),
                SD = sd(abs(Actual-Predicted))/sqrt(length(Actual)),
                Mean.Actual = mean(Actual),
                R2 = (sum((Actual - mean(Actual))^2) - sum((Actual-Predicted)^2)) / sum((Actual - mean(Actual))^2) ))
    
    
  }
}


#############
#############
#### SHOTS
#############
#############

########
### NEG BIN
########

# [1] 1
# [1] "Bundesliga"
# [1] "gam_nb"
# [1] 0.2326078
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 1 × 4
# Error     SD Mean.Actual    R2
# <dbl>  <dbl>       <dbl> <dbl>
#   1  3.46 0.0283        12.7 0.242
# 
# [1] 2
# [1] "SerieA"
# [1] "gam_nb"
# [1] 0.2327856
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 1 × 4
# Error     SD Mean.Actual    R2
# <dbl>  <dbl>       <dbl> <dbl>
#   1  3.50 0.0267        12.9 0.273
# 
# [1] 3
# [1] "LaLiga"
# [1] "gam_nb"
# [1] 0.2172425
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 1 × 4
# Error     SD Mean.Actual    R2
# <dbl>  <dbl>       <dbl> <dbl>
#   1  3.38 0.0269        11.9 0.218
# 
# [1] 4
# [1] "Ligue1"
# [1] "gam_nb"
# [1] 0.2151203
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 1 × 4
# Error     SD Mean.Actual    R2
# <dbl>  <dbl>       <dbl> <dbl>
#   1  3.24 0.0248        11.7 0.206
# 
# [1] 5
# [1] "PremierLeague"
# [1] "gam_nb"
# [1] 0.2271707
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 1 × 4
# Error     SD Mean.Actual    R2
# <dbl>  <dbl>       <dbl> <dbl>
#   1  3.53 0.0287        12.7 0.314






#######
## ZIP
#######

# [1] 1
# [1] "Bundesliga"
# [1] "gam_zip"
# [1] 0.1526592
# `summarise()` has grouped output by 'gameId'. You can override using the
# `.groups` argument.
# # A tibble: 1 × 3
# Error     SD Mean.Actual
# <dbl>  <dbl>       <dbl>
#   1  10.3 0.0532        12.7
# 
# [1] 2
# [1] "SerieA"
# [1] "gam_zip"
# [1] 0.1533707
# `summarise()` has grouped output by 'gameId'. You can override using the
# `.groups` argument.
# # A tibble: 1 × 3
# Error     SD Mean.Actual
# <dbl>  <dbl>       <dbl>
#   1  10.6 0.0550        12.9
# 
# [1] 3
# [1] "LaLiga"
# [1] "gam_zip"
# [1] 0.1420459
# `summarise()` has grouped output by 'gameId'. You can override using the
# `.groups` argument.
# # A tibble: 1 × 3
# Error     SD Mean.Actual
# <dbl>  <dbl>       <dbl>
#   1  10.0 0.0511        12.0
# 
# [1] 4
# [1] "Ligue1"
# [1] "gam_zip"
# [1] 0.1395233
# `summarise()` has grouped output by 'gameId'. You can override using the
# `.groups` argument.
# # A tibble: 1 × 3
# Error     SD Mean.Actual
# <dbl>  <dbl>       <dbl>
#   1  9.56 0.0599        11.5
# 
# [1] 5
# [1] "PremierLeague"
# [1] "gam_zip"
# [1] 0.1529256
# `summarise()` has grouped output by 'gameId'. You can override using the
# `.groups` argument.
# # A tibble: 1 × 3
# Error     SD Mean.Actual
# <dbl>  <dbl>       <dbl>
#   1  10.3 0.0657        12.9




#############
#############
#### CORNERS
#############
#############

######
### NEGBIN
######

# [1] 1
# [1] "Bundesliga"
# [1] "gam_nb"
# [1] 0.09630124
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 1 × 4
# Error     SD Mean.Actual    R2
# <dbl>  <dbl>       <dbl> <dbl>
#   1  1.98 0.0163        4.82 0.168
# 
# [1] 2
# [1] "SerieA"
# [1] "gam_nb"
# [1] 0.09979055
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 1 × 4
# Error     SD Mean.Actual    R2
# <dbl>  <dbl>       <dbl> <dbl>
#   1  2.03 0.0155        5.07 0.195
# 
# [1] 3
# [1] "LaLiga"
# [1] "gam_nb"
# [1] 0.09838156
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 1 × 4
# Error     SD Mean.Actual    R2
# <dbl>  <dbl>       <dbl> <dbl>
#   1  2.02 0.0159        4.97 0.169
# 
# [1] 4
# [1] "Ligue1"
# [1] "gam_nb"
# [1] 0.09355288
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 1 × 4
# Error     SD Mean.Actual    R2
# <dbl>  <dbl>       <dbl> <dbl>
#   1  1.93 0.0164        4.71 0.167
# 
# [1] 5
# [1] "PremierLeague"
# [1] "gam_nb"
# [1] 0.101506
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 1 × 4
# Error     SD Mean.Actual    R2
# <dbl>  <dbl>       <dbl> <dbl>
#   1  2.08 0.0165        5.25 0.220


######
### ZIP: NOT AVAILABLE
######




