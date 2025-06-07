#######
## Stage 5p5: Doing significance testing for:
##            * Individual terms (in baseline model with no interactions)
##            * Interaction terms (by adding them one at a time to the baseline model)
##   fitting models for each individual season in each league 
##    (to keep sample size moderate and hence keep the p-values more meaningful).
##
##  Saving the resulting p-values into .Robj files as follows:
##
## save(my.obj, file=paste0("Fitted_Objects/",
##                         ifelse(bucketed, "BUCKETED_", ""),
##                         "MY_OBJ_Team_AND_Season_Mixed_Eff_",
##                         "MBM_TESTING_TERMS_INTERACTIONS_BY_SEASON_PVALS_AIC_BIC_Shots.Robj"))
##
##  to be later fed to "Stage5p6_JQAS_P_VALUES_FOR_INTERACTIONS_ETC.R"
##
## NOTE: If you want to switch from one offensive statistic to another, 
##       use "Ctrl+F" with "Match Case" turned out in order 
##       to replace all occurrences of "Corner" for "Shot" (), or vice versa.
####


library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
# library(DHARMa)


###
## Whether to bucket extreme scores (3+), red card diffs (2+) and minutes (45+)
###
bucketed <- TRUE


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we want to still keep 1st half extra time
keep.1st.half.extra <- TRUE


dir.create("Fitted_Objects/")

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

# league <- league.name[1]

## Covered: "BASELINE", "NO_REDCARDDIFF", "NO_WINPROBDIFF", "NO_SCOREDIFF", "NO_MINUTE", "NO_HOMEAWAY",
##          "SCOREDIFF_MINUTE_INT", "REDCARDDIFF_MINUTE_INT",
##          "WINPROBEDIFF_SCOREDIFF_INT", "SCOREDIFF_REDCARDDIFF_INT",
##          "WINPROBEDIFF_REDCARDDIFF_INT", "WINPROBEDIFF_MINUTE_INT"


# load(paste0("LEAVE_SEASON_OUT_PREDICTIONS_gam_nb_obj", "_", string_add, "_Shots.Robj"))

# Score.Diff, Minute.clean, RedCard.Diff, Weighted.Win.Prob
interact.vars.list <- list(NULL,
                           c("Score.Diff", "Minute.clean"),
                           c("Score.Diff", "RedCard.Diff"),
                           c("Score.Diff", "Weighted.Win.Prob"),
                           c("RedCard.Diff", "Minute.clean"),
                           c("RedCard.Diff", "Weighted.Win.Prob"),
                           c("Weighted.Win.Prob", "Minute.clean")
)


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

print("Shots")

p.values <- AIC.values <- BIC.values <- list()

for (i in 1:length(interact.vars.list)){
  
  interact.vars <- interact.vars.list[[i]]
  
  # load(file=paste0("Fitted_Objects/",
  #                  ifelse(bucketed, "BUCKETED_", ""),
  #                  "MY_OBJ_Team_AND_Season_Mixed_Eff_",
  #                  "MBM_TESTING_TERMS_INTERACTIONS_BY_SEASON_PVALS_AIC_BIC_Shots.Robj"))
  # 
  # p.values <- my.obj[[1]]
  # AIC.values <- my.obj[[2]]
  # BIC.values <- my.obj[[3]]
  
  
  
  p.values[[i]] <- list()
  AIC.values[[i]] <- list()
  BIC.values[[i]] <- list()
  
  
  cat("\n")
  cat("\n")
  
  print("Interacting variables (or excluded variable, if singular):")
  print(interact.vars)
  
  for (league in league.name){
    
    p.values[[i]][[league]] <- list()
    AIC.values[[i]][[league]] <- list()
    BIC.values[[i]][[league]] <- list()
    
    cat("\n")
    
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
    
    
    
    
    ######
    ## Fitting SPLINE models
    ######
    
    
    for (year in levels(our.df.cut$season)){
      print(year)
      
    glm.obj <- glm.nb.obj <- glm.ziP.obj <- list()
    gam.obj <- gam.nb.obj <- gam.ziP.obj <- list()
    
    if (keep.1st.half.extra){
      
      
      redcard.df <- length(unique(our.df.cut[our.df.cut$season == year,]$RedCard.Diff))
      score.df <- ifelse(length(unique(our.df.cut[our.df.cut$season == year,]$Score.Diff)) < 10,
                         length(unique(our.df.cut[our.df.cut$season == year,]$Score.Diff)),
                         10)
      
      base_formula <- "Shots ~ s(Score.Diff,k=score.df) + half_id + s(Minute.clean, by=half_id) + s(Weighted.Win.Prob) + HomeAway + s(RedCard.Diff,k=redcard.df) + s(Team, bs='re')"
      
      
      if (length(interact.vars) == 2){
        if ("Minute.clean" %in% interact.vars){
          if ("RedCard.Diff" == interact.vars[1]){
            add_formula <- paste0("ti(", interact.vars[1], ", ", interact.vars[2], ", k=c(redcard.df, 5), by = half_id)", collapse=" ")
          } else if ("RedCard.Diff" == interact.vars[2]) {
            add_formula <- paste0("ti(", interact.vars[1], ", ", interact.vars[2], ", k=c(5, redcard.df), by = half_id)", collapse=" ")
          } else {
            add_formula <- paste0("ti(", interact.vars[1], ", ", interact.vars[2], ", by = half_id)", collapse=" ")
          }
          
        } else if ("RedCard.Diff" == interact.vars[1]) {
          add_formula <- paste0("ti(", interact.vars[1], ", ", interact.vars[2], ", k=c(redcard.df, 5))", collapse=" ")
        } else if ("RedCard.Diff" == interact.vars[2]) {
          add_formula <- paste0("ti(", interact.vars[1], ", ", interact.vars[2], ", k=c(5, redcard.df))", collapse=" ")
        } else {
          add_formula <- paste0("ti(", interact.vars[1], ", ", interact.vars[2], ")", collapse = "")
        }
        
        full.formula <- paste0(base_formula, " + ", add_formula, collapse="")
        
      } else if (is.null(interact.vars)){
        full.formula <- base_formula
      }
      
      
      
      gam.nb.obj[[league]] <- gam(as.formula(full.formula) ,
                                  family = "nb", 
                                  data= our.df.cut %>% filter(season == year)
      )
      
      # print("Summary:")
      
      # print(summary(gam.nb.obj[[league]]))
      sum.obj <- summary(gam.nb.obj[[league]])
      
      print("p-values:")
      
      print(sum.obj$s.table[,4])
      # print(paste0("c(", paste0(sum.obj$s.table[,4], collapse=","), ")", collapse=""))
      
      
      print("AIC:")
      print(AIC(gam.nb.obj[[league]]))
      print("BIC:")
      print(BIC(gam.nb.obj[[league]]))
      
      p.values[[i]][[league]][[as.character(year)]] <- sum.obj$s.table[,4]
      AIC.values[[i]][[league]][[as.character(year)]] <- AIC(gam.nb.obj[[league]])
      BIC.values[[i]][[league]][[as.character(year)]] <- BIC(gam.nb.obj[[league]])
      
      
      rm(gam.nb.obj)
      
      my.obj <- list(p.values, AIC.values, BIC.values)
      save(my.obj, file=paste0("Fitted_Objects/",
                               ifelse(bucketed, "BUCKETED_", ""),
                               "MY_OBJ_Team_AND_Season_Mixed_Eff_",
                               "MBM_TESTING_TERMS_INTERACTIONS_BY_SEASON_PVALS_AIC_BIC_Shots.Robj"))
      
      
      
      
    } else {
      
       }
    
    
    }
    
    cat("\n")
    
    ## End of main loop
  }
}
