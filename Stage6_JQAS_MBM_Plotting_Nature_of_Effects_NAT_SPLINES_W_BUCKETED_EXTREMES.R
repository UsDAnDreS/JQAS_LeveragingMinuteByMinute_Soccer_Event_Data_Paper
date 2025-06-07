#######
##  Stage 6: Code yielding Figure 5 of the Supplement,
##           showing the nature of estimated effects for 
##           score diff, red card diff, win prob diff, game minute in 1st and 2nd halves respectively,
##           for the case of
##           SIMPLER NATURAL CUBIC SPLINES WITH BUCKETED EXTREMES
#######


library(readr)
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(MASS)
library(xtable)
library(mgcv)
library(gratia)
library(ggplot2)  
library(patchwork)
library(splines)


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we want to still keep 1st half extra time
keep.1st.half.extra <- TRUE

## Using the reduced version of ZIP?
reduced.ver <- FALSE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")







#######
### SHOTS
#######


all_effects_shots <- NULL


for (league in league.name){
  print(league)
  
  #########
  ### SHOTS
  #########
  
  ####### NOT SURE IF NEED TO ACTUALLY IMPORT "our.df.cut"
  
  load(paste0("Fitted_Objects/NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_gam_nb_obj_Team_AND_Season_Mixed_Eff_",
              ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
              ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
              "_", league,
              "_Shots.Robj"))
  
  
  our.df.cut <- read.csv(paste0(league,
                                ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                                ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                                "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  
  ## REMOVING NA betting data
  our.df.cut <- our.df.cut %>% filter(!is.na(Weighted.Win.Prob))
  
  
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

  
  
  
  # ns.obj[[1]]$coefficients
  # draw(parametric_effects(ns.obj[[1]]))
  
  df.minute <- 5
  df.winprob <- 5
  
  
  ########
  # # Predict the effect of Score.Diff
  ########
  
  score_diff_effect <- data.frame(
    Score.Diff = seq(min(our.df.cut$Score.Diff),
                     max(our.df.cut$Score.Diff), length.out = 100),
    # Score.Diff = seq(min(our.df.cut$Score.Diff), 
    #                  max(our.df.cut$Score.Diff), length.out = 100),
    
    RedCard.Diff = 0,
    # RedCard.Diff = mean(our.df.cut$RedCard.Diff, na.rm = TRUE),  # set to mean
    Minute.clean = 25,  # set to mean
    Weighted.Win.Prob = 0,  # set to mean
    HomeAway = factor("Home", levels = c("Home", "Away")),  # setting to "Home"
    half_id = "2",  # set to mean
    Team=0,
    season=0
  )
  
  
  # pred <- predict(gam.nb.obj, newdata = score_diff_effect,  se.fit = TRUE)
  pred <- predict(ns.obj[[league]], newdata = score_diff_effect,  se.fit = TRUE)
  
  
  # Extract the fitted values and standard errors for the smooth term of Score.Diff
  score_diff_effect$fit <- pred$fit
  score_diff_effect$se <- pred$se.fit
  
  # Calculate the 95% confidence intervals
  score_diff_effect$upper <- score_diff_effect$fit + 1.96 * score_diff_effect$se
  score_diff_effect$lower <- score_diff_effect$fit - 1.96 * score_diff_effect$se
  
  
  ggplot(score_diff_effect, aes(x = Score.Diff, y = fit)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +  # Confidence bands
    labs(x = "Score Difference", y = "Effect on Shots") +
    theme_minimal()
  
  
  
  ########
  # # Predict the effect of RedCard.Diff
  ########
  
  
  redcard_diff_effect <- data.frame(
    # Score.Diff = seq(min(our.df.cut$Score.Diff),
    #                  max(our.df.cut$Score.Diff), length.out = 100),
    Score.Diff = 0,
    
    RedCard.Diff = seq(min(our.df.cut$RedCard.Diff),
                       max(our.df.cut$RedCard.Diff), length.out = 100),
    # RedCard.Diff = mean(our.df.cut$RedCard.Diff, na.rm = TRUE),  # set to mean
    Minute.clean = 25,  # set to mean
    Weighted.Win.Prob = 0,  # set to mean
    HomeAway = factor("Home", levels = c("Home", "Away")),  # setting to "Home"
    half_id = "2",  # set to mean
    Team=0,
    season=0
  )
  
  # pred <- predict(gam.nb.obj, newdata = score_diff_effect,  se.fit = TRUE)
  pred <- predict(ns.obj[[league]], newdata = redcard_diff_effect,  se.fit = TRUE)
  
  
  # Extract the fitted values and standard errors for the smooth term of Score.Diff
  redcard_diff_effect$fit <- pred$fit
  redcard_diff_effect$se <- pred$se.fit
  
  # Calculate the 95% confidence intervals
  redcard_diff_effect$upper <- redcard_diff_effect$fit + 1.96 * redcard_diff_effect$se
  redcard_diff_effect$lower <- redcard_diff_effect$fit - 1.96 * redcard_diff_effect$se
  
  
  ggplot(redcard_diff_effect, aes(x = RedCard.Diff, y = fit)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +  # Confidence bands
    labs(x = "Score Difference", y = "Effect on Shots") +
    theme_minimal()
  
  
  
  
  ########
  # # Predict the effect of Game Minute in the 1st half
  ########
  
  
  minute_effect_1st_half <- data.frame(
    Score.Diff = 0,
    RedCard.Diff = 0,
    # Minute.clean = 25,  # set to mean
    Minute.clean = seq(min(our.df.cut$Minute.clean),
                       max(our.df.cut$Minute.clean), length.out = 100),
    Weighted.Win.Prob = 0,  # set to mean
    HomeAway = factor("Home", levels = c("Home", "Away")),  # setting to "Home"
    half_id = "1",  # set to mean
    Team= 0,
    season=0
  )
  
  # pred <- predict(gam.nb.obj, newdata = score_diff_effect,  se.fit = TRUE)
  pred <- predict(ns.obj[[league]], newdata = minute_effect_1st_half,  se.fit = TRUE)
  
  
  # Extract the fitted values and standard errors for the smooth term of Score.Diff
  minute_effect_1st_half$fit <- pred$fit
  minute_effect_1st_half$se <- pred$se.fit
  
  # Calculate the 95% confidence intervals
  minute_effect_1st_half$upper <- minute_effect_1st_half$fit + 1.96 * minute_effect_1st_half$se
  minute_effect_1st_half$lower <- minute_effect_1st_half$fit - 1.96 * minute_effect_1st_half$se
  
  
  ggplot(minute_effect_1st_half, aes(x = Minute.clean, y = fit)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +  # Confidence bands
    labs(x = "Score Difference", y = "Effect on Shots") +
    theme_minimal()
  
  
  
  
  ########
  # # Predict the effect of Game Minute in the 2nd half
  ########
  
  
  minute_effect_2nd_half <- data.frame(
    Score.Diff = 0,
    RedCard.Diff = 0,
    # Minute.clean = 25,  # set to mean
    Minute.clean = seq(min(our.df.cut$Minute.clean),
                       max(our.df.cut$Minute.clean), length.out = 100),
    Weighted.Win.Prob = 0,  # set to mean
    HomeAway = factor("Home", levels = c("Home", "Away")),  # setting to "Home"
    half_id = "2",  # set to mean
    Team=0,
    season=0
  )
  
  # pred <- predict(gam.nb.obj, newdata = score_diff_effect,  se.fit = TRUE)
  pred <- predict(ns.obj[[league]], newdata = minute_effect_2nd_half,  se.fit = TRUE)
  
  
  # Extract the fitted values and standard errors for the smooth term of Score.Diff
  minute_effect_2nd_half$fit <- pred$fit
  minute_effect_2nd_half$se <- pred$se.fit
  
  # Calculate the 95% confidence intervals
  minute_effect_2nd_half$upper <- minute_effect_2nd_half$fit + 1.96 * minute_effect_2nd_half$se
  minute_effect_2nd_half$lower <- minute_effect_2nd_half$fit - 1.96 * minute_effect_2nd_half$se
  
  
  ggplot(minute_effect_2nd_half, aes(x = Minute.clean, y = fit)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +  # Confidence bands
    labs(x = "Score Difference", y = "Effect on Shots") +
    theme_minimal()
  
  score_diff_effect$Type <- "Score Difference"
  redcard_diff_effect$Type <- "Red Card Difference"
  minute_effect_1st_half$Type <- "Minute, 1st Half"
  minute_effect_2nd_half$Type <- "Minute, 2nd Half"
  
  all_effects_shots <- rbind(all_effects_shots,
                             rbind(score_diff_effect,
                                   redcard_diff_effect,
                                   minute_effect_1st_half,
                                   minute_effect_2nd_half) %>% mutate(League = league))
  
}

all_effects_shots






all_effects_corners <- NULL


for (league in league.name){
  print(league)
  
  #########
  ### CORNERS
  #########
  
  ####### NOT SURE IF NEED TO ACTUALLY IMPORT "our.df.cut"
  
  load(paste0("Fitted_Objects/NAT_SPLINE_W_BUCKETED_EXTREME_CATEGORIES_gam_nb_obj_Team_AND_Season_Mixed_Eff_",
              ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
              ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
              "_", league,
              "_Corners.Robj"))
  
  
  our.df.cut <- read.csv(paste0(league,
                                ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                                ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                                "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  
  ## REMOVING NA betting data
  our.df.cut <- our.df.cut %>% filter(!is.na(Weighted.Win.Prob))
  
  
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
  ##    * Putting 2+ red card diffs as "2"
  ##    * Putting 45+ minutes as "45"
  ########
  
  our.df.cut <- our.df.cut %>%
    mutate(Score.Diff = ifelse(abs(Score.Diff) >= 3, sign(Score.Diff)*3, Score.Diff),
           RedCard.Diff = ifelse(abs(RedCard.Diff) >= 2, sign(RedCard.Diff)*2, RedCard.Diff),
           Minute.clean = ifelse(Minute.clean >= 45, 45, Minute.clean))
  
  
  # ns.obj[[1]]$coefficients
  # draw(parametric_effects(ns.obj[[1]]))
  
  df.minute <- 5
  df.winprob <- 5
  
  
  ########
  # # Predict the effect of Score.Diff
  ########
  
  score_diff_effect <- data.frame(
    Score.Diff = seq(min(our.df.cut$Score.Diff),
                     max(our.df.cut$Score.Diff), length.out = 100),
    # Score.Diff = seq(min(our.df.cut$Score.Diff), 
    #                  max(our.df.cut$Score.Diff), length.out = 100),
    
    RedCard.Diff = 0,
    # RedCard.Diff = mean(our.df.cut$RedCard.Diff, na.rm = TRUE),  # set to mean
    Minute.clean = 25,  # set to mean
    Weighted.Win.Prob = 0,  # set to mean
    HomeAway = factor("Home", levels = c("Home", "Away")),  # setting to "Home"
    half_id = "2",  # set to mean
    Team=0,
    season=0
  )
  
  
  # pred <- predict(gam.nb.obj, newdata = score_diff_effect,  se.fit = TRUE)
  pred <- predict(ns.obj[[league]], newdata = score_diff_effect,  se.fit = TRUE)
  
  
  # Extract the fitted values and standard errors for the smooth term of Score.Diff
  score_diff_effect$fit <- pred$fit
  score_diff_effect$se <- pred$se.fit
  
  # Calculate the 95% confidence intervals
  score_diff_effect$upper <- score_diff_effect$fit + 1.96 * score_diff_effect$se
  score_diff_effect$lower <- score_diff_effect$fit - 1.96 * score_diff_effect$se
  
  
  ggplot(score_diff_effect, aes(x = Score.Diff, y = fit)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +  # Confidence bands
    labs(x = "Score Difference", y = "Effect on Corners") +
    theme_minimal()
  
  
  
  ########
  # # Predict the effect of RedCard.Diff
  ########
  
  
  redcard_diff_effect <- data.frame(
    # Score.Diff = seq(min(our.df.cut$Score.Diff),
    #                  max(our.df.cut$Score.Diff), length.out = 100),
    Score.Diff = 0,
    
    RedCard.Diff = seq(min(our.df.cut$RedCard.Diff),
                       max(our.df.cut$RedCard.Diff), length.out = 100),
    # RedCard.Diff = mean(our.df.cut$RedCard.Diff, na.rm = TRUE),  # set to mean
    Minute.clean = 25,  # set to mean
    Weighted.Win.Prob = 0,  # set to mean
    HomeAway = factor("Home", levels = c("Home", "Away")),  # setting to "Home"
    half_id = "2",  # set to mean
    Team=0,
    season=0
  )
  
  # pred <- predict(gam.nb.obj, newdata = score_diff_effect,  se.fit = TRUE)
  pred <- predict(ns.obj[[league]], newdata = redcard_diff_effect,  se.fit = TRUE)
  
  
  # Extract the fitted values and standard errors for the smooth term of Score.Diff
  redcard_diff_effect$fit <- pred$fit
  redcard_diff_effect$se <- pred$se.fit
  
  # Calculate the 95% confidence intervals
  redcard_diff_effect$upper <- redcard_diff_effect$fit + 1.96 * redcard_diff_effect$se
  redcard_diff_effect$lower <- redcard_diff_effect$fit - 1.96 * redcard_diff_effect$se
  
  
  ggplot(redcard_diff_effect, aes(x = RedCard.Diff, y = fit)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +  # Confidence bands
    labs(x = "Score Difference", y = "Effect on Shots") +
    theme_minimal()
  
  
  
  
  ########
  # # Predict the effect of Game Minute in the 1st half
  ########
  
  
  minute_effect_1st_half <- data.frame(
    Score.Diff = 0,
    RedCard.Diff = 0,
    # Minute.clean = 25,  # set to mean
    Minute.clean = seq(min(our.df.cut$Minute.clean),
                       max(our.df.cut$Minute.clean), length.out = 100),
    Weighted.Win.Prob = 0,  # set to mean
    HomeAway = factor("Home", levels = c("Home", "Away")),  # setting to "Home"
    half_id = "1",  # set to mean
    Team= 0,
    season=0
  )
  
  # pred <- predict(gam.nb.obj, newdata = score_diff_effect,  se.fit = TRUE)
  pred <- predict(ns.obj[[league]], newdata = minute_effect_1st_half,  se.fit = TRUE)
  
  
  # Extract the fitted values and standard errors for the smooth term of Score.Diff
  minute_effect_1st_half$fit <- pred$fit
  minute_effect_1st_half$se <- pred$se.fit
  
  # Calculate the 95% confidence intervals
  minute_effect_1st_half$upper <- minute_effect_1st_half$fit + 1.96 * minute_effect_1st_half$se
  minute_effect_1st_half$lower <- minute_effect_1st_half$fit - 1.96 * minute_effect_1st_half$se
  
  
  ggplot(minute_effect_1st_half, aes(x = Minute.clean, y = fit)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +  # Confidence bands
    labs(x = "Score Difference", y = "Effect on Shots") +
    theme_minimal()
  
  
  
  
  ########
  # # Predict the effect of Game Minute in the 2nd half
  ########
  
  
  minute_effect_2nd_half <- data.frame(
    Score.Diff = 0,
    RedCard.Diff = 0,
    # Minute.clean = 25,  # set to mean
    Minute.clean = seq(min(our.df.cut$Minute.clean),
                       max(our.df.cut$Minute.clean), length.out = 100),
    Weighted.Win.Prob = 0,  # set to mean
    HomeAway = factor("Home", levels = c("Home", "Away")),  # setting to "Home"
    half_id = "2",  # set to mean
    Team=0,
    season=0
  )
  
  # pred <- predict(gam.nb.obj, newdata = score_diff_effect,  se.fit = TRUE)
  pred <- predict(ns.obj[[league]], newdata = minute_effect_2nd_half,  se.fit = TRUE)
  
  
  # Extract the fitted values and standard errors for the smooth term of Score.Diff
  minute_effect_2nd_half$fit <- pred$fit
  minute_effect_2nd_half$se <- pred$se.fit
  
  # Calculate the 95% confidence intervals
  minute_effect_2nd_half$upper <- minute_effect_2nd_half$fit + 1.96 * minute_effect_2nd_half$se
  minute_effect_2nd_half$lower <- minute_effect_2nd_half$fit - 1.96 * minute_effect_2nd_half$se
  
  
  ggplot(minute_effect_2nd_half, aes(x = Minute.clean, y = fit)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +  # Confidence bands
    labs(x = "Score Difference", y = "Effect on Shots") +
    theme_minimal()
  
  score_diff_effect$Type <- "Score Difference"
  redcard_diff_effect$Type <- "Red Card Difference"
  minute_effect_1st_half$Type <- "Minute, 1st Half"
  minute_effect_2nd_half$Type <- "Minute, 2nd Half"
  
  all_effects_corners <- rbind(all_effects_corners,
                               rbind(score_diff_effect,
                                     redcard_diff_effect,
                                     minute_effect_1st_half,
                                     minute_effect_2nd_half) %>% mutate(League = league))
  
}

all_effects_corners







min.y <- -3
max.y <- -1

plot.scorediff_shots <- ggplot(all_effects_shots %>% filter(Type == "Score Difference"), aes(x = Score.Diff, y = fit, group=League, color=League)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue", color=NA) +  # Confidence bands
  labs(x = "Score Difference", y = "Effect on Shots") +
  theme_minimal()  +
  ylim(min.y, max.y) +
  theme(legend.position = "none")

plot.redcarddiff_shots <- ggplot(all_effects_shots %>% filter(Type == "Red Card Difference"), aes(x = RedCard.Diff, y = fit, group=League, color=League)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue", color=NA) +  # Confidence bands
  labs(x = "Red Card Difference", y = "Effect on Shots") +
  theme_minimal()  +
  ylim(min.y, max.y) +
  theme(legend.position = "none")

plot.minute.1st_shots <- ggplot(all_effects_shots %>% filter(Type == "Minute, 1st Half"), aes(x = Minute.clean, y = fit, group=League, color=League)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue", color=NA) +  # Confidence bands
  labs(x = "Minute, 1st Half", y = "Effect on Shots") +
  theme_minimal()  +
  ylim(min.y, max.y) +
  theme(legend.position = "none")

plot.minute.2nd_shots <- ggplot(all_effects_shots %>% filter(Type == "Minute, 2nd Half"), aes(x = Minute.clean, y = fit, group=League, color=League)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue", color=NA) +  # Confidence bands
  labs(x = "Minute, 2nd Half", y = "Effect on Shots") +
  theme_minimal()  +
  ylim(min.y, max.y) +
  theme(legend.position = "none")





min.y <- -4
max.y <- -2

plot.scorediff_corners <- ggplot(all_effects_corners %>% filter(Type == "Score Difference"), aes(x = Score.Diff, y = fit, group=League, color=League)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue", color=NA) +  # Confidence bands
  labs(x = "Score Difference", y = "Effect on Corners") +
  scale_x_continuous(breaks = -3:3,
                     labels=c("-3\nor more", "-2", "-1", "0", "+1", "+2", "+3\nor more")) +
  theme_minimal()  +
  ylim(min.y, max.y) +
  theme(legend.position = "none")

plot.redcarddiff_corners <- ggplot(all_effects_corners %>% filter(Type == "Red Card Difference"), aes(x = RedCard.Diff, y = fit, group=League, color=League)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue", color=NA) +  # Confidence bands
  labs(x = "Red Card Difference", y = "Effect on Corners") +
  theme_minimal()  +
  ylim(min.y, max.y) +
  scale_x_continuous(breaks = -2:2,
                     labels=c("-2\nor more", "-1", "0", "+1", "+2\nor more")) + 
  theme(legend.position = "none")

plot.minute.1st_corners <- ggplot(all_effects_corners %>% filter(Type == "Minute, 1st Half"), aes(x = Minute.clean, y = fit, group=League, color=League)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue", color=NA) +  # Confidence bands
  labs(x = "Minute, 1st Half", y = "Effect on Corners") +
  theme_minimal()  +
  ylim(min.y, max.y) +
  scale_x_continuous(breaks = c(5,15,25,35,45),
                     labels=c("5", "15", "25", "35", "45+")) +
  theme(legend.position = "none")

plot.minute.2nd_corners <- ggplot(all_effects_corners %>% filter(Type == "Minute, 2nd Half"), aes(x = Minute.clean, y = fit, group=League, color=League)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue", color=NA) +  # Confidence bands
  labs(x = "Minute, 2nd Half", y = "Effect on Corners") +
  theme_minimal()  +
  ylim(min.y, max.y) +
  scale_x_continuous(breaks = c(5,15,25,35,45),
                     labels=c("5", "15", "25", "35", "45+")) +
  theme(legend.position = "none")


legend_plot <- ggplot(all_effects_corners %>% filter(Type == "Minute, 2nd Half"), aes(x = Minute.clean, y = fit, group=League, color=League)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue", color=NA) +  # Confidence bands
  labs(x = "Minute, 2nd Half", y = "Effect on Corners") +
  theme_minimal()  +
  ylim(min.y, max.y)

legend_grob <- ggplotGrob(legend_plot)$grobs[[which(sapply(ggplotGrob(legend_plot)$grobs, function(x) x$name) == "guide-box")]]

library(gridExtra)

all.plots <- list(plot.scorediff_shots, plot.scorediff_corners,
                  plot.redcarddiff_shots, plot.redcarddiff_corners,
                  plot.minute.1st_shots, plot.minute.1st_corners,
                  plot.minute.2nd_shots, plot.minute.2nd_corners)

grid.arrange(
  do.call(gridExtra::arrangeGrob, c(all.plots, ncol = 2)),
  legend_grob,  # The legend as a separate grob
  ncol = 2,      # Two columns: one for the plots and one for the legend
  widths = c(7, 2))

