######
## Stage 12p1: The actual testing of the forecasting performance. 
##              Feeding the adjusted shots/corners from "Stage12_JQAS_Forecasting_Performance_Preparing_Objects.R",
##        then, for each season
##          * Use first half of the season to train (19 games), 
##          * second half to forecast (19 games)
##  The model is:
##      Final score differential of the game = home_team_average_shots + away_teams_average_shots
##
##  In the end, we calculate:
##      * Matched-pairs testing results of RMSE & MAE between actual and adjusted feature sets.
##      * Shapiro-Wilk p-values, to confirm normality.
##      * The MAE/RMSE of each individual model (as opposed to matched-pairs differences)
##
#######


library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)
library(nnet)

## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we use models where "Minute.clean" was included
include.minute <- FALSE

## Proportion of observations in the training set (0.5, 0.66)
prop.train <- 0.50


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")


load("Corners_final.df.Robj")



final.df <- final.df %>%
  arrange(League, season, gamedDate)

final.df$Outcome <- ifelse(final.df$Score.Diff > 0, 
                           "W",
                           ifelse(final.df$Score.Diff  == 0, 
                                  "D",
                                  "L"))

## Calculating the average shots (actual and adjusted) up to that point in the season
final.df <- final.df %>%
  left_join(final.df %>%
  group_by(Team) %>%
  summarise(Mean.Corners.Actual = mean(Corners),
            Mean.Corners.Adjusted = mean(Corners.Adj)))

## Making it into a wider format where each observation is a single game:
##  Home variables + Away variables
final.df <- final.df %>%
  pivot_wider(id_cols=c(League, season,gameId, gamedDate), names_from = HomeAway, values_from=c(Team,
                                                                                                Corners, Corners.Adj,
                                                                                                Mean.Corners.Actual, Mean.Corners.Adjusted, 
                                                                                                Score.Diff, Outcome))

View(head(final.df, 20))

# A list, with element per each league, and data.frame with row per season
mae.rmse <- list()
  
for (league in league.name){
  mae.rmse[[league]] <- data.frame()
  
for (curr.season in sort(unique(final.df$season[final.df$League == league]))){
  interm.df <- final.df %>% dplyr::filter(League == league, season == curr.season)
  
  interm.df %>%
    group_by()
  train.df <- interm.df[1:round(prop.train*nrow(interm.df)),]
  test.df <- interm.df[-c(1:round(prop.train*nrow(interm.df))),]
  
  # mult.obj.actual <- multinom(Outcome_Home ~ Mean.Corners.Actual_Home + Mean.Corners.Actual_Away, 
  #                             data= train.df)
  # mult.obj.adjust <- multinom(Outcome_Home ~ Mean.Corners.Adjusted_Home + Mean.Corners.Adjusted_Away, 
  #                             data= train.df)
  
  lm.obj.actual <- lm(Score.Diff_Home ~ Mean.Corners.Actual_Home + Mean.Corners.Actual_Away, 
                      data= train.df)
  lm.obj.adjust <- lm(Score.Diff_Home ~ Mean.Corners.Adjusted_Home + Mean.Corners.Adjusted_Away, 
                      data= train.df)
  
  lm.pred.actual <- predict(lm.obj.actual, newdata=test.df)
  lm.pred.adjust <- predict(lm.obj.adjust, newdata=test.df)
  
  
  
  mae.rmse[[league]] <- rbind(mae.rmse[[league]],
    data.frame(Year = curr.season,
               mae.actual = mean(abs(lm.pred.actual - test.df$Score.Diff_Home)),
               sd.mae.actual = sd(abs(lm.pred.actual - test.df$Score.Diff_Home))/sqrt(length(lm.pred.actual)),
               rmse.actual = sqrt(mean((lm.pred.actual - test.df$Score.Diff_Home)^2)),
               mae.adjust = mean(abs(lm.pred.adjust - test.df$Score.Diff_Home)),
               sd.mae.adjust = sd(abs(lm.pred.adjust - test.df$Score.Diff_Home))/sqrt(length(lm.pred.adjust)),
               rmse.adjust = sqrt(mean((lm.pred.adjust - test.df$Score.Diff_Home)^2))))
  
}
  
}



#######
## DOING A MATCHED-PAIRS T-TEST for EACH LEAGUE for EACH YEAR:
######



for (j in 1){
  cat("\n")
  cat("\n")
  cat("\n")
  print("All individual years:")
  print(mae.rmse)

  cat("\n")
  print("MAE comparisons:")
  print("Mean difference:")
  print(sapply(mae.rmse, function(x) mean(x$mae.actual - x$mae.adjust)))
  print("T-test P-values (Holm-corrected, 5 comparisons):")
  print(p.adjust(sapply(mae.rmse, function(x) t.test(x$mae.actual - x$mae.adjust)$p.value)))
  print("Wilcox Rank Test P-values (Holm-corrected, 5 comparisons):")
  print(p.adjust(sapply(mae.rmse, function(x) wilcox.test(x$mae.actual - x$mae.adjust)$p.value)))
  print("Shapiro-Wilk Test for normality:")
  print(p.adjust(sapply(mae.rmse, function(x) shapiro.test(x$mae.actual - x$mae.adjust)$p.value)))
  print("Effect size:")
  print(sapply(mae.rmse, function(x) mean(x$mae.actual - x$mae.adjust)/sd(x$mae.actual - x$mae.adjust)))
  print("95% confidence intervals:")
  print(sapply(mae.rmse, function(x) t.test(x$mae.actual - x$mae.adjust)$conf.int))

  cat("\n")
  print("RMSE comparisons:")
  print("Mean difference:")
  print(sapply(mae.rmse, function(x) mean(x$rmse.actual - x$rmse.adjust)))
  print("T-test P-values (Holm-corrected, 5 comparisons):")
  print(p.adjust(sapply(mae.rmse, function(x) t.test(x$rmse.actual - x$rmse.adjust)$p.value)))
  print("Wilcox Rank Test P-values (Holm-corrected, 5 comparisons):")
  print(p.adjust(sapply(mae.rmse, function(x) wilcox.test(x$rmse.actual - x$rmse.adjust)$p.value)))
  print("Shapiro-Wilk Test for normality:")
  print(p.adjust(sapply(mae.rmse, function(x) shapiro.test(x$rmse.actual - x$rmse.adjust)$p.value)))
  print("Effect size:")
  print(sapply(mae.rmse, function(x) mean(x$rmse.actual - x$rmse.adjust)/sd(x$rmse.actual - x$rmse.adjust)))
  print("95% confidence intervals:")
  print(sapply(mae.rmse, function(x) t.test(x$rmse.actual - x$rmse.adjust)$conf.int))
}




for (j in 1){
  print("All individual years:")
  print(mae.rmse)

  print("MAE comparisons:")
  print(sapply(mae.rmse, function(x) mean(x$mae.actual)))
  print(sapply(mae.rmse, function(x) mean(x$mae.adjust)))
  print(sapply(mae.rmse, function(x) sd(x$mae.actual)/sqrt(length(x$mae.actual))))
  print(sapply(mae.rmse, function(x) sd(x$mae.adjust)/sqrt(length(x$mae.adjust))))


  print("RMSE comparisons:")
  print(sapply(mae.rmse, function(x) mean(x$rmse.actual)))
  print(sapply(mae.rmse, function(x) mean(x$rmse.adjust)))
  print(sapply(mae.rmse, function(x) sd(x$rmse.actual)/sqrt(length(x$rmse.actual))))
  print(sapply(mae.rmse, function(x) sd(x$rmse.adjust)/sqrt(length(x$rmse.adjust))))
}






#########
#########
### SHOTS
#########
#########

## MEAN ERRORS for: Actual vs Adjusted (first two rows are the mean errors, second two rows are sd/sqrt(n))

# [1] "RMSE comparisons:"
# Bundesliga        SerieA        LaLiga        Ligue1 PremierLeague 
# 1.790468      1.572541      1.601750      1.581286      1.669130 
# Bundesliga        SerieA        LaLiga        Ligue1 PremierLeague 
# 1.782135      1.564823      1.586107      1.570159      1.660114 
# Bundesliga        SerieA        LaLiga        Ligue1 PremierLeague 
# 0.03658745    0.02975089    0.02869222    0.04346945    0.03398228 
# Bundesliga        SerieA        LaLiga        Ligue1 PremierLeague 
# 0.03625887    0.02942593    0.02768711    0.04205454    0.03473071 

#### MATCHED SAMPLE COMPARISON TESTS (MAIN STUFF)

# [1] "RMSE comparisons:"
# [1] "Mean difference:"
# Bundesliga        SerieA        LaLiga        Ligue1 PremierLeague 
# 0.008333135   0.007718015   0.015642588   0.011126666   0.009015855 
# [1] "T-test P-values (Holm-corrected, 5 comparisons):"
# Bundesliga        SerieA        LaLiga        Ligue1 PremierLeague 
# 0.0016085018  0.0016085018  0.0005416456  0.0049557282  0.0049557282 
# [1] "Wilcox Rank Test P-values (Holm-corrected, 5 comparisons):"
# Bundesliga        SerieA        LaLiga        Ligue1 PremierLeague 
# 0.0009155273  0.0012207031  0.0012207031  0.0083618164  0.0012207031 
# [1] "Shapiro-Wilk Test for normality:"
# Bundesliga        SerieA        LaLiga        Ligue1 PremierLeague 
# 0.25429288    0.25429288    0.82008966    0.11095046    0.09257257 
# [1] "Effect size:"
# Bundesliga        SerieA        LaLiga        Ligue1 PremierLeague 
# 1.1569945     1.1911607     1.3734128     0.9499353     0.9531171 
# [1] "95% confidence intervals:"
# Bundesliga      SerieA      LaLiga     Ligue1 PremierLeague
# [1,] 0.00434458 0.004129839 0.009335252 0.00464018   0.003554197
# [2,] 0.01232169 0.011306190 0.021949924 0.01761315   0.014477512



#########
#########
### CORNERS
#########
#########


#### MATCHED SAMPLE COMPARISON TESTS (MAIN STUFF)

# [1] "RMSE comparisons:"
# [1] "Mean difference:"
# Bundesliga        SerieA        LaLiga        Ligue1 PremierLeague 
# 0.016638191   0.011352243   0.031667516   0.020811011   0.009059792 
# [1] "T-test P-values (Holm-corrected, 5 comparisons):"
# Bundesliga        SerieA        LaLiga        Ligue1 PremierLeague 
# 5.283456e-04  7.004087e-04  2.714556e-06  1.734654e-04  5.283456e-04 
# [1] "Wilcox Rank Test P-values (Holm-corrected, 5 comparisons):"
# Bundesliga        SerieA        LaLiga        Ligue1 PremierLeague 
# 0.0003051758  0.0008544922  0.0003051758  0.0008544922  0.0003662109 
# [1] "Shapiro-Wilk Test for normality:"
# Bundesliga        SerieA        LaLiga        Ligue1 PremierLeague 
# 0.01650129    1.00000000    1.00000000    1.00000000    0.48713714 
# [1] "Effect size:"
# Bundesliga        SerieA        LaLiga        Ligue1 PremierLeague 
# 1.304856      1.116428      2.234250      1.506383      1.370144 
# [1] "95% confidence intervals:"
# Bundesliga      SerieA     LaLiga     Ligue1 PremierLeague
# [1,] 0.009576934 0.005721192 0.02381840 0.01316040    0.00524197
# [2,] 0.023699448 0.016983294 0.03951663 0.02846162    0.01287761
