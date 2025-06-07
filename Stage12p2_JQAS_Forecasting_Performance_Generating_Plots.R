######
## Stage 12p2: Source code producing Figure 7 from the main manuscript
##
##
## Creating the plot, where:
## 1. Each league will be reflected on the Y axis (as a separate row)
## 2. Each year's difference in RMSE between actual and adjusted predictions will be a horizontally placed point
## 3. The 95% confidence intervals will be marked with bars
## 4. There will be two panels: one for shots, one for corners.
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


# A list, with element per each league, and data.frame with row per season
mae.rmse_Shots <- mae.rmse_Corners <- list()




######
## Shots
######

load("Shots_final.df.Robj")


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
              summarise(Mean.Shots.Actual = mean(Shots),
                        Mean.Shots.Adjusted = mean(Shots.Adj)))

## Making it into a wider format where each observation is a single game:
##  Home variables + Away variables
final.df <- final.df %>%
  pivot_wider(id_cols=c(League, season,gameId, gamedDate), names_from = HomeAway, values_from=c(Team,
                                                                                                Shots, Shots.Adj,
                                                                                                Mean.Shots.Actual, Mean.Shots.Adjusted, 
                                                                                                Score.Diff, Outcome))



for (league in league.name){
  mae.rmse_Shots[[league]] <- data.frame()
  
  for (curr.season in sort(unique(final.df$season[final.df$League == league]))){
    interm.df <- final.df %>% dplyr::filter(League == league, season == curr.season)
    
    interm.df %>%
      group_by()
    train.df <- interm.df[1:round(prop.train*nrow(interm.df)),]
    test.df <- interm.df[-c(1:round(prop.train*nrow(interm.df))),]
    

    lm.obj.actual <- lm(Score.Diff_Home ~ Mean.Shots.Actual_Home + Mean.Shots.Actual_Away, 
                        data= train.df)
    lm.obj.adjust <- lm(Score.Diff_Home ~ Mean.Shots.Adjusted_Home + Mean.Shots.Adjusted_Away, 
                        data= train.df)
    
    lm.pred.actual <- predict(lm.obj.actual, newdata=test.df)
    lm.pred.adjust <- predict(lm.obj.adjust, newdata=test.df)
    
    
    
    mae.rmse_Shots[[league]] <- rbind(mae.rmse_Shots[[league]],
                                        data.frame(Year = curr.season,
                                                   mae.actual = mean(abs(lm.pred.actual - test.df$Score.Diff_Home)),
                                                   sd.mae.actual = sd(abs(lm.pred.actual - test.df$Score.Diff_Home))/sqrt(length(lm.pred.actual)),
                                                   rmse.actual = sqrt(mean((lm.pred.actual - test.df$Score.Diff_Home)^2)),
                                                   mae.adjust = mean(abs(lm.pred.adjust - test.df$Score.Diff_Home)),
                                                   sd.mae.adjust = sd(abs(lm.pred.adjust - test.df$Score.Diff_Home))/sqrt(length(lm.pred.adjust)),
                                                   rmse.adjust = sqrt(mean((lm.pred.adjust - test.df$Score.Diff_Home)^2))))
    
  }
  
  mae.rmse_Shots[[league]]$rmse.diff <- mae.rmse_Shots[[league]]$rmse.actual - mae.rmse_Shots[[league]]$rmse.adjust
  mae.rmse_Shots[[league]]$mean.rmse.diff <- mean(mae.rmse_Shots[[league]]$rmse.actual - mae.rmse_Shots[[league]]$rmse.adjust)
  
  CI <- t.test(mae.rmse_Shots[[league]]$rmse.actual - mae.rmse_Shots[[league]]$rmse.adjust)$conf.int
  mae.rmse_Shots[[league]]$Left.95.CI <- CI[1]
  mae.rmse_Shots[[league]]$Right.95.CI <- CI[2]
  
  p.val <- t.test(mae.rmse_Shots[[league]]$rmse.actual - mae.rmse_Shots[[league]]$rmse.adjust)$p.value
  mae.rmse_Shots[[league]]$p.val <- p.val
  
}







######
## Corners
######

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



for (league in league.name){
  mae.rmse_Corners[[league]] <- data.frame()
  
  for (curr.season in sort(unique(final.df$season[final.df$League == league]))){
    interm.df <- final.df %>% dplyr::filter(League == league, season == curr.season)
    
    interm.df %>%
      group_by()
    train.df <- interm.df[1:round(prop.train*nrow(interm.df)),]
    test.df <- interm.df[-c(1:round(prop.train*nrow(interm.df))),]
    

    lm.obj.actual <- lm(Score.Diff_Home ~ Mean.Corners.Actual_Home + Mean.Corners.Actual_Away, 
                        data= train.df)
    lm.obj.adjust <- lm(Score.Diff_Home ~ Mean.Corners.Adjusted_Home + Mean.Corners.Adjusted_Away, 
                        data= train.df)
    
    lm.pred.actual <- predict(lm.obj.actual, newdata=test.df)
    lm.pred.adjust <- predict(lm.obj.adjust, newdata=test.df)
    
    
    
    mae.rmse_Corners[[league]] <- rbind(mae.rmse_Corners[[league]],
                                data.frame(Year = curr.season,
                                           mae.actual = mean(abs(lm.pred.actual - test.df$Score.Diff_Home)),
                                           sd.mae.actual = sd(abs(lm.pred.actual - test.df$Score.Diff_Home))/sqrt(length(lm.pred.actual)),
                                           rmse.actual = sqrt(mean((lm.pred.actual - test.df$Score.Diff_Home)^2)),
                                           mae.adjust = mean(abs(lm.pred.adjust - test.df$Score.Diff_Home)),
                                           sd.mae.adjust = sd(abs(lm.pred.adjust - test.df$Score.Diff_Home))/sqrt(length(lm.pred.adjust)),
                                           rmse.adjust = sqrt(mean((lm.pred.adjust - test.df$Score.Diff_Home)^2))))
    
    
    
  }
  
  mae.rmse_Corners[[league]]$rmse.diff <- mae.rmse_Corners[[league]]$rmse.actual - mae.rmse_Corners[[league]]$rmse.adjust
  mae.rmse_Corners[[league]]$mean.rmse.diff <- mean(mae.rmse_Corners[[league]]$rmse.actual - mae.rmse_Corners[[league]]$rmse.adjust)
  
  CI <- t.test(mae.rmse_Corners[[league]]$rmse.actual - mae.rmse_Corners[[league]]$rmse.adjust)$conf.int
  mae.rmse_Corners[[league]]$Left.95.CI <- CI[1]
  mae.rmse_Corners[[league]]$Right.95.CI <- CI[2]
  
  p.val <- t.test(mae.rmse_Corners[[league]]$rmse.actual - mae.rmse_Corners[[league]]$rmse.adjust)$p.value
  mae.rmse_Corners[[league]]$p.val <- p.val
  
  
}



mae.rmse_Shots
mae.rmse_Corners


## Arranging the list into a single dataframe
mae.rmse_Shots.df <- mae.rmse_Corners.df <- NULL

for (league in league.name){
  mae.rmse_Shots.df <- rbind(mae.rmse_Shots.df,
                             data.frame(League=league,
                                        mae.rmse_Shots[[league]]))
  mae.rmse_Corners.df <- rbind(mae.rmse_Corners.df,
                             data.frame(League=league,
                                        mae.rmse_Corners[[league]]))
}



#####
## PLOTTING
#####

# 703, 477

library(ggplot2)
set.seed(3)



combined.df <- rbind(
  data.frame(Type = "Shots", mae.rmse_Shots.df),
  data.frame(Type = "Corners", mae.rmse_Corners.df)
)

combined.df$League <- factor(combined.df$League)
levels(combined.df$League) <- c("Bundesliga", "La Liga", "Ligue 1", "Premier League", "Serie A")

### Adding the p-values
combined.df$label <- paste0("\ \ \ p-val = ", 
                            sprintf("%.4f", combined.df$p.val),
                            ifelse(combined.df$p.val < 0.001,
                                   "***",
                                   ifelse(combined.df$p.val < 0.01,
                                          "**",
                                          ifelse(combined.df$p.val < 0.05, 
                                                 "*",
                                                 "")) ))

combined.df$Type <- ifelse(combined.df$Type =="Shots", "1. Shots", "2. Corners")

p1 <- ggplot(combined.df, aes(x = League, y = mean.rmse.diff, colour = League)) +
  geom_errorbar(aes(ymax = Left.95.CI, ymin = Right.95.CI), width = 0.4) +
  geom_point(size = 3.5) +
  geom_point(aes(x = League, y = rmse.diff), size = 1.5) +
  geom_text(aes(label = label), 
            position = position_nudge(x = 0.3),  # Nudges label upward
            size = 3,
            show.legend = FALSE) +
  geom_segment(aes(x = 5.2, xend = 0, y = 0, yend = 0), 
               linetype = "dashed", 
               color = "black", 
               linewidth = 0.5) +
  xlab("") +
  ylab(expression(RMSE[actual]- RMSE[adjusted])) +
  facet_wrap(~Type) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(combined.df$League)),
                   expand = expansion(mult = c(0, 0.2))) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(hjust = 0, color = "black"),
    axis.text.x = element_text(color = "black")
  ) +
  ggtitle("Difference in Root Mean Squared Error when Predicting Score Differential",
          subtitle = "Using actual statistics vs adjusted"
    # "Out-of-sample Prediction Error Difference (Actual - Adjusted)"
    )



## Width: 926, Height: 488
p1 +
  annotate("text", x=5.5, y=0, label=expression(H[0]: d == 0))
