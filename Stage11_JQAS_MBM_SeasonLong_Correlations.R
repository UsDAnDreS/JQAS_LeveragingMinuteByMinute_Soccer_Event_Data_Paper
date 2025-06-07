######
## Stage 11: Source code producing Tables 5a & 5b from the main manuscript,
##           where correlations are calculated between shots/corners and 
##            * Points earned (3pts for win, 1pt for draw, 0 for loss)
##            * Shots/corners when up/down 1+ goal,
##            * Shots/corners when up/down 1+ man
##   for adjusted shots/corners per game, as opposed to actual shots/corners per game.
##
## NOTE: If you want to switch from one offensive statistic to another, 
##       use "Ctrl+F" with "Match Case" turned out in order 
##       to replace all occurrences of "Corner" for "Shot" (), or vice versa.
######


library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we use models where "Minute.clean" was included
include.minute <- FALSE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

# load(file="gam_nb_obj_Corners.Robj")

load(file=paste0("gam_nb_obj",  
                 ifelse(remove.extra, "_NO_EXTRA_TIME", ""), 
                 "_Corners.Robj"))

corr.df.points <- corr.df.lead <- corr.df.trail <- corr.df.upmen <- corr.df.downmen <- NULL


for (league in league.name){

  print(league)

  cat("\n")

  #######
  ### LOADING, SETTING UP DATA
  #######

  #  our.df <- read.csv(paste0(league, "_NO_EXTRA_TIME_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))

  ## To be used for prediction (including everything, so that totals correspond to full data)
  our.df.pred <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))

  #######
  ### GETTING ADJUSTMENTS
  #######

    ### 1. SCORE DIFFERENTIAL

    # Getting the DIFFERENTIALS (log-scale "linear" effects)
    all.score.diffs <- c(min(our.df.pred$Score.Diff):max(our.df.pred$Score.Diff))
    log.score.diff.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                                 newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                                                      Score.Diff = all.score.diffs))) -
      as.numeric(predict(gam.nb.obj[[league]],
                         newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                              Score.Diff = rep(0, length(all.score.diffs)))))

    names(log.score.diff.effects) <- all.score.diffs
    log.score.diff.effects

    # Multiplicative effects
    exp(log.score.diff.effects)

    # Doing the multiplication
    our.df.pred$Adj.Coef.ScoreDiff <-
      exp(-log.score.diff.effects)[sapply(our.df.pred$Score.Diff, function(x) which(names(log.score.diff.effects) == x))]





    ### 2. RED CARD DIFF

    all.redcard.diffs <- c(min(our.df.pred$RedCard.Diff):max(our.df.pred$RedCard.Diff))
    log.redcard.diff.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                                   newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                                                        RedCard.Diff = all.redcard.diffs))) -
      as.numeric(predict(gam.nb.obj[[league]],
                         newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                              RedCard.Diff = rep(0, length(all.redcard.diffs)))))

    names(log.redcard.diff.effects) <- all.redcard.diffs
    log.redcard.diff.effects



    # Multiplicative effects
    exp(log.redcard.diff.effects)

    # Doing the multiplication
    our.df.pred$Adj.Coef.RedCardDiff <-
      exp(-log.redcard.diff.effects)[sapply(our.df.pred$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]



    ### 3. HOME/AWAY FACTOR

    all.homeaway <- unique(our.df.pred$HomeAway)
    log.homeaway.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                               newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                                                    HomeAway = all.homeaway))) -
      as.numeric(predict(gam.nb.obj[[league]],
                         newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                              HomeAway = rep("Home", length(all.homeaway)))))

    names(log.homeaway.effects) <- all.homeaway
    log.homeaway.effects




    # Multiplicative effects
    exp(log.homeaway.effects)

    # Doing the multiplication
    our.df.pred$Adj.Coef.HomeAway <-
      exp(-log.homeaway.effects)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]

    our.df.pred <- our.df.pred %>%
      mutate(Corners.Adj = Corners*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway
             #,Corners.Adj.No.Min = Corners*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway
      )



  ### MAKING SURE TEAM NAMES ARE CORRECT (there's still some other leagues getting mistakenly mixed in here)
  team.names <- read.csv(paste0("Odds_Data/Final_matched_teams_", league, ".csv"))$Team_ESPN
  our.df.pred <- our.df.pred %>% filter(Team %in% team.names)


  # ###
  # cat("\n")
  # print(paste0("Top-10 single game shifts, ", ifelse(include.minute, "WITH MINUTE:", "NO MINUTE:")))
  #


  final.df <- our.df.pred %>%
    group_by(gameId, Team, season) %>%
    summarise(Corners = sum(Corners),
              Corners.Adj= sum(Corners.Adj),
              Corners.Diff = Corners.Adj - Corners,
              Points = ifelse(Score.Diff[1] > 0, 3,
                              ifelse(Score.Diff[1] == 0, 1,
                                     0))
              # Corners.Adj.No.Min = sum(Corners.Adj.No.Min),
              # Corners.Diff.No.Min = Corners.Adj.No.Min - Corners
    ) %>%
    ungroup() %>%
    group_by(Team, season) %>%
    summarise(Corners.Per.Game = mean(Corners),
              Corners.Adj.Per.Game = mean(Corners.Adj),
              Corners.Diff.Per.Game = Corners.Adj.Per.Game - Corners.Per.Game,
              Points.Per.Game = mean(Points),
              Points.Total = sum(Points),
              n.games = length(unique(gameId))) %>%
    ungroup() %>%
    group_by(season) %>%
    mutate(Corners.Per.Game.Rank = rank(-Corners.Per.Game, ties="first"),
           Corners.Adj.Per.Game.Rank = rank(-Corners.Adj.Per.Game, ties="first"),
           Points.Per.Game.Rank = rank(-Points.Per.Game, ties="first"),
           Points.Total.Rank = rank(Points.Total, ties="first")) %>%
    dplyr::select(Team, season, n.games,
                  Corners.Per.Game, Corners.Per.Game.Rank,
                  Corners.Adj.Per.Game, Corners.Adj.Per.Game.Rank,
                  Points.Per.Game, Points.Per.Game.Rank,
                  Points.Total, Points.Total.Rank)







  # print(head(final.df %>% filter(Corners.Diff.Per.Game > 0), 10))
  # print(head(final.df  %>% filter(Corners.Diff.Per.Game < 0), 10))

  # Corners in the lead (Minutes in the lead), Corners shorthanded (Minutes shorthanded)


  ## Getting Corners in the lead, Corners shorthanded

  scorediff.diff.ranks <- our.df.pred %>%
    # filter(Score.Diff > 0) %>%
    group_by(gameId, Team, season) %>%
    summarise(Corners.Lead = sum(Corners[Score.Diff > 0]),
              Minutes.Lead = sum(Score.Diff > 0),
              Corners.Trail = sum(Corners[Score.Diff < 0]),
              Minutes.Trail = sum(Score.Diff < 0)) %>%
    ungroup() %>%
    group_by(Team, season) %>%
    summarise(Corners.Lead.Per.Game = mean(Corners.Lead),
              Minutes.Lead.Per.Game = mean(Minutes.Lead),
              Corners.Trail.Per.Game = mean(Corners.Trail),
              Minutes.Trail.Per.Game = mean(Minutes.Trail),
              n.games = length(unique(gameId))) %>%
    ungroup() %>%
    #filter(season==2020)
    group_by(season) %>%
    mutate(Corners.Lead.Per.Game.Rank = rank(-Corners.Lead.Per.Game, ties="first"),
           Minutes.Lead.Per.Game.Rank = rank(-Minutes.Lead.Per.Game, ties="first"),
           Corners.Trail.Per.Game.Rank = rank(-Corners.Trail.Per.Game, ties="first"),
           Minutes.Trail.Per.Game.Rank = rank(-Minutes.Trail.Per.Game, ties="first")) %>%
    dplyr::select(Team, season, n.games,
                  Corners.Lead.Per.Game, Corners.Lead.Per.Game.Rank,
                  Minutes.Lead.Per.Game, Minutes.Lead.Per.Game.Rank,
                  Corners.Trail.Per.Game, Corners.Trail.Per.Game.Rank,
                  Minutes.Trail.Per.Game, Minutes.Trail.Per.Game.Rank)
  # arrange(desc(Corners.Lead.Per.Game))


  redcard.diff.ranks <- our.df.pred %>%
    #  filter(RedCard.Diff > 0) %>%
    group_by(gameId, Team, season) %>%
    summarise(Corners.DownMen = sum(Corners[RedCard.Diff > 0]),
              Minutes.DownMen = sum(RedCard.Diff > 0),
              Corners.UpMen = sum(Corners[RedCard.Diff < 0]),
              Minutes.UpMen = sum(RedCard.Diff < 0)) %>%
    ungroup() %>%
    group_by(Team, season) %>%
    summarise(Corners.DownMen.Per.Game = mean(Corners.DownMen),
              Minutes.DownMen.Per.Game = mean(Minutes.DownMen),
              Corners.UpMen.Per.Game = mean(Corners.UpMen),
              Minutes.UpMen.Per.Game = mean(Minutes.UpMen),
              n.games = length(unique(gameId))) %>%
    ungroup() %>%
    #filter(season==2020)
    group_by(season) %>%
    mutate(Corners.DownMen.Per.Game.Rank = rank(-Corners.DownMen.Per.Game, ties="first"),
           Minutes.DownMen.Per.Game.Rank = rank(-Minutes.DownMen.Per.Game, ties="first"),
           Corners.UpMen.Per.Game.Rank = rank(-Corners.UpMen.Per.Game, ties="first"),
           Minutes.UpMen.Per.Game.Rank = rank(-Minutes.UpMen.Per.Game, ties="first")) %>%
    dplyr::select(Team, season, n.games,
                  Corners.DownMen.Per.Game, Corners.DownMen.Per.Game.Rank,
                  Minutes.DownMen.Per.Game, Minutes.DownMen.Per.Game.Rank,
                  Corners.UpMen.Per.Game, Corners.UpMen.Per.Game.Rank,
                  Minutes.UpMen.Per.Game, Minutes.UpMen.Per.Game.Rank)
  #  arrange(desc(Corners.DownMen.Per.Game))



  #####
  ## CALCULATE AND REPORT CORRELATIONS BETWEEN:
  ##    * Actual Corners per game AND Points per game
  ##    * Adjusted Corners per game AND Points per game
  ##    * Rank of actual Corners per game AND Rank in Points per game
  ##    * Rank of adjusted Corners per game AND Rank in Points per game
  ## for EACH SEASON.
  ##
  ##  Then the same, but SWAPPING the "Points per game" with:
  ##    * Corners generated while leading
  ##    * Corners generated while trailing
  ##    * Corners generated while up 1+ man
  ##    * Corners generated while down 1+ man
  ######


  corr.df.points <- rbind(corr.df.points,
                          final.df %>%
                            group_by(season) %>%
                            summarise(Actual.Val.Corr=cor(Corners.Per.Game, Points.Per.Game),
                                      Actual.Rank.Corr=cor(Corners.Per.Game.Rank, Points.Per.Game.Rank),
                                      Adjust.Val.Corr=cor(Corners.Adj.Per.Game, Points.Per.Game),
                                      Adjust.Rank.Corr=cor(Corners.Adj.Per.Game.Rank, Points.Per.Game.Rank)) %>%
                            mutate(League = league, Type="1. Points"))


  corr.df.lead <- rbind(corr.df.lead,
                        final.df %>%
                          full_join(scorediff.diff.ranks, by=c("season", "Team")) %>%
                          group_by(season) %>%
          summarise(Actual.Val.Corr=cor(Corners.Per.Game, Corners.Lead.Per.Game),
                    Actual.Rank.Corr=cor(Corners.Per.Game.Rank, Corners.Lead.Per.Game.Rank),
                    Adjust.Val.Corr=cor(Corners.Adj.Per.Game, Corners.Lead.Per.Game),
                    Adjust.Rank.Corr=cor(Corners.Adj.Per.Game.Rank, Corners.Lead.Per.Game.Rank)) %>%
            mutate(League = league, Type="2. Corners Up 1+ Goal"))


  corr.df.trail <- rbind(corr.df.trail,
                         final.df %>%
          full_join(scorediff.diff.ranks, by=c("season", "Team")) %>%
          group_by(season) %>%
          summarise(Actual.Val.Corr=cor(Corners.Per.Game, Corners.Trail.Per.Game),
                    Actual.Rank.Corr=cor(Corners.Per.Game.Rank, Corners.Trail.Per.Game.Rank),
                    Adjust.Val.Corr=cor(Corners.Adj.Per.Game, Corners.Trail.Per.Game),
                    Adjust.Rank.Corr=cor(Corners.Adj.Per.Game.Rank, Corners.Trail.Per.Game.Rank)) %>%
            mutate(League = league, Type="3. Corners Down 1+ Goal"))



  corr.df.upmen <- rbind(corr.df.upmen,
                         final.df %>%
                           full_join(redcard.diff.ranks, by=c("season", "Team")) %>%
                           group_by(season) %>%
          summarise(Actual.Val.Corr=cor(Corners.Per.Game, Corners.UpMen.Per.Game),
                    Actual.Rank.Corr=cor(Corners.Per.Game.Rank, Corners.UpMen.Per.Game.Rank),
                    Adjust.Val.Corr=cor(Corners.Adj.Per.Game, Corners.UpMen.Per.Game),
                    Adjust.Rank.Corr=cor(Corners.Adj.Per.Game.Rank, Corners.UpMen.Per.Game.Rank)) %>%
            mutate(League = league, Type="4. Corners Up 1+ Men"))




  corr.df.downmen <- rbind(corr.df.downmen,
                           final.df %>%
                             full_join(redcard.diff.ranks, by=c("season", "Team")) %>%
                             group_by(season) %>%
          summarise(Actual.Val.Corr=cor(Corners.Per.Game, Corners.DownMen.Per.Game),
                    Actual.Rank.Corr=cor(Corners.Per.Game.Rank, Corners.DownMen.Per.Game.Rank),
                    Adjust.Val.Corr=cor(Corners.Adj.Per.Game, Corners.DownMen.Per.Game),
                    Adjust.Rank.Corr=cor(Corners.Adj.Per.Game.Rank, Corners.DownMen.Per.Game.Rank)) %>%
            mutate(League = league, Type="5. Corners Down 1+ Men"))


  cat("\n")
  cat("\n")
}



cat("\n")
cat("Correlations with Points")
print(corr.df.points)

cat("\n")
cat("Correlations with Stats while Leading")
print(corr.df.lead)

cat("\n")
cat("Correlations with Stats while Trailing")
print(corr.df.trail)

cat("\n")
cat("Correlations with Stats while Up 1+ Men")

print(corr.df.upmen)

cat("\n")
cat("Correlations with Stats while Down 1+ Men")

print(corr.df.downmen)



our.df.values <-
rbind(corr.df.points,
      corr.df.lead,
      corr.df.trail,
      corr.df.upmen,
      corr.df.downmen)

save(our.df.values, file="Corners_our.df.values.Robj")


load(file="Shots_our.df.values.Robj")


ggplot(data=our.df.values %>% 
         rename(Actual = Actual.Val.Corr,
                Adjusted=Adjust.Val.Corr) %>%
         pivot_longer(cols=c("Actual", "Adjusted")),
       aes(x=name, y=value)) +
  facet_wrap(~Type, ncol=5) +
  geom_boxplot() +
  ylab("Correlation") +
  xlab("Statistic Type")  +
  ggtitle("Value correlations for Corners")


ggplot(data=our.df.values %>% 
         rename(Actual = Actual.Rank.Corr,
                Adjusted=Adjust.Rank.Corr) %>%
         pivot_longer(cols=c("Actual", "Adjusted")),
       aes(x=name, y=value)) +
  facet_wrap(~Type, ncol=5) +
  geom_boxplot() +
  ylab("Correlation") +
  xlab("Statistic Type") +
  ggtitle("Rank correlations for Corners")
  


our.df.values %>% 
  rename(Actual = Actual.Val.Corr,
         Adjusted=Adjust.Val.Corr) %>%
  group_by(Type) %>%
  summarise(mean.actual = mean(Actual),
            SD.actual = sd(Actual),
            SE.actual=sd(Actual)/sqrt(length(Actual)),
            mean.adjust = mean(Adjusted),
            SD.adjust=sd(Adjusted),
            SE.adjust = sd(Adjusted)/sqrt(length(Adjusted)))



#####
## DO THOSE AS SIDE-BY-SIDE TABLES IN THE PAPER???
#####

######
## SHOTS
#######

# A tibble: 5 × 5
# Type                  mean.actual SE.actual mean.adjust SE.adjust
# <chr>                       <dbl>     <dbl>       <dbl>     <dbl>
# 1 1. Points                  0.718    0.0129       0.787    0.0105 
# 2 2. Shots Up 1+ Goal        0.829    0.00892      0.894    0.00621
# 3 3. Shots Down 1+ Goal     -0.254    0.0214      -0.370    0.0195 
# 4 4. Shots Up 1+ Men         0.190    0.0282       0.129    0.0279 
# 5 5. Shots Down 1+ Men       0.0505   0.0240       0.0888   0.0242 


#######
### CORNERS
#######

# A tibble: 5 × 5
# Type                    mean.actual sd.actual mean.adjust sd.adjust
# <chr>                         <dbl>     <dbl>       <dbl>     <dbl>
# 1 1. Points                    0.603     0.0186      0.737    0.0138 
# 2 2. Corners Up 1+ Goal        0.714     0.0162      0.849    0.00963
# 3 3. Corners Down 1+ Goal     -0.0565    0.0284     -0.252    0.0270 
# 4 4. Corners Up 1+ Men         0.151     0.0282      0.0933   0.0286 
# 5 5. Corners Down 1+ Men       0.0590    0.0307      0.0901   0.0301 