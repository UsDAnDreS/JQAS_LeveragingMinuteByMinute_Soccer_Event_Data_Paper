#####
## Stage 8p2: Code yielding Figure 6 of the main manuscript,
##            capturing the multiplicative coefficients for statistical adjustments
##            (along with their uncertainty bands).
##
##            It utilizes .Robj files obtained from "Stage8_JQAS_MBM_Statistical_Adjustment_NEGBIN_BUCKETED.R"
#####

library(tidyverse)

#########
#########
#### SCORE DIFF
#########
#########

# Obtained the coefficients from "Stage8_MBM_Statistical_Adjustment_NEGBIN_BUCKETED.R"
load(file="my.obj.adj.coefs.shots.Robj")

my.obj.shots[[1]]$Bundesliga$Value <- rownames(my.obj.shots[[1]]$Bundesliga)


our.df.score <- NULL

for (j in 1:5){
  my.obj.shots[[1]][[j]]$Value <- rownames(my.obj.shots[[1]][[j]])
  my.obj.shots[[1]][[j]]$League <- names(my.obj.shots[[1]])[j]
  my.obj.shots[[1]][[j]]$Statistic <- "1. Shots"
  our.df.score <- rbind(our.df.score,
                  my.obj.shots[[1]][[j]])
}

our.df.score$Value <- as.numeric(our.df.score$Value)



load(file="my.obj.adj.coefs.corners.Robj")


my.obj.corners[[1]]$Bundesliga$Value <- rownames(my.obj.corners[[1]]$Bundesliga)


# our.df.score <- NULL

for (j in 1:5){
  my.obj.corners[[1]][[j]]$Value <- rownames(my.obj.corners[[1]][[j]])
  my.obj.corners[[1]][[j]]$League <- names(my.obj.corners[[1]])[j]
  my.obj.corners[[1]][[j]]$Statistic <- "2. Corners"
  our.df.score <- rbind(our.df.score,
                  my.obj.corners[[1]][[j]])
}




ggplot(our.df.score, aes(x = Value, y = Adj, group = interaction(Statistic, League), color = League, linetype = Statistic)) +
  geom_point(size = 2) +  # Plot the average values
  # geom_jitter(size = 2, width = 0.2, height = 0) + 
  geom_errorbar(aes(ymin = Adj.Max, ymax = Adj.Min, linetype = Statistic), width = 0.2
                # , position = position_jitter(width = 0.2, height = 0)
                ) +  # Add error bars
  geom_line() +  # Add lines connecting points within the same Statistic and League
  theme_minimal() +  # Minimal theme
  labs(title = "Multiplicative Coefficients for shot attempts generated during respective score (A) and red card (B) differentials to project those on the scenario of a tied game with equal number of men.", 
       x = "Score Differential", y = "Value")  # Titles and labels




#########
#########
#### RED CARD DIFF
#########
#########

# Obtained the coefficients from "Stage8_MBM_Statistical_Adjustment_NEGBIN_BUCKETED.R"
load(file="my.obj.adj.coefs.shots.Robj")

my.obj.shots[[2]]$Bundesliga$Value <- rownames(my.obj.shots[[2]]$Bundesliga)


our.df.redcard <- NULL

for (j in 1:5){
  my.obj.shots[[2]][[j]]$Value <- rownames(my.obj.shots[[2]][[j]])
  my.obj.shots[[2]][[j]]$League <- names(my.obj.shots[[2]])[j]
  my.obj.shots[[2]][[j]]$Statistic <- "1. Shots"
  our.df.redcard <- rbind(our.df.redcard,
                        my.obj.shots[[2]][[j]])
}

our.df.redcard$Value <- as.numeric(our.df.redcard$Value)




load(file="my.obj.adj.coefs.corners.Robj")


my.obj.corners[[2]]$Bundesliga$Value <- rownames(my.obj.corners[[2]]$Bundesliga)


# our.df.redcard <- NULL

for (j in 1:5){
  my.obj.corners[[2]][[j]]$Value <- rownames(my.obj.corners[[2]][[j]])
  my.obj.corners[[2]][[j]]$League <- names(my.obj.corners[[2]])[j]
  my.obj.corners[[2]][[j]]$Statistic <- "2. Corners"
  our.df.redcard <- rbind(our.df.redcard,
                        my.obj.corners[[2]][[j]])
}



#################

our.df.score$Context <- " Score Differential"
our.df.redcard$Context <- "Red Card Differential"

our.df.score$Value <- factor(our.df.score$Value)
levels(our.df.score$Value) <- c("-1", "-2", "-3\nor more", "0", "+1", "+2", "+3\nor more")
our.df.score$Value <- factor(our.df.score$Value, 
                             levels=c("-3\nor more", "-2\nor more", "-2", "-1", "0", "+1", "+2", "+2\nor more", "+3\nor more"))

our.df.redcard$Value <- factor(our.df.redcard$Value)
levels(our.df.redcard$Value) <- c("-1", "-2\nor more", "0", "+1", "+2\nor more")

our.df.redcard$Value <- factor(our.df.redcard$Value, 
                               levels=c("-3\nor more", "-2\nor more", "-2", "-1", "0", "+1", "+2", "+2\nor more", "+3\nor more"))




our.df <- rbind(our.df.score,
                our.df.redcard)
# our.df$Value <- factor()

our.df$League <- factor(our.df$League)
levels(our.df$League) <- c("Bundesliga",  "La Liga", 
                           "Ligue 1", "Premier League", "Serie A")

## Width 653, Height 450

ggplot(our.df, aes(x = Value, y = Adj, group = interaction(Statistic, League), color = League, linetype = Statistic)) +
  geom_point(size = 2) +  # Plot the average values
  scale_y_continuous(
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25),
    labels= as.character(c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25)),
    limits=c(0.4,3.5)
  ) +
  # geom_jitter(size = 2, width = 0.2, height = 0) + 
  geom_errorbar(aes(ymin = Adj.Max, ymax = Adj.Min, linetype = Statistic), width = 0.2
                # , position = position_jitter(width = 0.2, height = 0)
  ) +  # Add error bars
  geom_line() +  # Add lines connecting points within the same Statistic and League
  theme_minimal() +  # Minimal theme
  facet_wrap(~Context, strip.position = "bottom", scales = "free_x") +
  scale_x_discrete(
   # breaks = function(x) sort(unique(x)),  # Custom breaks based on data range
    labels = function(x) unique(x)  # Custom labels based on index
  )  +
 # ylim(0.4, 3.5) +
  labs(title = "Multiplicative Coefficients for Adjusting Shot Attempts and Corner Kicks",
      subtitle = "(Projecting on the scenario of a tied home game with equal number of men).", 
       x = "", y = "Multiplicative Coefficient (with 95% CI)")  # Titles and labels
