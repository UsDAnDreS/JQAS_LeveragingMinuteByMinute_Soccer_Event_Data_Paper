#######
##  Stage 6: Code yielding Figure 8 in the Appendix of the main manuscript,
##           showing the nature of estimated effects for 
##           score diff, red card diff, win prob diff, game minute in 1st and 2nd halves respectively,
##           for the case of
##           SMOOTHING SPLINES (no bucketing of extremes)
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


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we want to still keep 1st half extra time
keep.1st.half.extra <- TRUE

## Using the reduced version of ZIP?
reduced.ver <- FALSE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")



#########
### SHOTS
#########

league <- "Bundesliga"
load(paste0("Fitted_Objects/gam_nb_obj_Team_Season_Additive_Mixed_Eff_",
            ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
            ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
            "_", league,
            "_Shots.Robj"))


# # Compare smooths across the models
Bundesliga <- gam.nb.obj[["Bundesliga"]]
rm(gam.nb.obj)

all.smooths <- list()

for (league in league.name[-1]){
  
  load(paste0("Fitted_Objects/gam_nb_obj_Team_Season_Additive_Mixed_Eff_",
              ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
              ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
              "_", league,
              "_Shots.Robj"))
  
  all.smooths[[league]] <- gam.nb.obj[[league]]
  rm(gam.nb.obj)
}

comp <- gratia::compare_smooths(model = Bundesliga,
                                'Serie A' = all.smooths[["SerieA"]], 
                                'Ligue 1' = all.smooths[["Ligue1"]], 
                                "Premier League" = all.smooths[["PremierLeague"]], 
                                "La Liga" = all.smooths[["LaLiga"]])


# Draw the comparison and store the plot objects
plots <- gratia::draw(comp)

# Set the desired y-axis limits
y_limits <- c(-1.0, 1.0)  # Example y-axis limits

# Extract individual plots from the patchwork object
plot_list <- list()
for (i in 1:length(plots)) {
  plot_list[[i]] <- plots[[i]]
}

# Plots 1 & 2: s(Minute.clean) x half_id
# Plot 3: s(RedCard.Diff)
# Plot 4: s(Score.Diff)
# Plot 6: s(Weighted.Win.Prob)

# Adjust the y-axis limits for the second plot and remove legends for all plots
adjusted_plots_shots <- lapply(seq_along(plots), function(i) {
  plot <- plots[[i]]
  if (i %in% c(1,2)){
    plot + 
      labs(y = "Shots/Min", x= paste0("Minute, ", ifelse(i==1, "1st", "2nd"), " half")) +
      coord_cartesian(ylim = y_limits) + 
      theme_minimal() +
      theme(plot.title = element_blank())
  } else if (i == 3) {
    plot + 
      labs(y = "Shots/Min", x= "Red Card Difference") +
      geom_vline(xintercept = c(-2:2), linetype = "dashed", color = "black") +
      coord_cartesian(ylim = y_limits) + 
      scale_x_continuous(breaks = -2:2) +  # Set custom breaks on x-axis
      theme_minimal() +
      theme(plot.title = element_blank())
  } else if (i == 4) {
    plot + 
      labs(y = "Shots/Min", x= "Score Difference") +
      geom_vline(xintercept = c(-2:2), linetype = "dashed", color = "black") +
      coord_cartesian(ylim = y_limits) + 
      scale_x_continuous(breaks = -8:8) +  # Set custom breaks on x-axis
      theme_minimal() +
      theme(plot.title = element_blank())
  } else if (i==6){
    plot +
      labs(y = "Shots/Min", x= ifelse(i==1, "Minute", "Win Probability Difference")) +
      coord_cartesian(ylim = y_limits) + 
      theme_minimal() +
      theme(plot.title = element_blank())
  } 
})


## For the slides presentation
# adjusted_plots_shots_short1 <- adjusted_plots_shots[c(3, 2)]
# adjusted_plots_shots_short2 <- adjusted_plots_shots[c(1, 4)]
adjusted_plots_shots_short1 <- adjusted_plots_shots[c(4,3,2)]
adjusted_plots_shots_short2 <- adjusted_plots_shots[c(4,3,2)]


# Reorder the plots based on the x-axis variable
# adjusted_plots_shots <- adjusted_plots_shots[c(3, 2, 1)]
adjusted_plots_shots <- adjusted_plots_shots[c(4,3,6,1,2)]

wrap_plots(adjusted_plots_shots) +
  plot_layout(ncol = 1, byrow=FALSE) +
  plot_layout(guides = "collect")  &
  guides(color = guide_legend(title = "Leagues"), fill = guide_legend(title = "Leagues"))




########
## CORNERS
########

league <- "Bundesliga"
load(paste0("Fitted_Objects/gam_nb_obj_Team_Season_Additive_Mixed_Eff_",
            ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
            ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
            "_", league,
            "_Corners.Robj"))


# # Compare smooths across the models
Bundesliga <- gam.nb.obj[["Bundesliga"]]
rm(gam.nb.obj)

all.smooths <- list()

for (league in league.name[-1]){
  
  load(paste0("Fitted_Objects/gam_nb_obj_Team_Season_Additive_Mixed_Eff_",
              ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
              ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
              "_", league,
              "_Corners.Robj"))
  
  all.smooths[[league]] <- gam.nb.obj[[league]]
  rm(gam.nb.obj)
}

comp <- gratia::compare_smooths(model = Bundesliga,
                                'Serie A' = all.smooths[["SerieA"]], 
                                'Ligue 1' = all.smooths[["Ligue1"]], 
                                "Premier League" = all.smooths[["PremierLeague"]], 
                                "La Liga" = all.smooths[["LaLiga"]])


# Draw the comparison and store the plot objects
plots <- gratia::draw(comp)

# Set the desired y-axis limits
y_limits <- c(-1.0, 1.0)  # Example y-axis limits

# Extract individual plots from the patchwork object
plot_list <- list()
for (i in 1:length(plots)) {
  plot_list[[i]] <- plots[[i]]
}

# Plots 1 & 2: s(Minute.clean) x half_id
# Plot 3: s(RedCard.Diff)
# Plot 4: s(Score.Diff)
# Plot 6: s(Weighted.Win.Prob)

# Adjust the y-axis limits for the second plot and remove legends for all plots
adjusted_plots_corners <- lapply(seq_along(plots), function(i) {
  plot <- plots[[i]]
  if (i %in% c(1,2)){
    plot + 
      labs(y = "Corners/Min", x= paste0("Minute, ", ifelse(i==1, "1st", "2nd"), " half")) +
      coord_cartesian(ylim = y_limits) + 
      theme_minimal() +
      theme(plot.title = element_blank())
  } else if (i == 3) {
    plot + 
      labs(y = "Corners/Min", x= "Red Card Difference") +
      geom_vline(xintercept = c(-2:2), linetype = "dashed", color = "black") +
      coord_cartesian(ylim = y_limits) + 
      scale_x_continuous(breaks = -2:2) +  # Set custom breaks on x-axis
      theme_minimal() +
      theme(plot.title = element_blank())
  } else if (i == 4) {
    plot + 
      labs(y = "Corners/Min", x= "Score Difference") +
      geom_vline(xintercept = c(-2:2), linetype = "dashed", color = "black") +
      coord_cartesian(ylim = y_limits) + 
      scale_x_continuous(breaks = -8:8) +  # Set custom breaks on x-axis
      theme_minimal() +
      theme(plot.title = element_blank())
  } else if (i==6){
    plot +
      labs(y = "Corners/Min", x= ifelse(i==1, "Minute", "Win Probability Difference")) +
      coord_cartesian(ylim = y_limits) + 
      theme_minimal() +
      theme(plot.title = element_blank())
  } 
})


## For the slides presentation
# adjusted_plots_corners_short1 <- adjusted_plots_corners[c(3, 2)]
# adjusted_plots_corners_short2 <- adjusted_plots_corners[c(1, 4)]
adjusted_plots_corners_short1 <- adjusted_plots_corners[c(4,3,2)]
adjusted_plots_corners_short2 <- adjusted_plots_corners[c(4,3,2)]


# Reorder the plots based on the x-axis variable
# adjusted_plots_shots <- adjusted_plots_shots[c(3, 2, 1)]
adjusted_plots_corners <- adjusted_plots_corners[c(4,3,6,1,2)]

wrap_plots(adjusted_plots_shots) +
  plot_layout(ncol = 1, byrow=FALSE) +
  plot_layout(guides = "collect")  &
  guides(color = guide_legend(title = "Leagues"), fill = guide_legend(title = "Leagues"))





#######
## COMBINING SHOTS & CORNERS
#######

# Combine the adjusted plots back into a patchwork object
adjusted_patchwork <- wrap_plots(append(adjusted_plots_shots, adjusted_plots_corners))

#print(adjusted_plots_shots)

# Set the size of each plot (adjust as needed)
adjusted_patchwork <- adjusted_patchwork + 
  plot_layout(ncol = 2, byrow=FALSE)

# Print the adjusted patchwork object with one common legend
#

## For PAPER (with WIN PROB DIFFERENCE)
## width: 737;   height: 488
print(adjusted_patchwork + 
        plot_layout(guides = "collect") & 
        guides(color = guide_legend(title = "Leagues"), fill = guide_legend(title = "Leagues")))



# ##### For the SLIDES PRESENTATION
# 
# ## 1. SCORE & REDCARD DIFF
# 
# # Combine the adjusted plots back into a patchwork object
# adjusted_patchwork_short1 <- wrap_plots(append(adjusted_plots_shots_short1, adjusted_plots_corners_short1))
# 
# #print(adjusted_plots_shots)
# 
# # Set the size of each plot (adjust as needed)
# adjusted_patchwork_short1 <- adjusted_patchwork_short1 + 
#   plot_layout(ncol = 2, byrow=FALSE)
# 
# # Print the adjusted patchwork object with one common legend
# #
# # Width: 698; Height: 465
# print(adjusted_patchwork_short1 + 
#         plot_layout(guides = "collect") & 
#         guides(color = guide_legend(title = "Leagues"), fill = guide_legend(title = "Leagues")))
# 
# 
# ## 2. Minute
# 
# # Combine the adjusted plots back into a patchwork object
# adjusted_patchwork_short2 <- wrap_plots(append(adjusted_plots_shots_short2, adjusted_plots_corners_short2))
# 
# #print(adjusted_plots_shots)
# 
# # Set the size of each plot (adjust as needed)
# adjusted_patchwork_short2 <- adjusted_patchwork_short2 + 
#   plot_layout(ncol = 2, byrow=FALSE)
# 
# # Print the adjusted patchwork object with one common legend
# #
# # Width: 746  Height: 457
# print(adjusted_patchwork_short2 + 
#         plot_layout(guides = "collect") & 
#         guides(color = guide_legend(title = "Leagues"), fill = guide_legend(title = "Leagues")))
# 
