####
## Stage 3p9: We use the .Robj files from Stage 3p8 that contain 
##            all goodness-of-fit test p-values, 
##            to create the Figure 1 plot 
#####

####
## For each model (Poisson, Neg Bin, Normal, Log-Normal), we use:
##      - All uniformity p-values (along with Holm correction)
##      - All dispersion p-values (along with Holm correction)
##      - All outlier p-values (along with Holm correction)
##      - All zero-inflation p-values (for POIS vs NEGBIN specifically, along with Holm correction)
####

library(tidyverse)

load("Fitted_Objects/BUCKETED_all_pvals_DHARMa_BY_SEASON_Shots.Robj")

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")
model.names <- names(all.pvals[["Bundesliga"]])
# names(all.pvals[["Bundesliga"]]) <- paste(c(1:4), ". ", model.names, sep="")
# model.names <- paste(c(1:4), ". ", model.names, sep="")

### Poisson
final.pvals.df <- NULL

for (model in model.names){
for (league in league.name){
  all.years <- names(all.pvals[[league]][[model]])
  for (year in all.years){
    final.pvals.df <- rbind(final.pvals.df,
                            data.frame(t(all.pvals[[league]][[model]][[year]]),
                              Model.Type=model,
                              League = league,
                              Year=year))
  }
}
}


#######
### Doing HOLM's CORRECTION
#######

final.pvals.df$Uniformity.Adj <- final.pvals.df$Uniformity
final.pvals.df$Dispersion.Adj <- final.pvals.df$Dispersion
final.pvals.df$Outliers.Adj <- final.pvals.df$Outliers
final.pvals.df$ZeroInfl.Adj <- final.pvals.df$ZeroInfl


all.unique.model.names <- unique(final.pvals.df$Model.Type)
for (model in all.unique.model.names){
  final.pvals.df$Uniformity.Adj[final.pvals.df$Model.Type == model] <- p.adjust(final.pvals.df$Uniformity[final.pvals.df$Model.Type == model])
  final.pvals.df$Dispersion.Adj[final.pvals.df$Model.Type == model] <- p.adjust(final.pvals.df$Dispersion[final.pvals.df$Model.Type == model])
  final.pvals.df$Outliers.Adj[final.pvals.df$Model.Type == model] <- p.adjust(final.pvals.df$Outliers[final.pvals.df$Model.Type == model])
  final.pvals.df$ZeroInfl.Adj[final.pvals.df$Model.Type == model] <- p.adjust(final.pvals.df$ZeroInfl[final.pvals.df$Model.Type == model])
}


final.pvals.df$Dispersion.Adj[final.pvals.df$Model.Type %in% c("Normal", "LogNormal")] <- NA


final.pvals.df$Model.Type <- factor(final.pvals.df$Model.Type)
levels(final.pvals.df$Model.Type) <- c("4. LogNormal", "2. NegBin", "3. Normal", "1. Poisson")
final.pvals.df$Dispersion[final.pvals.df$Model.Type %in% c("3. Normal", "4. LogNormal")] <- NA
final.pvals.df$Model.Type <- as.character(final.pvals.df$Model.Type)

final.pvals.df.pivoted <- final.pvals.df %>%
  pivot_longer(cols=Uniformity.Adj:ZeroInfl.Adj,
               names_to = "Test.Type",
               values_to = "P.val.adj")
final.pvals.df.pivoted$Test.Type <- factor(final.pvals.df.pivoted$Test.Type)
levels(final.pvals.df.pivoted$Test.Type) <- c("3. Overdispersion", "2. Outliers", "1. Uniformity (KS)", "4. Zero Inflation")
final.pvals.df.pivoted$Test.Type <- as.character(final.pvals.df.pivoted$Test.Type)


## Width 648, Height: 451

ggplot(final.pvals.df.pivoted, 
       aes(x = Model.Type, 
           y = P.val.adj, color = League)) +
  geom_jitter(width = 0.1, height = 0) +  # Add jitter
  labs(title = "Holm-Adjusted P-values for Respective DHARMa Diagnostic Tests",
       subtitle = "Season-by-Season Fits for Shot Attempts as Response",
       x = "Model Type",
       y = "P-value") +
  #theme_minimal() +
 # scale_y_log10() +
#  ylim(c(e)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
  # geom_hline(yintercept = 0.01, linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("text", x = -Inf, y = 0.05, label = "0.05", vjust = -0.5, hjust = -0.1, size = 3, color = "black") +
  # annotate("text", x = -Inf, y = 0.01, label = "0.01", vjust = -0.5, hjust = -0.1, size = 3, color = "black") +
  facet_wrap(~Test.Type)
#scale_color_manual(values = c("X" = "blue", "Y" = "red"))  # Custom colors
