#####
### Stage 5p6: Source code yielding the Figure 4. 
###            It extracts all the relevant p-values from .Robj files 
###             from Stage 5p5 ("Stage5p5_JQAS_MBM_TESTING_TERMS_AND_INTERACTIONS_BY_YEAR"),
###            applies Holm-correction, and provides a two-panel plot (one for shots, another - for corners),
###            with each league represented by a different color, p-values on y-axis,
###            terms being tested on x-axis.
#####


library(tidyverse)

########
## SHOTS
########

load("Fitted_Objects/BUCKETED_MY_OBJ_Team_AND_Season_Mixed_Eff_MBM_TESTING_TERMS_INTERACTIONS_BY_SEASON_PVALS_AIC_BIC.Robj")
my.obj[[1]]

names(my.obj[[1]]) <- c(c("Score.Diff_Minute.clean"),
                   c("Score.Diff_RedCard.Diff"),
                   c("Score.Diff_Weighted.Win.Prob"),
                   c("RedCard.Diff_Minute.clean"),
                   c("RedCard.Diff_Weighted.Win.Prob"),
                   c("Weighted.Win.Prob_Minute.clean"),
                   c("Everyone"))
league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

# the "p.value.df" will have 4 columns:
# Term (interaction or individual), League, Year, p-value (either single, or Bonferroni corrected minimum one)
p.value.df <- NULL

## First 6 have p-values for interactions

for (j in 1:6){
  for (league in league.name){
    years <- names(my.obj[[1]][[j]][[league]])
    p.vals <- sapply(my.obj[[1]][[j]][[league]], function(x){
      z <- x[grep("ti\\(", names(x))];
      p.val <- abs(min(z)*length(z))
      ifelse(p.val > 1, 1, p.val)
      })
    p.value.df.interm <- data.frame(Term = names(my.obj[[1]])[j],
                                    League = league,
                                    Year = years,
                                    P.val = p.vals
                                    )
    
    p.value.df <- rbind(p.value.df,
                        p.value.df.interm)
  }
  
}


## The 7th is just individual p-values (except for "Minute:half_id" interaction)

all.term.names <- names(my.obj[[1]][[7]][[1]][[1]])
all.term.names <- all.term.names[all.term.names != "s(Team)"]
all.term.names <- unique(str_remove(all.term.names, ":.*"))

for (term in all.term.names){
  for (league in league.name){
    years <- names(my.obj[[1]][[7]][[league]])
    p.vals <- sapply(my.obj[[1]][[7]][[league]], function(x){
      z <- x[grep(term, names(x), fixed=T)];
      p.val <- abs(min(z)*length(z))
      ifelse(p.val > 1, 1, p.val)
    })
    p.value.df.interm <- data.frame(Term = term,
                                    League = league,
                                    Year = years,
                                    P.val = p.vals
    )
    
    p.value.df <- rbind(p.value.df,
                        p.value.df.interm)
  }
}


#######
### Doing HOLM's CORRECTION
#######

p.value.df$P.val.adj <- p.value.df$P.val

all.unique.term.names <- unique(p.value.df$Term)
for (term in all.unique.term.names){
  p.value.df$P.val.adj[p.value.df$Term == term] <- p.adjust(p.value.df$P.val[p.value.df$Term == term])
}






tail(p.value.df)


# Width 574; Height 411

p.value.df$Term <- as.factor(p.value.df$Term)
levels(as.factor(p.value.df$Term))
levels(p.value.df$Term) <- c("Red.Card.Diff x Minute", "Red.Card.Diff x Win.Prob",
                             "Minute", "Red.Card.Diff", "Score.Diff", "Win.Prob.Diff",
                             "Score.Diff x Minute", "Score.Diff x Red.Card.Diff",
                             "Score.Diff x Win.Prob.Diff", "Win.Prob.Diff x Minute")

ggplot(p.value.df, aes(x = factor(Term, levels = c("Score.Diff","Red.Card.Diff", "Win.Prob.Diff", "Minute",
                                                   "Score.Diff x Minute", "Score.Diff x Red.Card.Diff", "Score.Diff x Win.Prob.Diff",
                                                   "Red.Card.Diff x Minute", "Red.Card.Diff x Win.Prob",
                                                   "Win.Prob.Diff x Minute")), 
                       y = P.val.adj, color = League)) +
  geom_jitter(width = 0.1, height = 0) +  # Add jitter
  labs(title = "Holm-Adjusted P-values for Respective Effects\nIn Explaining Shot Attempts",
       x = "Effect",
       y = "P-value (Log-Scaled)") +
  theme_minimal() +
  scale_y_log10() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0.01, linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("text", x = -Inf, y = 0.05, label = "0.05", vjust = -0.5, hjust = -0.1, size = 3, color = "black") +
  annotate("text", x = -Inf, y = 0.01, label = "0.01", vjust = -0.5, hjust = -0.1, size = 3, color = "black")
  #scale_color_manual(values = c("X" = "blue", "Y" = "red"))  # Custom colors



p.value.df$Stat.Type <- "1. Shot Attempts"
final.df <- p.value.df



########
### CORNERS
########


load("Fitted_Objects/BUCKETED_MY_OBJ_Team_AND_Season_Mixed_Eff_MBM_TESTING_TERMS_INTERACTIONS_BY_SEASON_PVALS_AIC_BIC_Corners.Robj")
my.obj[[1]]

names(my.obj[[1]]) <- c(c("Score.Diff_Minute.clean"),
                        c("Score.Diff_RedCard.Diff"),
                        c("Score.Diff_Weighted.Win.Prob"),
                        c("RedCard.Diff_Minute.clean"),
                        c("RedCard.Diff_Weighted.Win.Prob"),
                        c("Weighted.Win.Prob_Minute.clean"),
                        c("Everyone"))
league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

# the "p.value.df" will have 4 columns:
# Term (interaction or individual), League, Year, p-value (either single, or Bonferroni corrected minimum one)
p.value.df <- NULL

## First 6 have p-values for interactions

for (j in 1:6){
  for (league in league.name){
    years <- names(my.obj[[1]][[j]][[league]])
    p.vals <- sapply(my.obj[[1]][[j]][[league]], function(x){
      z <- x[grep("ti\\(", names(x))];
      p.val <- abs(min(z)*length(z))
      ifelse(p.val > 1, 1, p.val)
    })
    p.value.df.interm <- data.frame(Term = names(my.obj[[1]])[j],
                                    League = league,
                                    Year = years,
                                    P.val = p.vals
    )
    
    p.value.df <- rbind(p.value.df,
                        p.value.df.interm)
  }
  
}


## The 7th is just individual p-values (except for "Minute:half_id" interaction)

all.term.names <- names(my.obj[[1]][[7]][[1]][[1]])
all.term.names <- all.term.names[all.term.names != "s(Team)"]
all.term.names <- unique(str_remove(all.term.names, ":.*"))

for (term in all.term.names){
  for (league in league.name){
    years <- names(my.obj[[1]][[7]][[league]])
    p.vals <- sapply(my.obj[[1]][[7]][[league]], function(x){
      z <- x[grep(term, names(x), fixed=T)];
      p.val <- abs(min(z)*length(z))
      ifelse(p.val > 1, 1, p.val)
    })
    p.value.df.interm <- data.frame(Term = term,
                                    League = league,
                                    Year = years,
                                    P.val = p.vals
    )
    
    p.value.df <- rbind(p.value.df,
                        p.value.df.interm)
  }
}


#######
### Doing HOLM's CORRECTION
#######

p.value.df$P.val.adj <- p.value.df$P.val

all.unique.term.names <- unique(p.value.df$Term)
for (term in all.unique.term.names){
  p.value.df$P.val.adj[p.value.df$Term == term] <- p.adjust(p.value.df$P.val[p.value.df$Term == term])
}






tail(p.value.df)


# Width 574; Height 411

p.value.df$Term <- as.factor(p.value.df$Term)
levels(as.factor(p.value.df$Term))
levels(p.value.df$Term) <- c("Red.Card.Diff x Minute", "Red.Card.Diff x Win.Prob",
                             "Minute", "Red.Card.Diff", "Score.Diff", "Win.Prob.Diff",
                             "Score.Diff x Minute", "Score.Diff x Red.Card.Diff",
                             "Score.Diff x Win.Prob.Diff", "Win.Prob.Diff x Minute")

ggplot(p.value.df, aes(x = factor(Term, levels = c("Score.Diff","Red.Card.Diff", "Win.Prob.Diff", "Minute",
                                                   "Score.Diff x Minute", "Score.Diff x Red.Card.Diff", "Score.Diff x Win.Prob.Diff",
                                                   "Red.Card.Diff x Minute", "Red.Card.Diff x Win.Prob",
                                                   "Win.Prob.Diff x Minute")), 
                       y = P.val.adj, color = League)) +
  geom_jitter(width = 0.1, height = 0) +  # Add jitter
  labs(title = "Holm-Adjusted P-values for Respective Effects\nIn Explaining Corner Kicks",
       x = "Effect",
       y = "P-value (Log-Scaled)") +
  theme_minimal() +
  scale_y_log10() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0.01, linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("text", x = -Inf, y = 0.05, label = "0.05", vjust = -0.5, hjust = -0.1, size = 3, color = "black") +
  annotate("text", x = -Inf, y = 0.01, label = "0.01", vjust = -0.5, hjust = -0.1, size = 3, color = "black")
#scale_color_manual(values = c("X" = "blue", "Y" = "red"))  # Custom colors


p.value.df$Stat.Type <- "2. Corner Kicks"
final.df <- rbind(final.df,
                  p.value.df)




#########
## COMBINING THE PLOTS
##########

# Width: 713; Height: 488
ggplot(final.df, aes(x = factor(Term, levels = c("Score.Diff","Red.Card.Diff", "Win.Prob.Diff", "Minute",
                                                   "Score.Diff x Minute", "Score.Diff x Red.Card.Diff", "Score.Diff x Win.Prob.Diff",
                                                   "Red.Card.Diff x Minute", "Red.Card.Diff x Win.Prob",
                                                   "Win.Prob.Diff x Minute")), 
                       y = P.val.adj, color = League)) +
  geom_jitter(width = 0.1, height = 0) +  # Add jitter
  facet_wrap(~Stat.Type) +
  labs(title = "Holm-Adjusted P-values for Respective Effects\nIn Explaining Respective Offensive Statistics (Shots or Corners)",
       # subtitle = "(Shot Attempts or Corner Kicks)",
       x = "Effect",
       y = "P-value (Log-Scaled)") +
  # theme_minimal() +
  scale_y_log10() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0.01, linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("text", x = -Inf, y = 0.05, label = "0.05", vjust = -0.5, hjust = -0.1, size = 3, color = "black") +
  annotate("text", x = -Inf, y = 0.01, label = "0.01", vjust = -0.5, hjust = -0.1, size = 3, color = "black")


