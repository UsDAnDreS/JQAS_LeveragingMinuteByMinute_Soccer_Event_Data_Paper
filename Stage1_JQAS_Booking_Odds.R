####
## Stage 1: Putting together the booking odds files (one for each league)
##          Eventually writing them into a file as follows:
##
## write.csv(file=paste0("full_booking_odds_file_Sami_files_", league, ".csv"),
##          full.book.df,
##          row.names = F)
####


library(tidyverse)

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

for (league in league.name){
  print(league)
  
####
## Obtaining odds for each league
####
  
path.name <- paste0("Odds_Data/Oddsportal Odds/", league, "-Odds/")
all.book.files <- list.files(path.name)

our.book.df <- NULL
for (j in 1:length(all.book.files)){
  our.book.df <- rbind(our.book.df,
                       read.csv(paste0(path.name, all.book.files[j]), header=T))
}

dim(our.book.df)

## For Premier League, the date format is different for some reason
## (It's "9-May-10" instead of "9 May 2010")
if (league == "PremierLeague"){
  our.book.df$Date <- format(as.Date(our.book.df$Date, format = "%d-%B-%y"), "%d %b %Y")
}




##########
## Creating "Weighted.Win.Prob.Home"
##  p_win*1 + p_draw*0 + p_loss*(-1)
## (making sure that none of the p's are Inf or Negative)
##########

our.book.df$Weighted.Win.Prob.Home <- ifelse(our.book.df$HomeBet > 1 & our.book.df$DrawBet > 1 & our.book.df$AwayBet > 1,
                                             (1)*1/our.book.df$HomeBet  + (0)*1/our.book.df$DrawBet + (-1)* 1/our.book.df$AwayBet,
                                             NA)
summary(our.book.df$Weighted.Win.Prob.Home)
View(our.book.df)


#####
## Getting MATCHED TEAM NAME LISTS (between ESPN & OddsPortal)
#####

name.match.list <- read.csv(paste0("Odds_Data/Final_matched_teams_", league, ".csv"))

#######
## Joining the ESPN names onto the Booking odds 
## (because each Booking odds name will have a UNIQUE ESPN name match,
##  NOT VICE VERSA)
#######

full.book.df <- our.book.df %>%
  left_join(name.match.list,
            by = c("HomeTeam" = "Team_ODDS")) %>%
  left_join(name.match.list,
            by = c("AwayTeam" = "Team_ODDS")) %>%
  rename(HomeTeam.espn = Team_ESPN.x,
         AwayTeam.espn = Team_ESPN.y)

write.csv(file=paste0("full_booking_odds_file_Sami_files_", league, ".csv"),
          full.book.df,
          row.names = F)

}