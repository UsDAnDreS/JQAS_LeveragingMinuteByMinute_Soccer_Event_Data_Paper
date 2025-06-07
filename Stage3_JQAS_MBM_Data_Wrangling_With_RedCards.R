####
## Stage 3: Based on the merged files from Stage 2,
##          creating the minute-by-minute event data set, where
##          all events that took place during a given minute are aggregated into 1 observation
##
##          Eventually writing them into a file as follows:
##
## write.csv(file=paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
##                       ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
##                       "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"), final.df, row.names=F)
####


######
## NOTE: there are DIFFERENT GAMES under the SAME GAME_ID!!!
## E.g. 252451 shows up as both: "Cologne FC vs someone" and "Hertha vs VfL Wolfsburg"
## => THAT TURNS OUT TO BE THE ONLY CASE, but it SHOULD BE PRUDENT TO CREATE OUR OWN ID VARIABLE based on:
##    gameId, gamedDate, HomeTeam, AwayTeam


## SOME STUFF SEEMS OUT OF ORDER:
##      - Either the homeScore, awayScore  isn't always sequentially increasing
##      - Or something is going on with the minutes
##
##  CHECK: 252731 2008-09-20 Bayern Munich  vs Werder
##
## ISSUE: if there's a score on min 45+ on the first half, then it doesn't change for minutes that are equal to or lower in the 2nd half
##        FIX: Need to find all the moments where there's lower scores for later observations, and set them to the previous, higher, score


## There are BAD ENTRY issues with detecting PARENTHESIZED TEXT STUFF:
##    * Some are due to poor recording practice (not standardized.. so the parentheses aren't reserved solely for marking the teams)
##    * Others due to bad string matching (the team mentioned in commentary gets matched to wrong team in the ESPN header)




library(tidyverse)
library(stringdist)


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- FALSE

## If we want to still keep 1st half extra time
## (That one actually doesn't kick in unless "remove.extra" were TRUE.. so we DO keep the 1st half extra time)
keep.1st.half.extra <- FALSE


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")


for (league in league.name){
  
  print(league)
  
our.df <- read.csv(paste0(league, "_Sami_All_Minutes_With_Score_Updated_WITH_BOOKING_ODDS.csv"),
                   colClasses= c(Minute="character"))
head(our.df,100)

# our.df %>% group_by(gameId) %>%
#   summarise(n.dates = length(unique(gamedDate)),
#             n.HomeTeams = length(unique(HomeTeam)),
#             n.AwayTeams = length(unique(AwayTeam))) %>%
#   filter(n.dates > 1 | n.HomeTeams > 1 | n.AwayTeams > 1)

our.df <- our.df %>%
  mutate(ID = group_indices(our.df, .dots=c("gameId", "gamedDate", "HomeTeam", "AwayTeam", "HomeBet", "DrawBet", "AwayBet", "Weighted.Win.Prob.Home"))) %>%
  arrange(ID)

print(length(unique(our.df$ID)))

###
## CHECK FOR >2 OR <2 NAMES IN PARENTHESES
###

library(stringr)
# Get the parenthesis and what is inside
parenth.list <- str_extract_all(our.df$Event, "\\([^()]+\\)")
# VAR.entries <- 

our.df$Mentioned.Team <- NA
our.df$Mentioned.Team[sapply(parenth.list, length) == 1] <- unlist(parenth.list[sapply(parenth.list, length) == 1])
our.df$Mentioned.Team[str_detect(our.df$Event, "VAR Decision")] <- NA

bad.games.w.too.many.or.too.few.team.mentions <- our.df %>%
  group_by(ID) %>%
  summarise(n.team.mentions = length(unique(Mentioned.Team))) %>%
  filter(n.team.mentions != 3) %>%
  .[["ID"]]


# print(dim(our.df %>%
#   group_by(gameId, gamedDate) %>%
#   summarise(n.team.mentions = length(unique(Mentioned.Team))) %>%
#   filter(n.team.mentions <3 )))
# 
# many.mention.ids <- our.df %>%
#   mutate(gamedDate = as.Date(gamedDate)) %>%
#   group_by(gameId, gamedDate) %>%
#   summarise(n.team.mentions = length(unique(Mentioned.Team))) %>%
#   filter(n.team.mentions == 3, gamedDate <= "2012-01-01") %>% 
#     .[["gameId"]]
# 
# View(our.df %>%
#        filter(gameId %in% many.mention.ids))
# 
# View(our.df %>%
#        filter(gameId == 278309))

print("Number of games with too many (>2 per game) or too few (<2 per game) team mentions, to be dropped:")
print(length(bad.games.w.too.many.or.too.few.team.mentions))

dim(our.df)
our.df <- our.df %>% 
  filter(!ID %in% bad.games.w.too.many.or.too.few.team.mentions)



## Bad games with good mentions (3, two teams + NA)

bad.games.w.good.mentions.but.bad.stats <- our.df %>%
  group_by(ID) %>%
  summarise(n.team.mentions = length(unique(Mentioned.Team)), total.parenth = length(Mentioned.Team[!is.na(Mentioned.Team)])) %>%
  filter(n.team.mentions == 3, total.parenth < 15) %>%
  .[["ID"]]

bad.games.w.good.mentions.but.bad.stats


print("Number of games with correct # of unique mentions (2), but too small total mentions (<15), to be dropped:")
print(length(bad.games.w.good.mentions.but.bad.stats))

dim(our.df)
our.df <- our.df %>% 
  filter(!ID %in% bad.games.w.good.mentions.but.bad.stats)


## In Serie A: one match abandoned early due to weather conditions (6 parentheses);
## In Ligue 1: 
##    * 9 parenth in a badly recorded match of 2012/2013 season; 
##    * 12 parenth in a badly recorded match of 2013/2014 season
## 15 is a good cutoff number

# our.df %>%
#   group_by(ID) %>%
#   summarise(n.team.mentions = length(unique(Mentioned.Team)), total.parenth = length(Mentioned.Team[!is.na(Mentioned.Team)])) %>%
#   filter(n.team.mentions == 3) %>%
#   arrange(total.parenth)
#   
# View(our.df %>%
#        filter(ID == 1771))





#######
## FIND ALL GAMES WHERE ONE TEAM HAD 0 RECORDED STATS
##  (CLEARLY ITS MENTIONS GOT ASSIGNED TO THE WRONG TEAM)
#######

# ## Checking the closest string match of parenthesised team names (used in game commentary) to either of Home or Away official ESPN team names
# ## Counting if both Home & Away team found their match in the commentary (n.matches = 2)
# ## Otherwise - means that only one of the teams found their match (so BAD MATCHING happened)
# 
# n.match.df <- our.df %>% 
#   group_by(gameId, Mentioned.Team) %>%
#   summarise(Which.Team = c("HomeTeam", "AwayTeam")[which.min(c(stringdist(HomeTeam[1], Mentioned.Team[1]), stringdist(AwayTeam[1], Mentioned.Team[1])))]) %>%
#   group_by(gameId) %>%
#   summarise(n.matches = length(unique(Which.Team)))
#   
# 
# all.bad.matches <- unique(n.match.df$gameId[n.match.df$n.matches < 2])
# all.bad.matches
# 
# ## Bundesliga: 0 games (out of 4550)
# ## Serie A: 29 games (out of 5627)
# ## La Liga: 1 game (out of ...)
# ## Ligue 1: 38 games (out of 5453)
# ## Premier League: 23 games (out of 4750)


## Combining it with checking the STATISTICAL OUTPUTS:
##    If one side's outputs are fully 0, it pretty much definitely means that it got mistakenly skipped

all.truly.bad.games <- our.df %>%
  # filter(gameId %in% all.bad.matches) %>%
  group_by(gameId, gamedDate, HomeTeam, AwayTeam) %>%
  summarise(
    homeGoals = sum(Goal == "Home"),
    homeFouls = sum(Foul == "Home"),
    homeCorners = sum(Corner == "Home"),
    homeRedCards = sum(RedCard == "Home"),
    homeShots = sum(ShotAttempts == "Home"),
    homeSaves = sum(Saves == "Home"),
    awayGoals = sum(Goal == "Away"),
    awayFouls = sum(Foul == "Away"),
    awayCorners = sum(Corner == "Away"),
    awayRedCards = sum(RedCard == "Away"),
    awayShots = sum(ShotAttempts == "Away"),
    awaySaves = sum(Saves == "Away")
  ) %>% filter( (homeGoals == 0 & homeFouls == 0 & homeCorners == 0 & homeRedCards == 0 & homeShots == 0 & awaySaves == 0) | 
                  (awayGoals == 0 & awayFouls == 0 & awayCorners == 0 & awayRedCards == 0 & awayShots == 0 & homeSaves == 0))


# View(all.truly.bad.games)



print("Dropping the games with 0 recorded stats for one of the teams (virtually impossible):")
print(length(unique(all.truly.bad.games$gameId)))
print("out of")
print(length(unique(our.df$gameId)))


our.df <- our.df %>% 
  filter(!gameId %in% unique(all.truly.bad.games$gameId))








###
## CHECK FOR "MATCH ENDS" PRE-MATURE ENTRIES
## (check "PREMATURITY" by seeing if:
##      1. It's the last entry
##      2. If not, whether the following entry has a higher minute entry
###

bad.game.ending.entries <- our.df %>%
  group_by(ID) %>%
  summarise(ind.match.end = tail(which(str_detect(tolower(Event), "match ends|second half ends")), 1),
            minute.after.fail = ifelse(ind.match.end == 1,
                                       F,
                                       Minute[ind.match.end] > Minute[ind.match.end-1]))  %>%
  filter(minute.after.fail == T) %>%
  select(ID,  
         ind.match.end)

## Simply REMOVE the problematic observations (don't move them anywhere.. there are more problems)
## (Some are still coming with '45.1' minute)
for (j in 1:length(bad.game.ending.entries$ID)){
  our.ind <- which(our.df$ID == bad.game.ending.entries$ID[j])
  bad.ind <- our.ind[bad.game.ending.entries$ind.match.end[j]]
  our.df <- our.df[-bad.ind, ]
}



# View(our.df %>%
#        filter(ID==1509))


###
## Check for UNORDERED MINUTES
## (a clear issue on several occasions)
## 
## ... TOUGH TO RESOLVE.. there are issues that are too inconsistent to be able to pin down
## (starting minutes of the 2nd half - can be 45, 46, 50, etc)
###

# our.ind <- which(str_detect(tolower(our.df$Event), "second half begins"))
# View(our.df[sort(c(our.ind, our.ind-1, our.ind+1)), ] %>% select(ID, Minute, Event))
# 
# our.ind <- which(str_detect(tolower(our.df$Event), "first half ends"))
# View(our.df[sort(c(our.ind, our.ind-1, our.ind+1)), ] %>% select(Minute, Event))
# table(our.df[sort(c(our.ind, our.ind-1, our.ind+1)), ]$Minute)


###########
##########
### CLEANING UP MINUTES
### CREATING HALF_ID variable
###########
############

our.df$Minute.num <- as.numeric(our.df$Minute) 
sort(unique(our.df$Minute.num))


## REMOVE GAMES WITH BAD MINUTES (above... 120?)

bad.game.ids <- unique(our.df$gameId[which(our.df$Minute.num > 121)])
bad.game.ids

our.df <- our.df %>%
  filter(!gameId %in% bad.game.ids)
dim(our.df)


# Summing up the "45.10" into "45+10 = 55"
our.df$Minute.clean <- sapply(strsplit(our.df$Minute, "\\."),
                              function(x) sum(as.numeric(x)))

# Half id: anything that has <46 is in 1st half 
# (even the extra time is converted to a number under 46, e.g. "45.1" is 45+1)

our.df$half_id <- ifelse(our.df$Minute.num < 46, 
                         1, 2)


print("First half times (minute):")
print(summary(our.df$Minute.clean[our.df$half_id == 1]))

print("Second half times (minute):")
print(summary(our.df$Minute.clean[our.df$half_id == 2]))

### There's one TERRIBLE observation for Serie A, with "45+46 min" added in the 1st half => GOTTA REMOVE IT
our.df <- our.df[!(our.df$half_id == 1 & our.df$Minute.clean > 90), ]


#######
######
## FILLING IN THE 0's FOR MISSING MINUTES
######
#######

# !!!!

# View(main.df)

# !!! GIVES A WEIRD WARNING !!!
our.df <- our.df %>% full_join(our.df %>%
                        group_by(ID, gameId, half_id,  HomeTeam, AwayTeam, gamedDate,
                                 HomeBet, DrawBet, AwayBet, Weighted.Win.Prob.Home) %>%
                        summarise(Minute.clean = c(ifelse(half_id == 1, 1, 46):max(Minute.clean)))) %>%
  group_by(ID, gameId) %>%
  arrange(desc(half_id), desc(Minute.clean), .by_group=TRUE)
# !!!!

stat.columns <- c("Goal", "Foul", "Corner", "YellowCard", "RedCard", "ShotAttempts", "Saves")

our.df[, stat.columns] <- sapply(our.df[, stat.columns], function(x) ifelse(is.na(x), "Null", x))






########
## MAKING HOMESCORE and AWAYSCORE in PROPER ORDER, based on GOAL COLUMN ("HOME"/"AWAY")
########


# our.df$unique.id <- paste0(our.df$gameId, our.df$gamedDate, our.df$HomeTeam)

our.df$homeScore.old <- our.df$homeScore
our.df$homeScore <- unlist(tapply(our.df$Goal, our.df$ID, function(x){log.vec <- x == "Home";
if (sum(log.vec) > 0){
  apply(sapply(which(log.vec), function(z) c(rep(1, z), rep(0,length(log.vec) - z))),
        1,
        sum)} else {
          rep(0, length(log.vec))
        }} ))

our.df$awayScore.old <- our.df$awayScore
our.df$awayScore <- unlist(tapply(our.df$Goal, our.df$ID, function(x){log.vec <- x == "Away";
if (sum(log.vec) > 0){
  apply(sapply(which(log.vec), function(z) c(rep(1, z), rep(0, length(log.vec) - z))),
        1,
        sum)} else {
          rep(0, length(log.vec))
        }} ))

# View(our.df %>% filter(ID==3))
# 
# View(our.df %>%
#        # filter(gameId == 252451 )
#        filter(gameId == 252731)
# )


##########
########
## DATES
########
##########

# our.df$gamedDate <- str_replace(our.df$gamedDate, "^\\s", "")
# our.df$gamedDate <- as.Date(our.df$gamedDate, "%B %d %Y")

tail(our.df$gamedDate)

dim(our.df)




########
## MAKING A VARIABLE KEEPING TRACK OF RED-CARDED MEN, based on REDCARD COLUMN ("HOME"/"AWAY")
########

## REDEFINE REDCARD to also include SECOND YELLOWS !!!
table(our.df$RedCard)
our.df$RedCard <- ifelse((our.df$YellowCard != "Null") & str_detect(tolower(our.df$Event), "second yellow"),
                         our.df$YellowCard,
                         our.df$RedCard)

our.df$homeRedCardTot <- unlist(tapply(our.df$RedCard, our.df$ID, function(x){log.vec <- x == "Home";
if (sum(log.vec) > 0){
  apply(sapply(which(log.vec), function(z) c(rep(1, z), rep(0,length(log.vec) - z))),
        1,
        sum)} else {
          rep(0, length(log.vec))
        }} ))

our.df$awayRedCardTot <- unlist(tapply(our.df$RedCard, our.df$ID, function(x){log.vec <- x == "Away";
if (sum(log.vec) > 0){
  apply(sapply(which(log.vec), function(z) c(rep(1, z), rep(0, length(log.vec) - z))),
        1,
        sum)} else {
          rep(0, length(log.vec))
        }} ))






### Old code checking bad games
#  bad.gameIds <- our.df %>%
#   group_by(gameId, gamedDate, HomeTeam, AwayTeam) %>%
#   summarise(homeScore.diff=diff(homeScore.old),
#             awayScore.diff=diff(awayScore.old)) %>%
#   filter(homeScore.diff > 0 | awayScore.diff > 0) %>%
#   .[["gameId"]] %>% unique()
# 
# View(our.df %>%
#   filter(gameId %in% bad.gameIds) %>%
#   select(ID, Minute, Event, homeScore, awayScore, homeScore, awayScore, homeScore.old, awayScore.old))

# View(our.df %>% filter(gameId %in% bad.gameIds))


# tail(sort(unique(as.Date(our.df$gamedDate))))


our.df <- our.df %>%
  group_by(ID) %>%
  mutate(homeScore = c(lead(homeScore)[-length(homeScore)], 0),
         awayScore = c(lead(awayScore)[-length(awayScore)], 0))



## 1. Checking if a game has max minute of <= 90, then we don't count stuff at minute 90 
##   OR 1st half stuff at minute 45+
## (As those practices of STACKING typically apply to both end of 1st & 2nd halves)
##
## 2. Remove all post-45min observations in 1st half. They mess up the balance, doubling up on early 2nd half minutes


if (remove.extra){
  #our.df <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
  # print(dim(our.df))
  
  ## Max 1st half minute
  # bad.1st.half.game.ids <- our.df %>%
  #   group_by(gameId, ID, half_id) %>%
  #   summarize(max.minute = max(Minute.clean)) %>%
  #   filter(half_id == 1, max.minute <= 45) %>%
  #   ungroup() %>%
  #   dplyr::select(gameId) %>% .[[1]]
  
  
  ## Max game minute  
  bad.game.ids <- our.df %>%
    group_by(gameId, ID) %>%
    summarize(max.minute = max(Minute.clean)) %>%
    filter(max.minute <= 90) %>%
    ungroup() %>%
    dplyr::select(gameId) %>% .[[1]]
  
  
  
  if (keep.1st.half.extra){
    our.df <- our.df %>% 
      filter(!(gameId %in% bad.game.ids & (Minute.clean >= 90 | (half_id == 1 & Minute.clean >= 45))) 
            # & !(half_id == 1 & Minute.clean > 45)
      )
  } else {
    our.df <- our.df %>% 
      filter(!(gameId %in% bad.game.ids & (Minute.clean >= 90 | (half_id == 1 & Minute.clean >= 45))) 
             & !(half_id == 1 & Minute.clean > 45)
      )
  }
  
}



# write.csv(our.df, file="A5_CUMSUM_RedCard_AS.csv",
#           row.names=F)


upd.df <- our.df %>%
  group_by(ID, gameId, gamedDate, HomeTeam, AwayTeam, half_id, Minute.clean, homeScore, awayScore, homeRedCardTot, awayRedCardTot, HomeBet, DrawBet, AwayBet, Weighted.Win.Prob.Home) %>%
  summarise(
            # min.minute = ifelse(homeScore[1] == 0 & awayScore[1] == 0, 
            #                     ifelse(half_id[1] == 1, 0, 45), min(Minute.clean)),
            # max.minute = max(Minute.clean),
            homeGoals = sum(Goal == "Home"),
            awayGoals = sum(Goal == "Away"),
            homeFouls = sum(Foul == "Home"),
            awayFouls = sum(Foul == "Away"),
            homeCorners = sum(Corner == "Home"),
            awayCorners = sum(Corner == "Away"),
            homeRedCards = sum(RedCard == "Home"),
            awayRedCards = sum(RedCard == "Away"),
            homeShots = sum(ShotAttempts == "Home"),
            awayShots = sum(ShotAttempts == "Away"),
            homeSaves = sum(Saves == "Home"),
            awaySaves = sum(Saves == "Away")
  ) %>% 
  ungroup() %>%
  group_by(ID, gameId, half_id) %>%
  arrange(desc(Minute.clean), .by_group = TRUE)



######
## CHECKING MIN.SPENT == 0 !!
######

# bad.IDs <- unique(upd.df %>% filter(min.spent <0 | (min.spent == 0 & ())) %>% .[["ID"]])
# View(upd.df %>% filter(min.spent == 0))


# View(upd.df[sort(c(our.ind, our.ind+1, our.ind-1)), ] %>%
#        select(homeScore, awayScore, min.minute, max.minute, min.spent))

main.df <- upd.df %>%
  mutate(Score.Diff = homeScore - awayScore,
         RedCard.Diff = homeRedCardTot - awayRedCardTot) %>%
  group_by(ID, gameId, gamedDate, HomeTeam, AwayTeam, half_id, Minute.clean, Score.Diff, RedCard.Diff, HomeBet, DrawBet, AwayBet, Weighted.Win.Prob.Home) %>%
  summarise(#minutes.spent = sum(min.spent),
            homeGoals = sum(homeGoals), awayGoals = sum(awayGoals),
            homeFouls = sum(homeFouls), awayFouls = sum(awayFouls),
            homeCorners = sum(homeCorners), awayCorners = sum(awayCorners), 
            homeRedCards = sum(homeRedCards), awayRedCards = sum(awayRedCards), 
            homeShots = sum(homeShots), awayShots = sum(awayShots),
            homeSaves = sum(homeSaves), awaySaves = sum(awaySaves)) %>% 
  ungroup() %>%
  group_by(ID, gameId) %>%
  arrange(desc(half_id), desc(Minute.clean), .by_group = TRUE)

dim(main.df)
# View(main.df)










## Saving it for MULTI-RESPONSE VARIABLE
# write.csv(file=paste0(league, "_Sami_intermed_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"), main.df, row.names=F)


home.df <- data.frame(main.df[, c("ID", "gameId", "gamedDate", "HomeTeam", "half_id", "Minute.clean", "Score.Diff", "RedCard.Diff", "HomeBet", "DrawBet", "Weighted.Win.Prob.Home")], 
                      HomeAway="Home",
                      main.df[, c(colnames(main.df)[str_detect(colnames(main.df), "home")])])
colnames(home.df) <- str_remove(colnames(home.df), "home")
colnames(home.df)[colnames(home.df) == "HomeTeam"] <- "Team"
colnames(home.df)[colnames(home.df) == "HomeBet"] <- "WinBet"
colnames(home.df)[colnames(home.df) == "Weighted.Win.Prob.Home"] <- "Weighted.Win.Prob"



away.df <- data.frame(main.df[, c("ID", "gameId", "gamedDate", "AwayTeam", "half_id", "Minute.clean", "Score.Diff", "RedCard.Diff", "AwayBet", "DrawBet", "Weighted.Win.Prob.Home")], 
                      HomeAway="Away",
                      main.df[, c(colnames(main.df)[str_detect(colnames(main.df), "away")])])
away.df$Score.Diff <- -away.df$Score.Diff
away.df$RedCard.Diff <- -away.df$RedCard.Diff
away.df$Weighted.Win.Prob.Home <- -away.df$Weighted.Win.Prob.Home

colnames(away.df) <- str_remove(colnames(away.df), "away")
colnames(away.df)[colnames(away.df) == "AwayTeam"] <- "Team"
colnames(away.df)[colnames(away.df) == "AwayBet"] <- "WinBet"
colnames(away.df)[colnames(away.df) == "Weighted.Win.Prob.Home"] <- "Weighted.Win.Prob"
colnames(away.df)

final.df <- rbind(home.df, away.df) %>% 
  arrange(ID, desc(half_id), desc(Minute.clean), abs(Score.Diff))


## There are 2 TERRIBLE games in "Ligue1": 
##  have commentary from MU vs Mcity, weird 22-13 scoring game... which all leads to GIGANTIC score differentials
##  (-35 to 35)

if (league == "Ligue1"){
  final.df <- final.df %>% filter(!(gameId %in% c("252145","252310")))
}

final.df <- final.df %>% mutate(ID = factor(ID), gameId = factor(gameId))
write.csv(file=paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""),
                      ifelse(keep.1st.half.extra, "_KEEP_1ST_HALF_EXTRA", ""),
                      "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"), final.df, row.names=F)

}