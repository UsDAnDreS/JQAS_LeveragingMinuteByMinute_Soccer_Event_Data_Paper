# JQAS_LeveragingMinuteByMinute_Soccer_Event_Data_Paper

All the source code files for the "Leveraging Minute-by-Minute Soccer Match Event Data to Adjust Team’s Offensive Production for Game Context" paper, going from data collection and reformatting (Stages 1 through 3), onto model fitting and producing all the figures/tables/relevant analyses (Stage 4 onward).

The data files for play-by-play text commentary data were too large ("[league-name]_A5.csv"), so we had to use zip compression - please make sure to "un-zip" it before running the code. Data files with booking odds are contained in the "Odds_Data" folder and are maintained as is.

For simplicity, we also included the data files that result after all the cleaning and reformatting of Stage 3, but we also had to zip those due to them being too large ("[league-name]_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.zip").

Note that, in many cases, it takes a really long time to run, and can take up significant computational resources. That especially applies to Stages 4 & 5, where the large models get fitted and leave-one-season-out cross-validation takes a while. We have provided as many of intermediate and fitted files as we could given the upload size constraints, and those can be found in "Fitted_Objects" and "Intermediate_Files" folders. For the "Intermediate_Files" folder, make sure to first move the files from it into the main directory, in order for respective source code to work. Nonetheless, chances are, many of the files won't be there and you'll have to run everything from Stage 4 onward to make sure you have everything.
