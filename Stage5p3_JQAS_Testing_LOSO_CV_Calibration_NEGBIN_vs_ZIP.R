#########
## Stage 5p3: Code generating Figure 2 of the main manuscript, which captures
##            the LOSO CV calibration plots, showing predicted vs observed shot rates per minute.
##            It is a panel of 5 plots (one per league), 
##            comparing Negative Binomial and Zero-Inflated Poisson (ZIP) fits.
##
#########

library(tidyverse)


## We're bucketing the extreme categories here
bucketed <- TRUE

## Whether to only check the performance on non-zero counts
only.nonzeros <- FALSE


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we want to still keep 1st half extra time
keep.1st.half.extra <- TRUE




league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")
completed.models <- c("gam", "gam_nb", "gam_zip"
                      # , "gam_normal", "gam_lognormal"
)

errors.mat <- list()
full.df <- NULL

for (j in 1:5){
  
  cat("\n")
  print(j)
  league <- league.name[j]
  print(league)
  
  errors.mat[[j]] <- list()
  
    
    # print(model)
    load(paste0(ifelse(bucketed, "BUCKETED_", ""),
                "LEAVE_SEASON_OUT_PREDICTIONS_NEW_TEAM_AND_SEASON_",
                "gam_nb_obj_", league,
                "_Shots.Robj"))
    
    
    # load(paste0("LEAVE_SEASON_OUT_PREDICTIONS_NEW_gam_obj_Shots.Robj"))
    # load(paste0("LEAVE_SEASON_OUT_PREDICTIONS_NEW_gam_zip_obj_Shots.Robj"))
    
    error.vec <- c()
    
    print(table(pred.list.nb[[1]]$Year))
    
    # if (model == "gam_nb"){
    #   pred.list <- pred.list.nb
    # } else if (model == "gam_zip") {
    #   pred.list <- pred.list.zip
    # } else if (model == "gam_normal"){
    #   pred.list <- pred.list.normal
    # } else if (model == "gam_lognormal"){
    #   pred.list <- pred.list.lognormal
    # }
    # 
    # if (only.nonzeros){
    #   pred.list[[league.name[j]]] <- pred.list[[league.name[j]]] %>% filter(True != 0)
    # }
    
    
    
    
    ##########
    ### CALIBRATION PLOTS
    ##########
    
    
    ######
    ## For NEG BIN
    ######
    
    y <- pred.list.nb[[league]]$True
    y.pred <- pred.list.nb[[league]]$Pred
    summary(y.pred)
    
    print("NEG BIN: Mean Absolute Error")
    print(mean(abs(y-y.pred)))
    print("NEG BIN: Root Mean Squared Error")
    print(sqrt(mean((y-y.pred)^2)))
    
    the.rate.grid <- seq(0,max(y.pred), by=0.01) 
    
    bin.vec <- cut(y.pred, the.rate.grid)
    prop.response <- tapply(y, bin.vec, mean)
    
    
    
    #########
    ### NOTE: A HANDFUL OF OVER-PREDICTED COUNTS for TEAMS THAT HAVE A MAN ADVANTAGE
    ##        (The score diff & minutes are pretty regular though, 
    ##        so the overfitting on those isn't seemingly as much of an issue)
    #########
    
    # print(ggplot(data=data.frame(x = the.rate.grid[-1], y = prop.response, z=as.numeric(table(bin.vec))),
    #              aes(x=x,y=y, size=3*(z/sum(z)))) +
    #         geom_point() +
    #         # geom_smooth(method="loess") +
    #         geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    #        # ylim(0,) +
    #         theme(legend.position = "none") +
    #         ggtitle(paste0(model, " predictions, ", league
    #                        #, "_", year
    #         )) +
    #         xlab("Estimated rate") +
    #         ylab("Observed rate"))
    
    
    ### Weighted mean error
    z <- table(bin.vec)/length(bin.vec)
    weight.err.vec <- abs((the.rate.grid[-1] -0.005)  -  prop.response)*z
    mean.err <- mean(weight.err.vec, na.rm=T)
    print("weighted mean error:")
    print(mean.err)
    print("st. err (sd/sqrt(n)):")
    st.err <- sd(weight.err.vec, na.rm=T)/sqrt(length(weight.err.vec[!is.na(weight.err.vec)]))
    print(st.err)
    
    
    
    
    ######
    ## For ZERO-INFLATED POISSON
    ######
    
    load(paste0(ifelse(bucketed, "BUCKETED_", ""),
                "LEAVE_SEASON_OUT_PREDICTIONS_NEW_TEAM_AND_SEASON_",
                "gam_zip_obj_", league,
                "_Shots.Robj"))
    
    y.zip <- pred.list.zip[[league]]$True
    y.zip.pred <- pred.list.zip[[league]]$Pred
    # summary(y.pred)
    
    print("ZIP: Mean Absolute Error")
    print(mean(abs(y.zip-y.zip.pred)))
    print("ZIP: Root Mean Squared Error")
    print(sqrt(mean((y.zip-y.zip.pred)^2)))
    
    
    the.rate.grid.zip <- seq(0,max(y.zip.pred), by=0.01) 
    
    bin.vec.zip <- cut(y.zip.pred, the.rate.grid.zip)
    prop.response.zip <- tapply(y.zip, bin.vec.zip, mean)
    
    print(table(pred.list.zip[[1]]$Year))
    
    
    
    #########
    ### NOTE: A HANDFUL OF OVER-PREDICTED COUNTS for TEAMS THAT HAVE A MAN ADVANTAGE
    ##        (The score diff & minutes are pretty regular though, 
    ##        so the overfitting on those isn't seemingly as much of an issue)
    #########
    
    # print(ggplot(data=data.frame(x = the.rate.grid.zip[-1], y = prop.response.zip, z=as.numeric(table(bin.vec.zip))),
    #              aes(x=x,y=y, size=3*(z/sum(z)))) +
    #         geom_point() +
    #         # geom_smooth(method="loess") +
    #         geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    #         # ylim(0,0.24) +
    #         theme(legend.position = "none") +
    #         ggtitle(paste0(model, " predictions, ", league
    #                        #, "_", year
    #         )) +
    #         xlab("Estimated rate") +
    #         ylab("Observed rate"))
    
    
    ### Weighted mean error
    z.zip <- table(bin.vec.zip)/length(bin.vec.zip)
    weight.err.vec.zip <- abs((the.rate.grid.zip[-1] -0.005)  -  prop.response.zip)*z.zip
    mean.err.zip <- mean(weight.err.vec.zip, na.rm=T)
    print("weighted mean error:")
    print(mean.err.zip)
    print("st. err (sd/sqrt(n)):")
    st.err.zip <- sd(weight.err.vec.zip, na.rm=T)/sqrt(length(weight.err.vec.zip[!is.na(weight.err.vec.zip)]))
    print(st.err.zip)
    
    
    
    #########
    
    full.df <- rbind(
      full.df,
      data.frame(x = the.rate.grid[-1], y = prop.response, z=as.numeric(table(bin.vec)), League=league, Model = "Negative Binomial"),
      data.frame(x = the.rate.grid.zip[-1], y = prop.response.zip, z=as.numeric(table(bin.vec.zip)), League = league, Model = "Zero-Inflated Poisson")
    )
    
  }

##  Width 647, Height 411
print(ggplot(data=full.df,
             aes(x=x,y=y, # size=3*(z/sum(z)), 
                 size=z,
                 col=Model)) +
       # geom_point(aes(size=3*(z/sum(z)))) +
       # geom_point(aes(size=scale(z))) +
        geom_point() +
        facet_wrap(~League) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        ggtitle(paste0("Leave-One-Season-Out (LOSO) Cross-Validation Results,\nPredicted vs Observed Shot Attempt Rates Per Minute, "
                       #, "_", year
        )) +
        xlab("Estimated rate") +
        ylab("Observed rate") +
        ylim(0,0.5) +
        # scale_size_continuous(name = "Actual Value", breaks = pretty(full.df$z), labels = pretty(full.df$z))
        scale_size_continuous(
          name = "No. of Observations", # Legend title
          range = c(0, 6), # Standardized visual size range
          breaks = scales::pretty_breaks(n = 6) # Ensure reasonable breakpoints for legend
        )
      
      # scale_size_continuous(
        #   breaks = c(100, 500, 1000),  # Specify fewer breaks for the legend
        #   name = "Number of drives"
        # )
      
)



# df <- data.frame(
#   x = rnorm(100),
#   y = rnorm(100),
#   size_var = runif(100, 1000, 5000) # Large values
# )
# 
# # Plot
# ggplot(df, aes(x = x, y = y, size = size_var)) +  # Keep original values in mapping
#   geom_point() +  
#   scale_size_continuous(
#     name = "Actual Value", # Legend title
#     range = c(1, 3), # Standardized visual size range
#     breaks = scales::pretty_breaks(n = 4) # Ensure reasonable breakpoints for legend
#   ) +
#   theme_minimal()






# [1] 1
# [1] "Bundesliga"
# 
# 2009  2010  2011  2012  2013  2014  2015  2016  2017  2018  2019  2020  2021  2022 
# 51772 52254 53878 51740 59044 58166 58710 53704 59090 59332 59402 59232 59226 59488 
# 2023 
# 60048 
# [1] "weighted mean error:"
# [1] 4.620166e-05
# [1] "st. err (sd/sqrt(n)):"
# [1] 8.148379e-06
# 
# 2009  2010  2011  2012  2013  2014  2015  2016  2017  2018  2019  2020  2021  2022 
# 51772 52254 53878 51740 59044 58166 58710 53704 59090 59332 59402 59232 59226 59488 
# 2023 
# 60048 
# [1] "weighted mean error:"
# [1] 0.01083376
# [1] "st. err (sd/sqrt(n)):"
# [1] 0.004989265
# 
# [1] 2
# [1] "SerieA"
# 
# 2009  2010  2011  2012  2013  2014  2015  2017  2018  2019  2020  2021  2022  2023 
# 66858 65546 66602 61386 73660 72728 73604 74064 74184 74530 74608 72710 74098 75102 
# [1] "weighted mean error:"
# [1] 3.545579e-05
# [1] "st. err (sd/sqrt(n)):"
# [1] 7.657384e-06
# 
# 2009  2010  2011  2012  2013  2014  2015  2017  2018  2019  2020  2021  2022 
# 66858 65546 66602 61386 73660 72728 73604 74064 74184 74530 74608 72710 74098 
# [1] "weighted mean error:"
# [1] 0.007925963
# [1] "st. err (sd/sqrt(n)):"
# [1] 0.003749906
# 
# [1] 3
# [1] "LaLiga"
# 
# 2009  2010  2011  2012  2013  2014  2015  2017  2018  2019  2020  2021  2023 
# 66048 65374 65570 55726 66832 73690 73180 73110 73194 74414 75154 74332 74266 
# [1] "weighted mean error:"
# [1] 6.962243e-05
# [1] "st. err (sd/sqrt(n)):"
# [1] 1.409772e-05
# 
# 2009  2010  2011  2012  2013  2014  2015  2017  2018  2019  2020  2021  2023 
# 66048 65374 65570 55726 66832 73690 73180 73110 73194 74414 75154 74332 74266 
# [1] "weighted mean error:"
# [1] 0.01039262
# [1] "st. err (sd/sqrt(n)):"
# [1] 0.005342782
# 
# [1] 4
# [1] "Ligue1"
# 
# 2009  2010  2011  2012  2013  2014  2015  2016  2017  2018  2019  2020  2021  2023 
# 59300 64592 63858 55504 73014 73602 72900 73494 73314 74150 72498 54856 73662 73580 
# [1] "weighted mean error:"
# [1] 4.801398e-05
# [1] "st. err (sd/sqrt(n)):"
# [1] 1.284627e-05
# 
# 2009  2010  2011  2012  2013  2014  2015  2016  2017  2018 
# 59300 64592 63858 55504 73014 73602 72900 73494 73314 74150 
# [1] "weighted mean error:"
# [1] 0.006794849
# [1] "st. err (sd/sqrt(n)):"
# [1] 0.003627786
# 
# [1] 5
# [1] "PremierLeague"
# 
# 2010  2011  2012  2013  2014  2015  2016  2017  2018  2019  2020  2021  2022  2023 
# 52076 51036 25856 51528 66738 74386 67210 75358 75222 75276 74678 74432 70644 74376 
# [1] "weighted mean error:"
# [1] 4.504544e-05
# [1] "st. err (sd/sqrt(n)):"
# [1] 9.674592e-06
# 
# 2010  2011  2012  2013  2014  2015  2016  2017  2018  2019  2020  2021  2022  2023 
# 52076 51036 25856 51528 66738 74386 67210 75358 75222 75276 74678 74432 70644 74376 
# [1] "weighted mean error:"
# [1] 0.006876127
# [1] "st. err (sd/sqrt(n)):"
# [1] 0.002947507