# load packages
pak <- c("plyr",
         "tidyverse",
         "NobBS",
         "aweek",
         "cowplot",
         "lubridate",
         "vegetarian",
         #"brms",
         "rprojroot",
         "zoo",
         "EpiEstim",
         "foreign",
         "viridis",
         "ggjoy",
         "ggplot2",
         "ggpubr"
         )
lapply(pak,require,character.only=TRUE)
#For purposes of discussion, we reported the exponentiated form of the mean logarithmic score (the geometric mean of the assigned probabilities) to provide a metric on the scale of 0 (no certainty of the outcome) to 1 (complete certainty of the outcome). In addition, we estimated other metrics describing the performance of point estimates (mean absolute error (MAE), root mean square error (RMSE), and relative root mean square error (rRMSE)) and the 95% prediction interval (PI) coverage, and of these, focus on comparing the rRMSE and 95% PI coverage across approaches.
