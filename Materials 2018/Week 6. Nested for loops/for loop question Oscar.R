data <- read.csv("/home/oscar/MEGAsync/R Wizardry/Materials 2018/Week 3/compounds_stats.csv")


results <- NULL

for (compound in unique(data$compound))
  for (salinity in unique(data$salinity))
    for (group in unique(data$group)){
      count <- length(data[data$compound == compound & 
                             data$salinity == salinity & 
                             data$group == group & 
                             !is.na(data$methane_mean), "day" ])
      results <- rbind(results, data.frame(compound, salinity, group,count))                                                  
    }

results


############################################


########### with apply





# Count number of days with apply
apply(data[ , c("compound", "salinity", "group", "day")], 1, 
      function(x) length(data$day["compound"], ))





### Make fancy table using the output dataframe