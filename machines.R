machine <- read.csv("P3-Machine-Utilization.csv")
machine$Utilization <- 1 -machine$Percent.Idle
head(machine, 25)

# Date Times in R
machine$POSIXTime <- as.POSIXct(machine$Timestamp, format="%d/%m/%Y %H:%M")
head(machine, 25)
machine$Timestamp <- NULL
machine <- machine[,c(4,1,2,3)]
head(machine, 25)

# Lists
RL1 <- machine[machine$Machine == "RL1",]
RL1$Machine <- factor(RL1$Machine)
summary(RL1)

# vector with the minimum, average and maximum utilization observed for the RL1 Machine
util_stats_rl1 = c(min(RL1$Utilization, na.rm=T),
                   mean(RL1$Utilization, na.rm=T),
                   max(RL1$Utilization, na.rm=T))
util_stats_rl1

# checks if the utilization for the RL1 Machine ever falls below 90%
util_under_90 <- length(which(RL1$Utilization < 0.90)) > 0
util_under_90

list_rl1 = list("RL1", util_stats_rl1, util_under_90)
list_rl1

names(list_rl1)
names(list_rl1) <- c("Machine", "Stats", "LowThreshold")
list_rl1

# Extracting list components
# [] returns list
# [[]] will return the actual object
# $ same as [[]] but prettier

list_rl1
list_rl1[1]
list_rl1[[1]]
list_rl1$Machine

list_rl1[2]
typeof(list_rl1[2])
list_rl1[[2]]
typeof(list_rl1[[2]])
list_rl1$Stats
typeof(list_rl1$Stats)

# Access max utilization only
list_rl1[[2]][3]

list_rl1[3]
list_rl1[[3]]
list_rl1$LowThreshold

# Adding list components
list_rl1[4] <- "New Information"
list_rl1$UnknownHours <- RL1[is.na(RL1$Utilization),"POSIXTime"]
list_rl1

# Deleting list components
list_rl1[4] <- NULL
list_rl1

# Numeration shifted
list_rl1[4]

list_rl1$Data <- RL1
list_rl1$AllMachines <- machine

summary(list_rl1)

# Subsetting
list_rl1[[4]][1]

sublist_rl1 <- list_rl1[c("Machine", "Stats")]
sublist_rl1

# Timeseries plot
install.packages("ggplot2")
library(ggplot2)

p <- ggplot(data=machine)
tseries <- p + geom_line(aes(x=POSIXTime, y=Utilization, color=Machine), size=1.2) + facet_grid(Machine~.) + geom_hline(yintercept = 0.9, color="Gray", size=1.2, linetype=3)

list_rl1$Plot <- tseries 
list_rl1
