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
