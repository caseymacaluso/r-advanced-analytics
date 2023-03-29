# Basic Import: fin <- read.csv("P3-Future-500-The-Dataset.csv")
fin <- read.csv("P3-Future-500-The-Dataset.csv", na.strings = c(""))
fin
head(fin)
tail(fin)
str(fin)
summary(fin)

fin$Name <- factor(fin$Name)
fin$Industry <- factor(fin$Industry)
fin$State <- factor(fin$State)
fin$City <- factor(fin$City)
fin$Revenue <- factor(fin$Revenue)
fin$Expenses <- factor(fin$Expenses)
fin$Growth <- factor(fin$Growth)

fin$ID <- factor(fin$ID)
fin$Inception <- factor(fin$Inception)

str(fin)

# Factor Variable Trap

as.numeric(fin$Revenue) # Puts the factor levels in as opposed to the actual revenue values
# Convert to character first, then numeric

# sub() and gsub()
# Substitute values for specified inputs
fin$Expenses <- gsub(" Dollars","", fin$Expenses)
fin$Expenses <- gsub(",","",fin$Expenses)
head(fin)

fin$Revenue <- gsub("\\$","",fin$Revenue)
fin$Revenue <- gsub(",","",fin$Revenue)
head(fin)
str(fin)

fin$Growth <- gsub("%","",fin$Growth)
head(fin)
str(fin)

# After removing special characters, these fields have been converted to char data type
# Now we can convert to numeric
fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Growth <- as.numeric(fin$Growth)
str(fin)

# Missing Data
# Updated Import: fin <- read.csv("P3-Future-500-The-Dataset.csv", na.strings = c(""))
head(fin, 24)
fin[!complete.cases(fin),]

# Filtering: using which() for non-missing data
fin[fin$Revenue == 9746272,]
# includes records where revenue is NA
# we can use which() to specify only records where condition is TRUE
fin[which(fin$Revenue == 9746272),]

# Filtering: using is.na() for missing data
fin[is.na(fin$Expenses),]
fin[is.na(fin$State),]

# Backup
fin_backup <- fin

# Removing records w/ missing data
fin[!complete.cases(fin),] # All records w/ NA in one or more columns
fin[is.na(fin$Industry),] # portion of data w/ NA for Industry column
fin[!is.na(fin$Industry),] # portion of data w/o NA for Industry column
fin <- fin[!is.na(fin$Industry),] # Set dataframe to filtered data (i.e. data w/o NA in Industry)

# Resetting the index
fin
rownames(fin) <- NULL
fin

# Replacing missing data: Factual Analysis
fin[!complete.cases(fin),] # All records w/ NA in one or more columns
fin[is.na(fin$State),]
fin[is.na(fin$State) & fin$City == "New York",]
fin[is.na(fin$State) & fin$City == "New York","State"] <- "NY"
fin[is.na(fin$State) & fin$City == "San Francisco","State"] <- "CA"

#Check
fin[!complete.cases(fin),]

#Replacing data: Median Imputation
# Replace values in Employees column that have Industry set to "Retail"
med_emp_retail <- median(fin[fin$Industry == "Retail", "Employees"], na.rm = TRUE)
fin[is.na(fin$Employees) & fin$Industry=="Retail",]
fin[is.na(fin$Employees) & fin$Industry=="Retail","Employees"] <- med_emp_retail
head(fin)

# Replace values in Employees column that have Industry set to "Financial Services"
med_emp_finserv <- median(fin[fin$Industry == "Financial Services", "Employees"], na.rm = TRUE)
fin[is.na(fin$Employees) & fin$Industry=="Financial Services",]
fin[is.na(fin$Employees) & fin$Industry=="Financial Services","Employees"] <- med_emp_finserv
fin[330,]

# Replace values in Growth column that have Industry set to "Construction"
med_growth_constr <- median(fin[fin$Industry == "Construction", "Growth"], na.rm = TRUE)
fin[is.na(fin$Growth) & fin$Industry=="Construction",]
fin[is.na(fin$Growth) & fin$Industry=="Construction","Growth"] <- med_growth_constr
fin[8,]

#Check
fin[!complete.cases(fin),]

# Replace missing values in Revenue column with medians based on Industry value
med_rev_constr = median(fin[fin$Industry == "Construction", "Revenue"], na.rm = TRUE)
fin[is.na(fin$Revenue) & fin$Industry=="Construction",]
fin[is.na(fin$Revenue) & fin$Industry=="Construction","Revenue"] <- med_rev_constr

# Replace missing values in Expenses column with medians based on Industry value
# Only want to replace expenses that also have NA for Profit. Otherwise, can derive Expenses value from Revenue and Profit
med_exp_constr = median(fin[fin$Industry == "Construction", "Expenses"], na.rm = TRUE)
fin[is.na(fin$Expenses) & fin$Industry=="Construction" & is.na(fin$Profit),]
fin[is.na(fin$Expenses) & fin$Industry=="Construction" & is.na(fin$Profit), "Expenses"] <- med_exp_constr

# Derive missing Expenses value by subtracting Revenue and Profit for that record
fin[is.na(fin$Expenses),"Expenses"] <- fin[is.na(fin$Expenses),"Revenue"] - fin[is.na(fin$Expenses),"Profit"]

# Derive missing Profit value by subtracting Revenue and Expenses for that record
fin[is.na(fin$Profit),"Profit"] <- fin[is.na(fin$Profit),"Revenue"] - fin[is.na(fin$Profit),"Expenses"]

#Check
fin[15,]
fin[c(8,42),]

# Visualization
install.packages("ggplot2")
library(ggplot2)

p <- ggplot(data=fin)
p + geom_point(aes(x=Revenue, y=Expenses, color=Industry, size=Profit))

d <- ggplot(data=fin, aes(x=Revenue, y=Expenses, color=Industry))
d + geom_point() + geom_smooth(fill=NA, linewidth=1.2)

f <- ggplot(data=fin, aes(x=Industry, y=Growth, color=Industry))
f + geom_jitter() + geom_boxplot(size=1, alpha=0.5, outlier.color = NA)
