# Fabien Millet (fabien.millet@axa-insurance.co.uk)
  
# 1. Prepare your working environment and your database
## 1.a. Libraries
# Load libraries :
Lib <- "Y:/Work/R/win-library/3.2"
library("data.table", lib.loc=Lib)
library("stringr", lib.loc=Lib)
library("date", lib.loc=Lib)
library("ggplot2", lib.loc=Lib)
library("googleVis", lib.loc=Lib)
library("gridExtra", lib.loc=Lib)


## 1.b. Load your database

# Where did you save your file ?
my_path <- "Y:/Work/Training/Claims Analytics group training/ClaimsTraining_PreWork_Pack2/Exercise_Pack2"
filePath <- paste(my_path, "/claims_database.csv", sep="")

# Load the database using the fread() function :
DT <- fread(filePath, header = TRUE, sep=";")


## 1.c. Check the shape of your database

# Number of observations :
dim(DT) # 10,000 obs of 11 variables

# Number of variables :


# Display the first lines of the database :
head(DT)

# 2. Understand how your dataset was loaded by R

## 2.b. Variable type check

# Check variables types using lapply and class functions :
sapply(DT, class)

## 2.c. Change some variable types

### (i) Deal with dates

# Q1 : date is read as character

# Create a "date2" variable with a proper date format from the initial "date" variable :
#DT$date2 <- as.Date(DT$date, "%Y-%m-%d")
DT <- DT[, date2 := as.Date(date, "%Y-%m-%d")]
class(DT$date2)

# Create time features
#DT <- DT[, c("Year", "Month", "Day") := list(year(date2), month(date2), weekdays(date2))]
DT <- DT[, ':=' (Year = year(date2), Month = month(date2), Day = weekdays(date2))]
head(DT)

### (ii) Deal with categorical variables

#Q2 : categorical variables: coverage, repairer_agreement, severity, drivable, category, brand

# Overwrite the categorical variables to change their type to "factor" :

#DT <- DT[, lapply( .(coverage, repairer_agreement), as.factor)] # /!\ keeps only factor - how to keep everything with lapply?

# OK but without lapply
for (col in c("coverage", "repairer_agreement", "severity", "drivable", "category", "brand")) set(DT, j=col, value=as.factor(DT[[col]]))

# OK but reorders columns and need to type all col names?
# DT <- DT[, lapply(.SD, as.factor), by = .(claim_id, date, sum_insured, cost, power, date2, Year, Month, Day), .SDcols = c("coverage", "repairer_agreement", "severity", "drivable", "category", "brand") ]

# DT2 <- DT2[, coverage := as.factor(coverage)]
# DT2 <- DT2[, ':=' (coverage = as.factor(coverage), brand = as.factor(brand))]
# DT2 <- DT2[, ':=' (lapply( .(coverage, repairer_agreement), as.factor))]

# Check that you actually changed the variable types
sapply(DT, class)



## 1.
# Display the modalities of the "coverage" variable :
levels(DT$coverage)

# Display the modalities of the "brand" variable :
levels(DT$brand)

# Display the number of modalities of the "brand" variable :
length(levels(DT$brand))

#Q3 : The "brand" variable contains 54 different categories. 
 

#2.
# Count the number of vehicles repaired for each category :
summary(DT$category)
DT[, .(Claims_Vol = .N), by = category]

#Q4 : Very few luxury cars versus other vehicles

#3.
# Derive the number claims handled by each type of garage per type of coverage :
DT.Agreement.Coverage <- DT[, .(Claims_Vol = .N), by = .(repairer_agreement, coverage)]
DT.Agreement.Coverage[,order(Claims_Vol)]
DT.Agreement.Coverage[coverage == "Glass breakage"]

#Q5 : agreement_prefered handle more glass breakage than others

#4.
# Change the "severity" variable to an ordered factor one :
DT[, severity := as.ordered(severity)]
summary(DT$severity)
DT[, .N, by = severity]

# 3. Explore the database using wonderful charts

## 3.a. Create a "Calendar view"

### (i) Aggregate the cost and number of claims per day

# Create a the database DT_perDay :
# Compute the average cost per day in the same table

# DT_perDay <- DT[, .(FreqDay = .N, TotalCostDay = sum(cost)) ,by = date2]
# DT_perDay <- DT_perDay[, AverageCostDay := TotalCostDay / FreqDay]
DT_perDay <- DT[, .(FreqDay = .N, TotalCostDay = sum(cost), AverageCostDay = sum(cost) / .N) ,by = date2]



### (ii) Create the "Calendar view"


# Load the ggvis package :
library(googleVis)
op <- options(gvis.plot.tag='chart')

# Create your calendar view using the gvisCalendar function
# Total cost per day
gvis1 <- gvisCalendar(data=DT_perDay, datevar="date2", numvar="TotalCostDay", options=list(width=1100, height=600))
plot(gvis1)
# Average cost per day
gvis2 <- gvisCalendar(data=DT_perDay, datevar="date2", numvar="AverageCostDay", options=list(width=1100, height=600))
plot(gvis2)
# Number of claims per day
gvis3 <- gvisCalendar(data=DT_perDay, datevar="date2", numvar="FreqDay", options=list(width=1100, height=600))
plot(gvis3)

#Q6 : Your answer


### (iii) View it as a time series

# Plot the claims frequency per day using "ggplot2" package :
line_chart <- ggplot(data=DT_perDay, aes(x=date2, y=AverageCostDay))
line_chart + geom_line() +
  labs(title = "Average repair cost per day") + 
  labs(x = "Date", y = "Repair cost") + geom_smooth()


#Q7 : Your answer

## 3.b. Create a bar chart

# Number of claims per coverage type :
DT_Coverage <- DT[, .(ClaimsVolume = .N), by = coverage]

# Number of claims per claims severity :
DT_Severity <- DT[, .(ClaimsVolume = .N), by = severity]

# Bar chart 1 : claims count per coverage type
bar_chart1 <- ggplot(data=DT_Coverage, aes(x=coverage, y=ClaimsVolume, fill=coverage)) 
bar_chart1 + geom_bar(stat = "identity")

bar_chart1 + geom_bar(stat = "identity")+
  labs(title = "Claims volume per coverage type",
       x = "Coverage type", y = "Claims volume") + 
  theme(legend.position = "left", axis.text.x = element_blank())
  

# Bar chart 2 : claims count per severity
bar_chart2 <- ggplot(data=DT_Severity, aes(x=severity, y=ClaimsVolume, fill = severity)) 
bar_chart2 + geom_bar(stat = "identity")+
  labs(title = "Claims volume per severity",
       x = "Severity", y = "Claims volume") + 
  theme(legend.position = "left", axis.text.x = element_blank())

## 3.c. Create a box plot

# Boxplot 1 : claims count per coverage type - Cost per category instead
boxplot1 <- ggplot(data=DT, aes(x=category, y=cost, fill = category)) 
boxplot1 + geom_boxplot()

# Bar chart 2 : claims count per severity - Cost per drivability instead
boxplot2 <- ggplot(data=DT, aes(x=drivable, y=cost))
boxplot2 + geom_boxplot(aes(fill = drivable))


#Q8 : Non drivable vehicles have a higher average cost and a broader range of values



# 4. Claims reporting


## 4.a. Trend reporting


# Compute claims count and average cost per year :
DT_Year <- DT[, .(FreqYear = .N, TotalCost = sum(cost), Average = sum(cost) / .N ), by = Year]
DT_Year$Average[DT_Year$Year == 2014] / DT_Year$Average[DT_Year$Year == 2013] - 1
DT_Year$Average[DT_Year$Year == 2015] / DT_Year$Average[DT_Year$Year == 2014] - 1

p1 <- ggplot(data = DT_Year, aes(x=as.character(Year), y=FreqYear)) + 
  geom_bar(stat = "identity") +
  labs(title = "Claims volume per accident year", x = "Accident year", y = "Claims volume")
p2 <- ggplot(data = DT_Year, aes(x=as.character(Year), y=Average, group = 1)) + 
  geom_line() +
  labs(title = "Claims average cost per accident year", x = "Accident year", y = "Claims severity")

grid.arrange(p1, p2, ncol = 1, nrow = 2)
rm(p1, p2)

#Q9 : 8.5% average cost increase from 2013 to 2014. 2% decrease 2015 vs 2014, but low volume of claims in 2015  

# Compute claims count and average cost per month :
DT_Month <- DT[, .(FreqYear = .N, TotalCost = sum(cost), Average = sum(cost) / .N ), by = paste(Year, ifelse(Month <= 9, paste("0", Month, sep=""), Month), sep = "-")]
colnames(DT_Month)[colnames(DT_Month) == "paste"] <- "Month"
DT_Month <- DT_Month[ order(Month)]
DT_Month

p1 <- ggplot(data = DT_Month, aes(x=as.character(Month), y=FreqYear)) + 
  geom_bar(stat = "identity") +
  labs(title = "Claims volume per accident year", x = "Accident year", y = "Claims volume")
p2 <- ggplot(data = DT_Month, aes(x=as.character(Month), y=Average, group = 1)) + 
  geom_line() +
  geom_smooth() + 
  labs(title = "Claims average cost per accident year", x = "Accident year", y = "Claims severity")

grid.arrange(p1, p2, ncol = 1, nrow = 2)
rm(p1, p2)

#Q10 : - Spike of claims volume and severity in June 2014
#      - April 2015: very low volume and severity
#      - Decreasing volume trend (maturing effect?)
#      - Increasing severity trend (2014 increase not only driven by June spike)

## 4.b. Qualify repairer agreement
Stats <- DT[, .(Vol = .N, Avg = sum(cost) / .N), by = c("repairer_agreement", "severity")]
Stats2 <- DT[, .(Vol = .N, cost = sum(cost), Avg = sum(cost) / .N, CostVsSumInsd = sum(cost) / sum(sum_insured), AvgPower = sum(power) / .N), 
             by = c("repairer_agreement", "category")]
Stats3 <- DT[, .(Vol = .N, cost = sum(cost), Avg = sum(cost) / .N, CostVsSumInsd = sum(cost) / sum(sum_insured), AvgPower = sum(power) / .N), 
             by = c("repairer_agreement", "coverage")]

write.table(Stats, "clipboard", sep = "\t", row.names = FALSE)

# NB: all severity 1 drivable, all (except 19) other severities non drivable: do not use drivable
# NB: No agreeement has the greater proportion of trucks and buses (almost all buses)


P1 <- ggplot(data=Stats, aes(x=severity, y=Avg, col = repairer_agreement))
P1 + geom_line(aes(group = repairer_agreement)) +
  labs(title = "Average repair cost by agreement") + 
  labs(x = "Severity", y = "Repair cost")

P2 <- ggplot(data=Stats, aes(x=severity, y=CostVsSumInsd, col = repairer_agreement))
P2 + geom_line(aes(group = repairer_agreement)) +
  labs(title = "Average repair cost vs sum insured") + 
  labs(x = "Severity", y = "cost / sum_insured")

P3 <- ggplot(data=Stats2, aes(x=category, y = Avg, col = repairer_agreement))
P3 + geom_line(aes(group = repairer_agreement)) +
  labs(title = "Average cost by category") + 
  labs(x = "Category", y = "cost")

P4 <- ggplot(data=Stats3, aes(x=coverage, y = Avg, col = repairer_agreement))
P4 + geom_line(aes(group = repairer_agreement)) +
  labs(title = "Average cost by coverage") + 
  labs(x = "Coverage", y = "cost")

# Prefered better for severity 1 but cost and cost/sum_insured slightly better for no agreement for severity 2/3
# Other agreement: cost / sum_insured far greater than prefered or no: different mix
# No agreeement more expensive for all categories exeptconvertible and coupe (few vehicle)


# 5. Some insights about code optimization

## 5.a. Example 1 : compute the same calculation a lot of times

# Create the conversion function

Ex_Rates <- data.table(Currency = c("GBP", "USD", "CHF", "CAD", "AUD", "INR", "HUF", "THB", "AED"), 
                       Rate = c(0.70, 1.06, 1.09, 1.41, 1.47, 70.69, 312.26, 37.92, 3.90))

Conversion <- function(DT, Cur_Name, Rate) {
  DT_TEMP <- copy(DT) # /!\ need to use copy. If we use DT_TEMP <- DT, then modify DT_TEMP, DT is modified 
  DT_TEMP[, ':=' (TotalCost_Cur = TotalCost * Rate, Average_Cur = Average * Rate)]
  colnames(DT_TEMP)[colnames(DT_TEMP) == "TotalCost_Cur"] <- paste("TotalCost", Cur_Name, sep = "_")
  colnames(DT_TEMP)[colnames(DT_TEMP) == "Average_Cur"] <- paste("Average", Cur_Name, sep = "_")
  return(DT_TEMP)
}

# Apply it : conversion to USD, GBP, CHF, JPY, KRW, TRY, ...

DT_Year2 <- Conversion(DT_Year, Ex_Rates$Currency[1], Ex_Rates$Rate[1])
for(i in 2:length(unique(Ex_Rates$Currency))) {
  DT_Year2 <- Conversion(DT_Year2, Ex_Rates$Currency[i], Ex_Rates$Rate[i])
}

# Display head of the final table
DT_Year
DT_Year2


## 5.b. Example 2 : change your database format

# Year and month by statement
DT_byMonth <- DT[, .(AverageCost = sum(cost) / .N), by = c("Year", "Month")]

# Create the new database wanted
DT_byMonth2 <- DT_byMonth[, 
                          .(AvgCost_Jan = sum(ifelse(Month == 1, AverageCost, 0)),
                            AvgCost_Feb = sum(ifelse(Month == 2, AverageCost, 0)),
                            AvgCost_Mar = sum(ifelse(Month == 3, AverageCost, 0)),
                            AvgCost_Apr = sum(ifelse(Month == 4, AverageCost, 0)),
                            AvgCost_May = sum(ifelse(Month == 5, AverageCost, 0)),
                            AvgCost_Jun = sum(ifelse(Month == 6, AverageCost, 0)),
                            AvgCost_Jul = sum(ifelse(Month == 7, AverageCost, 0)),
                            AvgCost_Aug = sum(ifelse(Month == 8, AverageCost, 0)),
                            AvgCost_Sep = sum(ifelse(Month == 9, AverageCost, 0)),
                            AvgCost_Oct = sum(ifelse(Month == 10, AverageCost, 0)),
                            AvgCost_Nov = sum(ifelse(Month == 11, AverageCost, 0)),
                            AvgCost_Dec = sum(ifelse(Month == 12, AverageCost, 0))) ,
                          by = "Year"]

# Display the final database
DT_byMonth2


# Create the new database wanted
DT_byMonth3 <- copy(DT_byMonth)
for(i in 1:12) {
  DT_byMonth3 <- DT_byMonth3[, paste("AvgCost_", i, sep="") := sum(ifelse(Month == i, AverageCost, 0)), by = "Year"]
}

DT_byMonth3 <- DT_byMonth3[, lapply(.SD, mean), by=Year]

# Display the final database
DT_byMonth3



