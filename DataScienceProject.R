# ---
# title: Data Science Project
# author: Rafael Leite
# ---
  
# Sets Work Directory
setwd("C:/Users/Rafael Zufi/Desktop/COSC 4931")

# Loads Data Set
dataset <- read.csv(file = "AirplaneCrashesSince1908.csv", header=TRUE, sep=",")

library(ggplot2)

library(date)

# Creates new column in dataset which contains only the year of the crash
dataset$Date <- as.Date(dataset$Date, "%m/%d/%Y")

# Creates new data set 'days' which has 2 columns: the day of the month on which the accident happened and the frequency of accidents on that given day
days <- as.integer(format(dataset$Date, "%d"))
days = data.frame(table(days))

# Graph for Number of Crashes per Day
ggplot(days, aes(x = days, y = Freq, group = 2)) +
  geom_point(size = 2, shape = 21, fill = "blue") +
  geom_line(linetype = 1, size = 1, color = "blue") +
  ggtitle("Number of Crashes per Day")

# Creates new data set 'months' which has 2 columns: the month of the accident and the frequency of accidents on that given month
months <- as.integer(format(dataset$Date, "%m"))
months = data.frame(table(months))

# Graph for Number of Crashes per Month
ggplot(months, aes(x = months, y = Freq, group = 2)) +
  geom_point(size = 2, shape = 21, fill = "blue") +
  geom_line(linetype = 1, size = 1, color = "blue") +
  ggtitle("Number of Crashes per Month")

# Creates new data set 'years' which has 2 columns: the year of the accident and the frequency of accidents on that given year
years <- as.integer(format(dataset$Date, "%Y"))
years = data.frame(table(years))

# Graph for Number of Crashes per Year
ggplot(years, aes(x = years, y = Freq, group = 2)) +
  geom_point(size = 2, shape = 21, fill = "blue") +
  geom_line(linetype = 1, size = 1, color = "blue") +
  scale_x_discrete(breaks = seq(1908, 2009, 5)) +
  ggtitle("Number of Crashes per Year")

library(data.table)

fatalities <- dataset

# Creates new column in fatalities which contains only the day of the crash
fatalities$Day = as.integer(format(dataset$Date, "%d"))

# Creates new column in fatalities which contains only the month of the crash
fatalities$Month = as.integer(format(dataset$Date, "%m"))

# Creates new column in fatalities which contains only the year of the crash
fatalities$Year = as.integer(format(dataset$Date, "%Y"))

# Removes missing values for the 'Fatalities' attribute
fatalities = as.data.table(subset(fatalities, !is.na(fatalities$Fatalities)))

# Adds up all fatalities by day
days_deaths = fatalities[, sum(Fatalities), by=Day]
# Renames second attribute to 'deaths'
names(days_deaths)[names(days_deaths) == 'V1'] <- 'deaths'

# Adds up all fatalities by month
months_deaths = fatalities[, sum(Fatalities), by=Month]
# Renames second attribute to 'deaths'
names(months_deaths)[names(months_deaths) == 'V1'] <- 'deaths'

# Adds up all fatalities by year
years_deaths = fatalities[, sum(Fatalities), by=Year]
# Renames second attribute to 'deaths'
names(years_deaths)[names(years_deaths) == 'V1'] <- 'deaths'

# Graph for Number of Fatalities per Day
ggplot(days_deaths, aes(x = Day, y = deaths, group = 2)) +
  geom_point(size = 2, shape = 21, fill = "red") +
  geom_line(linetype = 1, size = 1, color = "red") +
  scale_x_continuous(breaks = seq(1, 31, 1)) +
  ggtitle("Number of Fatalities per Day")

# Graph for Number of Fatalities per Month
ggplot(months_deaths, aes(x = Month, y = deaths, group = 2)) +
  geom_point(size = 2, shape = 21, fill = "red") +
  geom_line(linetype = 1, size = 1, color = "red") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  ggtitle("Number of Fatalities per Month")

# Graph for Number of Fatalities per Year
ggplot(years_deaths, aes(x = Year, y = deaths, group = 2)) +
  geom_point(size = 2, shape = 21, fill = "red") +
  geom_line(linetype = 1, size = 1, color = "red") +
  scale_x_continuous(breaks = seq(1908, 2009, 5)) +
  ggtitle("Number of Fatalities per Year")

# Creates new data set 'airlines' which has 2 columns: name of the airline and number of fatalities
airlines = aggregate(dataset$Fatalities, by=list(Airline=dataset$Operator), FUN=sum)
# Renames second attribute to 'deaths'
names(airlines)[names(airlines) == 'x'] <- 'Deaths'
# Removes missing values for the 'Deaths' attribute
airlines = as.data.table(subset(airlines, !is.na(airlines$Deaths)))
# Removes missing value for the 'Airline' attribute
airlines = airlines[-c(1)]

# Selects Top 10 Airlines with most Deaths
top10m <- head(airlines[order(airlines$Deaths,decreasing=T),],.0043*nrow(airlines))

# Selects Top 10 Airlines with least Deaths (at least 1 death)
top10l = as.data.table(subset(airlines, airlines$Deaths >= 1))
top10l <- tail(top10l[order(top10l$Deaths,decreasing=T),],.004*nrow(top10l))

# Graph for the Top 10 Airlines with the most Deaths
ggplot(top10m, aes(x = reorder(factor(Airline), Deaths), y = Deaths)) +
  labs(x = "Airlines") +
  geom_bar(stat = "identity", fill="red") +
  ggtitle("Top 10 Airlines with the most Deaths")

# Graph for the Top 10 Airlines with the least Deaths
ggplot(top10l, aes(x = reorder(factor(Airline), Deaths), y = Deaths)) +
  labs(x = "Airlines") +
  geom_bar(stat = "identity", fill="blue") +
  ggtitle("Top 10 Airlines with the least Deaths")