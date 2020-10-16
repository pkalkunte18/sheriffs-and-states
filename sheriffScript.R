#Libraries used:
library(tidyverse)
library(stringr)

#step 1: import the data - imported - now subsect
data <- electedsRaw[, c('RDIndex2019', 'State', 'white', 'census2010Pop', 'white%')]

#Step 2: time for some cleaning
#extract the number from the character percent
data <- data %>% mutate(white_perc = as.numeric(`white%`)) %>%
  select('RDIndex2019', 'State', 'white', 'census2010Pop', 'white_perc') %>% drop()

#convert the white binary column to 1 and 0, 1 based on white
data$isWhite <- as.numeric(factor(data$white, levels = c("Non-White", "White", "Unknown"))) - 1
data <- data[data$isWhite != 2, ] #264 candidates left - we know all of them are either white (1) or nonwhite (0). 

#view the data
print(tail(data))

#now group the data by state, and summarize their percent white
byState <- data %>% group_by(State) %>% 
  summarize(count = n(), MeanWhiteSher = mean(as.double(isWhite)), 
              MeanWhiteProp = mean(white_perc)) %>% 
  mutate(sherPopDif = abs(MeanWhiteSher - MeanWhiteProp))

#export the ByState dataset
write.csv(byState, "byStateData.csv")

#print sorted by
#count
sortedByCount <- byState[order(byState$count),]
head(sortedByCount)
tail(sortedByCount)
#difference
sortedByDif <- byState[order(byState$sherPopDif),]
head(sortedByDif)
tail(sortedByDif, n = 7)


#Scatterplot of meanWhiteSher and meanWhiteProp
#What's the trend of meanWhiteSher compared to meanWhiteProp?
scatter <- ggplot(byState, aes(x=MeanWhiteSher, y=MeanWhiteProp)) + 
  geom_point() + geom_smooth(method='lm', formula= y~x) +
  ggtitle("Sheriff Whiteness vs. State Whiteness") +
  xlab("Percent White Sheriff") + ylab("Percent White State")
scatter

#Scatterplot of meanWhiteSher and meanWhiteProp
#What's the trend of meanWhiteSher compared to meanWhiteProp?
scatter2 <- ggplot(byState, aes(x=count, y=sherPopDif)) + 
  geom_point() + geom_smooth(method='lm', formula= y~x) +
  ggtitle("Count Vs. Difference") +
  xlab("Count") + ylab("Difference Between Sheriffs and States")
scatter2

#oddly enough, there seems to be a negative trend between difference and whiteState
scatter3 <- ggplot(byState, aes(x=sherPopDif, y=MeanWhiteProp)) + 
  geom_point() + geom_smooth(method='lm', formula= y~x) + 
  ggtitle("Difference Vs. State Whiteness") +
  xlab("Difference") + ylab("State Whiteness")
scatter3

#distribution of state differences via barchart
#What's the distribution of differences
hist <- ggplot(data = byState, aes(x = sherPopDif)) +
  geom_histogram(bins = 20) + geom_vline(xintercept = mean(byState$sherPopDif)) + 
  ggtitle("Frequency of Percent Difference between State and Sheriff Whiteness") +
  xlab("Difference between State and Sheriff Demographics") + ylab("Frequency")
hist