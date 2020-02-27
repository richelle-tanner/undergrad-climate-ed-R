#"Using publicly-available long-term climate records in undergraduate interdisciplinary big data curriculum"
#For distribution with curriculum described in above article, produces sample figures in article

####packages####
#install packages - only needs to be done once on each computer
#for more information on packages, type "?package" or "??packagecommand" (filling in with your specific query) into the console
install.packages("plyr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("devtools")
#special install for ggbiplot
library(devtools)
install_github("vqv/ggbiplot")

#load packages - needs to be done every R session
library(plyr)
library(dplyr)
library(lubridate)
library(ggbiplot)

####import and clean data####
ddr <- "~/Documents/R_Data/Climate_Project/"
#replace "city" with your data
#it is best practice to leave your raw data intact, just in case you need to reference the full dataset later
cityTMAX <- read.csv(paste(ddr, "cityTMAX.csv", sep=""))
cityTMIN <- read.csv(paste(ddr, "cityTMIN.csv", sep=""))

#omit NA data (no data rows) for both TMAX and TMIN
#this command allows you to look at the rows that have NAs
cityTMAX[!complete.cases(cityTMAX),]
#this command actually omits NAs and makes a new data frame free of NAs (so the raw dataset remains intact)
newCityTMAX <- na.omit(cityTMAX)

cityTMIN[!complete.cases(cityTMIN),]
newCityTMIN <- na.omit(cityTMIN)

#find averages and standard deviations
#"na.rm" is set to TRUE (do remove NAs), but it is not necessary because you already removed NAs above
averageCityTMAX <- mean(newCityTMAX$TMAX)
sdCityTMAX <- sd(newCityTMAX$TMAX, na.rm = TRUE)
averageCityTMIN <- mean(newCityTMIN$TMIN)
sdCityTMIN <- sd(newCityTMIN$TMIN, na.rm = TRUE)

#calculate climate anomalies
#this column is calculated to help calculate the average difference by year from the grand mean below
newCityTMAX$anomaly <- averageCityTMAX - newCityTMAX$TMAX
newCityTMIN$anomaly <- averageCityTMIN - newCityTMIN$TMIN

####calculate yearly averages####
#ddply is a useful command that performs multiple sets of calculations in one, row-wise
#here it is summarizing the difference of the average temperature by year from the grand mean (over the entire period) 
#by taking the mean of newCityTMAX$anomaly for each YR
city_yearly_mean_TMAX = ddply(newCityTMAX, .(YR), summarise, mean_temp_diff = mean(anomaly))
city_yearly_mean_TMIN = ddply(newCityTMIN, .(YR), summarise, mean_temp_diff = mean(anomaly))

#merge TMAX and TMIN data frames
city_yearly_mean_diff <- merge(city_yearly_mean_TMAX, city_yearly_mean_TMIN, by="YR")
#rename columns with descriptive names
colnames(city_yearly_mean_diff) <- c("Year", "Mean_Temp_Diff_TMAX", "Mean_Temp_Diff_TMIN")

####calculate occurrence of climate change and UHI####
#if either TMIN or TMAX slope is significant and positive, climate change is present
#if both slopes are significant, and the TMIN slope is 3X the TMAX slope, UHI is present
#for UHI, the TMAX slope could be negative or positive. TMIN slope always has to be positive.
#visually, this means that UHI is shown by the two lines converging (see below for plotting instructions)

#ANOVA test
#this demonstrates a one way ANOVA where Year is a predictor of TMAX and/or TMIN
city_lmTMAX <- lm(Mean_Temp_Diff_TMAX ~ Year, data=city_yearly_mean_diff)
city_lmTMIN <- lm(Mean_Temp_Diff_TMIN ~ Year, data=city_yearly_mean_diff)

#read results in the console window
#Pr(>|t|) is the p value, where the stars indicate significance
#Estimate is the effect size, or the slope
#The intercept p value is to be ignored, "Year" is your predictor variable
summary(city_lmTMAX)
summary(city_lmTMIN)

#In the example dataset provided:
#TMAX slope is -0.019948 
#TMIN slope is -0.002054
#TMAX slope is negative and signficant, therefore climate change is not present.
#Only TMAX slope is significant, therefore UHI presence cannot be evaluated.
#However if they were both significant, there would be no indication of UHI.

#export a file with your statistical results
write.csv(summary(city_lmTMAX), file = "~/Documents/R_Data/Climate_Project/cityANOVA_TMAX.csv")
write.csv(summary(city_lmTMIN), file = "~/Documents/R_Data/Climate_Project/cityANOVA_TMIN.csv")


####plot yearly averages of TMIN and TMAX####
#this is a visualization of climate change and UHI

#figure out range for x and y
#find the minimum and maximum in TMIN and TMAX datasets, respectively, and add a buffer of 1
#Y values
city_minimum <- min(city_yearly_mean_TMIN$mean_temp_diff) - 1
city_maximum <- max(city_yearly_mean_TMAX$mean_temp_diff) + 1
#X values
city_start <- newCityTMAX$YR[1]
city_end <- 2015 #this should be replaced with whatever the latest full year of data is

#plot TMAX first, in red
plot(Mean_Temp_Diff_TMAX ~ Year, data=city_yearly_mean_diff, xlim=range(city_start, city_end), 
     ylim=range(city_minimum, city_maximum), col="red", main="City Yearly Averages Trend", xlab="Year", 
     ylab="Yearly Temperature Anomaly") 
#add the points for TMIN, in blue
points(Mean_Temp_Diff_TMIN ~ Year, data=city_yearly_mean_diff, col="blue") 

#calculate linear regression lines (similar to ANOVA above)
city_linregTMAX <- lm(Mean_Temp_Diff_TMAX ~ Year, data=city_yearly_mean_diff)
city_linregTMIN <- lm(Mean_Temp_Diff_TMIN ~ Year, data=city_yearly_mean_diff)

#add the line to the plot in the same colors as the points
abline(city_linregTMAX, col="red")
abline(city_linregTMIN, col="blue")

#save the plot shown in the window 
#always try to save as a PDF, not an image file
savePlot(filename="~/Documents/R_Data/Climate_Project/cityYearlyAveragesTrendPlot.pdf")

####calculate extremes####
#an extreme is defined by the average +/- 1 standard deviation (e.g. TMAX + 1 SD)
#averageCityTMAX + sdCityTMAX is the TMAX extreme limit
#averageCityTMIN - sdCityTMIN is the TMIN extreme limit

#this command does a lot of things at once, and produces a single data frame
#see line-by-line comments for specific functions being performed
cityTMAXextremes <- newCityTMAX %>% 
  mutate(date = as.Date(paste(YR, MO, DA, sep = "-"))) %>%  # concatenates YR MO DA into an ISO date, convert column into date type
  filter(TMAX > (averageCityTMAX + sdCityTMAX)) %>% # finds all instances where there is an extreme, defined above
  group_by(YR) %>%
  summarise(number_of_days = n(), # count number of rows in each group
            first_date = min(date), #report first date of the year that has an extreme
            last_date = max(date)) #report last date of the year that has an extreme

#see above for comments, this is just being done on TMIN
cityTMINextremes <- newCityTMIN %>% 
  mutate(date = as.Date(paste(YR, MO, DA, sep = "-"))) %>% 
  filter(TMIN < (averageCityTMIN - sdCityTMIN)) %>%
  group_by(YR) %>%
  summarise(number_of_days = n(),
            first_date = min(date),
            last_date = max(date))

#use lubridate function to get julian date (day of the year) for optional plotting
cityTMAXextremes$first_date_julian <- yday(cityTMAXextremes$first_date)
cityTMAXextremes$last_date_julian <- yday(cityTMAXextremes$last_date)
cityTMINextremes$first_date_julian <- yday(cityTMINextremes$first_date)
cityTMINextremes$last_date_julian <- yday(cityTMINextremes$last_date)

#export a file with extremes
write.csv(cityTMAXextremes, file = "~/Documents/R_Data/Climate_Project/cityTMAXextremes.csv")
write.csv(cityTMINextremes, file = "~/Documents/R_Data/Climate_Project/cityTMINextremes.csv")

####plot yearly hot and cold extremes####

#figure out range for x and y
#find the minimum and maximum in TMIN and TMAX datasets, respectively, and add a buffer of 1
#Y values
city_minimumEx <- min(cityTMINextremes$number_of_days) - 1
city_maximumEx <- max(cityTMAXextremes$number_of_days) + 1
#X values
#x range should be the same as above plots
city_startEx <- cityTMAXextremes$YR[1]
city_endEx <- 2015 #this should be replaced with whatever the latest full year of data is

#plot TMAX first, in red
plot(number_of_days ~ YR, data=cityTMAXextremes, xlim=range(city_startEx, city_endEx), 
     ylim=range(city_minimumEx, city_maximumEx), col="red", main="City Yearly Extremes Trend", xlab="Year", 
     ylab="Number Of Extreme Days") 
#add the points for TMIN, in blue
points(number_of_days ~ YR, data=cityTMINextremes, col="blue") 

#calculate linear regression lines
city_linregTMAXex <- lm(number_of_days ~ YR, data=cityTMAXextremes)
city_linregTMINex <- lm(number_of_days ~ YR, data=cityTMINextremes)

#add the line to the plot in the same colors as the points
abline(city_linregTMAXex, col="red")
abline(city_linregTMINex, col="blue")

#save the plot shown in the window 
#always try to save as a PDF, not an image file
savePlot(filename="~/Documents/R_Data/Climate_Project/cityYearlyExtremesTrendPlot.pdf")

####incorporate land use data: PCAs####
#this file has the results of the climate change and UHI from the above analysis, represented as 1 (present) and 0 (not present)
#climate change = CC in the header
#it also has whether UHI and/or climate change results were significant (NA if not - also accompanied by a 0 in both climate change and UHI columns)
#cities should represent dataset used by the entire class - one row = one city
#for explanation of region, see main text
landUse <- read.csv(paste(ddr, "allLandUse.csv", sep=""))

#header codes: U = urban, R = rural, A = agriculture; 1990 = baseline, 2010 = current (should be updated to present)

#omit non-significant datasets
newLandUse <- na.omit(landUse)
#delete column that previously had NAs, we don't need it
#setting something as NULL deletes it
newLandUse$Significance <- NULL

#just looking at current dataset in this example, so get rid of baseline columns
newLandUse$X1990U <- NULL
newLandUse$X1990A <- NULL
newLandUse$X1990R <- NULL

#we don't need the city names, so hide them
newLandUse$City <- NULL

#save the regional groupings before we manipulate the data frame
#as.factor is so that they are discrete categories instead of a continuous scale
Region <- as.factor(newLandUse$Region)

#sometimes imported data isn't in the right format for calculations
#we can convert to numeric format so this isn't a problem anymore
#to be safe, we convert to a character first and then to a number so that cells from Excel with a lot of data within one cell don't mess up the conversion
#this is similar to the function above, where it makes everything numeric but does it by row
newLandUse <- sapply(newLandUse, function(x) as.numeric(as.character(x)))

#run a principle components analysis
#see here for more about PCAs: https://ourarchive.otago.ac.nz/bitstream/handle/10523/7534/OUCS-2002-12.pdf?sequence=1&isAllowed=y 
landUse.pca <- prcomp(newLandUse, scale=TRUE)
#looking at this object shows you the eigenvalue (PC) weights and the relative loadings on the PCs for each input variable 
landUse.pca

#look at the PCA in the viewer
print(ggbiplot(landUse.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE, groups=Region))

#the length of the arrow shows the magnitude of the effect, and the directionality is also expressed
#numbers are cities, grouped in ellipses by region (colored)
#only the first two PCs are shown (X and Y axes) and in parentheses, they have the amount of variance explained

####future challenges for students####

#can you automate the above code to loop through a list of all of your assigned cities?
#hint: look up function(), if statements, and for statements

#can you recreate these graphs in ggplot2?

#what other plots could you use to represent the data?
