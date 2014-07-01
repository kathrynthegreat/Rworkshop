Exploring Data
========================================================

Loading stuff we need.


library(plyr)

setwd("/Users/kathrynvasilaky/SkyDrive/R/NYCData")
data <- read.csv("/Users/kathrynvasilaky/SkyDrive/R/NYCData/data.csv")
path <- getwd()#"/Users/kathrynvasilaky/SkyDrive/R/NYCData"  
path <- paste(path,'/', sep='')

```

Explore the data
Look at the first 10 columns, the first 2 rows
How many unique groups are there? ( Answer:unique(data$group), summary(data) )
How many missing values 
  

```{r}
#Subset the data to not missings
data <- subset(data, group!='' )
#Eliminate observations with sum = NA or sum = 0
data <- subset(data, !is.na(sum))
data <- subset(data, sum > 0.0)
```

Replace missings with 0


```{r}
data$x1[is.na(data$x2)] <- 0
data$y1[is.na(data$y1)] <- 0

#Normalize the data to sum to 10

for (i in 1:nrow(data)) {
    row <- data[i,]
    if (row$sum != 10.0) { 
      #normalizing the summed column
      normed <- 10/row$sum
      data[i,]$x1 <- normed * data[i,]$x1
      data[i,]$y1 <- normed * data[i,]$y1
    }
}


```

Merging Files
Now let us merge in a file on the group variable
What type of variable is group? (A: typeof(data$group))
Need to convert group from factor to string type to match with gps


```{r}

#Merge in gps data
data$group <- as.character(data$group)
gps <- read.csv("/Users/kathrynvasilaky/SkyDrive/R/NYCData/gps.csv")

#creating variable to merge on
gps$group<- as.character(gps$group)
data_gps <- merge(x = data, y = gps, by = "group", data.x=TRUE)
#What happens if dat.x is set to "FALSE"?

```


Want a table of averages by group


```{r}
#Creating an indicator variable for groups that made a purchase
data_gps$Bought <- (data_gps$Buy=="Y")
#Creating an indicator variable for x1 positive
data_gps$x1 <- (data_gps$x1>0 )
#Create a new variable for the whole dataset
data_gps$total <- 'Total'

```

Table 1    			
Averages by Group							
    
```{r}

output <- ddply(data_gps, .(group), function(df)c(Mean = mean(df$x1),
                                                Mean2=mean(df$y1),
                                                North_Lat = unique(df$North_Lat), 
                                                West_Long = unique(df$West_Long)))

output2 <- ddply(data_gps, .(total), function(df)c(MeanX = mean(df$x1),
                                                MeanY=mean(df$y1),
                                                North_Lat = unique(df$North_Lat), 
                                                West_Long = unique(df$West_Long)))

#Hard way to rename
names(output)[3]<-'Average Output x1'

#Easier plyr way to rename
rename(output2,c('MeanX'='Average Output x1'))
#Does the latter actually change the variable name? 


#Create a table combining the group averages and total averages
table1 <- rbind(output, output2)
write.csv(file='groups_data.csv', x=table1)






#http://www.packtpub.com/article/customizing-graphics-creating-bar-chart-scatterplot-r
MeanX <- output$MeanX
MeanY <- total_avg$MeanY

MeanX <- "Average 1"
MeanY <- "Average 2"
LabelX <- c(0, 10)
LabelY <- c(0, 350)

bardataMethodsDurationRainbowColors <- rainbow(length(bardataMethodsDurationBars))
bardataMethodsDurationSpecificColors <- c("lightgrey", "white", "navy", "darkgrey")

barplot(height = bardataMethodsDurationBars,
main = bardataMethodsDurationLabelMain,
xlab = LabelX,
ylab = LabelY,
ylim=bardataMethodsDurationLimY,
names.arg =c("Take Home", "Personal Savings", "Insurance", "Community Savings"), col = bardataMethodsDurationRainbowColors,horiz=F,las=1,cex.names=.75,cex.lab=.75, cex.axis=.75, cex.main=1 )



#BARPLOT OF MEAN CHOICES IN data SESSIONS
#t<-cbind(TakeHome=total_avg$MeanTakeHome,Savings=total_avg$MeanPersonalSavings,Insurance=total_avg$MeanInsurance)
#barplot(t)
#title(main = list("data Sessions, Mean DR Pesos", font = 1))

  
```

Table 1a 
data gropus by season: Average # Pesos  						
		takehome	personalsavings	commuitysavings1si2no	febmarch aprilmayjune julyaugsept 	lat	long

```{r}
#hacked, uses data2.csv after stata crap
#also don't normalize data2
output <- ddply(data_gps, c('gropu'), function(df)c(MeanTakeHome = mean(df$takehome), 
                                                                    MeanPersonalSavings = mean(df$personalsavings),
                                                                    MeanCommunitySavings = mean(df$commuitysavings1si2no),
                                                                    MeanFebMarch = mean(df$febmarch),
                                                                    MeanAprilMayJune = mean(df$aprilmayjune),
                                                                    MeanJulyAugSept = mean(df$julyaugsept),
                                                                    North_Lat = unique(df$North_Lat), 
                                                                    West_Long = unique(df$West_Long) ))

total_avg <- ddply(data_gps, c('sean_dummy'), function(df)c(MeanTakeHome = mean(df$takehome), 
                                                                    MeanPersonalSavings = mean(df$personalsavings),
                                                                    MeanCommunitySavings = mean(df$commuitysavings1si2no),
                                                                    MeanFebMarch = mean(df$febmarch),
                                                                    MeanAprilMayJune = mean(df$aprilmayjune),
                                                                    MeanJulyAugSept = mean(df$julyaugsept)))

names(total_avg)[1]<-'gropu'
total_avg$North_Lat <- NA
total_avg$West_Long <- NA
table1 <- rbind(output, total_avg)
write.csv(file='2.gropus_cumulative.csv', x=table1)

#http://www.packtpub.com/article/customizing-graphics-creating-bar-chart-scatterplot-r
meanDurationFire <- total_avg$MeanTakeHome
meanDurationAmbush <- total_avg$MeanPersonalSavings
meanDurationHeadToHead <-total_avg$MeanFebMarch
meanDurationSurround <- total_avg$MeanAprilMayJune
meanINS3<-total_avg$MeanJulyAugSept
bardataMethodsDurationBars <- c(meanDurationFire,
meanDurationAmbush, meanDurationHeadToHead,
meanDurationSurround,meanINS3)

bardataMethodsDurationLabelMain <- "data Sessions"
bardataMethodsDurationLabelX <- "Average dataocation"
bardataMethodsDurationLabelY <- "DR Pesos"
bardataMethodsDurationLimX <- c(0, 10)
bardataMethodsDurationLimY <- c(0, 200)

bardataMethodsDurationRainbowColors <- rainbow(length(bardataMethodsDurationBars))
bardataMethodsDurationSpecificColors <- c("lightgrey", "white", "navy", "lightblue", "darkgrey")

barplot(height = bardataMethodsDurationBars,
main = bardataMethodsDurationLabelMain,
xlab = bardataMethodsDurationLabelX,
ylab = bardataMethodsDurationLabelY,
ylim=bardataMethodsDurationLimY,
names.arg =c("Take Home", "Personal \n Savings", "Feb-March \n Insurance", "Apr-May-Ju \n Insurance", "July-Aug-Sept \n Insurance"), col = bardataMethodsDurationRainbowColors,horiz=F,las=1,cex.names=.65,cex.lab=.75, cex.axis=.75, cex.main=1)

```



Table 2
gropus offerred cumulative index: Average # Pesos  	(cumulative)						
	takehome	personalsavings	commuitysavings1si2no	amountdrpfebmarch	amountdrpaprilmayjune	amountdrpjulyaugsept	lat	long
```{r}

data_gps_cumulative <- subset(data_gps, CumulativeInsurance)
output <- ddply(data_gps_cumulative, c('gropu'), function(df)c(MeanTakeHome = mean(df$takehome), 
                                                                    MeanPersonalSavings = mean(df$personalsavings),
                                                                    MeanCommunitySavings = mean(df$commuitysavings1si2no),
                                                                    MeanFebMarch = mean(df$amountdrpfebmarch),
                                                                    MeanAprilMayJune = mean(df$amountdrpaprilmayjune),
                                                                    MeanJulyAugSept = mean(df$amountdrpjulyaugsept),
                                                                    North_Lat = unique(df$North_Lat), 
                                                                    West_Long = unique(df$West_Long) ))

total_avg <- ddply(data_gps_cumulative, c('sean_dummy'), function(df)c(MeanTakeHome = mean(df$takehome), 
                                                                    MeanPersonalSavings = mean(df$personalsavings),
                                                                    MeanCommunitySavings = mean(df$commuitysavings1si2no),
                                                                    MeanFebMarch = mean(df$amountdrpfebmarch),
                                                                    MeanAprilMayJune = mean(df$amountdrpaprilmayjune),
                                                                    MeanJulyAugSept = mean(df$amountdrpjulyaugsept)))

names(total_avg)[1]<-'gropu'
total_avg$North_Lat <- NA
total_avg$West_Long <- NA
table1 <- rbind(output, total_avg)
write.csv(file='2.gropus_cumulative.csv', x=table1)

#http://www.packtpub.com/article/customizing-graphics-creating-bar-chart-scatterplot-r
meanDurationFire <- total_avg$MeanTakeHome
meanDurationAmbush <- total_avg$MeanPersonalSavings
meanDurationHeadToHead <-total_avg$MeanFebMarch
meanDurationSurround <- total_avg$MeanAprilMayJune
meanINS3<-total_avg$MeanJulyAugSept
bardataMethodsDurationBars <- c(meanDurationFire,
meanDurationAmbush, meanDurationHeadToHead,
meanDurationSurround,meanINS3)

bardataMethodsDurationLabelMain <- "Cumulative Index Sessions"
bardataMethodsDurationLabelX <- "Average dataocation"
bardataMethodsDurationLabelY <- "DR Pesos"
bardataMethodsDurationLimX <- c(0, 10)
bardataMethodsDurationLimY <- c(0, 200)

bardataMethodsDurationRainbowColors <- rainbow(length(bardataMethodsDurationBars))
bardataMethodsDurationSpecificColors <- c("lightgrey", "white", "navy", "lightblue", "darkgrey")

barplot(height = bardataMethodsDurationBars,
main = bardataMethodsDurationLabelMain,
xlab = bardataMethodsDurationLabelX,
ylab = bardataMethodsDurationLabelY,
ylim=bardataMethodsDurationLimY,
names.arg =c("Take Home", "Personal \n Savings", "Feb-March \n Insurance", "Apr-May-Ju \n Insurance", "July-Aug-Sept \n Insurance"), col = bardataMethodsDurationRainbowColors,horiz=F,las=1,cex.names=.75,cex.lab=.75, cex.axis=.75, cex.main=1)

#bardataMethodsDurationLegendLabels <- c("Take Home", "Personal \n Savings", "Feb-Mar \n Insurance", "Apr#-May-Ju \n Insurance", "July-Aug-Sept \n Insurance")
#legend("top", legend = bardataMethodsDurationLegendLabels,
#fill = bardataMethodsDurationSpecificColors)

```  
  
Table 3
  gropus offerred monthly index: Average # Pesos  	(not cumulative)											
	takehome	personalsavings	commuitysavings1si2no	amountdrpfebruary	amountdrpmarch	amountdrpapril	amountdrpmay	amountdrpjune	amountdrpjuly	amountdrpaugust	amountdrpsept	lat	long
  
```{r}

data_gps_monthly <- subset(data_gps, !CumulativeInsurance)
output <- ddply(data_gps_monthly, c('gropu'), function(df)c(MeanTakeHome = mean(df$takehome), 
                                                                    MeanPersonalSavings = mean(df$personalsavings),
                                                                    MeanCommunitySavings = mean(df$commuitysavings1si2no),
                                                                    MeanFebruary = mean(df$amountdrpfebruary),
                                                                    MeanMarch = mean(df$amountdrpmarch),
                                                                    MeanApril = mean(df$amountdrpapril),                                                                    
                                                                    MeanMay = mean(df$amountdrpmay),                                                                    
                                                                    MeanJune = mean(df$amountdrpjune),
                                                                    MeanJuly = mean(df$amountdrpjuly),
                                                                    MeanAugust = mean(df$amountdrpaugust),
                                                                    MeanSept = mean(df$amountdrpsept),
                                                                    North_Lat = unique(df$North_Lat), 
                                                                    West_Long = unique(df$West_Long) ))

total_avg <- ddply(data_gps_monthly, c('sean_dummy'), function(df)c(MeanTakeHome = mean(df$takehome), 
                                                                    MeanPersonalSavings = mean(df$personalsavings),
                                                                    MeanCommunitySavings = mean(df$commuitysavings1si2no),
                                                                    MeanFebruary = mean(df$amountdrpfebruary),
                                                                    MeanMarch = mean(df$amountdrpmarch),
                                                                    MeanApril = mean(df$amountdrpapril),                                                                    
                                                                    MeanMay = mean(df$amountdrpmay),                                                                    
                                                                    MeanJune = mean(df$amountdrpjune),
                                                                    MeanJuly = mean(df$amountdrpjuly),
                                                                    MeanAugust = mean(df$amountdrpaugust),
                                                                    MeanSept = mean(df$amountdrpsept) ))

names(total_avg)[1]<-'gropu'
total_avg$North_Lat <- NA
total_avg$West_Long <- NA

table1 <- rbind(output, total_avg)
write.csv(file='3.gropus_offered_monthly.csv', x=table1)


#http://www.packtpub.com/article/customizing-graphics-creating-bar-chart-scatterplot-r
meanDurationFire <- total_avg$MeanTakeHome
meanDurationAmbush <- total_avg$MeanPersonalSavings
meanDurationHeadToHead <-total_avg$MeanFebruary
meanDurationSurround <- total_avg$MeanMarch
meanINS3<-total_avg$MeanApril
meanINS4<-total_avg$MeanMay
meanINS5<-total_avg$MeanJune
meanINS6<-total_avg$MeanJuly
meanINS7<-total_avg$MeanAugust
meanINS8<-total_avg$MeanSept

bardataMethodsDurationBars <- c(meanDurationFire,
meanDurationAmbush, meanDurationHeadToHead,
meanDurationSurround,meanINS3,meanINS4,meanINS5,meanINS6,meanINS7,meanINS8)

bardataMethodsDurationLabelMain <- "Monthly Index Sessions"
bardataMethodsDurationLabelX <- "Average dataocation"
bardataMethodsDurationLabelY <- "DR Pesos"
bardataMethodsDurationLimX <- c(0, 10)
bardataMethodsDurationLimY <- c(0, 80)

bardataMethodsDurationRainbowColors <- rainbow(length(bardataMethodsDurationBars))
#bardataMethodsDurationSpecificColors <- c("lightgrey", "white", "navy", "lightblue", "darkgrey")

barplot(height = bardataMethodsDurationBars,
main = bardataMethodsDurationLabelMain,
xlab = bardataMethodsDurationLabelX,
ylab = bardataMethodsDurationLabelY,
ylim=bardataMethodsDurationLimY,
names.arg =c("Take Home", "Personal \n Savings", "Feb \n Insurance", "March \n Insurance", "April \n Insurance", "May \n Insurance", "June \n Insurance", "July \n Insurance", "Aug \n Insurance", "Sept \n Insurance"), col = bardataMethodsDurationRainbowColors,horiz=F,las=1,cex.names=.65,cex.lab=.75, cex.axis=.75, cex.main=1)

```  
  

Table 4    
gropus offerred only individual insurance:Average # Pesos		(individual only)
```{r}

data_gps_individual <- subset(data_gps, data_gps$OnlyIndividualInsurance)
output <- ddply(data_gps_individual, c('gropu'), function(df)c(MeanTakeHome = mean(df$takehome), 
                                                                   MeanPersonalSavings = mean(df$personalsavings), 
                                                                   MeanCommunitySavings = mean(df$commuitysavings1si2no), 
                                                                   MeanInsurance = mean(df$insurance), 
                                                                   North_Lat = unique(df$North_Lat), 
                                                                   West_Long = unique(df$West_Long) ))

total_avg <- ddply(data_gps_individual, c('sean_dummy'), function(df)c(MeanTakeHome = mean(df$takehome), 
                                                                     MeanPersonalSavings = mean(df$personalsavings),
                                                                     MeanCommunitySavings = mean(df$commuitysavings1si2no), 
                                                                     MeanInsurance = mean(df$insurance) ))

names(total_avg)[1]<-'gropu'
total_avg$North_Lat <- NA
total_avg$West_Long <- NA

table1 <- rbind(output, total_avg)
write.csv(file='4.gropus_offered_individual.csv', x=table1)




#http://www.packtpub.com/article/customizing-graphics-creating-bar-chart-scatterplot-r
meanDurationFire <- total_avg$MeanTakeHome
meanDurationAmbush <- total_avg$MeanPersonalSavings
meanDurationHeadToHead <-total_avg$MeanInsurance
hi<-total_avg$MeanCommunitySavings
bardataMethodsDurationBars <- c(meanDurationFire,
meanDurationAmbush, meanDurationHeadToHead,hi)

bardataMethodsDurationLabelMain <- "Individual Sessions"
bardataMethodsDurationLabelX <- "Average dataocation"
bardataMethodsDurationLabelY <- "DR Pesos"
bardataMethodsDurationLimX <- c(0, 10)
bardataMethodsDurationLimY <- c(0, 350)

bardataMethodsDurationRainbowColors <- rainbow(length(bardataMethodsDurationBars))
bardataMethodsDurationSpecificColors <- c("lightgrey", "white", "navy", "darkgrey")

barplot(height = bardataMethodsDurationBars,
main = bardataMethodsDurationLabelMain,
xlab = bardataMethodsDurationLabelX,
ylab = bardataMethodsDurationLabelY,
ylim=bardataMethodsDurationLimY,
names.arg =c("Take Home", "Personal Savings", "Insurance", "Community Savings"), col = bardataMethodsDurationRainbowColors,horiz=F,las=1,cex.names=.75,cex.lab=.75, cex.axis=.75, cex.main=1)


```

  
Table 5			
gropus offerred Group Insurance: Average # Pesos		not individual						
		takehome	personalsavings	commuitysavings1si2no	buyinsurancewritegforgroupandifo	insurance	lat	long
    
```{r}
data_gps_group <- subset(data_gps, !OnlyIndividualInsurance)
output <- ddply(data_gps_group, c('gropu','buyinsurancewritegforgroupandifo'), function(df)c(MeanTakeHome = mean(df$takehome), 
                                                                    MeanPersonalSavings = mean(df$personalsavings),
                                                                    MeanCommunitySavings = mean(df$commuitysavings1si2no),
                                                                    Count = nrow(df),
                                                                    MeanInsurance = mean(df$insurance),
                                                                    North_Lat = unique(df$North_Lat), 
                                                                    West_Long = unique(df$West_Long) ))

total_avg <- ddply(data_gps_group, c('sean_dummy','buyinsurancewritegforgroupandifo'),  function(df)c(MeanTakeHome = mean(df$takehome), 
                                                                    MeanPersonalSavings = mean(df$personalsavings),
                                                                    MeanCommunitySavings = mean(df$commuitysavings1si2no),
                                                                    Count = nrow(df),
                                                                    MeanInsurance = mean(df$insurance)))
total_avg1 <- ddply(data_gps_group, c('sean_dummy'),  function(df)c(MeanTakeHome = mean(df$takehome), 
                                                                    MeanPersonalSavings = mean(df$personalsavings),
                                                                    MeanCommunitySavings = mean(df$commuitysavings1si2no),
                                                                    Count = nrow(df),
                                                                    MeanInsurance = mean(df$insurance)))


names(total_avg)[1]<-'gropu'
total_avg$North_Lat <- NA
total_avg$West_Long <- NA

names(total_avg1)[1]<-'gropu'
total_avg1$North_Lat <- NA
total_avg1$West_Long <- NA

table1 <- rbind(output, total_avg, totalavg1)
write.csv(file='5.gropus_offered_groups.csv', x=table1)


