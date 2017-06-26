##################################################################################
#Loading Data into R
##################################################################################
# library(rmongodb)
# myConnection = mongo.create();
# ABC = mongo.find.all(myConnection, 'Assignment.Crashes');
# myDataFrame = do.call(rbind.data.frame,ABC);
# View(myDataFrame)
myDataFrame <- read.csv('crashes.csv')

##################################################################################
## Case 1: Street lights off
##Sub setting data according to light type equal to dark street light off
##################################################################################
x.sub <- subset(myDataFrame, LIGHT_CONDITION == "Dark Street lights off")
View(x.sub)

##################################################################################
##Classifying accident count according to suburb
##################################################################################
library(ggplot2)
ggplot(data = x.sub, aes(x = LGA_NAME,fill = LGA_NAME)) + geom_bar() + coord_flip()


##################################################################################
##Finding top 5 suburbs 
##################################################################################
counts_lo <- table(x.sub$LGA_NAME)
summary(x.sub$LGA_NAME)
write.table(counts_lo,"C:/Users/JATESH/Desktop/Dataset/New folder/top5_street_lights_off.txt", row.names=FALSE) #for making .txt file
barplot(sort(counts_lo,decreasing=TRUE)[1:5],
        main="Top 5 cities where accidents due to street lights off", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(90, 70, 50, 40, 30))
box()


##################################################################################
##Sub setting data according to top 5 cities where accident occur more because of 
##street lights being off
##################################################################################
topcity <-subset(x.sub, LGA_NAME == "CASEY" | LGA_NAME == "MELBOURNE" | LGA_NAME == "MONASH" | 
                   LGA_NAME == "MELTON" | LGA_NAME == "HUME")
View(topcity)

##################################################################################
##Plotting number of accident distribution with respect to year in   
##top 5 cities due to street lights being off
##################################################################################
library(lubridate)
accident_date <- data.frame(DateTime = topcity$ACCIDENT_DATE)
View(accident_date)
year_collection <- c()
for (j in accident_date)
{
  date_dmy <- dmy(j)
  year_extract <- as.numeric(year(date_dmy))
  year_collection <- as.data.frame(year_extract)
}
View(year_collection)
counts_acci_year <- table(year_collection)
View(counts_acci_year)
barplot(sort(counts_acci_year,decreasing=TRUE)[1:6],
        main="Distribution of accidents with respect to year", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(110, 90, 70, 50, 40, 30))
box()

##################################################################################
##Plotting trend of number of accident with respect to year in   
##top 5 cities due to street lights being off
##################################################################################
plot(counts_acci_year,type = "o",col = "red",  xlab = "Year", ylab = "Number of Accidents",
     main = "Yearly number of accidents distribution")

##################################################################################
##Plotting number of accident distribution with respect to day of week  in   
##top 5 cities due to street lights being off
##################################################################################
library(lubridate)
accident_day <- data.frame(DateTime = topcity$DAY_OF_WEEK)
View(accident_day)

counts_acci_day <- table(accident_day,exclude=NULL)
View(counts_acci_day)
barplot(sort(counts_acci_day,decreasing=TRUE)[1:7],
        main="Distribution of accidents with respect to weekday", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(110, 100, 90, 70, 50, 40, 30))
box()
  
##################################################################################
##Plotting number of accident distribution with respect to hour in   
##top 5 cities due to street lights being off
##################################################################################
library(lubridate)
accident_hour <- data.frame(DateTime = topcity$ACCIDENT_TIME)
View(accident_hour)
hour_collection <- c()
for (k in accident_hour)
{
  time_hms <- hms(k)
  hour_extract <- as.numeric(hour(time_hms))
  hour_collection <- as.data.frame(hour_extract)
}
View(hour_collection)
counts_acci_hour <- table(hour_collection)
View(counts_acci_hour)
barplot(sort(counts_acci_hour,decreasing=TRUE)[1:10],
        main="Distribution of accidents with respect to hour", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(110, 100, 90, 70, 60, 50, 40, 30, 20, 10))
box()

##################################################################################
## Case 2: No Street Lights
##Sub setting data according to light type equal to no street light
##################################################################################
x.sub1 <- subset(myDataFrame, LIGHT_CONDITION == "Dark No street lights")
View(x.sub1)

##################################################################################
##Classifying accident count according to suburb
##################################################################################
library(ggplot2)
ggplot(data = x.sub1, aes(x = LGA_NAME,fill = LGA_NAME)) + geom_bar() + coord_flip()


##################################################################################
##Finding top 5 suburbs 
##################################################################################
counts_lnp <- table(x.sub1$LGA_NAME)
summary(x.sub1$LGA_NAME)
write.table(counts_lnp,"C:/Users/JATESH/Desktop/Dataset/New folder/top5_no_street_lights.txt", row.names=FALSE) #for making .txt file
barplot(sort(counts_lnp,decreasing=TRUE)[1:5],
        main="Top 5 cities where accidents due to no street lights", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(90, 70, 50, 40, 30))
box()




##################################################################################
##Sub setting data according to top 5 cities where accident occur more because of 
##street lights being off
##################################################################################
topcity1 <-subset(x.sub1, LGA_NAME == "YARRA RANGES" | LGA_NAME == "CASEY" | LGA_NAME == "GEELONG" | 
                    LGA_NAME == "CARDINIA" | LGA_NAME == "MITCHELL")
View(topcity1)

##################################################################################
##Plotting number of accident distribution with respect to year in   
##top 5 cities due to no street lights
##################################################################################
library(lubridate)
accident_date_1 <- data.frame(DateTime = topcity1$ACCIDENT_DATE)
View(accident_date_1)
year_collection_1 <- c()
for (l in accident_date_1)
{
  date_dmy_1 <- dmy(l)
  year_extract_1 <- as.numeric(year(date_dmy_1))
  year_collection_1 <- as.data.frame(year_extract_1)
}
View(year_collection_1)
counts_acci_year_1 <- table(year_collection_1)
View(counts_acci_year_1)
barplot(sort(counts_acci_year_1,decreasing=TRUE)[1:6],
        main="Distribution of accidents with respect to year", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(100, 90, 70, 50, 40, 30))
box()


##################################################################################
##Plotting trend of number of accident with respect to year in   
##top 5 cities due to no street lights
##################################################################################
plot(counts_acci_year_1,type = "o",col = "red",  xlab = "Year", ylab = "Number of Accidents",
     main = "Yearly number of accidents distribution")

##################################################################################
##Plotting number of accident distribution with respect to day of week  in   
##top 5 cities due to no street lights
##################################################################################
library(lubridate)
accident_day_1 <- data.frame(DateTime = topcity1$DAY_OF_WEEK)
View(accident_day_1)

counts_acci_day_1 <- table(accident_day_1,exclude=NULL)
View(counts_acci_day_1)
barplot(sort(counts_acci_day_1,decreasing=TRUE)[1:7],
        main="Distribution of accidents with respect to weekday", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(110, 100, 90, 70, 50, 40, 30))
box()

##################################################################################
##Plotting number of accident distribution with respect to hour in   
##top 5 cities due to no street lights
##################################################################################
library(lubridate)
accident_hour_1 <- data.frame(DateTime = topcity1$ACCIDENT_TIME)
View(accident_hour_1)
hour_collection_1 <- c()
for (m in accident_hour_1)
{
  time_hms_1 <- hms(m)
  hour_extract_1 <- as.numeric(hour(time_hms_1))
  hour_collection_1 <- as.data.frame(hour_extract_1)
}
View(hour_collection_1)
counts_acci_hour_1 <- table(hour_collection_1)
View(counts_acci_hour_1)
barplot(sort(counts_acci_hour_1,decreasing=TRUE)[1:10],
        main="Distribution of accidents with respect to year", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(110, 100, 90, 70, 60, 50, 40, 30, 20, 10))
box()



##################################################################################
##Viewing number of accidents caused by alcohol involvement
##################################################################################

library(ggplot2)
library(data.table)
ggplot(data = myDataFrame, aes(x = ALCOHOL_RELATED,fill =ALCOHOL_RELATED)) + 
  geom_bar()+ coord_flip()
#qplot(factor(ALCOHOL_RELATED), data=myDataFrame, geom="bar", 
#fill=factor(ALCOHOL_RELATED))

##################################################################################
##Problem 2: Analyzing accidents occuring due to alcohol consumption
##Sub setting data according alcohol related
##################################################################################
alcohol_involved <- subset(myDataFrame, ALCOHOL_RELATED == 'Yes')
View(alcohol_involved)

##################################################################################
##Classifying accident count according to suburb
##################################################################################
library(ggplot2)
ggplot(data = alcohol_involved, aes(x = LGA_NAME,fill = LGA_NAME)) + geom_bar() + coord_flip()

##################################################################################
##Finding top 5 suburbs 
##################################################################################
counts_al <- table(alcohol_involved$LGA_NAME)
summary_al(alcohol_involved$LGA_NAME)
write.table(counts_al,"C:/Users/JATESH/Desktop/Dataset/New folder/alcohol_table.txt", row.names=FALSE) #for making .txt file
barplot(sort(counts_al,decreasing=TRUE)[1:5],
        main="Top 5 cities where accidents occur due to alcohol intake", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(90, 70, 50, 40, 30))
box()

##################################################################################
##Sub setting data according to top 5 cities where accident occur more due to
##consumption of alcohol
##################################################################################
topcity_al <-subset(alcohol_involved, LGA_NAME == "CASEY" | LGA_NAME == "GEELONG" | LGA_NAME == "BRIMBANK" | 
                   LGA_NAME == "DANDENONG" | LGA_NAME == "YARRA RANGES")
View(topcity_al)

##################################################################################
##Demographics of Injury type in top 5 cities
##################################################################################
Slices_al <- c(sum(topcity_al$INJ_OR_FATAL),sum(topcity_al$FATALITY),sum(topcity_al$SERIOUSINJURY),
            sum(topcity_al$NONINJURED))
Slices_al
lbls_al <- c("Injured", "Fatality","Serious Injury", "Not Injured")
pct_al <- round(Slices_al/sum(Slices_al)*100)
lbls_al <- paste(lbls_al, pct_al) # add percents to labels 
lbls_al <- paste(lbls_al,"%",sep="") # ad % to labels 
pie(Slices_al,labels = lbls_al, col=rainbow(length(lbls_al)),
    main="Pie Chart of Injury Type")
library(plotrix)
pie3D(Slices_al,labels=lbls_al,explode=0.1,
      main="Pie Chart of Injury Type")

##################################################################################
##Demographics of people type involved in accidents of top 5 cities
##################################################################################
Slices_al_1 <- c(sum(topcity_al$PASSENGER),sum(topcity_al$BICYCLIST),sum(topcity_al$DRIVER),
             sum(topcity_al$MOTORIST),sum(topcity_al$PEDESTRIAN))
Slices_al_1
lbls_al_1 <- c("Passenger", "Bicyclist","Driver", "Motorist", "Pedestrian")
pct_al_1 <- round(Slices_al_1/sum(Slices_al_1)*100)
lbls_al_1 <- paste(lbls_al_1, pct_al_1) # add percents to labels 
lbls_al_1 <- paste(lbls_al_1,"%",sep="") # ad % to labels 
pie(Slices_al_1,labels = lbls_al_1, col=rainbow(length(lbls_al_1)),
    main="Pie Chart of Person Type")
library(plotrix)
pie3D(Slices_al_1,labels=lbls_al_1,explode=0.1,
      main="Pie Chart of Person Type")

##################################################################################
##Demographics of vehicle type involved in accidents of top 5 cities
##################################################################################
Slices_al_2 <- c(sum(topcity_al$HEAVYVEHICLE),sum(topcity_al$PASSENGERVEHICLE),sum(topcity_al$PUBLICVEHICLE),
             sum(topcity_al$MOTORCYCLE))
Slices_al_2
lbls_al_2 <- c("Heavy Vehicle", "Passenger Vehicle","Public Vehicle", "Motor Cycle")
pct_al_2 <- round(Slices_al_2/sum(Slices_al_2)*100)
pct_al_2
lbls_al_2 <- paste(lbls_al_2, pct_al_2) # add percents to labels 
lbls_al_2 <- paste(lbls_al_2,"%",sep="") # ad % to labels 
pie(Slices_al_2,labels = lbls_al_2, col=rainbow(length(lbls_al_2)),
    main="Pie Chart of Vehicle Type")
library(plotrix)
pie3D(Slices_al_2,labels=lbls_al_2,explode=0.2,
      main="Pie Chart of Vehicle Type ")

##################################################################################
##Road Types
##################################################################################

counts_al_1 <- table(topcity_al$RMA, topcity_al$LGA_NAME)
counts_al_1
csel_counts_al_1 <- counts_al_1[,c("CASEY", "GEELONG","BRIMBANK","DANDENONG","YARRA RANGES")];
csel_counts_al_1
rsel_counts_al_1 <- csel_counts_al_1[c("Arterial Highway","Arterial Other","Freeway","Local Road","Non Arterial"),];
rsel_counts_al_1

barplot(rsel_counts_al_1, main="Accident Distribution by road type",
        xlab="City_Names",ylab = "Number of Accidents", col = c("darkblue","red","green","yellow","orange"),
        legend = c("Arterial Highway", "Arterial Other", "Freeway","Local Road","Non Arterial"), 
        args.legend = list(title = "Roads", x = "topright", cex = .5),ylim = c(0,150))

##################################################################################
##Speed Analysis
##################################################################################
counts_al_2 <- table(topcity_al$SPEED_ZONE, topcity_al$LGA_NAME);
counts_al_2
csel_counts_al_2 <- counts_al_2[,c("CASEY", "GEELONG","BRIMBANK","DANDENONG","YARRA RANGES")];
csel_counts_al_2
rsel_counts_al_2 <- csel_counts_al_2[c("30km/hr","40 km/hr","50 km/hr","60 km/hr","70 km/hr",
                                 "75 km/hr","80 km/hr","90 km/hr","100 km/hr","110 km/hr"),];
rsel_counts_al_2
barplot(rsel_counts_al_2, main="Accident Distribution by Speed",
        xlab="City_Names",ylab = "Number of Accidents", 
        col = c("darkblue","red","green","yellow","orange","blue",
                "brown","violet","purple","grey"),
        legend = c("30km/hr","40 km/hr","50 km/hr","60 km/hr","70 km/hr",
                   "75 km/hr","80 km/hr","90 km/hr","100 km/hr","110 km/hr"), 
        args.legend = list(title = "Speed", x = "topright", cex = .5),ylim = c(0, 250))

##################################################################################
##Accident Type 
##################################################################################
counts_al_3 <- table(topcity_al$ACCIDENT_TYPE, topcity_al$LGA_NAME);
counts_al_3
csel_counts_al_3 <- counts_al_3[,c("CASEY", "GEELONG","BRIMBANK","DANDENONG","YARRA RANGES")];
csel_counts_al_3

barplot(csel_counts_al_3, main="Accident Distribution by Accident Type",
        xlab="City_Names",ylab = "Number of Accidents", 
        col = c("darkblue","red","green","yellow","orange","blue",
                "brown","violet","grey"),
        legend = c("Collision with a fixed object","collision with some other object",
                   "Collision with vehicle","Fall from or in moving vehicle",
                   "No collision and no object struck",
                   "Other accident","Struck animal","Struck Pedestrian","Vehicle overturned"), 
        args.legend = list(title = "Accident Type", x = "topright", cex = .5),ylim = c(0, 250))

##################################################################################
##Plotting number of accident distribution with respect to year in   
##top 5 cities where accidents occur due to alcohol consumption
##################################################################################
library(lubridate)
accident_date_al <- data.frame(DateTime = topcity_al$ACCIDENT_DATE)
View(accident_date_al)
year_collection_al <- c()
for (j in accident_date_al)
{
  date_dmy_al <- dmy(j)
  year_extract_al <- as.numeric(year(date_dmy_al))
  year_collection_al <- as.data.frame(year_extract_al)
}
View(year_collection_al)
counts_acci_year_al <- table(year_collection_al)
View(counts_acci_year_al)
barplot(sort(counts_acci_year_al,decreasing=TRUE)[1:6],
        main="Distribution of accidents with respect to year", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(110, 90, 70, 50, 40, 30))
box()

##################################################################################
##Plotting number of accident distribution with respect to day of week  in   
##top 5 cities due to alcohol involvement
##################################################################################
library(lubridate)
accident_day_al <- data.frame(DateTime = topcity_al$DAY_OF_WEEK)
View(accident_day_al)

counts_acci_day_al <- table(accident_day_al,exclude=NULL)
View(counts_acci_day_al)
barplot(sort(counts_acci_day_al,decreasing=TRUE)[1:7],
        main="Distribution of accidents with respect to weekday", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(110, 90, 70, 50, 40, 30, 10))
box()

##################################################################################
##Plotting number of accident distribution with respect to hour in   
##top 5 cities due to alcohol involvement
##################################################################################
library(lubridate)
accident_hour_al <- data.frame(DateTime = topcity_al$ACCIDENT_TIME)
View(accident_hour_al)
hour_collection_al <- c()
for (z in accident_hour_al)
{
  time_hms_al <- hms(z)
  hour_extract_al <- as.numeric(hour(time_hms_al))
  print(hour_extract_al)
  hour_collection_al <- as.data.frame(hour_extract_al)
}
View(hour_collection_al)
counts_acci_hour_al <- table(hour_collection_al)
View(counts_acci_hour_al)
barplot(sort(counts_acci_hour_al,decreasing=TRUE)[1:10],
        main="Distribution of accidents with respect to hour", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(110, 100, 90, 70, 60, 50, 40, 30, 20, 10))
box()

##################################################################################
##Plotting trend of number of accident with respect to year in   
##top 5 cities due to alcohol involvement
##################################################################################
plot(counts_acci_year_al,type = "o",col = "red",  xlab = "Year", ylab = "Number of Accidents",
     main = "Yearly number of accidents distribution")










##################################################################################
##################################################################################
##Hit And Run
##################################################################################
##################################################################################


##################################################################################
##Viewing number of accidents by Hit and run flag
##################################################################################
library(ggplot2)
library(data.table)
ggplot(data = myDataFrame, aes(x = HIT_RUN_FLAG,fill =HIT_RUN_FLAG)) + 
  geom_bar()+ coord_flip()
#qplot(factor(HIT_RUN_FLAG), data=myDataFrame, geom="bar", 
#fill=factor(HIT_RUN_FLAG))


##################################################################################
##Problem 4: Analyzing accidents in which police have to catch hit and run cases
##Sub setting data according hit and run
##################################################################################
hit_run <- subset(myDataFrame, HIT_RUN_FLAG == 'Yes')
View(hit_run)

##################################################################################
##Classifying accident count according to suburb
##################################################################################

ggplot(data = hit_run, aes(x = LGA_NAME,fill = LGA_NAME)) + geom_bar() + coord_flip()

##################################################################################
##Finding top 5 suburbs 
##################################################################################
counts_hr <- table(hit_run$LGA_NAME)
summary(hit_run$LGA_NAME)
write.table(counts_hr,"C:/Users/JATESH/Desktop/Dataset/New folder/hit_run_table.txt", row.names=FALSE) #for making .txt file
barplot(sort(counts_hr,decreasing=TRUE)[1:5],
        main="Top 5 cities where accidents involve hit and run", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(90, 70, 50, 40, 30))
box()


##################################################################################
##Sub setting data according to top 5 cities where accident include hit and run
##################################################################################
topcity_hr <-subset(hit_run, LGA_NAME == "MELBOURNE" | LGA_NAME == "BRIMBANK" | LGA_NAME == "DANDENONG" | 
                   LGA_NAME == "MORELAND" | LGA_NAME == "GEELONG")
View(topcity_hr)

##################################################################################
##Demographics of Injury type in top 5 cities
##################################################################################
Slices_hr <- c(sum(topcity_hr$INJ_OR_FATAL),sum(topcity_hr$FATALITY),sum(topcity_hr$SERIOUSINJURY),
               sum(topcity_hr$NONINJURED))
Slices_hr
lbls_hr <- c("Injured", "Fatality","Serious Injury", "Not Injured")
pct_hr <- round(Slices_hr/sum(Slices_hr)*100)
lbls_hr <- paste(lbls_hr, pct_hr) # add percents to labels 
lbls_hr <- paste(lbls_hr,"%",sep="") # ad % to labels 
pie(Slices_hr,labels = lbls_hr, col=rainbow(length(lbls_hr)),
    main="Pie Chart of Injury Type")
library(plotrix)
pie3D(Slices_hr,labels=lbls_hr,explode=0.1,
      main="Pie Chart of Injury Type")

##################################################################################
##Demographics of people type involved in accidents of top 5 cities
##################################################################################
Slices_hr_1 <- c(sum(topcity_hr$BICYCLIST),sum(topcity_hr$PASSENGER),sum(topcity_hr$DRIVER),
                 sum(topcity_hr$MOTORIST),sum(topcity_hr$PEDESTRIAN))
Slices_hr_1
lbls_hr_1 <- c("Bicyclist", "Passenger","Driver", "Motorist", "Pedestrian")
pct_hr_1 <- round(Slices_hr_1/sum(Slices_hr_1)*100)
lbls_hr_1 <- paste(lbls_hr_1, pct_hr_1) # add percents to labels 
lbls_hr_1 <- paste(lbls_hr_1,"%",sep="") # ad % to labels 
pie(Slices_hr_1,labels = lbls_hr_1, col=rainbow(length(lbls_hr_1)),
    main="Pie Chart of Person Type")
library(plotrix)
pie3D(Slices_hr_1,labels=lbls_hr_1,explode=0.1,
      main="Pie Chart of Person Type")

##################################################################################
##Demographics of vehicle type involved in accidents of top 5 cities
##################################################################################
Slices_hr_2 <- c(sum(topcity_hr$HEAVYVEHICLE),sum(topcity_hr$PASSENGERVEHICLE),sum(topcity_hr$PUBLICVEHICLE),
                 sum(topcity_hr$MOTORCYCLE))
Slices_hr_2
lbls_hr_2 <- c("Heavy Vehicle", "Passenger Vehicle","Public Vehicle", "Motor Cycle")
pct_hr_2 <- round(Slices_hr_2/sum(Slices_hr_2)*100)
pct_hr_2
lbls_hr_2 <- paste(lbls_hr_2, pct_hr_2) # add percents to labels 
lbls_hr_2 <- paste(lbls_hr_2,"%",sep="") # ad % to labels 
pie(Slices_hr_2,labels = lbls_hr_2, col=rainbow(length(lbls_hr_2)),
    main="Pie Chart of Vehicle Type")
library(plotrix)
pie3D(Slices_hr_2,labels=lbls_hr_2,explode=0.2,
      main="Pie Chart of Vehicle Type ")

##################################################################################
##Road Types
##################################################################################

counts_hr_1 <- table(topcity_hr$RMA, topcity_hr$LGA_NAME)
counts_hr_1
csel_counts_hr_1 <- counts_hr_1[,c("MELBOURNE", "BRIMBANK","DANDENONG","MORELAND","GEELONG")];
csel_counts_hr_1
rsel_counts_hr_1 <- csel_counts_hr_1[c("Arterial Highway","Arterial Other","Freeway","Local Road","Non Arterial"),];
rsel_counts_hr_1

barplot(rsel_counts_hr_1, main="Accident Distribution by road type",
        xlab="City_Names",ylab = "Number of Accidents", col = c("darkblue","red","green","yellow","orange"),
        legend = c("Arterial Highway", "Arterial Other", "Freeway","Local Road","Non Arterial"), 
        args.legend = list(title = "Roads", x = "topright", cex = .5),ylim = c(0, 370))

##################################################################################
##Speed Analysis
##################################################################################
counts_hr_2 <- table(topcity_hr$SPEED_ZONE, topcity_hr$LGA_NAME);
counts_hr_2
csel_counts_hr_2 <- counts_hr_2[,c("MELBOURNE", "BRIMBANK","DANDENONG","MORELAND","GEELONG")];
csel_counts_hr_2
rsel_counts_hr_2 <- csel_counts_hr_2[c("30km/hr","40 km/hr","50 km/hr","60 km/hr","70 km/hr",
                                       "75 km/hr","80 km/hr","90 km/hr","100 km/hr","110 km/hr"),];
rsel_counts_hr_2
barplot(rsel_counts_hr_2, main="Accident Distribution by Speed",
        xlab="City_Names",ylab = "Number of Accidents", 
        col = c("darkblue","red","green","yellow","orange","blue",
                "brown","violet","purple","grey"),
        legend = c("30km/hr","40 km/hr","50 km/hr","60 km/hr","70 km/hr",
                   "75 km/hr","80 km/hr","90 km/hr","100 km/hr","110 km/hr"), 
        args.legend = list(title = "Speed", x = "topright", cex = .5),ylim = c(0,430))

##################################################################################
##Accident Type 
##################################################################################
counts_hr_3 <- table(topcity_hr$ACCIDENT_TYPE, topcity_hr$LGA_NAME);
counts_hr_3
csel_counts_hr_3 <- counts_hr_3[,c("MELBOURNE", "BRIMBANK","DANDENONG","MORELAND","GEELONG")];
csel_counts_hr_3

barplot(csel_counts_hr_3, main="Accident Distribution by Accident Type",
        xlab="City_Names",ylab = "Number of Accidents", 
        col = c("darkblue","red","green","yellow","orange","blue",
                "brown","violet","grey"),
        legend = c("Collision with a fixed object","collision with some other object",
                   "Collision with vehicle","Fall from or in moving vehicle",
                   "No collision and no object struck",
                   "Other accident","Struck animal","Struck Pedestrian","Vehicle overturned"), 
        args.legend = list(title = "Accident Type", x = "topright", cex = .5),ylim = c(0, 420))

##################################################################################
##Plotting number of accident distribution with respect to year in   
##top 5 cities involving hit and run
##################################################################################
library(lubridate)
accident_date_hr <- data.frame(DateTime = topcity_hr$ACCIDENT_DATE)
View(accident_date_hr)
year_collection_hr <- c()
for (j in accident_date_hr)
{
  date_dmy_hr <- dmy(j)
  year_extract_hr <- as.numeric(year(date_dmy_hr))
  year_collection_hr <- as.data.frame(year_extract_hr)
}
View(year_collection_hr)
counts_acci_year_hr <- table(year_collection_hr)
View(counts_acci_year_hr)
barplot(sort(counts_acci_year_hr,decreasing=TRUE)[1:6],
        main="Distribution of accidents with respect to year", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(110, 90, 70, 50, 40, 30))
box()

##################################################################################
##Plotting number of accident distribution with respect to day of week  in   
##top 5 cities involving hit and run
##################################################################################
library(lubridate)
accident_day_hr <- data.frame(DateTime = topcity_hr$DAY_OF_WEEK)
View(accident_day_hr)

counts_acci_day_hr <- table(accident_day_hr,exclude=NULL)
View(counts_acci_day_hr)
barplot(sort(counts_acci_day_hr,decreasing=TRUE)[1:7],
        main="Distribution of accidents with respect to weekday", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(110, 90, 70, 50, 40, 30, 10))
box()

##################################################################################
##Plotting number of accident distribution with respect to hour in   
##top 5 cities involving hit and run
##################################################################################
library(lubridate)
accident_hour_hr <- data.frame(DateTime = topcity_hr$ACCIDENT_TIME)
View(accident_hour_hr)
hour_collection_hr <- c()
for (z in accident_hour_hr)
{
  time_hms_hr <- hms(z)
  hour_extract_hr <- as.numeric(hour(time_hms_hr))
  hour_collection_hr <- as.data.frame(hour_extract_hr)
}
View(hour_collection_hr)
counts_acci_hour_hr <- table(hour_collection_hr)
View(counts_acci_hour_hr)
barplot(sort(counts_acci_hour_hr,decreasing=TRUE)[1:10],
        main="Distribution of accidents with respect to year", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(110, 100, 90, 70, 60, 50, 40, 30, 20, 10))
box()

##################################################################################
##Plotting trend of number of accident with respect to year in   
##top 5 cities involving hit and run
##################################################################################
plot(counts_acci_year_hr,type = "o",col = "red",  xlab = "Year", ylab = "Number of Accidents",
     main = "Yearly number of accidents distribution")










##################################################################################
##################################################################################
##Police Involved
##################################################################################
##################################################################################






##################################################################################
##Viewing number of accidents requiring police to attend
##################################################################################

library(ggplot2)
library(data.table)
ggplot(data = myDataFrame, aes(x = POLICE_ATTEND,fill =POLICE_ATTEND)) + 
  geom_bar()+ coord_flip()
#qplot(factor(POLICE_ATTEND), data=myDataFrame, geom="bar", 
#fill=factor(POLICE_ATTEND))


##################################################################################
## Problem 5: Analyzing accidents in which police have attend
##################################################################################
police_involve <- subset(myDataFrame, POLICE_ATTEND == 'Yes')
View(police_involve)

##################################################################################
##Classifying accident count according to suburb
##################################################################################

ggplot(data = police_involve, aes(x = LGA_NAME,fill = LGA_NAME)) + geom_bar() + coord_flip()

##################################################################################
##Finding top 5 suburbs 
##################################################################################
counts_pi <- table(police_involve$LGA_NAME)
counts_pi
summary(police_involve$LGA_NAME)
write.table(counts_pi,"C:/Users/JATESH/Desktop/Dataset/New folder/police_involve.txt", row.names=FALSE) #for making .txt file
barplot(sort(counts_pi,decreasing=TRUE)[1:5],
        main="Top 5 cities where accidents require police involvement", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(90, 70, 50, 40, 30))
box()


##################################################################################
##Sub setting data according to top 5 cities where accident include hit and run
##################################################################################
topcity_pi <-subset(police_involve, LGA_NAME == "MELBOURNE" | LGA_NAME == "CASEY" | LGA_NAME == "DANDENONG" | 
                      LGA_NAME == "GEELONG" | LGA_NAME == "BRIMBANK")
View(topcity_pi)

##################################################################################
##Demographics of Injury type in top 5 cities
##################################################################################
Slices_pi <- c(sum(topcity_pi$INJ_OR_FATAL),sum(topcity_pi$FATALITY),sum(topcity_pi$SERIOUSINJURY),
               sum(topcity_pi$NONINJURED))
Slices_pi
lbls_pi <- c("Injured", "Fatality","Serious Injury", "Not Injured")
pct_pi <- round(Slices_pi/sum(Slices_pi)*100)
lbls_pi <- paste(lbls_pi, pct_pi) # add percents to labels 
lbls_pi <- paste(lbls_pi,"%",sep="") # ad % to labels 
pie(Slices_pi,labels = lbls_pi, col=rainbow(length(lbls_pi)),
    main="Pie Chart of Injury Type")
library(plotrix)
pie3D(Slices_pi,labels=lbls_pi,explode=0.1,
      main="Pie Chart of Injury Type")

##################################################################################
##Demographics of people type involved in accidents of top 5 cities
##################################################################################
Slices_pi_1 <- c(sum(topcity_pi$BICYCLIST),sum(topcity_pi$PASSENGER),sum(topcity_pi$DRIVER),
                 sum(topcity_pi$MOTORIST),sum(topcity_pi$PEDESTRIAN))
Slices_pi_1
lbls_pi_1 <- c("Bicyclist", "Passenger","Driver", "Motorist", "Pedestrian")
pct_pi_1 <- round(Slices_pi_1/sum(Slices_pi_1)*100)
lbls_pi_1 <- paste(lbls_pi_1, pct_pi_1) # add percents to labels 
lbls_pi_1 <- paste(lbls_pi_1,"%",sep="") # ad % to labels 
pie(Slices_pi_1,labels = lbls_pi_1, col=rainbow(length(lbls_pi_1)),
    main="Pie Chart of Person Type")
library(plotrix)
pie3D(Slices_pi_1,labels=lbls_pi_1,explode=0.1,
      main="Pie Chart of Person Type")

##################################################################################
##Demographics of vehicle type involved in accidents of top 5 cities
##################################################################################
Slices_pi_2 <- c(sum(topcity_pi$HEAVYVEHICLE),sum(topcity_pi$PASSENGERVEHICLE),sum(topcity_pi$PUBLICVEHICLE),
                 sum(topcity_pi$MOTORCYCLE))
Slices_pi_2
lbls_pi_2 <- c("Heavy Vehicle", "Passenger Vehicle","Public Vehicle", "Motor Cycle")
pct_pi_2 <- round(Slices_pi_2/sum(Slices_pi_2)*100)
pct_pi_2
lbls_pi_2 <- paste(lbls_pi_2, pct_pi_2) # add percents to labels 
lbls_pi_2 <- paste(lbls_pi_2,"%",sep="") # ad % to labels 
pie(Slices_pi_2,labels = lbls_pi_2, col=rainbow(length(lbls_pi_2)),
    main="Pie Chart of Vehicle Type")
library(plotrix)
pie3D(Slices_pi_2,labels=lbls_pi_2,explode=0.2,
      main="Pie Chart of Vehicle Type ")

##################################################################################
##Road Types
##################################################################################

counts_pi_1 <- table(topcity_pi$RMA, topcity_pi$LGA_NAME)
counts_pi_1
csel_counts_pi_1 <- counts_pi_1[,c("MELBOURNE", "CASEY","DANDENONG","GEELONG","BRIMBANK")];
csel_counts_pi_1
rsel_counts_pi_1 <- csel_counts_pi_1[c("Arterial Highway","Arterial Other","Freeway","Local Road","Non Arterial"),];
rsel_counts_pi_1

barplot(rsel_counts_pi_1, main="Accident Distribution by road type",
        xlab="City_Names",ylab = "Number of Accidents", col = c("darkblue","red","green","yellow","orange"),
        legend = c("Arterial Highway", "Arterial Other", "Freeway","Local Road","Non Arterial"), 
        args.legend = list(title = "Roads", x = "topright", cex = .5),ylim = c(0, 4000))

##################################################################################
##Speed Analysis
##################################################################################
counts_pi_2 <- table(topcity_pi$SPEED_ZONE, topcity_pi$LGA_NAME);
counts_pi_2
csel_counts_pi_2 <- counts_pi_2[,c("MELBOURNE", "CASEY","DANDENONG","GEELONG","BRIMBANK")];
csel_counts_pi_2
rsel_counts_pi_2 <- csel_counts_pi_2[c("30km/hr","40 km/hr","50 km/hr","60 km/hr","70 km/hr",
                                       "75 km/hr","80 km/hr","90 km/hr","100 km/hr","110 km/hr"),];
rsel_counts_pi_2
barplot(rsel_counts_pi_2, main="Accident Distribution by Speed",
        xlab="City_Names",ylab = "Number of Accidents", 
        col = c("darkblue","red","green","yellow","orange","blue",
                "brown","violet","purple","grey"),
        legend = c("30km/hr","40 km/hr","50 km/hr","60 km/hr","70 km/hr",
                   "75 km/hr","80 km/hr","90 km/hr","100 km/hr","110 km/hr"), 
        args.legend = list(title = "Speed", x = "topright", cex = .4),ylim = c(0,4500))

##################################################################################
##Accident Type 
##################################################################################
counts_pi_3 <- table(topcity_pi$ACCIDENT_TYPE, topcity_pi$LGA_NAME);
counts_pi_3
csel_counts_pi_3 <- counts_pi_3[,c("MELBOURNE", "CASEY","DANDENONG","GEELONG","BRIMBANK")];
csel_counts_pi_3

barplot(csel_counts_pi_3, main="Accident Distribution by Accident Type",
        xlab="City_Names",ylab = "Number of Accidents", 
        col = c("darkblue","red","green","yellow","orange","blue",
                "brown","violet","grey"),
        legend = c("Collision with a fixed object","collision with some other object",
                   "Collision with vehicle","Fall from or in moving vehicle",
                   "No collision and no object struck",
                   "Other accident","Struck animal","Struck Pedestrian","Vehicle overturned"), 
        args.legend = list(title = "Accident Type", x = "topright", cex = .5),ylim = c(0, 5000))

##################################################################################
##Plotting number of accident distribution with respect to year in   
##top 5 cities requiring police involvement
##################################################################################
library(lubridate)
accident_date_pi <- data.frame(DateTime = topcity_pi$ACCIDENT_DATE)
View(accident_date_pi)
year_collection_pi <- c()
for (j in accident_date_pi)
{
  date_dmy_pi <- dmy(j)
  year_extract_pi <- as.numeric(year(date_dmy_pi))
  year_collection_pi <- as.data.frame(year_extract_pi)
}
View(year_collection_pi)
counts_acci_year_pi <- table(year_collection_pi)
View(counts_acci_year_pi)
barplot(sort(counts_acci_year_pi,decreasing=TRUE)[1:6],
        main="Distribution of accidents with respect to year", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(110, 90, 70, 50, 40, 30))
box()

##################################################################################
##Plotting number of accident distribution with respect to day of week  in   
##top 5 cities requiring police involvement
##################################################################################
library(lubridate)
accident_day_pi <- data.frame(DateTime = topcity_pi$DAY_OF_WEEK)
View(accident_day_pi)

counts_acci_day_pi <- table(accident_day_pi,exclude=NULL)
View(counts_acci_day_pi)
barplot(sort(counts_acci_day_pi,decreasing=TRUE)[1:7],
        main="Distribution of accidents with respect to weekday", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(110, 90, 70, 50, 40, 30, 10))
box()

##################################################################################
##Plotting number of accident distribution with respect to hour in   
##top 5 cities requiring police involvement
##################################################################################
library(lubridate)
accident_hour_pi <- data.frame(DateTime = topcity_pi$ACCIDENT_TIME)
View(accident_hour_pi)
hour_collection_pi <- c()
for (z in accident_hour_pi)
{
  time_hms_pi <- hms(z)
  hour_extract_pi <- as.numeric(hour(time_hms_pi))
  hour_collection_pi <- as.data.frame(hour_extract_pi)
}
View(hour_collection_pi)
counts_acci_hour_pi <- table(hour_collection_pi)
View(counts_acci_hour_pi)
barplot(sort(counts_acci_hour_pi,decreasing=TRUE)[1:10],
        main="Distribution of accidents with respect to hour", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(110, 100, 90, 70, 60, 50, 40, 30, 20, 10))
box()

##################################################################################
##Plotting trend of number of accident with respect to year in   
##top 5 cities requiring police involvement
##################################################################################
plot(counts_acci_year_pi,type = "o",col = "red",  xlab = "Year", ylab = "Number of Accidents",
     main = "Yearly number of accidents distribution")

