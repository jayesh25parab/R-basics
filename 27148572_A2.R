
##################################################################################
#Loading Data into R
##################################################################################
library(rmongodb)
myConnection = mongo.create();
ABC = mongo.find.all(myConnection, 'Assignment.Crashes');
myDataFrame = do.call(rbind.data.frame,ABC);
View(myDataFrame)


##################################################################################
##Viewing number of accidents by Accident Type
##################################################################################

library(ggplot2)
library(data.table)
ggplot(data = myDataFrame, aes(x = LIGHT_CONDITION,fill =LIGHT_CONDITION)) + 
geom_bar()+ coord_flip()
#qplot(factor(LIGHT_CONDITION), data=myDataFrame, geom="bar", 
#fill=factor(LIGHT_CONDITION))


##################################################################################
##Sub setting data according to light type equal to dark street light off
##################################################################################
x.sub <- subset(myDataFrame, LIGHT_CONDITION == "Dark Street lights off")
View(x.sub)


##################################################################################
##Classifying accident count according to suburb
##################################################################################

ggplot(data = x.sub, aes(x = LGA_NAME,fill = LGA_NAME)) + geom_bar() + coord_flip()


##################################################################################
##Finding top 5 suburbs 
##################################################################################
counts <- table(x.sub$LGA_NAME)
summary(x.sub$LGA_NAME)
write.table(counts,"C:/Users/JATESH/Desktop/Dataset/New folder/export_table.txt", row.names=FALSE) #for making .txt file
barplot(sort(counts,decreasing=TRUE)[1:5],
        main="Top 5 cities where accidents due to street lights off", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(90, 70, 50, 40, 30))
box()

##################################################################################
##Demographics of Injury type in top 5 cities
##################################################################################
topcity <-subset(x.sub, LGA_NAME == "CASEY" | LGA_NAME == "MELBOURNE" | LGA_NAME == "MONASH" | 
                   LGA_NAME == "MELTON" | LGA_NAME == "HUME")
View(topcity)
Slices <- c(sum(topcity$INJ_OR_FATAL),sum(topcity$FATALITY),sum(topcity$SERIOUSINJURY),
            sum(topcity$NONINJURED))
Slices
lbls <- c("Injured", "Fatality","Serious Injury", "Not Injured")
pct <- round(Slices/sum(Slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(Slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Injury Type")
library(plotrix)
pie3D(Slices,labels=lbls,explode=0.1,
      main="Pie Chart of Injury Type")

##################################################################################
##Demographics of people type involved in accidents of top 5 cities
##################################################################################
Slices1 <- c(sum(topcity$BICYCLIST),sum(topcity$PASSENGER),sum(topcity$DRIVER),
             sum(topcity$MOTORIST),sum(topcity$PEDESTRIAN))
Slices1
lbls1 <- c("Bicyclist", "Passenger","Driver", "Motorist", "Pedestrian")
pct1 <- round(Slices1/sum(Slices1)*100)
pct1
lbls1 <- paste(lbls1, pct1) # add percents to labels 
lbls1 <- paste(lbls1,"%",sep="") # ad % to labels 
pie(Slices1,labels = lbls1, col=rainbow(length(lbls)),
    main="Pie Chart of Person Type")
library(plotrix)
pie3D(Slices1,labels=lbls1,explode=0.1,
      main="Pie Chart of Person Type ")


##################################################################################
##Demographics of vehicle type involved in accidents of top 5 cities
##################################################################################
Slices2 <- c(sum(topcity$HEAVYVEHICLE),sum(topcity$PASSENGERVEHICLE),sum(topcity$MOTORCYCLE),
             sum(topcity$PUBLICVEHICLE))
Slices2
lbls2 <- c("Heavy Vehicle", "Passenger Vehicle","Motor Cycle", "Public Vehicle")
pct2 <- round(Slices2/sum(Slices2)*100)
pct2
lbls2 <- paste(lbls2, pct2) # add percents to labels 
lbls2 <- paste(lbls2,"%",sep="") # ad % to labels 
pie(Slices2,labels = lbls2, col=rainbow(length(lbls)),
    main="Pie Chart of Vehicle Type")
library(plotrix)
pie3D(Slices2,labels=lbls2,explode=0.2,
      main="Pie Chart of Vehicle Type ")
##################################################################################
##Alcohol Involvement
##################################################################################

ggplot(topcity, aes(LGA_NAME, ALCOHOL_RELATED)) + geom_jitter()+ facet_wrap(~ DAY_OF_WEEK) 
ggplot(topcity, aes(LGA_NAME, ALCOHOL_RELATED)) + geom_point()+ facet_wrap(~ DAY_OF_WEEK)


##################################################################################
##Road Types
##################################################################################

counts_1 <- table(topcity$RMA, topcity$LGA_NAME)
counts_1
csel_counts_1 <- counts_1[,c("CASEY", "MELBOURNE","MONASH","MELTON","HUME")];
csel_counts_1
rsel_counts_1 <- csel_counts_1[c("Arterial Highway","Arterial Other","Freeway","Local Road","Non Arterial"),];
rsel_counts_1

barplot(rsel_counts_1, main="Accident Distribution by road type",
        xlab="City_Names",ylab = "Number of Accidents", col = c("darkblue","red","green","yellow","orange"),
        legend = c("Arterial Highway", "Arterial Other", "Freeway","Local Road","Non Arterial"), 
        args.legend = list(title = "Roads", x = "topright", cex = .7),ylim = c(0, 18))


##################################################################################
##Speed Analysis
##################################################################################
counts_2 <- table(topcity$SPEED_ZONE, topcity$LGA_NAME);
counts_2
csel_counts_2 <- counts_2[,c("CASEY", "MELBOURNE","MONASH","MELTON","HUME")];
csel_counts_2
rsel_counts_2 <- csel_counts_2[c("30km/hr","40 km/hr","50 km/hr","60 km/hr","70 km/hr",
                     "75 km/hr","80 km/hr","90 km/hr","100 km/hr","110 km/hr"),];
rsel_counts_2
barplot(rsel_counts_2, main="Accident Distribution by Speed",
        xlab="City_Names",ylab = "Number of Accidents", 
        col = c("darkblue","red","green","yellow","orange","blue",
                "brown","violet","purple","grey"),
        legend = c("30km/hr","40 km/hr","50 km/hr","60 km/hr","70 km/hr",
                   "75 km/hr","80 km/hr","90 km/hr","100 km/hr","110 km/hr"), 
        args.legend = list(title = "Speed", x = "topright", cex = .5),ylim = c(0, 20))

##################################################################################
##Accident Type 
##################################################################################
counts_3 <- table(topcity$ACCIDENT_TYPE, topcity$LGA_NAME);
counts_3
csel_counts_3 <- counts_3[,c("CASEY", "MELBOURNE","MONASH","MELTON","HUME")];
csel_counts_3

barplot(csel_counts_3, main="Accident Distribution by Accident Type",
        xlab="City_Names",ylab = "Number of Accidents", 
        col = c("darkblue","red","green","yellow","orange","blue",
                "brown","violet","grey"),
        legend = c("Collision with a fixed object","collision with some other object",
                   "Collision with vehicle","Fall from or in moving vehicle",
                   "No collision and no object struck",
                   "Other accident","Struck animal","Struck Pedestrian","Vehicle overturned"), 
        args.legend = list(title = "Accident Type", x = "topright", cex = .5),ylim = c(0, 20))




############last###########









##################################################################################
##Sub setting data according to light type equal to dark street light off
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
counts_11 <- table(x.sub1$LGA_NAME)
summary(x.sub1$LGA_NAME)
write.table(counts_11,"C:/Users/JATESH/Desktop/Dataset/New folder/export_table_1.txt", row.names=FALSE) #for making .txt file
barplot(sort(counts_11,decreasing=TRUE)[1:5],
        main="Top 5 cities where accidents due to street lights off", col="blue", xlab="City", 
        ylab="Number of Accidents",
        border="red", density=c(90, 70, 50, 40, 30))
box()

##################################################################################
##Demographics of Injury type in top 5 cities
##################################################################################
topcity1 <-subset(x.sub1, LGA_NAME == "YARRA RANGES" | LGA_NAME == "CASEY" | LGA_NAME == "GEELONG" | 
                   LGA_NAME == "CARDINIA" | LGA_NAME == "MITCHELL")
View(topcity1)
Slices_C2 <- c(sum(topcity1$INJ_OR_FATAL),sum(topcity1$FATALITY),sum(topcity1$SERIOUSINJURY),
            sum(topcity1$NONINJURED))
Slices_C2
lbls_C2 <- c("Injured", "Fatality","Serious Injury", "Not Injured")
pct_C2 <- round(Slices_C2/sum(Slices_C2)*100)
lbls_C2 <- paste(lbls_C2, pct_C2) # add percents to labels 
lbls_C2 <- paste(lbls_C2,"%",sep="") # ad % to labels 
pie(Slices_C2,labels = lbls_C2, col=rainbow(length(lbls_C2)),
    main="Pie Chart of Injury Type")
library(plotrix)
pie3D(Slices_C2,labels=lbls_C2,explode=0.1,
      main="Pie Chart of Injury Type")

##################################################################################
##Demographics of people type involved in accidents of top 5 cities
##################################################################################
Slices_C21 <- c(sum(topcity1$BICYCLIST),sum(topcity1$PASSENGER),sum(topcity1$DRIVER),
             sum(topcity1$MOTORIST),sum(topcity1$PEDESTRIAN))
Slices_C21
lbls_C21 <- c("Bicyclist", "Passenger","Driver", "Motorist", "Pedestrian")
pct_C21 <- round(Slices_C21/sum(Slices_C21)*100)
pct_C21
lbls_C21 <- paste(lbls_C21, pct_C21) # add percents to labels 
lbls_C21 <- paste(lbls_C21,"%",sep="") # ad % to labels 
pie(Slices_C21,labels = lbls_C21, col=rainbow(length(lbls_C21)),
    main="Pie Chart of Person Type")
library(plotrix)
pie3D(Slices_C21,labels=lbls_C21,explode=0.1,
      main="Pie Chart of Person Type ")


##################################################################################
##Demographics of vehicle type involved in accidents of top 5 cities
##################################################################################
Slices_C21 <- c(sum(topcity1$HEAVYVEHICLE),sum(topcity1$PASSENGERVEHICLE),sum(topcity1$MOTORCYCLE),
             sum(topcity1$PUBLICVEHICLE))
Slices_C21
lbls_C22 <- c("Heavy Vehicle", "Passenger Vehicle","Motor Cycle", "Public Vehicle")
pct_C22 <- round(Slices_C21/sum(Slices_C21)*100)
pct_C22
lbls_C22 <- paste(lbls_C22, pct_C22) # add percents to labels 
lbls_C22 <- paste(lbls_C22,"%",sep="") # ad % to labels 
pie(Slices_C21,labels = lbls_C22, col=rainbow(length(lbls_C22)),
    main="Pie Chart of Vehicle Type")
library(plotrix)
pie3D(Slices_C21,labels=lbls_C22,explode=0.2,
      main="Pie Chart of Vehicle Type ")
##################################################################################
##Alcohol Involvement
##################################################################################

ggplot(topcity1, aes(LGA_NAME, ALCOHOL_RELATED)) + geom_jitter()+ facet_wrap(~ DAY_OF_WEEK) 
ggplot(topcity1, aes(LGA_NAME, ALCOHOL_RELATED)) + geom_point()+ facet_wrap(~ DAY_OF_WEEK)


##################################################################################
##Road Types
##################################################################################

counts_11 <- table(topcity1$RMA, topcity1$LGA_NAME)
counts_11
csel_counts_11 <- counts_11[,c("CARDINIA", "YARRA RANGES","GEELONG","MITCHELL","CASEY")];
csel_counts_11
rsel_counts_11 <- csel_counts_11[c("Arterial Highway","Arterial Other","Freeway",
                                 "Local Road","Non Arterial"),];
rsel_counts_11

barplot(rsel_counts_11, main="Accident Distribution by road type",
        xlab="City_Names",ylab = "Number of Accidents", col = c("darkblue","red","green","yellow","orange"),
        legend = c("Arterial Highway", "Arterial Other", "Freeway","Local Road","Non Arterial"), 
        args.legend = list(title = "Roads", x = "topright", cex = .7),ylim = c(0, 400))


##################################################################################
##Speed Analysis
##################################################################################
counts_22 <- table(topcity1$SPEED_ZONE, topcity1$LGA_NAME);
counts_22
csel_counts_22 <- counts_22[,c("CARDINIA", "YARRA RANGES","GEELONG","MITCHELL","CASEY")];
csel_counts_22
rsel_counts_22 <- csel_counts_22[c("30km/hr","40 km/hr","50 km/hr","60 km/hr","70 km/hr",
                                 "75 km/hr","80 km/hr","90 km/hr","100 km/hr","110 km/hr"),];
rsel_counts_22
barplot(rsel_counts_22, main="Accident Distribution by Speed",
        xlab="City_Names",ylab = "Number of Accidents", 
        col = c("darkblue","red","green","yellow","orange","blue",
                "brown","violet","purple","grey"),
        legend = c("30km/hr","40 km/hr","50 km/hr","60 km/hr","70 km/hr",
                   "75 km/hr","80 km/hr","90 km/hr","100 km/hr","110 km/hr"), 
        args.legend = list(title = "Speed", x = "topright", cex = .5),ylim = c(0, 450))

##################################################################################
##Accident Type
##################################################################################
counts_33 <- table(topcity1$ACCIDENT_TYPE, topcity1$LGA_NAME);
counts_33
csel_counts_33 <- counts_33[,c("CARDINIA", "YARRA RANGES","CASEY","GEELONG","MITCHELL")];
csel_counts_33

barplot(csel_counts_33, main="Accident Distribution by Accident Type",
        xlab="City_Names",ylab = "Number of Accidents", 
        col = c("darkblue","red","green","yellow","orange","blue",
                "brown","violet","grey"),
        legend = c("Collision with a fixed object","collision with some other object",
                   "Collision with vehicle","Fall from or in moving vehicle",
                   "No collision and no object struck",
                   "Other accident","Struck animal","Struck Pedestrian","Vehicle overturned"), 
        args.legend = list(title = "Accident Type", x = "topright", cex = .5),ylim = c(0, 400))