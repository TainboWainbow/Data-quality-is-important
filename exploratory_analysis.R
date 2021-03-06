#Making sure that we are in the right working directory(adjust the code to your environment!)
setwd("C:/Users/User/Desktop/MY_DIRECTORY")
getwd()
dat=read.csv("Data.txt",header=TRUE,sep=",")

#first I check the structure of the data
head(dat)
str(dat)

##########################CLEANING DATA###########################################################
str(dat) #the id's shouldn't be considered as numbers
library(dplyr)
dat<-mutate_at(dat,vars(order_id:user_id),funs(as.character))
library(tidyr)
dat<-separate(dat,created_at,c("date","hour"),sep=" ")
library(lubridate)
dat$date<-mdy(dat$date)
dat$hour<-hm(dat$hour) 
str(dat) #now all the id's are in character format and created_at in POSIXct format (date and time)

#now we look for NA values
sum(is.na(dat))
summary(dat)  #there is no NA values, great! (...or this is where we doubt a little and say, "It's clean. Too....clean") 

#Now the data is ready for analysis
#There were no instructions given, so I will do some exploratory analysis with a question in mind
######################EXPLORATORY ANALYSIS#########################################################

colnames(dat) #getting to know the variables!

#WE WANT TO ANSWER THE QUESTION:
  #When do customers buy the most? (time of the day regardless of the amount spent)
table(dat$total_items)
par(mfrow=c(1,1))
plot(dat$hour) 

## INTREPRETATION of graph ##
#If there are distinct congregates in the graph, it means most users purchase at specific times.
#However, if plots are uniformly distributed in the graph, then it means users consistently buys
#at every hour.

## OBSERVATION of graph 1 ##
#In general, there is no distrint groups on the graph. It seems like users buy items day and night
#and midnight.Would the graphs be different with different numbers of items purchased?
#We try this:

par(mfrow=c(3,3))
for (i in c(1,2,3,4,5,6,8,2000)){
  temp<-dat[(dat$total_items==i),]
  plot((temp$hour),xlab=i)
}

## OBSERVATION of graph 2 ##
#Again, we see no distint congregates- except the graphs for 5,8,2000 items which can be
#explained by the (naturally) lack of data of high-number-purchasers and that 2000 items
#are bought at a consistently precise time, probably used by a machine to make purchases.
#Is it possible that the problem is in the method of data collection?
#We explore the idea by checking the hour and date columns:

table((dat$date)) 
#The data was collected in a period of month and seems like similar numbers of data were collected per day.
#We finally have a look at the plots by day:

par(mfrow=c(3,3))
for (i in 1:30){
  temp<-dat[(dat$date==ymd(paste("2017-03-",as.character(i),sep=""))),]
  plot((temp$hour),xlab=paste("Day",i))
  if (i%%9==0){par(mfrow=c(3,3))}
}

## OBSERVATION of graph 3 ##
#Again, we see no distinct plots in groups. They are all uniformly distributed all throughtout the days from 
#March 1-March 30. We can safely assume that the quality of the data is not good or that there is a problem 
#in the collection of data.

#At this point, it may not be worth doing analysis until the quality of data is ensured
#(we don't want draw false conclusions).


################################ SUMMARY ########################################
#we make the following hypotheses:

##It seems that for every hour and minute there is a customer making purchases. 
#this could mean either
  #1. the platform is booming with customers at all times. There must be a reason people 
      #buy at times when most people are sleeping (maybe, waiting for re-stock because of 
      #quick sell of the item?). It's a huge success! What's the secret sauce!!?!
  #2. the location of purchases made was not limited to one country when collecting data and the
      #recorded time is set to a specific time zone therefore the density of time of purchase 
      #is uniform.
  #3. the data obtained to analyze is biased. It was not collected randomly, meaning the data 
      #does not well-represent customer behavior). Further investigation on the method of 
      #data collection can be done.
  #4. the data is man-made just for the sake of! And therefore, lack of randomization :)
      #This can be improved, though, by simluation, if there is knowlegdge on the type of 
      #distributions and other known statistical information about customer bahavior!
