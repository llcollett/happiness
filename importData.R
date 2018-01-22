#packages
library(tidyverse)

#working directory
setwd("C:/Users/LauraAcer/Documents/Data Science/Happiness/")

#initial dataset
daylio<-read_csv("daylio_20180116.csv")
daylio<-subset(daylio,daylio$year=="2017")
daylio$date<-paste(daylio$date,daylio$year,sep=" ")
daylio$date<-as.Date(daylio$date,"%d %B %Y")

#convert mood to number, where 5 is great and 1 is awful
daylio$mood<-as.integer(factor(daylio$mood, 
             levels=c("awful","rubbish","mediocre","good","great"),
             ordered=TRUE))
#convert weekdays to numbers
daylio$weekday<-as.integer(factor(daylio$weekday,
                levels=c("Monday","Tuesday","Wednesday","Thursday",
                         "Friday","Saturday","Sunday"),
                ordered=FALSE))

#convert activities variable into multiple indicator variables
daylio$sophie<-ifelse(grepl("sophie",daylio$activities),1,0)
daylio$friends<-ifelse(grepl("friends",daylio$activities),1,0)
daylio$family<-ifelse(grepl("family",daylio$activities),1,0)
daylio$stats<-ifelse(grepl("stats",daylio$activities),1,0)
daylio$work<-ifelse(grepl("work",daylio$activities),1,0)
daylio$outside<-ifelse(grepl("outside",daylio$activities),1,0)
daylio$adventure<-ifelse(grepl("adventure",daylio$activities),1,0)
daylio$gardening<-ifelse(grepl("gardening",daylio$activities),1,0)
daylio$camping<-ifelse(grepl("camping",daylio$activities),1,0)
daylio$hiking<-ifelse(grepl("hiking",daylio$activities),1,0)
daylio$swimming<-ifelse(grepl("swimming",daylio$activities),1,0)
daylio$walking<-ifelse(grepl("walking",daylio$activities),1,0)
daylio$yoga<-ifelse(grepl("yoga",daylio$activities),1,0)
daylio$climbing<-ifelse(grepl("climbing",daylio$activities),1,0)
daylio$running<-ifelse(grepl("running",daylio$activities),1,0)
daylio$cycling<-ifelse(grepl("cycling",daylio$activities),1,0)
daylio$lifting<-ifelse(grepl("lifting",daylio$activities),1,0)
daylio$softball<-ifelse(grepl("softball",daylio$activities),1,0)
daylio$lifeadmin<-ifelse(grepl("life admin",daylio$activities),1,0)
daylio$podcasting<-ifelse(grepl("podcasting",daylio$activities),1,0)
daylio$reading<-ifelse(grepl("reading",daylio$activities),1,0)
daylio$crafts<-ifelse(grepl("crafts",daylio$activities),1,0)
daylio$games<-ifelse(grepl("games",daylio$activities),1,0)
daylio$music<-ifelse(grepl("music",daylio$activities),1,0)
daylio$television<-ifelse(grepl("television",daylio$activities),1,0)
daylio$cooking<-ifelse(grepl("cooking",daylio$activities),1,0)
daylio$driving<-ifelse(grepl("driving",daylio$activities),1,0)
daylio$activities<-NULL


#label data
daylio$moodbin<-cut(daylio$mood,breaks=c(0,3,Inf),
                    labels=c("negative","positive"))
daylio$moodgreat<-cut(daylio$mood,breaks=c(0,4,Inf),
                      labels=c("not great","great"))
daylio$mood<-factor(daylio$mood,levels=c(1,2,3,4,5),
                    labels=c("awful","rubbish","mediocre",
                             "good","great"))
daylio$weekday<-factor(daylio$weekday,levels=c(1,2,3,4,5,6,7),
                       labels=c("monday","tuesday","wednesday",
                                "thursday","friday","saturday",
                                "sunday"))
daylio$sophie<-factor(daylio$sophie,levels=c(0,1),
                      labels=c("no","yes"))
daylio$friends<-factor(daylio$friends,levels=c(0,1),
                       labels=c("no","yes"))
daylio$family<-factor(daylio$family,levels=c(0,1),
                      labels=c("no","yes"))
daylio$stats<-factor(daylio$stats,levels=c(0,1),
                     labels=c("no","yes"))
daylio$work<-factor(daylio$work,levels=c(0,1),
                     labels=c("no","yes"))
daylio$outside<-factor(daylio$outside,levels=c(0,1),
                       labels=c("no","yes"))
daylio$adventure<-factor(daylio$adventure,levels=c(0,1),
                         labels=c("no","yes"))
daylio$gardening<-factor(daylio$gardening,levels=c(0,1),
                         labels=c("no","yes"))
daylio$camping<-factor(daylio$camping,levels=c(0,1),
                       labels=c("no","yes"))
daylio$hiking<-factor(daylio$hiking,levels=c(0,1),
                      labels=c("no","yes"))
daylio$swimming<-factor(daylio$swimming,levels=c(0,1),
                        labels=c("no","yes"))
daylio$walking<-factor(daylio$walking,levels=c(0,1),
                       labels=c("no","yes"))
daylio$yoga<-factor(daylio$yoga,levels=c(0,1),
                    labels=c("no","yes"))
daylio$climbing<-factor(daylio$climbing,levels=c(0,1),
                        labels=c("no","yes"))
daylio$running<-factor(daylio$running,levels=c(0,1),
                       labels=c("no","yes"))
daylio$cycling<-factor(daylio$cycling,levels=c(0,1),
                       labels=c("no","yes"))
daylio$lifting<-factor(daylio$lifting,levels=c(0,1),
                       labels=c("no","yes"))
daylio$softball<-factor(daylio$softball,levels=c(0,1),
                        labels=c("no","yes"))
daylio$lifeadmin<-factor(daylio$lifeadmin,levels=c(0,1),
                         labels=c("no","yes"))
daylio$podcasting<-factor(daylio$podcasting,levels=c(0,1),
                          labels=c("no","yes"))
daylio$reading<-factor(daylio$reading,levels=c(0,1),
                       labels=c("no","yes"))
daylio$crafts<-factor(daylio$crafts,levels=c(0,1),
                      labels=c("no","yes"))
daylio$games<-factor(daylio$games,levels=c(0,1),
                     labels=c("no","yes"))
daylio$music<-factor(daylio$music,levels=c(0,1),
                     labels=c("no","yes"))
daylio$television<-factor(daylio$television,levels=c(0,1),
                          labels=c("no","yes"))
daylio$cooking<-factor(daylio$cooking,levels=c(0,1),
                       labels=c("no","yes"))
daylio$driving<-factor(daylio$driving,levels=c(0,1),
                       labels=c("no","yes"))
#final dataframe
daylio<-daylio[c("date","weekday","mood","moodbin","moodgreat",
                 "sophie","friends","family","stats","work",
                 "outside","adventure","gardening","camping",
                 "hiking","swimming","walking","yoga","climbing",
                 "running","cycling","lifting","softball",
                 "lifeadmin","podcasting","reading","crafts",
                 "games","music","television","cooking","driving",
                 "note")]
daylio<-as.data.frame(daylio)

#save dataframe
save(daylio,file="daylio.Rda")
