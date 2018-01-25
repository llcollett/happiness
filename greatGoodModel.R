#packages
library(tidyverse)
library(dplyr)
library(GGally)
library(broom)

#working directory
#setwd("C:/Users/LauraAcer/Documents/Data Science/Happiness/")
setwd("O:/Documents/Personal/Projects/Data Science/Drive/Happiness")

#load data
load("daylio.Rda")

#describe data
ggplot(data=daylio,aes(mood))+
  geom_bar(aes(y=(..count..)/sum(..count..)),fill=rgb(0,0.69,0.314))+
  labs(title="Distribution of mood",
       x="Mood",y="Proportion of total days")
ggsave("moods.png")
#i generally have okay days
ggplot(data=daylio,aes(weekday,fill=mood))+
  geom_bar(aes(y=(..count..)/sum(..count..)),
           position=position_stack(reverse=TRUE))+
  scale_fill_brewer(palette="Greens")+
  theme(axis.text.x=element_text(size=8))+
  labs(title="Distribution of mood over the week",
       x="Weekday",y="Proportion of total days",
       fill="Mood")
ggsave("days.png")

#to look at relationship between each predictor and outcome
table(daylio$sophie,daylio$moodbin)
table(daylio$friends,daylio$moodbin)
table(daylio$family,daylio$moodbin)
table(daylio$stats,daylio$moodbin)
table(daylio$work,daylio$moodbin)
table(daylio$outside,daylio$moodbin)
table(daylio$adventure,daylio$moodbin)
table(daylio$gardening,daylio$moodbin)#cannot include in moodbin
table(daylio$camping,daylio$moodbin)#cannot include in moodbin
table(daylio$hiking,daylio$moodbin)#possibly cannot include in moodbin
table(daylio$swimming,daylio$moodbin)
table(daylio$walking,daylio$moodbin)#possibly cannot include in moodbin
table(daylio$yoga,daylio$moodbin)
table(daylio$climbing,daylio$moodbin)
table(daylio$running,daylio$moodbin)
table(daylio$cycling,daylio$moodbin)
table(daylio$lifting,daylio$moodbin)
table(daylio$softball,daylio$moodbin)
table(daylio$lifeadmin,daylio$moodbin)
table(daylio$podcasting,daylio$moodbin)
table(daylio$reading,daylio$moodbin)
table(daylio$crafts,daylio$moodbin)#might be okay to include in moodbin
table(daylio$games,daylio$moodbin)
table(daylio$music,daylio$moodbin)
table(daylio$television,daylio$moodbin)
table(daylio$cooking,daylio$moodbin)#possibly cannot include in moodbin
table(daylio$driving,daylio$moodbin)#possibly cannot include in moodbin

#multiple variable correlation plot
f_lower<-function(data,mapping,...,low="white",high="steelblue") {
  ggplot(data=data,mapping=mapping)+
    geom_bin2d(...)+
    scale_fill_gradient(low=low,high=high)
}
f_diag<-function(data,mapping){
  ggplot(data=data,mapping=mapping)+
    geom_bar(aes(y=((..count..)/sum(..count..)),fill=daylio$mood))+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_brewer(palette="Greens")+
    ylab("Relative frequencies")
}
f_upper<-function(data,mapping){
  ggplot(data=data,mapping=mapping)+
    geom_jitter()
}
#plot to see if outside, adventure, camping and hiking might
#be collinear
ggpairs(daylio[, c("outside","adventure","camping","hiking")],
        lower=list(discrete=f_lower),upper=list(discrete=f_upper),
        diag=list(discrete=f_diag))
ggsave("panel.png")
#adventure very general term encompassing camping and hiking
#cannot include camping in pos/neg model so see if hiking
#is a subset of adventure
ggplot(data=daylio,aes(x=adventure,y=hiking))+
  geom_bin2d()+
  stat_bin2d(geom="text",aes(label=(..count..),size=2,fontface="bold"))+
  scale_fill_gradient(low="white",high="steelblue")

#hiking is mainly a subset of adventure so create hikeless adventure
daylio$hikelessadventure<-ifelse(daylio$hiking=="no" & 
                                   daylio$adventure=="yes",
                                 1,0)
daylio$hikelessadventure<-factor(daylio$hikelessadventure,
                                 levels=c(0,1),
                                 labels=c("no","yes"))

#see whether work and stats collinear, though i do not think so...
ggplot(data=daylio,aes(x=work,y=stats))+
  geom_bin2d()+
  stat_bin2d(geom="text",aes(label=(..count..),size=2,fontface="bold"))+
  scale_fill_gradient(low="white",high="steelblue")
#plenty of yes/no, no/yes combinations so fine.

#logistic regression
#first do not include those established cannot include above
#include hikelessadventure
model1<-glm(moodbin~weekday+sophie+friends+family+stats+work+
                    outside+hikelessadventure+hiking+swimming+
                    climbing+running+cycling+lifting+softball+
                    lifeadmin+podcasting+reading+crafts+games+
                    music+television+driving,
            family=binomial(link='logit'),data=daylio)
summary(model1)
ggcoef(model1,exponentiate=TRUE,exclude_intercept=TRUE,
       mapping=aes(x=estimate,y=term))
#taking out those collinear terms makes the model perform well

#see whether any days stick out as being more effect than others
#possibly might want a weekend term
weekdaym1<-glm(moodbin~weekday,family=binomial(link='logit'),
               data=daylio)
summary(weekdaym1)
#negative coefficients for weekdays and positive coefficients
#for weekend so dichotomise for weekend or not
daylio$weekend<-ifelse(daylio$weekday=="saturday" | 
                       daylio$weekday=="sunday",1,0)
daylio$weekend<-factor(daylio$weekend,levels=c(0,1),
                       labels=c("weekday","weekend"))
#check in a model
weekendm1<-glm(moodbin~weekend,family=binomial(link='logit'),
               data=daylio)
summary(weekendm1)
#include in main model
model2<-glm(moodbin~weekend+sophie+friends+family+stats+work+
                    outside+hikelessadventure+hiking+swimming+
                    climbing+running+cycling+lifting+softball+
                    lifeadmin+podcasting+reading+crafts+games+
                    music+television+driving,
            family=binomial(link='logit'),data=daylio)
summary(model2)
#remove those with highest p-value one by one
#keep those with p-values<=0.1
model3<-glm(moodbin~sophie+friends+stats+
              outside+swimming+
              climbing,
            family=binomial(link='logit'),data=daylio)
summary(model3)
ggcoef(model3,exponentiate=TRUE,exclude_intercept=TRUE,
       errorbar_size=1,errorbar_color=rgb(0,0.69,0.314),
       errorbar_height=0.5,color=rgb(0,0.69,0.314),
       conf.level=0.9,
       mapping=aes(x=estimate,y=term,size=p.value))+
  scale_size_continuous(trans="reverse")+
  scale_y_discrete(labels=
                     c("Climbing","Friends","Outside",
                       "Sophie","Stats","Swimming"))+
  labs(title="Model to see the effect of activities on Happiness",
       subtitle="Great/Good vs. Mediocre/Bad",
       x="Estimate of the Odds Ratio",y="",size="P-Value",
       caption="(Error bars are based on 90% Confidence Intervals)")
ggsave("happiness_moodbinplot.png")
