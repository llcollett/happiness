
#packages
library(tidyverse)
library(dplyr)
library(GGally)
library(broom)

#working directory
setwd("C:/Users/LauraAcer/Documents/Data Science/Happiness/")

#load data
load("daylio.Rda")

#think about which variables can be included. i.e. whether some
#empty cells or whether not enough yeses
table(daylio$sophie,daylio$moodgreat)
table(daylio$friends,daylio$moodgreat)
table(daylio$family,daylio$moodgreat)
table(daylio$stats,daylio$moodgreat)
table(daylio$work,daylio$moodgreat)
table(daylio$outside,daylio$moodgreat)
table(daylio$adventure,daylio$moodgreat)
table(daylio$gardening,daylio$moodgreat)#cannot include in moodgreat
table(daylio$camping,daylio$moodgreat)
table(daylio$hiking,daylio$moodgreat)
table(daylio$swimming,daylio$moodgreat)
table(daylio$walking,daylio$moodgreat)
table(daylio$yoga,daylio$moodgreat)
table(daylio$climbing,daylio$moodgreat)
table(daylio$running,daylio$moodgreat)
table(daylio$cycling,daylio$moodgreat)
table(daylio$lifting,daylio$moodgreat)
table(daylio$softball,daylio$moodgreat)
table(daylio$lifeadmin,daylio$moodgreat)
table(daylio$podcasting,daylio$moodgreat)
table(daylio$reading,daylio$moodgreat)
table(daylio$crafts,daylio$moodgreat)
table(daylio$games,daylio$moodgreat)
table(daylio$music,daylio$moodgreat)
table(daylio$television,daylio$moodgreat)
table(daylio$cooking,daylio$moodgreat)
table(daylio$driving,daylio$moodgreat)

#hiking is mainly a subset of adventure so create hikeless adventure
daylio$hikelessadventure<-ifelse(daylio$hiking=="no" & 
                                   daylio$adventure=="yes",
                                 1,0)
daylio$hikelessadventure<-factor(daylio$hikelessadventure,
                                 levels=c(0,1),
                                 labels=c("no","yes"))

#logistic regression
#first do not include those established cannot include above
gmodel1<-glm(moodgreat~weekday+sophie+friends+family+stats+work+
              outside+adventure+camping+hiking+swimming+
              walking+yoga+climbing+running+cycling+lifting+
              softball+lifeadmin+podcasting+reading+crafts+
              games+music+television+cooking+driving,
            family=binomial(link='logit'),data=daylio)
summary(gmodel1)
ggcoef(gmodel1,exponentiate=TRUE,exclude_intercept=TRUE,
       mapping=aes(x=estimate,y=term))
#taking out those collinear terms makes the model perform well

#see whether any days stick out as being more effect than others
#possibly might want a weekend term
gweekdaym1<-glm(moodgreat~weekday,family=binomial(link='logit'),
               data=daylio)
summary(gweekdaym1)
#mostly negative coefficients for weekdays and statistically
#significant positive coefficients for weekend so dichotomise 
#for weekend or not
daylio$weekend<-ifelse(daylio$weekday=="Saturday" | 
                         daylio$weekday=="Sunday",1,0)
daylio$weekend<-factor(daylio$weekend,levels=c(0,1),
                       labels=c("weekday","weekend"))
#check in a model
gweekendm1<-glm(moodgreat~weekend,family=binomial(link='logit'),
               data=daylio)
summary(gweekendm1)
ggcoef(gweekendm1,exponentiate=TRUE,exclude_intercept=TRUE,
       mapping=aes(x=estimate,y=term))
#include in main model
gmodel2<-glm(moodgreat~weekend+sophie+friends+family+stats+work+
              outside+hikelessadventure+hiking+swimming+
              climbing+running+cycling+lifting+softball+
              lifeadmin+podcasting+reading+crafts+games+
              music+television+driving,
            family=binomial(link='logit'),data=daylio)
summary(gmodel2)
#remove those with highest p-value one by one
#keep those with p-values<=0.1
gmodel3<-glm(moodgreat~sophie+friends+work+
              outside+hikelessadventure+hiking+swimming+
              climbing+reading+driving,
            family=binomial(link='logit'),data=daylio)
summary(gmodel3)
ggcoef(gmodel3,exponentiate=TRUE,exclude_intercept=TRUE,
       errorbar_size=1,errorbar_color="chartreuse4",
       errorbar_height=0.5,color="chartreuse4",
       conf.level=0.9,
       mapping=aes(x=estimate,y=term,size=p.value))+
  scale_size_continuous(trans="reverse")+
  scale_y_discrete(labels=
                     c("Climbing","Driving","Friends",
                       "Hike-less Adventure","Hiking",
                       "Outside","Reading","Sophie",
                       "Swimming","Work"))+
  labs(title="Model to see the effect of activities on Happiness",
       subtitle="Great vs. Good/Mediocre/Bad",
       x="Estimate",y="",size="P-value",
       caption="(Error bars are based on 90% Confidence intervals)")
ggsave("happiness_moodgreatplot.png")
