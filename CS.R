library("here")
library("skimr")
library("janitor")
library("dplyr")
library("tidyverse")
setwd("/Users/deyah/Desktop/Case Study/New Data")
daily_activity<-read.csv("dailyActivity_merged.csv")
daily_step<-read.csv("dailySteps_merged.csv")
daily_sleep<-read.csv("sleepDay_merged.csv")
daily_weight<-read.csv("weightLogInfo_merged.csv")
View(daily_activity)
View(daily_step)
View(daily_sleep)
View(daily_weight)
glimpse(daily_activity)
glimpse(daily_step)
glimpse(daily_sleep)
glimpse(daily_weight)
glimpse(daily_activity)
daily_activity <- daily_activity %>% mutate( Weekday = weekdays(as.Date(ActivityDate, "%m/%d/%Y")))
ggplot(data=daily_activity, aes(x=Weekday,fill=Weekday))+
  geom_bar()+labs(title = 'Recorded steps each day of the week')

daily_sleep <- daily_sleep %>% mutate( Weekday = weekdays(as.Date(date, "%m/%d/%Y")))
ggplot(data=daily_sleep, aes(x=Weekday,fill=Weekday))+
  geom_bar()+labs(title = 'Sleep Records each day of the week')
sleep_activity<-read.csv("Sleep_Activity.csv")
View(sleep_activity)
sleep_activity <- sleep_activity %>% mutate( Weekday = weekdays(as.Date(ActivityDate, "%d/%m/%Y")))
ggplot(data=sleep_activity)+geom_smooth(mapping=aes(x=TotalSteps,y=TotalMinutesAsleep,color=Weekday))+facet_wrap(~Weekday)+
  labs(title='Total Steps vs Total minutes sleep in each day of the week')
sleep_activity %>% summary()
daily_activity %>% summary()

percentage <- data.frame(
  level=c("Sedentary", "Lightly", "Fairly", "Very Active"),
  minutes=c(80.93,15.8,1.11,1.84)
)
percentage<- percentage %>% 
  arrange(desc(level)) %>%
  mutate(prop = minutes / sum(percentage$minutes) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(percentage, aes(x = "", y = minutes, fill = level)) +
  geom_bar(stat = "identity", width = 1) +
  labs(title="State of Minutes active")+
  coord_polar(theta = "y") +
  theme_void()+
  scale_fill_brewer(palette="Set3")



ggplot(data=daily_activity,aes(x=TotalSteps,y=Calories))+geom_point()+stat_smooth(method=lm) +
  labs(title="Total Steps taken Vs Total Calorie Burnt")
ggplot(data=daily_sleep,aes(x=TotalMinutesAsleep,y=TotalTimeInBed))+geom_point()+stat_smooth(method=lm)+
  labs(title="Total Minutes asleep Vs Total Time in bed")

daily_activity <- daily_activity %>% mutate( TotalMinutes = VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes+SedentaryMinutes)
hourly_step<-read.csv("hourlySteps_merged.csv")
View(hourly_step)
hourly_step$ActivityDatetime <- as.POSIXct(hourly_step$ActivityHour, format="%m/%d/%Y %I:%M:%S %p")
hourly_step$Hour <- format(hourly_step$ActivityDatetime, format = "%H")

ggplot(data=hourly_step, aes(x=Hour, y=StepTotal, fill=Hour))+
  geom_bar(stat="identity")+
  labs(title="Hourly Steps")
