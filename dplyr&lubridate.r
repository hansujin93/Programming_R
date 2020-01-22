library(ggplot2)
library(lubridate)
library(dplyr)
storm <- read.csv("Australia_severe_storms_1975-2015.csv",stringsAsFactors = FALSE)
dim(storm)
storm <- storm[, 1:(length(storm)-6)]
storm <- filter(storm, Database != "Waterspout")
head(storm)
dim(storm)
-----------------------------------------------------------------------------------------------------------------------------
tz_function<-function(State, Nearest.town){
  if (State=="QLD") { 
      add_tz<-"Australia/Queensland"
      } else if (State=="VIC") {
      add_tz<-"Australia/Victoria"
      } else if  (State=="SA") {
      add_tz<-"Australia/South"
      } else if  (State=="WA") {
      add_tz<-"Australia/West"
      } else if  (State=="TAS") {
      add_tz<-"Australia/Tasmania"
      } else if  (State=="NT") {
      add_tz<-"Australia/North"
      } else if  (State=="ACT") {
      add_tz<-"Australia/ACT"
      } else if  ((State=="NSW") & (grepl('broken hill', tolower(Nearest.town))==TRUE)) {
      add_tz<-"Australia/Broken_Hill"
      } else {
      add_tz<-"Australia/NSW"
  }
  return(add_tz)
}
storm<-mutate(storm, new_tz=NA)
for (i in 1:nrow(storm)) {
  storm$new_tz[i]<-tz_function(storm$State[i], storm$Nearest.town[i])
}
-----------------------------------------------------------------------------------------------------------------------------
storm<-group_by(storm, new_tz)
storm <- mutate(storm, UTC_time=with_tz(dmy_hm(Date.Time, tz=new_tz[1]), "UTC"))
head(storm)
storm <- mutate(storm, mm=month(dmy_hm(Date.Time))) %>%
  mutate(yy=year(dmy_hm(Date.Time)))
head(storm)
-----------------------------------------------------------------------------------------------------------------------------
storm_new <- storm %>% 
  group_by(Database, mm) %>%
  summarise(count_of_events=n())
ggplot(storm_new)+
  geom_line(aes(x=mm, y=count_of_events, colour=Database, linetype=Database))+
  scale_x_continuous(breaks=seq(1:12))+
  xlab("month") +
  ylab("count of events")
