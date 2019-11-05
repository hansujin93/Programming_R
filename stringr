library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
storm<-read.csv("Australia_severe_storms_1975-2015.csv")
storm$All.comments<-str_c(storm$Comments, 
                          storm$X, storm$X.1, storm$X.2, 
                          storm$X.3, storm$X.4)
storm<-select(storm, Event.ID, Database,
              Date.Time, State, All.comments)
print(sapply(storm, class))
-----------------------------------------------------------------------------------------------------------------------------
expr<-"[fF]lash flood(\\w+)?"
storm$flash_flood_TF<-str_detect(storm$All.comments, expr)

storm$Date.Time = dmy_hm(storm$Date.Time)
v<-as.data.frame(table(yy = format(storm$Date.Time, "%Y"), 
                       tf = storm$flash_flood_TF)) %>%
  filter(tf==TRUE)
ggplot(v, aes(yy, weight=Freq)) + 
  geom_bar() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  xlab("Year") +
  ylab("number of flash floods")
-----------------------------------------------------------------------------------------------------------------------------
storm<-mutate(storm, wind_speed=str_extract(storm$All.comments,
                                            "([0-9]{1,3})\\s*(knots|km/h|kts|kt)"))
for (i in 1:nrow(storm)){
  if ((is.na(storm$wind_speed[i])==FALSE)
      &(str_detect(storm$wind_speed[i], "km/h")==TRUE)){
      storm$wind_speed[i] = 
      paste(as.character(round(as.numeric(str_extract(storm$wind_speed[i],
                        "\\d+"))*(1/1.852))), "knots")
  } else{
  }
} 
for (j in 1:nrow(storm)){
  if (is.na(storm$wind_speed[j])==FALSE){
    storm$wind_speed[j] = str_extract(storm$wind_speed[j], "\\d+")
  } else{
  }
}
storm$wind_speed<-as.numeric(storm$wind_speed)
storm %>% filter(is.na(storm$wind_speed) == FALSE) %>% 
   ggplot(aes(x=State, y=wind_speed)) +
   geom_boxplot() +
   scale_x_discrete(name = "State")
                                     
