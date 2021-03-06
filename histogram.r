library(dplyr)
library(ggplot2)
accidents<-read.csv(file = "accidents2014.csv")
colnames(accidents)
dim(accidents)
-----------------------------------------------------------------------------------------------------------------------------
accidents <- select(accidents, -c("Accident.Date", 
                                  "Time..24hr.", "Road.Surface", 
                                  "Lighting.Conditions", "Weather.Conditions"))
-----------------------------------------------------------------------------------------------------------------------------
accidents <- filter(accidents, X1st.Road.Class!=1) %>% filter(Type.of.Vehicle==9)
accidents <- select(accidents, -c("Accident.Date", 
                                  "Time..24hr.", "Road.Surface", 
                                  "Lighting.Conditions", "Weather.Conditions"))
accidents <- filter(accidents, X1st.Road.Class!=1) %>% filter(Type.of.Vehicle==9)
-----------------------------------------------------------------------------------------------------------------------------
ggplot_hist = ggplot(accidents, aes(x=Age.of.Casualty)) +
                    geom_histogram(binwidth=10, center=5) +
                    xlab("Casualty age") +
                    ylab("No. of casualties")
qplot_hist = qplot(x=Age.of.Casualty, data=accidents, 
                   geom="histogram", binwidth=10, center=5,
                   xlab="Casualty age", ylab="No. of casualties")
ggplot_hist
