install.packages("readr")

library(readr)
library(ggplot2)
library(dplyr)
install.packages("rworldmap")
library(rworldmap)
library(maps)
library(ggmap)
install.packages("highcharter")
library(highcharter)
data <- read.csv("C:/Users/sagar/Desktop/data/globalterrorismdb_0617dist.csv")
View(data)
summary(data)
str(data)

data=rename(data,id=eventid,year=iyear,nation=country_txt,Region=region_txt,attack=attacktype1_txt,
            target=targtype1_txt,weapon=weaptype1_txt,Killed=nkill, wounded=nwound)

data$Killed=as.integer(data$Killed)


data$Killed[which(is.na(data$Killed))]=0


global_t <- data%>%group_by(year,nation,Region)%>%summarize(Total=n())

global_y <- global_t%>%group_by(year)%>%summarize(Total=sum(Total))

highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Global Terror attacks by year") %>% 
  hc_xAxis(categories = global_y$year) %>% 
  hc_add_series(data = global_y$Total,
                name = "no of attacks",colorByPoint = TRUE)


dataS=filter(data,Region %in% c("North America","Western Europe","Eastern Europe","Sub-Saharan Africa","Middle East & North Africa","Southeast Asia","South Asia"))
ggplot(dataS,aes(x=attack,color=attack,fill=attack))+
  geom_bar(position = "dodge")+ 
  theme(legend.position="bottom")+
  facet_grid(~Region)+
  scale_x_discrete("type of attacks") + 
  scale_y_continuous("Number of attacks")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

global_n <- global_t%>%group_by(nation)%>%summarize(Total=sum(Total))%>%arrange(desc(Total))%>%filter(Total>300)


highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Global Terror attacks by countries") %>% 
  hc_xAxis(categories = global_n$nation) %>% 
  hc_add_series(data = global_n$Total,
                name = "no of attacks",colorByPoint = TRUE)

global_r <- global_t%>%group_by(Region)%>%summarize(Total=sum(Total))%>%arrange(desc(Total))

highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Global Terror attacks by region") %>% 
  hc_xAxis(categories = global_r$Region) %>% 
  hc_add_series(data = global_r$Total,
                name = "no of attacks",colorByPoint = TRUE)

India<-filter(data,nation=="India")
IndiaK=filter(India,Killed!=0)
india_map<-getMap(resolution="low")
plot(india_map, xlim = c(72,75), ylim = c(8, 35), asp = 1,main = "Killed by Terror Attacks")
points(IndiaK$longitude,IndiaK$latitude,col="red",cex=.6)
