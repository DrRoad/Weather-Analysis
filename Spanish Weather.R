#########################################################################################################
## tu tiempo.es
#########################################################################################################

library(XML)
library(htmltab)
library(stringr)
library(ggplot2)
library(reshape2)

##Function that wraps and contains xpathSApply methods specific to this webpage.
##This extracts the info of interest from each webpage
parseTiempoResults <- function(url_html){
  ##On the tutiempo website the results are stored in a table
    results <- htmltab(doc=url_html, which=1)
    headings <- unlist(strsplit(names(results),">>"))
    headings <- headings[seq(1,length(headings),2)]
    headings <- gsub(" ", "", headings)
    names(results) <- headings
    results$url <- docName(url_html)
    results$month <- substr(str_match(results$url,"[0-9]*-"),1,2)
    results$year <- substr(str_match(results$url,"-[0-9]*"),2,5)
    row.names(results) <- NULL
  return(results)
}

##Use the XML package to return the html from each page and store this as a list which we will later parse (this will take a long time)

weather <- NULL
for( i in 2005:2014) {
  raw_html_data <- lapply(seq(1,12,1), function(x) htmlTreeParse(paste0("http://en.tutiempo.net/climate/",ifelse(nchar(x)==1,paste0("0",x),x),"-",i,"/ws-82020.html"), useInternalNodes=T))
  tutiempo <- do.call(rbind,lapply(raw_html_data, parseTiempoResults))
  weather <- rbind(weather, tutiempo)
}

## Some simple plots of the data with the base R plot functions

ts_t_average <- ts(weather$T)
ts_t_max <- ts(weather$TM)
ts_t_min <- ts(weather$Tm)
plot(ts_t_average)
lines(ts_t_max, col="red", add=TRUE)
lines(ts_t_min, col="blue", add=TRUE)


## Format the data for advanced plotting

weather$date <- as.Date(paste(weather$Day,weather$month,weather$year,sep="/"), format="%d/%m/%Y")
## Remove averavges from each monthly table
weather <- weather[complete.cases(weather),]
weather$temp_avg_daily <- as.numeric(weather$T)
weather$temp_max_daily <- as.numeric(weather$TM)
weather$temp_min_daily <- as.numeric(weather$Tm)
weather$humid_avg_daily <- as.numeric(weather$H)
weather$rain_avg_daily <- as.numeric(weather$PP)
## There are a small number of dates with incomplete data, ignore these
weather <- weather[complete.cases(weather),]

weather <- weather[,c("date","temp_avg_daily","temp_max_daily","temp_min_daily", "humid_avg_daily", "rain_avg_daily")]

weather_melted <- melt(weather, id.var="date")

## Advanced plotting of daily data with ggplot2

temp <- weather_melted[weather_melted$variable%in%c("temp_avg_daily","temp_max_daily", "temp_min_daily"),]
humid <- weather_melted[weather_melted$variable%in%c("humid_avg_daily"),]
rain <- weather_melted[weather_melted$variable%in%c("rain_avg_daily"),]


ggplot(temp, aes(x=date, y=value, col=variable))+
  geom_point()+
  geom_smooth(se=F) +
  theme(axis.text.y = element_text(size = 6),axis.text.x = element_text(size = 6), legend.position="bottom") +
  scale_colour_manual(values=c("#A1D99B", "#FC9272", "#9ECAE1"))


ggplot(humid, aes(x=date, y=value, col=variable))+
  geom_point()+
  geom_smooth(se=F) +
  theme(axis.text.y = element_text(size = 6),axis.text.x = element_text(size = 6), legend.position="bottom")+
  scale_colour_manual(values=c("#31A354"))

ggplot(humid, aes(x=date, y=value, col=variable))+
  geom_point()+
  geom_smooth(se=F) +
  theme(axis.text.y = element_text(size = 6),axis.text.x = element_text(size = 6), legend.position="bottom")+
  scale_colour_manual(values=c("#31A354"))

ggplot(rain[rain$value>0 & year(rain$date) == 2014 , ], aes(x=date, y=value, col=variable))+
  geom_point()+
  geom_smooth(se=F) +
  theme(axis.text.y = element_text(size = 6),axis.text.x = element_text(size = 6), legend.position="bottom")+
  scale_colour_manual(values=c("#3182BD"))

############################################################
## Finish looking at daily data calc monthly averages
############################################################

library(plyr)
library(lubridate)

temp_avg_monthly <- ddply(weather, .(year(date), month(date) ),  summarise, MaxT = mean(temp_avg_daily))
names(temp_avg_monthly) <- c("year", "month", "temperature")
temp_avg_monthly$year <- as.factor(temp_avg_monthly$year)
temp_avg_monthly$july <- as.factor(ifelse(temp_avg_monthly$month==7, TRUE, FALSE))

ggplot(temp_avg_monthly, aes(x=month, y=temperature, col=year))+
  geom_point()+
  geom_smooth(se=FALSE) + 
  theme(axis.text.y = element_text(size = 6),axis.text.x = element_text(size = 6), legend.position="bottom") +
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

temp_max <- temp[temp$variable=="temp_max_daily", ]
temp_max$year_month <- paste0(year(temp_max$date),"-",month(temp_max$date))
temp_max$july <- as.factor(ifelse(month(temp_max$date)==7, TRUE, FALSE))

library(ggthemes)

ggplot(temp_max, aes(x=year_month, y=value,colour=july))+geom_boxplot() +
  scale_x_discrete(breaks=c("2010-1","2011-1","2012-1","2013-1","2014-1"), labels=c("Jan 2010", "Jan 2011", "Jan 2012", "Jan 2013", "Jan 2014")) +
  theme(axis.text.y = element_text(size = 6),axis.text.x = element_text(size = 6), legend.position="none") +
  ggtitle("Trends in Maximum Temperature") +
  xlab("Month - Year")+
  ylab("Maximum Temperature")

p1 <- ggplot(temp_max, aes(x=year_month, y=value,colour=july))+geom_boxplot() +
  scale_x_discrete(breaks=c(paste(seq(2005, 2014, 1), 1, sep="-")), labels=c(paste("Jan", seq(2005, 2014, 1), sep=" "))) +
   scale_y_continuous(limits=c(0,41))+
  ggtitle("Maximum Temperature") +
  xlab("") +
  ylab("Temperature")+
  theme_economist()  +
  theme(legend.position="none")

temp_min <- temp[temp$variable=="temp_min_daily", ]
temp_min$year_month <- paste0(year(temp_min$date),"-",month(temp_min$date))
temp_min$july <- as.factor(ifelse(month(temp_min$date)==7, TRUE, FALSE))

p2 <- ggplot(temp_min, aes(x=year_month, y=value,colour=july))+geom_boxplot() +
  scale_x_discrete(breaks=c(paste(seq(2005, 2014, 1), 1, sep="-")), labels=c(paste("Jan", seq(2005, 2014, 1), sep=" "))) +
  scale_y_continuous(limits=c(0,41))+
  ggtitle("Minimum Temperature") +
  xlab("") +
  ylab("Temperature")+
  theme_economist()  +
  theme(legend.position="none")

temp_avg <- temp[temp$variable=="temp_avg_daily", ]
temp_avg$year_month <- paste0(year(temp_avg$date),"-",month(temp_avg$date))
temp_avg$july <- as.factor(ifelse(month(temp_avg$date)==7, TRUE, FALSE))

p3 <- ggplot(temp_avg, aes(x=year_month, y=value,colour=july))+geom_boxplot() +
  scale_x_discrete(breaks=c(paste(seq(2005, 2014, 1), 1, sep="-")), labels=c(paste("Jan", seq(2005, 2014, 1), sep=" "))) +
   scale_y_continuous(limits=c(0,41))+
  ggtitle("Average Temperature") +
  xlab("") +
  ylab("Temperature")+
  theme_economist()  +
  theme(legend.position="none")

 library(gridExtra)



pdf("Trends in Temperature.pdf")
grid.arrange(p1, p3, p2)
dev.off()

rain_avg <- rain[( month(rain$date)> 5 & month(rain$date)<10 ),]
rain_avg <- rain_avg[rain_avg$value > 0 ,]
rain_avg$year_month <- paste0(year(rain_avg$date),"-",month(rain_avg$date))
rain_avg$july <- as.factor(ifelse(month(rain_avg$date)==7, TRUE, FALSE))

pdf("Trends in Rainfall.pdf")

ggplot(rain_avg, aes(x=year_month, y=value,colour=july))+geom_boxplot() +
  ggtitle("Average Rainfall") +
  xlab("") +
  ylab("Rainfall")+
  theme_economist()  +
  theme(legend.position="none")
dev.off()

humid_avg <- humid[( month(humid$date)> 5 & month(humid$date)<10 ),]
humid_avg <- humid_avg[humid_avg$value > 0 ,]
humid_avg$year_month <- paste0(year(humid_avg$date),"-",month(humid_avg$date))
humid_avg$july <- as.factor(ifelse(month(humid_avg$date)==7, TRUE, FALSE))


pdf("Trends in Humidity.pdf")

ggplot(humid_avg, aes(x=year_month, y=value,colour=july))+geom_boxplot() +
  ggtitle("Average Humidity") +
  xlab("") +
  ylab("Humidity")+
  theme_economist()  +
  theme(legend.position="none")
dev.off()
