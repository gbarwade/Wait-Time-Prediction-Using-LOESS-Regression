library(magicfor)
library(writexl)
library(lubridate)
library(dplyr)
library(Metrics)
library(ggplot2)
library(data.table)

################# Data Prepration ####################################
dot <- fread("dot.csv")
dot=dot%>%mutate(Month=ifelse(dot$Month=="A) January",1,ifelse(dot$Month=="B) February",2,
                       ifelse(dot$Month=="C) March",3,ifelse(dot$Month=="D) April",4,
                       ifelse(dot$Month=="E) May",5,ifelse(dot$Month=="F) June",6,
                       ifelse(dot$Month=="G) July",7,ifelse(dot$Month=="H) August",8,
                       ifelse(dot$Month=="I) September",9,ifelse(dot$Month=="J) October",10,
                       ifelse(dot$Month=="K) November",11,12))))))))))))
dot$Month=as.factor(dot$Month)
dot=data.frame(Branch=as.factor(dot$Branch_Name),w=as.numeric(dot$waiting_time_seconds),
              day=as.numeric(dot$DayNumber),hour=as.numeric(dot$Hour),month=as.numeric(dot$Month), week=as.numeric(dot$WeekNumber),
              year=as.factor(dot$Year),date=as.Date(dot$day))
dot=arrange(dot,date)

ankeny=subset(dot,Branch=="Ames" & year=='2018'& (month==1 | month==2| month==3))
boxplot(w~date,
        data=ankeny,
        main="Different Boxplots for Each Day",
        xlab="Day",
        ylab="Wait time in seconds",
        col="orange",
        border="brown")

############################### Dubuque ####################################
for(i in c(1,4,7,10)) {
  ankeny=subset(dot,Branch=="Dubuque" & year=='2018'& (month==i | month==i+1| month==i+2))
  boxplot(w~date,
          data=ankeny,
          main="Different Boxplots for Each Day",
          xlab="Day",
          ylab="Wait time in seconds",
          col="orange",
          border="brown")
  
}

############################### Burlington ###################################

for(i in c(1,4,7,10)) {
  ankeny=subset(dot,Branch=="Burlington" & year=='2018'& (month==i | month==i+1| month==i+2))
  boxplot(w~date,
          data=ankeny,
          main="Different Boxplots for Each Day",
          xlab="Day",
          ylab="Wait time in seconds",
          col="orange",
          border="brown")
  
}

######################### Dubuque ######################################
for(i in c(1,4,7,10)) {
  ankeny=subset(dot,Branch=="Dubuque" & year=='2018'& (month==i | month==i+1| month==i+2))
  boxplot(w~date,
          data=ankeny,
          main="Different Boxplots for Each Day",
          xlab="Day",
          ylab="Wait time in seconds",
          col="orange",
          border="brown")
  
}

############################# Loess for Dubuque############################## 

db=na.omit(subset(dot,Branch=="Dubuque" & year=='2018' & (week==6 | week==7)))
test=na.omit(subset(dot,Branch=="Dubuque" & year=='2018' & week==8))
test.db=data.frame(b=test$day,c=test$hour,d=test$month,e=test$week)
test_w= test$w
a=db$w
b=db$day
c=db$hour 
d=db$month
e=db$week
loessMod10 <- loess(lm(a~b+c+e), span=0.3, degree = 1, control = loess.control(surface = "direct"))
p=predict(loessMod10, newdata = test.db )
MAE=mae(test_w,p)
RMSE=rmse(test_w,p)

x=1:length(p)
plot(x,test_w,col="red",
     main="Actual Vs Predicted",
     xlab="Index",
     ylab="Wait time in seconds")
points(x,p,col="green")

#################### Loess for Ames #####################################################

db=na.omit(subset(dot,Branch=="Ames" & year=='2018' & (week==20 | week==21)))
test=na.omit(subset(dot,Branch=="Ames" & year=='2018' & week==22))
test.db=data.frame(b=test$day,c=test$hour,d=test$month,e=test$week)
test_w= test$w
a=db$w
b=db$day
c=db$hour 
d=db$month
e=db$week
loessMod10 <- loess(lm(a~b+c+e), span=0.3, degree = 1, control = loess.control(surface = "direct"))
p=predict(loessMod10, newdata = test.db )
MAE=mae(test_w,p)
RMSE=rmse(test_w,p)

x=1:length(p)
plot(x,test_w,col="red",
     main="Actual Vs Predicted",
     xlab="Index",
     ylab="Wait time in seconds")
points(x,p,col="green")


# prediction for all weeks and all years
years=c('2017','2018','2019')
for (y in 1:3){
  ames=na.omit(subset(dot,Branch=="Ames" & year==years[y]))
  magic_for(silent = TRUE)
  for (i in ames$week[1]:ames$week[1]:(ames$week[dim(ames)[1]]-2)){
    db=subset(ames,(week==i | week==i+1))
    test=subset(ames, week==i+2)
    test.db=data.frame(b=test$day,c=test$hour,d=test$month,e=test$week)
    test_w= test$w
    a=db$w
    b=db$day
    c=db$hour 
    d=db$month
    e=db$week
    loessMod10 <- loess(lm(a~b+c+e), span=0.3, degree = 1, control = loess.control(surface = "direct"))
    p=predict(loessMod10, newdata = test.db )
    MAE=mae(test_w,p)
    RMSE=rmse(test_w,p)
    put(i+2,MAE,RMSE)
  }
  result=magic_result_as_dataframe()
  theme_set(theme_minimal())
  q=ggplot(result,aes(i+2,RMSE))+geom_line(aes(color="RMSE"),size=1)+
    geom_line(data=result,aes(i+2,MAE,color="MAE"),size=1)+labs(x="Week number",y="RMSE / MAE ",color="")+
    ggtitle(years[y])
  print(q)
  
}

