setwd("~/1ingenieria/data analysis")
#Reading the data
data=read.csv("data first assignment.csv")
head(data)
View(data)
dim(data)

#Question 1
table(data$HomeTeam,data$HTR)
library(ggplot2)
library(factoextra)
kmeans()

sumhtr=summary(factor(data$HTR))
sumftr=summary(factor(data$FTR))
#To obtain the histogram that explains the solution
ggplot(data,aes(x=HomeTeam,y=FTHG,col=FTR,fill=FTR))+geom_histogram(stat="identity")+coord_flip()
#To know who scored more
sum(data$FTHG)
sum(data$FTAG)

#Question 2
#To know who committed more faults
ggplot(data)+geom_histogram(aes(HF,..density..,fill="blue"),bins=10)+geom_histogram(aes(AF,-..density..,fill="red"),bins = 10)+labs(title="total faults committed")
mean(data$HF)
mean(data$AF)   
yellow=sum(data$HY,data$AY)
red=sum(data$HR,data$AR)
#To obatin the difference between faults and cards
totalt=sum(yellow,red)
totalf=sum(data$HF,data$AF)

#cluster
library(factoextra)
df=data.frame(data$FTHG,nrow(data),ncol(data))
o=kmeans(df,centers= 3,nstart = 25)
fviz_cluster(o,df)
#Question 3
data = read.csv("SP1.csv")
head(data)
mean_hwo = ((mean(data$B365H)+mean(data$BWH)+mean(data$IWH)+mean(data$LBH,na.rm = TRUE)+mean(data$PSH)+mean(data$VCH)+mean(data$WHH))/7)
mean_hdo = ((mean(data$B365D)+mean(data$BWD)+mean(data$IWD)+mean(data$LBD,na.rm = TRUE)+mean(data$PSD)+mean(data$VCD)+mean(data$WHD))/7)
mean_hao = ((mean(data$B365A)+mean(data$BWA)+mean(data$IWA)+mean(data$LBA,na.rm = TRUE)+mean(data$PSA)+mean(data$VCA)+mean(data$WHA))/7)
#We have used na.rm=True to deal with the NA values and be able to get a result.
total_home_odds= c(mean_hwo,mean_hdo,mean_hao)
lbls=c("Home Win Odds","Draw Odds","Away win Odds")
pct = round(total_home_odds/sum(total_home_odds)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(total_home_odds,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of House betting Odds")

#Question 4
df = data.frame( home_win_odds = c(data$PSCH, data$PSCD, data$PSCA),
                 house = factor(rep(c("Pinnacle closing home win odds", "Pinnacle closing draw odds", "Pinnacle closing away win odds"), each = nrow(data))))
ggplot(df)+
  aes( x = house, y = home_win_odds )+
  geom_boxplot()+
  labs(x = "Closing Odds", y = "Home, Draw, Away odds")

