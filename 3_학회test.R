library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(ggpmisc)
install.packages("ggbreak")
install.packages("gridGraphics")
library(ggbreak)
.libpath()
install.packages("TTR")
install.packages("forecast")
install.packages("tseries")
library(TTR)
library(forecast)
library(tseries)
#데이터 획득 및 정제(새벽2시 데이터로 정제)
cw1<-read.csv(file.choose(),fileEncoding="CP949",encoding="UTF-8")
suwi<-cw1$수위.해수면기준.
name<-cw1$관정명
date1<-cw1$측정일자/1000000
date1<-ymd(date1)
date_<-(cw1$측정일자)/100
date<-ymd_hm(date_)
time<-hour(date)
time
head(time)
head(date1)
dfcw1<-data.frame(name,date1,time,suwi)
head(dfcw1)
dfcw12<-subset(dfcw1, dfcw1$time==2)
head(dfcw12)
#기상자료 정리
aosrain<-read.csv(file.choose(),fileEncoding="CP949",encoding="UTF-8")
awsrain<-read.csv(file.choose(),fileEncoding="CP949",encoding="UTF-8")
head(aosrain)
head(awsrain)
rain<-subset(aosrain, aosrain$지점명=="철원")
head(rain)
date1<-rain$일시
rain<-rain$일강수량.mm.
dfrain_<-data.frame(date1,rain)
head(dfrain_)
#합치기
df_cw1<-merge(dfcw12,dfrain_,by="date1",all=TRUE)
head(df_cw1)
rain<-df_cw1$rain
date<-df_cw1$date1
name<-df_cw1$name
name
elm<-df_cw1$suwi
#이상치 는 제거하지 않음
boxplot(df_cw1$suwi)
boxplot(df_cw1$suwi)$stats #박스플롯을 통한 이상치 확인(상,하단 경계선)
max(na.omit(df_cw1$suwi)) #데이터 최대/최소값 확인
#필요시 삭제
# elm<-ifelse(suwi<79 | suwi>83.03, NA, suwi) #이상치는 확인하여 조정
dfcw1<-data.frame(name,date,elm,rain)
head(dfcw1)
#시계열 그dfmd2_#시계열 그래프 작성
summary(dfcw1$elm)
#변량 비율 구하기
a<-max(na.omit(dfcw1$elm))
b<-max(na.omit(dfcw1$rain))
max_ratio<-a/b
max_ratio
summary(dfcw1$elm)
p<-ggplot(dfcw1, aes(x=date))
p<-p+geom_line(aes(y=elm))+ geom_line(aes(y = rain*0.01),stat="identity",colour="darkblue")
p
summary(rain)
p<-p+scale_y_continuous(name = "EL.m", breaks=c(188,189,190,191,192,193,194),  ## 첫번째 Y축 이름 설정
                        sec.axis = sec_axis(~./0.01, name="precipitation", breaks = c(50,100,150,200,250,300,350)))
p
summary(dfcw1$elm)
av<-mean(na.omit(dfcw1$elm))
p<-p+ggtitle("철원1(2018-2023)")+geom_hline(yintercept=av,linetype='dashed', colour="blue")+theme(axis.text.x=element_text(angle=60))+scale_x_date(breaks="2 months", expand=c(0,0),labels=date_format("%Y-%b"))+theme(plot.title=element_text(hjust=0.5))+xlab("Date(YYYY-M)")
p#자르기
pp<-p+scale_y_break(c(2,188.5))+theme(axis.text.x =element_text(size=7))
pp
#자기상관분석
#데이터 na값 제거
nonadf<-na.omit(dfcw1)
#acf 결과확인(지연시간 등)
a<- acf(nonadf$elm, lag.max=200, type=c("correlation"), plot=TRUE)
plot(a$acf, ylim=c(-1,1), xlab="lag(day)", ylab="acf", main="철원1", type='l')
abline(h=0, lty=2, lwd=1.5) # 0값 추가
head(a)
summary(a)
#0수렴시간 확인
is.list(a)
b<-as.data.frame(a$acf)
b
abline(v=48, lty=2, lwd=1.5, col="red") # 0값 추가
#데이터 플롯 추가
text(locator(1), labels="48days")
#교차상관
result<-ccf(nonadf$elm,nonadf$rain,lag.max = 300)
summary(result)
head(result)
summary(result$acf)
d<-data.frame(result$lag, result$acf)
plot(d, ylim=c(-1,1),xlab="lag(day)", ylab="acf",main="철원1",type="l")
abline(h=0, lty=2, lwd=1.5)
abline(v=0,lty=2, lwd=1.5)
#최대상관계수 및 h 표기(ccf랑 cor이랑 다른 이유 확인할 것..)
dmax<-max(d$result.acf)
dmax
cor(nonadf$elm, nonadf$rain) #왜 다르지
dd<-subset(d, d$result.acf==dmax)  #왜 음수지
dd
ccc<-dd$result.lag
abline(v=dd$result.lag,lty=2, lwd=1.5, col="gray")
text(locator(1), labels=ccc, col="red")
text(locator(1), labels=",")
text(locator(1), labels=round(dmax,2))
d