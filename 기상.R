# 패키지 설치----
install.packages("tidyverse")
install.packages("scales")
install.packages("readxl")
install.packages("ggbreak")
install.packages("GGally")
install.packages("ggpubr")
install.packages("lawstat")
install.packages("nparcomp")
install.packages("psych")
install.packages('corrplot')
install.packages('writexl')
install.packages('fpp3')
install.packages('report')
install.packages('forecast')
library(tidyverse)
library(scales)
library(readxl)
library(scales)
library(ggbreak)
library(GGally)
library(ggpubr)
library(lawstat)
library(nparcomp)
library(psych)
library(corrplot)
library(writexl)
library(fpp3)
library(report)
library(forecast)
options(scipen = 100)

# 강수량 자료 정리하기----
aosrain <- read_xlsx(file.choose())  #전체정리_기상자료
view(aosrain)

rain <- aosrain %>% 
  mutate(일시           = ymd(일시),
         연도             = year(일시),
         월           = month(일시))

view(rain)


# 강수량 데이터 평균통계내기기----

rains <- rain %>%
  group_by(연도, 지점명) %>%
  summarize(total_rainfall = sum(`일강수량(mm)`, na.rm = TRUE))

view(rains)
# 연도별 모든 관측소의 연 강수량의 평균 계산
rainss <- rains %>%
  group_by(연도) %>%
  summarize(mean_annual_rainfall = mean(total_rainfall, na.rm = TRUE))

view(rainss)

getwd()
setwd("C:/Users/EKR/Desktop/할이리/충남/기상자료")

# 엑셀로 저장----
write_xlsx(x=rainss, path = "rain.xlsx")
write_xlsx(x=rain, path = "rain_raw.xlsx")
write_xlsx(x=rains, path = "rain_raw2.xlsx")

# 관측데이터 정리----
cw1<-read_xlsx(file.choose(),skip = 1)
view(cw1)
is.data.frame(cw1)
str(cw1) # 데이터 형식 확인

cw1 <- cw1 %>% 
  mutate(date           = as.numeric(측정일자),
         level = as.numeric(`수위(해수면기준)`)) %>% 
  mutate(date             = date/1000000,
         time           = ymd_hm(date/100)) %>% 
  mutate(date             = ymd(date),
         time           = hour(time))


#회귀분석
model<- lm(level~date, data= cw1)
summary(model)
coe <- coef(model)
coe
slope <- coe["date"]
slope

ggplot(cw1, aes(x=date, y=level))+
  #scale_y_continuous(limits = c(0,1))         +
  geom_line()+
  geom_smooth(method="lm", col="red")+
  labs(title="level trend", x="Date", y="EL.m")+
  geom_text(aes(x=as.Date('2020-01-01'), y=12, label=slope))




