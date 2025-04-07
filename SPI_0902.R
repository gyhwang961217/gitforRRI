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
install.packages('openxlsx')
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
library(openxlsx)
options(scipen = 100)

# 강수량 자료 정리하기----
spi <- read_xlsx(file.choose())  #전체정리_기상자료
view(spi)
SPI <- spi[,c("지점명","일시","SPI2")]
view(SPI)


spi_test <- SPI %>%
  mutate(일시           = ymd(일시),
         연도             = year(일시),
         월           = month(일시),
         일           = day(일시),
         일별         = format(일시, "%m-%d"))

#특정 조건 추출
spi_test <- subset(spi_test, spi_test$연도>=2023)

view(spi_test)

# 강수량 데이터 연평균으로 정리
spi24 <- spi_test %>%
  group_by(일시, 지점명) %>%
  summarize(spi24 = mean(SPI2, na.rm = TRUE))

view(spi24)

pivot <- spi24 %>%
  pivot_wider(names_from = 지점명, values_from = spi24)

view(pivot)


write_xlsx(x=pivot, path = "SPI2023.xlsx")




spi24s <- spi24 %>%
  group_by(지점명, 연도, 월) %>%
  summarize(spi24 = mean(spi24, na.rm = TRUE))

view(spi24s)

spi24ss <- spi24 %>%
  group_by(지점명, 연도) %>%
  summarize(spi24 = mean(spi24, na.rm = TRUE))

view(spi24ss)

write_xlsx(x=spi24s, path = "spi24.xlsx")
write_xlsx(x=spi24ss, path="spi24_mean.xlsx")




# 연도별 데이터를 분리
spi_total <- read_xlsx(file.choose())  
view(spi_total)
years<- split(spi_total, spi_total$연도)
wb <- createWorkbook()



for(year in names(years)) {
  addWorksheet(wb, year)
  writeData(wb, year, 
            years[[year]])
}

saveWorkbook(wb, "연도별_SPI.xlsx",overwrite=TRUE)

# 강수량 데이터 평균통계내기기----

rains <- rain %>%
  group_by(연도, 지점명) %>%
  summarize(total_rainfall = sum(`일강수량(mm)`, na.rm = TRUE))

view(rains)
# 연도별 모든 관측소의 연 강수량의 평균 계산


getwd()
setwd("C:/Users/EKR/Desktop/할이리/충남/")

# 엑셀로 저장----
write_xlsx(x=rainss, path = "rain.xlsx")
write_xlsx(x=rain, path = "rain_raw09.xlsx")
write_xlsx(x=rains, path = "rain_raw0902.xlsx")

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




