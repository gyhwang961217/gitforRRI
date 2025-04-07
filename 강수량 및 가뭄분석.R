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
install.packages("lubridate")
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
library(lubridate)
library(dplyr)
library(tidyr)
library(stats)
library(purrr)
options(scipen = 100)

#동해안 기상관측소 자료 정리----
#강수량 정리
rain<- read_xlsx(file.choose())
view(rain)
rain <- rain %>% 
  mutate(일시           = ymd(일시),
         연도             = year(일시),
         월           = month(일시))

view(rain)

rain_year <- rain %>%
  group_by(지점명, 연도) %>%
  summarize(rain_year = sum(`월합강수량(00~24h만)(mm)`, na.rm = TRUE))

view(rain_year)

rain_month <- rain %>%
  group_by(지점명, 월) %>%
  summarize(rain_month = mean(`월합강수량(00~24h만)(mm)`, na.rm = TRUE))

view(rain_month)

years<- split(rain_year, rain_year$연도)
wb <- createWorkbook()

for(year in names(years)) {
  addWorksheet(wb, year)
  writeData(wb, year, 
            years[[year]])
}

saveWorkbook(wb, "연도별_rain.xlsx",overwrite=TRUE)

#강수량 그래프
mean_totalrain <- rain_year %>%
  group_by(연도) %>%
  summarise(mean_rainfall = mean(rain_year, na.rm=TRUE))

view(mean_totalrain)

ggplot(mean_totalrain, aes(x=연도, y=mean_rainfall))  +
  geom_bar(stat = "identity", fill="skyblue")+
  labs(x="연도", y= "연강수량(mm)", title="동해안 기상관측소 연 강수량")+
  geom_hline(yintercept=mean(mean_totalrain$mean_rainfall), linetype="dashed", color="red", size=1) +
  annotate("text", x= mean(mean_totalrain$연도), y = mean(mean_totalrain$mean_rainfall),
           label=paste("평균:", round(mean(mean_totalrain$mean_rainfall),2)),
           color='black', vjust=-0.5, hjust=-2)+
  scale_x_continuous(breaks=seq(min(mean_totalrain$연도), max(mean_totalrain$연도), by=2))+
  ylim(0,2000)+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5))

# 월별 평균 내기----
view(rain)
view(rain_month)
pivot_data <- rain_month%>%
  dplyr::select(월, 지점명, rain_month) %>%
  pivot_wider(names_from = 월, values_from =rain_month) #오류뜨면 중복값 등 검수

view(pivot_data)
str(pivot_data)

# 마지막 행에 합계 추가
total_row <- data.frame(지점명 = "Total", t(colSums(pivot_data[,-1])))
colnames(total_row) <- colnames(pivot_data) #열 이름 통일일
view(total_row) 
pivot_data2 <- rbind(pivot_data, total_row)

view(pivot_data2)

#박스플롯 그리기----
#연도별 데이터로 그려야 함
rain_2 <- rain %>%
  group_by( 지점명,연도, 월) %>%
  summarize(rain_month = mean(`월합강수량(00~24h만)(mm)`, na.rm = TRUE))

view(rain_2)

pivot_monthdata <- rain_2 %>%
  dplyr::select(월, 연도, rain_month) %>%
    pivot_wider(names_from = 월, values_from = rain_month)

box_test <- pivot_monthdata[,-c(1,2)]
view(box_test)

boxplot(box_test, main="월별 강수량 분포", xlab="월", ylab="강수량", 
        col="tan", border="black", las=0, outline=FALSE, ylim=c(0,600))
abline(h=seq(0, max(box_test, na.rm=TRUE), by=100), col="gray", lty=2)


#SPI3 분석하기----
SPI3 <- read_xlsx(file.choose())
SPI3 <- SPI3 %>% 
  mutate(일시           = ymd(일시),
         연도             = year(일시),
         월           = month(일시),
         일           = day(일시))

view(SPI3)
SPI<- SPI3 %>%
   mutate(일별     = format(일시, "%m-%d"),
          월별     = format(일시, "%m"))  %>%
   arrange(일별)
view(SPI)

#데이터 검증을 위한 엑셀 저장
monthly_SPI <- split(SPI, SPI$월)

for (month in names(monthly_SPI)) {
  write_xlsx(monthly_SPI[[month]], paste0("data_month_", month, ".xlsx"))
}


#3개년만 해보기로함
spi_3years <- subset(SPI, SPI$연도 >= 2021)
view(spi_3years)

selected_spi<- spi_3years[, c("SPI3", "일별", "월별")]
view(selected_data)
selected_spi_2 <- selected_spi[,c("SPI3", "월별")]
view(selected_spi_2)

selected_spi_2$월별 <- as.numeric(selected_spi_2$월별)


str(selected_spi_2)

#boxplot() 함수를 사용하려면 데이터 형식을 범주형으로 바꿔야 함
selected_spi_2$월별 <- factor(selected_spi_2$월별)

boxplot(SPI3 ~ 월별, data = selected_spi_2,
        xlab = "월",
        ylab = "SPI3",
        col="tan", border="black", las=0, outline=FALSE, ylim=c(-4,4),
        yaxt = "n")
axis(2, at = seq(-4, 4, by = 1))
 

#기상학적 가뭄 주기 분석----




# 연도별 모든 관측소의 연 강수량의 평균 계산

getwd()
setwd("C:/Users/EKR/Desktop/할이리/충남/기상자료")

# 엑셀로 저장----
write_xlsx(x=SPI_sum, path = "SPI_sum.xlsx")
write_xlsx(x=rain, path = "rain_raw.xlsx")
write_xlsx(x=rains, path = "rain_raw2.xlsx")