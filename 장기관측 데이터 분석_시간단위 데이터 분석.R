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
library(zoo)
options(scipen = 100)

# 로우데이터 자료 정리하기----
setwd("C:/Users/EKR/Desktop/KRC/09_수자원학회 발표/")
getwd()

data <- read_xlsx(file.choose(), skip=1) #쓸데없는거 삭제(skip)
view(data)


#특정 데이터만 선택
data <- data[,c(1,2,3,7)]
view(data)

#날짜 구분
data1 <- data %>% 
  mutate(측정일자           = as.character(측정일자)) %>% 
  mutate(일자           = substr(측정일자, 1, 8)) %>%
  mutate(시간           = as.numeric(substr(측정일자, 9, 10)))


data2 <- data1 %>%
  mutate(날짜           = ymd(일자)) %>% 
  mutate(연도             = year(날짜),
         월           = month(날짜),
         일           = day(날짜))

data3<- data2 %>%
  mutate(일별     = format(날짜, "%m-%d"))

data3$datetime <- as.POSIXct(
  paste(data3$날짜, sprintf("%02d:00:00", data3$시간)), # 날짜와 시간 병합
  format = "%Y-%m-%d %H:%M:%S"
)

view(data3)
#if (!inherits(data3$datetime, "POSIXct")) {
#  data3$datetime <- as.POSIXct(data3$datetime, format = "%Y-%m-%d %H:%M:%S")
#}



data4 <- data3 %>% 
  mutate(ELm   = data3$`수위(해수면기준)`)



view(data4)
# 강수량 자료 합치기----
aosrain <- read_excel(file.choose(), 
                      col_names = c("지점", "지점명", "datetime", "강수량"), skip=1)
str(aosrain)
aosrain <-  aosrain %>% 
  mutate(날짜    = ymd(날짜))

view(aosrain)
#  강우자료 많을 시 추출 
rain<-subset(aosrain, aosrain$지점명=="포항")
df <- merge(data4,rain,by="datetime", all.x=TRUE)
df <- dplyr::select(.data = df, -지점, -지점명)

view(df)
view(rain)


# 시각화----
#원하는 날짜와 시간 설정
#포항
start_date <- as.POSIXct("2017-11-10 00:00:00")
end_date <- as.POSIXct("2017-11-20 24:00:00")

start_date <- as.POSIXct("2016-09-02 00:00:00")
end_date <- as.POSIXct("2016-09-22 24:00:00")


date_sequence <- seq(from = start_date, to = end_date, by = "hour")
date_df <- data.frame(datetime = date_sequence)
view(date_df)

df1 <- merge(date_df, df, by = "datetime", all.x = TRUE)
view(df1)
write_xlsx(x=df1, path = "경주5_수정.xlsx")
# 특정 연도만 보고싶을때
#df1<- subset(df, df$연도==2023)
dates <- df1$datetime
groundwater <- df1$`수위(해수면기준)`  # 지하수위
EC <- df1$`전기전도도(상부)`  # 강수량
groundwater <- as.numeric(groundwater)
EC <- as.numeric(EC)


#view(precipitation)
view(df1)

# 주축 (지하수위) 그래프 그리기
# 그래픽 여백 조정 (하단 여백 늘리기)
par(mar = c(5, 4, 4, 4) + 0.1)  # c(bottom, left, top, right)

# 주축 (지하수위) 그래프 그리기
plot(dates, groundwater, lwd=2, type = "l", col = "black", 
     xaxt = "n",# x축을 수동으로 설정하기 위해 xaxt = "n"
     ylim = c((min(groundwater, na.rm = TRUE)-0.1), (max(groundwater, na.rm = TRUE)+0.1)), 
     xlab = "날짜", ylab = "지하수위 (m)")

# 날짜 범위 및 축 설정
axis.POSIXct(1, at = seq(from = min(dates), to = max(dates), by = "2 days"), 
            # format = "%m-%d %H:%M",
            las=1,
            format = "%y-%m-%d",
            cex.axis = 0.8)  # 글자 크기 줄이기


target_date <- as.POSIXct("2017-11-15 14:00:00")
result_date <- as.POSIXct("2017-11-15 15:00:00")

abline(v = target_date, col = "red", lwd = 2, lty=2) 


result <- subset(df1, datetime == target_date)$ELm
result_2 <- subset(df1, datetime == result_date)$ELm

abline(h = result, col = "black", lwd = 0.9, lty=2) 
abline(h = result_2, col = "blue", lwd = 0.9, lty=2) 

total_result <- as.numeric(result)-as.numeric(result_2)
total_result


legend("bottomleft",  # 범례 위치 (대안: "topleft", "bottomright" 등)
       legend = c("지진발생시", "1시간 경과"), 
       col = c("black", "blue"),   # 선 색상
       lty = c(2, 2),                 # 선 유형
       lwd = c(0.9, 0.9)) 



#EC데이터
par(mar = c(5, 4, 4, 4) + 0.1) 
EC <- as.numeric(EC)
EC_min <- min(EC, na.rm = TRUE)-10
EC_max <- max(EC, na.rm = TRUE)+10
EC_min
EC_max
plot(dates, EC, lwd=2, type = "l", col = "blue", 
     xaxt = "n",  # x축과 y축을 새로 그리지 않도록 설정
     ylim = c(EC_min, EC_max), 
     xlab = "날짜", ylab = "EC")  # 기존 ylab을 숨기기

axis.POSIXct(1, at = seq(from = min(dates), to = max(dates), by = "2 day"), 
             format = "%m-%d %H:%M",
             las=1,
             cex.axis = 0.8)  

ECresult_date <- as.POSIXct("2017-11-15 22:00:00")
ECresult <- subset(df1, datetime == target_date)$`전기전도도(상부)`
ECresult_2 <- subset(df1, datetime == ECresult_date)$`전기전도도(상부)`

abline(h = ECresult, col = "black", lwd = 0.9, lty=2) 
abline(h = ECresult_2, col = "red", lwd = 0.9, lty=2) 
abline(v = target_date, col = "red", lwd = 2, lty=2) 


total_EC <- as.numeric(ECresult_2)-as.numeric(ECresult) 
total_EC











# 보조축 (강수량) 추가, 값을 음수로 변환하여 거꾸로 표시
par(new = TRUE)  # 새로운 그래프를 같은 창에 추가
plot(dates, precipitation, type = "h", col = "blue", 
     ylim = c(-5, 0),  # 강수량 범위를 0부터 500까지 반전
     axes = FALSE, xlab = "", ylab = "")

# 보조축 라벨 및 축 추가 (위에서 아래로 0~500으로 표시)
axis(4, at = seq(0, -10, by = -1), labels = seq(0, 10, by = 1), col = "black", col.axis = "black")
mtext("강수량 (mm)", side = 4, line = 3, col = "black")

target_date <- as.POSIXct("2017-11-15 14:00:00")
abline(v = target_date, col = "red", lwd = 2, lty=2) 

view(df)

