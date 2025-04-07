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
install.packages("anomalize")
install.packages("magick")
library(magick)
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
library(anomalize)
options(scipen = 100)

# 로우데이터 자료 정리하기----
setwd("C:/Users/EKR/Desktop/★_평가보고서 작성/")
getwd()

data <- read_xlsx(file.choose(), skip=1) #쓸데없는거 삭제(skip)
view(data)


#특정 데이터만 선택----
data <- data[,c(1,2,3,4,6,7,8,9)] #해수침투인 경우. 데이터 확인해보고 결정정
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


data4 <- data3 %>% 
  mutate(ELm   = data3$`수위(해수면기준)`)

data4<-subset(data3, data3$시간=="2")

data4 <- data4 %>% 
  mutate(`수위(해수면기준)`           = as.numeric(`수위(해수면기준)`)) %>% 
  mutate(`수온(상부)`           =  as.numeric(`수온(상부)`)) %>% 
  mutate(`수온(하부)`           =  as.numeric(`수온(하부)`)) %>% 
  mutate(`전기전도도(상부)`           =  as.numeric(`전기전도도(상부)`)) %>%
  mutate(`전기전도도(하부)`           =  as.numeric(`전기전도도(하부)`))
view(data4)
nrow(data4)

#이상치 제거(기본)----
elav <- mean(data4$`수위(해수면기준)`)
elav
# data14<-subset(data4, data4$`수위(지표면기준)`> 0 & data4$`수위(지표면기준)`<=100)
data14<-subset(data4, data4$`수온(상부)`>0 & data4$`수온(상부)`<=25)
data14<-subset(data14, data14$`전기전도도(상부)`>=0 & data14$`전기전도도(상부)`<=80000)

nrow(data4)-nrow(data14)
view(data14)

#이상치 제거(함수 적용)----
remove_outliers <- function(data4, `수위(해수면기준)`) {
  Q1 <-quantile(data4[["수위(해수면기준)"]], 0.25, na.rm=TRUE)
  Q3 <-quantile(data4[["수위(해수면기준)"]], 0.75, na.rm=TRUE)
  IQR <-Q3-Q1
  
  #하한과 상한 계산
  lower_bound <- Q1-1.5 * IQR
  upper_bound <- Q3+1.5 * IQR
  
  #이상치 제외 데이터 반환
  data4[data4[["수위(해수면기준)"]] >= lower_bound & data4[["수위(해수면기준)"]]<=upper_bound, ]
}

cdata4 <- remove_outliers(data4, "ELm")

#이상치 제거_계절성 반영----
data4_a <- data14 %>%
  time_decompose(`수위(해수면기준)`, method = "stl", frequency = "auto") %>%
  anomalize(remainder, method = "iqr") %>%
  time_recompose() %>%
  filter(anomaly == "No") # 이상치가 아닌 값만 필터링

view(data4_a)
nrow(data4) - nrow(data4_a)


data4 <- data4 %>%
  left_join(data4_a %>% dplyr::select(날짜, observed), by = "날짜") %>%
  mutate(ELm = if_else(!is.na(observed), observed, NA_real_))

view(data4)

# 차이가 0.5 이상인 값 제거----
chadata4 <- data14 %>%
  arrange(날짜) %>%  # 날짜를 기준으로 정렬
  mutate(diff = abs(`수위(해수면기준)` - lag(`수위(해수면기준)`))) %>%  # 바로 전 수위값과의 차이 계산
  filter(is.na(diff) | diff < 0.2)  # 차이가 0.2 미만인 데이터만 남기기

nrow(chadata4)-nrow(data4)
view(chadata4)

#한 번 더 제거----
chadata44 <- chadata4 %>%
  arrange(날짜) %>%  # 날짜를 기준으로 정렬
  mutate(diff = abs(`수위(해수면기준)` - lag(`수위(해수면기준)`))) %>%  # 바로 전 수위값과의 차이 계산
  filter(is.na(diff) | diff < 0.2)  # 차이가 0.2 미만인 데이터만 남기기

nrow(chadata4)-nrow(chadata44)

#data44 <- data4 %>%
#  left_join(chadata44 %>% dplyr::select(날짜, `수위(해수면기준)`) %>%
#              rename(ELm = `수위(해수면기준)`), by = "날짜")


#연속적으로 이상한 값 나오는 애들 제거함(계절성 제거로 해결)----
data4_a <- chadata44 %>%
  time_decompose(`수위(해수면기준)`, method = "stl", frequency = "auto") %>%
  anomalize(remainder, method = "iqr") %>%
  time_recompose() %>%
  filter(anomaly == "No") # 이상치가 아닌 값만 필터링

view(data4_a)
nrow(chadata44) - nrow(data4_a)


data44 <- data4 %>%
  left_join(data4_a %>% dplyr::select(날짜, observed), by = "날짜") %>%
  mutate(ELm = if_else(!is.na(observed), observed, NA_real_))

#수위값 NA인 경우 다른 측정값도 NA로 변환----
data444 <- data44 %>%
  mutate(
    `수온(상부)` = if_else(is.na(ELm), NA_real_, `수온(상부)`),
    `수온(하부)` = if_else(is.na(ELm), NA_real_, `수온(하부)`),
    `전기전도도(상부)` = if_else(is.na(ELm), NA_real_, `전기전도도(상부)`),
    `전기전도도(하부)` = if_else(is.na(ELm), NA_real_, `전기전도도(하부)`)
  )

# 결과 확인----
view(data444)

#중복데이터 제거하기
data5 <- data444 %>%
  distinct(날짜, .keep_all=TRUE)
view(data5)
# 강수량 자료 합치기----
aosrain <- read_excel(file.choose(), 
                      col_names = c("지점", "지점명", "날짜", "강수량"), skip=1)
str(aosrain)
aosrain <-  aosrain %>% 
  mutate(날짜    = ymd(날짜))

view(aosrain)
#  강우자료 많을 시 추출 
rain<-subset(aosrain, aosrain$지점명=="강릉")

view(rain)
df <- merge(data5,rain,by="날짜", all.x=TRUE)
df <- dplyr::select(.data = df, -지점, -지점명)

view(df)
view(rain)


# 시각화----
#원하는 날짜와 시간 설정
#포항
start_date <- as.Date("1900-01-01")
end_date <- as.Date("2024-11-30")
date_sequence <- seq(from = start_date, to = end_date, by = "day")
date_df <- data.frame(날짜 = date_sequence)

start_groundwater <- min(data5$날짜)
start_groundwater
fdate_df <- subset(date_df, date_df$날짜>=start_groundwater)

#data5 <- merge(fdate_df, data5, by = "날짜", all.x = TRUE)

df1 <- merge(fdate_df, df, by = "날짜", all.x = TRUE)
view(df1)
write_xlsx(x=df1, path = "연곡1_수정.xlsx")
# 특정 연도만 보고싶을때
#df1<- subset(df1, df1$연도==2023)
dates <- df1$날짜
groundwater <- df1$ELm
TEMP1 <- df1$`수온(상부)`
TEMP2 <- df1$`수온(하부)`
EC1 <- df1$`전기전도도(상부)` 
EC2 <- df1$`전기전도도(하부)`
precipitation <- df1$강수량

length(dates)
length(groundwater)

groundwater <- as.numeric(groundwater)
TEMP1 <- as.numeric(TEMP1)
TEMP2 <- as.numeric(TEMP2)
EC1 <- as.numeric(EC1)
EC2 <- as.numeric(EC2)
class(dates)
class(groundwater)
#view(precipitation)

min(na.omit(groundwater))
# 주축 (지하수위) 그래프 그리기
png("C:/Users/EKR/Desktop/graph/graph1.png", width = 635, height = 300)

par(mar = c(4, 4, 2, 4) + 0.1,    # 네모박스의 바깥 여백 설정 (상, 좌, 하, 우)
    xaxs = "i",                    # x축 여백 제거
    yaxs = "i")                    # y축 여백 제거

# 그래프 작성하기기
a<- min(dates)
b<- max(dates)
year_start <- as.Date(paste(format(a, "%Y"), "-01-01", sep=""))
year_start
year_end <- as.Date(paste(format(b, "%Y"), "-01-01", sep=""))
year_end

plot(dates, groundwater, lwd=2, type="l", col="black", 
     xaxt="n", ylim=c((min(groundwater, na.rm=TRUE)-1), (max(groundwater, na.rm=TRUE)+1)), 
     xlab="", ylab="Level (EL.m)", xlim=c(year_start, b))

axis.Date(1, at = seq(from = year_start, to = year_end, by = "2 years"), format = "%y/%m/%d")
abline(v = seq(from = year_start, to = year_end, by = "2 years"), col="black", lty=1)

# 보조축 (강수량) 추가, 값을 음수로 변환하여 거꾸로 표시
par(new = TRUE)  # 새로운 그래프를 같은 창에 추가
plot(dates, -precipitation, type = "h", col = "black", 
     ylim = c(-600, 0),  # 강수량 범위를 0부터 500까지 반전
     axes = FALSE, xlab = "", ylab = "")

#보조축 라벨 및 축 추가 (위에서 아래로 0~500으로 표시)
axis(4, at = seq(0, -600, by = -100), labels = seq(0, 600, by = 100), col = "black", col.axis = "black")
mtext("Rainfall (mm)", side = 4, line = 3, col = "black")

dev.off()


# 주축 (EC1,2) 그래프 그리기
png("C:/Users/EKR/Desktop/graph/graph2.png", width = 635, height = 300)
par(mar = c(4, 4, 2, 4) + 0.1,    # 네모박스의 바깥 여백 설정 (상, 좌, 하, 우)
    xaxs = "i",                    # x축 여백 제거
    yaxs = "i")                    # y축 여백 제거

# EC1, 2
plot(dates, EC1, lwd=2, type="l", col="black", 
     xaxt="n", ylim=c((min(EC1, na.rm=TRUE)-5000), (max(EC1, na.rm=TRUE)+10000)), 
     xlab="", ylab="EC (μS/㎝)", xlim=c(year_start, b))
lines(dates, EC2, lwd=2, col="red")
axis.Date(1, at = seq(from = year_start, to = year_end, by = "2 years"), format = "%y/%m/%d")
abline(v = seq(from = year_start, to = year_end, by = "2 years"), col="black", lty=1)


# 보조축 (강수량) 추가, 값을 음수로 변환하여 거꾸로 표시
par(new = TRUE)  # 새로운 그래프를 같은 창에 추가
plot(dates, -precipitation, type = "h", col = "black", 
     ylim = c(-600, 0),  # 강수량 범위를 0부터 500까지 반전
     axes = FALSE, xlab = "", ylab = "")

#보조축 라벨 및 축 추가 (위에서 아래로 0~500으로 표시)
axis(4, at = seq(0, -600, by = -100), labels = seq(0, 600, by = 100), col = "black", col.axis = "black")
mtext("Rainfall (mm)", side = 4, line = 3, col = "black")

dev.off()
# 주축 (수온) 그래프 그리기
png("C:/Users/EKR/Desktop/graph/graph3.png", width = 635, height = 300)
par(mar = c(4, 4, 2, 4) + 0.1,    # 네모박스의 바깥 여백 설정 (상, 좌, 하, 우)
    xaxs = "i",                    # x축 여백 제거
    yaxs = "i")                    # y축 여백 제거


# 수온
plot(dates, TEMP1, lwd=2, type="l", col="black", 
     xaxt="n", ylim=c((min(TEMP1, na.rm=TRUE)-1), (max(TEMP1, na.rm=TRUE)+1)), 
     xlab="", ylab="TEMP (°C)", xlim=c(year_start, b))
#lines(dates, TEMP2, lwd=2, col="red")
axis.Date(1, at = seq(from = year_start, to = year_end, by = "2 years"), format = "%y/%m/%d")
abline(v = seq(from = year_start, to = year_end, by = "2 years"), col="black", lty=1)


# 보조축 (강수량) 추가, 값을 음수로 변환하여 거꾸로 표시
par(new = TRUE)  # 새로운 그래프를 같은 창에 추가
plot(dates, -precipitation, type = "h", col = "black", 
     ylim = c(-600, 0),  # 강수량 범위를 0부터 500까지 반전
     axes = FALSE, xlab = "", ylab = "")

#보조축 라벨 및 축 추가 (위에서 아래로 0~500으로 표시)
axis(4, at = seq(0, -600, by = -100), labels = seq(0, 600, by = 100), col = "black", col.axis = "black")
mtext("Rainfall (mm)", side = 4, line = 3, col = "black")

dev.off()

#이미지 저장
# 이미지 불러오기
graph1 <- image_read("C:/Users/EKR/Desktop/graph1.png")
graph2 <- image_read("C:/Users/EKR/Desktop/graph2.png")
graph3 <- image_read("C:/Users/EKR/Desktop/graph3.png")

# 세로로 연결
combined <- image_append(c(graph1, graph2, graph3), stack = TRUE)

# 결과 이미지 저장
image_write(combined, "YG1.png")
