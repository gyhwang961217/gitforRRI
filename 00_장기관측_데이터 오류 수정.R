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
data <- data[,c(1,2,3,4,6,7)] #해수침투인 경우. 데이터 확인해보고 결정정
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
  mutate(`전기전도도(상부)`           =  as.numeric(`전기전도도(상부)`))
view(data4)
nrow(data4)

#이상치 제거(기본)----
elav <- mean(data4$`수위(해수면기준)`)
elav
# data14<-subset(data4, data4$`수위(지표면기준)`> 0 & data4$`수위(지표면기준)`<=100)
data14<-subset(data4, data4$`수온(상부)`>0 & data4$`수온(상부)`<=25)
data14<-subset(data14, data14$`전기전도도(상부)`>=0 & data14$`전기전도도(상부)`<=80000)

nrow(data4)- nrow(data14)
view(data14)

#이상치 제거(함수 적용)----

chadata4 <- data14 %>%
  arrange(날짜) %>%  # 날짜 순으로 정렬
  mutate(
    keep = {
      ref <- NA  # 기준 값 초기화
      sapply(seq_along(`수위(해수면기준)`), function(i) {
        if (is.na(ref) || abs(`수위(해수면기준)`[i] - ref) <= 2) {
          ref <<- `수위(해수면기준)`[i]  # 기준 값 업데이트
          TRUE  # 정상 값 유지
        } else {
          FALSE  # 비정상 값 제거
        }
      })
    }
  ) %>%
  filter(keep) %>%  # 정상 값만 유지
  dplyr::select(-keep)  # 보조 열 제거


view(chadata4)
nrow(chadata4)-nrow(data14)

data44 <- data4 %>%
  left_join(chadata4 %>% dplyr::select(날짜, `수위(해수면기준)`) %>%
              rename(ELm = `수위(해수면기준)`), by = "날짜")

view(data44)

#수위값 NA인 경우 다른 측정값도 NA로 변환----
data444 <- data44 %>%
  mutate(
    `수온(상부)` = if_else(is.na(ELm), NA_real_, `수온(상부)`),
   # `수온(하부)` = if_else(is.na(ELm), NA_real_, `수온(하부)`),
    `전기전도도(상부)` = if_else(is.na(ELm), NA_real_, `전기전도도(상부)`),
  # `전기전도도(하부)` = if_else(is.na(ELm), NA_real_, `전기전도도(하부)`)
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
rain<-subset(aosrain, aosrain$지점명=="충주")

view(rain)
df <- merge(data5,rain,by="날짜", all.x=TRUE)
df <- dplyr::select(.data = df, -지점, -지점명)

view(df)
view(rain)

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

view(df1)

# 연도별 평균 계산 및 새로운 데이터프레임 생성
# 연도별 평균 계산 및 새로운 데이터프레임 생성
result_df <- data.frame(
  관정명 = df1$관정명[1],  # 첫 번째 관측정 이름 (수동 입력)
  전기전도도_2024년_평균 = mean(df1 %>% filter(year(날짜) == 2024) %>% pull(`전기전도도(상부)`), na.rm = TRUE),  # 2024년 전기전도도 평균
  지하수위_전체_평균 = mean(df1$ELm, na.rm = TRUE),  # 전체 지하수위 평균
  지하수위_2024년_평균 = mean(df1 %>% filter(year(날짜) == 2024) %>% pull(ELm), na.rm = TRUE),  # 2024년 지하수위 평균
  전체_평균_2024년_평균 = mean(df1$ELm, na.rm = TRUE) - mean(df1 %>% filter(year(날짜) == 2024) %>% pull(ELm), na.rm = TRUE)  # 전체 평균 - 2024년 평균
)

# 결과 확인
view(result_df)





#시각화----
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
     xaxt="n", ylim=c((min(EC1, na.rm=TRUE)-200), (max(EC1, na.rm=TRUE)+200)), 
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
graph1 <- image_read("C:/Users/EKR/Desktop/graph/graph1.png")
graph2 <- image_read("C:/Users/EKR/Desktop/graph/graph2.png")
graph3 <- image_read("C:/Users/EKR/Desktop/graph/graph3.png")

# 세로로 연결
combined <- image_append(c(graph1, graph2, graph3), stack = TRUE)

# 결과 이미지 저장
image_write(combined, "gs1.png")
