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

# 로우데이터 SGI 자료 정리하기----
data <- read_xlsx(file.choose(), skip=1) #쓸데없는거 삭제(skip)
view(data)


#특정 데이터만 선택
data <- data[,c(1,2,3)]
view(data)

#날짜 구분
data1 <- data %>% 
  mutate(측정일자           = as.character(측정일자)) %>% 
  mutate(측정일자           = substr(측정일자, 1, 8))

data2 <- data1 %>%
  mutate(날짜           = ymd(측정일자)) %>% 
  mutate(연도             = year(날짜),
         월           = month(날짜),
         일           = day(날짜))

data3<- data2 %>%
  mutate(일별     = format(날짜, "%m-%d"))


data4 <- data3 %>% 
  mutate(ELm   = data3$`수위(해수면기준)`)


#무조건 처음부터 시작(SGI 산정을 위함)
start_date <- as.Date("1900-01-01")
end_date <- as.Date("2023-12-31")
date_sequence <- seq(from = start_date, to = end_date, by = "day")
date_df <- data.frame(날짜 = date_sequence)

start_groundwater <- min(data4$날짜)
fdate_df <- subset(date_df, date_df$날짜 >= start_groundwater)
data4 <- merge(fdate_df, data4, by = "날짜", all.x = TRUE)

view(data4)
#이동평균 구하기(5일)
omitlevel <- as.numeric(na.omit(data4$ELm))
ml <- rollmean(omitlevel, k = 5, fill = NA, align = "right")

# 원본 data4$ELm에서 NA가 아닌 인덱스에만 이동평균 값을 할당
non_na_indices <- which(!is.na(data4$ELm))
data4$이동평균[non_na_indices] <- ml[1:length(non_na_indices)]

#na값 앞뒤로 보간(너무 긴 결측만 빼고 보간)
data4$이동평균 <- na.approx(data4$이동평균, maxgap= 3, na.rm = FALSE)
view(data4)


# 자료 형식 정리하기----
#pivot_data <- data4 %>%
#  dplyr::select(일별, 연도, 이동평균) %>%
#  pivot_wider(names_from = 연도, values_from = 이동평균) #오류뜨면 중복값 등 검수

pivot_data <- data4 %>%
  dplyr::select(일별, 연도, 이동평균) %>%
  pivot_wider(names_from = 연도, values_from = 이동평균, values_fn = list)

view(pivot_data)

#pivot_Data<- pivot_data[, -ncol(pivot_data)]
pivot_Data<- pivot_data[, -3]


pivot_Data_end <- pivot_Data %>%
  arrange(일별)

#2월 29일 지워버리기
pivot_Data_end <- pivot_Data_end[-60,]
pivot_Data_end <- pivot_Data_end[-366,]
view(pivot_Data_end)
str(pivot_Data_end)

#필요시 사용
#pivot_Data_end <- pivot_Data_end[, names(pivot_Data_end) != "NA"]
#view(pivot_Data_end)


# 커널 밀도 함수 추정 및 cdf 구하기

#함수 정의
calculate_cdf <- function(row) {
  # 첫 번째 열과 마지막 열을 제외하고 숫자형으로 변환
  gw_levels <- as.numeric(unlist(row[-c(1, length(row))]))
  # KDE 수행
  density_est <- density(gw_levels, bw=0.8, n=100, na.rm = TRUE)
  # 누적 밀도 함수 생성
  density_cdf <- approxfun(density_est$x, 
                           cumsum(density_est$y) / sum(density_est$y))
  # 마지막 열 값 추출(2023년_아니면 다른걸로 선택)
  last_value <- as.numeric(row[length(row)])
  # 마지막 열 값을 누적 분포 함수에 적용하여 백분위수 계산
  return(density_cdf(last_value))
}



#is.data.frame(pivot_Data_end)
#str(pivot_Data_end)
#sapply(pivot_Data_end, class)
#오류가 나는 이유는 날짜 등에 중복값이 있기 때문이므로, 삭제 후 다시 분석할 것.

pivot_Data_end$CDF <- apply(pivot_Data_end, 1, calculate_cdf)
view(pivot_Data_end)

# SGI 수치 구하기
pivot_Data_end$SGI <- qnorm(pivot_Data_end$CDF)
view(pivot_Data_end)


setwd("C:/Users/EKR/Desktop/할이리/충남_240902/SGI/")
write_xlsx(x=pivot_Data_end, path = "홍성2.xlsx")


# 강수량 자료 합치기----
aosrain <- read_excel(file.choose(), 
                      col_names = c("지점", "지점명", "날짜", "강수량"), skip=1)
str(aosrain)
aosrain <-  aosrain %>% 
  mutate(날짜    = ymd(날짜))

view(aosrain)
#  강우자료 많을 시 추출 
rain<-subset(aosrain, aosrain$지점명=="광주")
df <- merge(data4,rain,by="날짜", all.x=TRUE)
df <- dplyr::select(.data = df, -지점, -지점명)


write_xlsx(x=df, path = "data.xlsx")

# 시각화----
#원하는 날짜 설정
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-31")
date_sequence <- seq(from = start_date, to = end_date, by = "day")
date_df <- data.frame(날짜 = date_sequence)
view(date_df)

df <- merge(date_df, df, by = "날짜", all.x = TRUE)

# 특정 연도만 보고싶을때
df1<- subset(df, df$연도==2023)
dates <- df1$날짜
groundwater <- df1$이동평균  # 지하수위
precipitation <- df1$강수량  # 강수량
SGI <- pivot_Data_end$SGI
#SGI <- pivot_Data_end$SGI[1:212]

#설정 기간(원하는 날짜)만 보고싶을 때
dates <- df$날짜
groundwater <- df$이동평균  # 지하수위
precipitation <- df$강수량  # 강수량
SGI <- pivot_Data_end$SGI
view(SGI)
view(dates)
view(groundwater)
view(precipitation)
# 그래프 창을 설정하여 주축과 보조축을 따로 설정
par(mar = c(4, 4, 2, 4) + 0.1,    # 네모박스의 바깥 여백 설정 (상, 좌, 하, 우)
    xaxs = "i",                    # x축 여백 제거
    yaxs = "i")                    # y축 여백 제거


# 주축 (지하수위) 그래프 그리기
plot(dates, groundwater, lwd=2, type = "l", col = "black", 
     xaxt = "n",
     ylim = c((min(groundwater, na.rm = TRUE)-5), (max(groundwater, na.rm = TRUE)+2)), 
     xlab = "날짜", ylab = "지하수위 (m)")
a<- min(dates)
b<- max(dates)
axis.Date(1, at = seq(from = as.Date(a), to = as.Date(b), by = "1 months"), format = "%y-%m")
# 보조축 (강수량) 추가, 값을 음수로 변환하여 거꾸로 표시
par(new = TRUE)  # 새로운 그래프를 같은 창에 추가
plot(dates, -precipitation, type = "h", col = "blue", 
     ylim = c(-600, 0),  # 강수량 범위를 0부터 500까지 반전
     axes = FALSE, xlab = "", ylab = "")

# 보조축 라벨 및 축 추가 (위에서 아래로 0~500으로 표시)
#axis(4, at = seq(0, -600, by = -100), labels = seq(0, 600, by = 100), col = "black", col.axis = "black")
#mtext("강수량 (mm)", side = 4, line = 3, col = "black")



#SGI 추가----
par(new = TRUE)
plot(dates, SGI,lwd=2, type = "l", col = "red", 
     xaxt = "n",
     ylim = c(-4,4), axes = FALSE,
     xlab = "", ylab = "")
#SGI 라벨 추가
axis(4, at = seq(-4, 4, by =1), labels = seq(-4,4, by = 1), col = "black", col.axis = "black")
mtext("SGI", side = 4, line = 3, col = "black")
# 범례 추가
legend("bottomright", legend = c("지하수위", "강수량", "SGI"), col = c("black", "blue", "red"), lty = 1)


#SGI 시각화하기----
testdata<- pivot_Data_end
view(testdata)
testdata[, 2:8] <- as.data.frame(testdata[, 2:8])
testdata[, 2:8] <- data.frame(lapply(testdata[, 2:8], function(x) as.numeric(as.character(x))))

testdata$최솟값 <- apply(testdata[, 2:7], 1, min, na.rm = TRUE)
testdata$최댓값 <- apply(testdata[, 2:7], 1, max, na.rm=TRUE)
view(testdata)
# 5% 값 계산
testdata$주의상한 <- as.numeric(testdata$최솟값 + 0.242 * (testdata$최댓값 - testdata$최솟값)) #24.2% 이상
testdata$주의하한 <- as.numeric(testdata$최솟값 + 0.1295 * (testdata$최댓값 - testdata$최솟값)) #12.95%~24.2%
testdata$경계상한 <- as.numeric(testdata$최솟값 + 0.054 * (testdata$최댓값 - testdata$최솟값)) #5.4%~12.95%
testdata$경계하한 <- as.numeric(testdata$최솟값 + 0.05 * (testdata$최댓값 - testdata$최솟값)) #5.4%이하


view(testdata)
testdata <- testdata %>%
  mutate(일자별 = as.Date(paste("2023", 일별), format = "%Y %m-%d"))

testdata <- testdata %>%
  mutate(max  = 일자별 + 1)

view(testdata)
str(testdata)
view(data4)
# ggplot으로 시각화
data5 <- na.omit(data4)
view(data5)
mina<-min(data5$이동평균)
maxa<-max(data5$이동평균)
mina
maxa
p <- ggplot(testdata, aes(x = 일자별, y = `2023`, group = 1))+
  geom_line(color = "black", size = 1, na.rm = TRUE)+  # 분위수 꺾은선 그래프
  geom_rect(aes(xmin = 일자별, xmax = max, ymin = 주의상한, ymax = 최댓값), 
            fill = "darkblue", alpha = 0.2, na.rm = TRUE) +  
  geom_rect(aes(xmin = 일자별, xmax = max, ymin = 주의하한, ymax = 주의상한), 
            fill = "green", alpha = 0.2, na.rm = TRUE) +
  geom_rect(aes(xmin = 일자별, xmax = max, ymin = 경계하한, ymax = 주의하한), 
            fill = "orange", alpha = 0.2, na.rm = TRUE) +  
  geom_rect(aes(xmin = 일자별, xmax = max, ymin = 최솟값, ymax = 경계하한), 
            fill = "red", alpha = 0.2, na.rm = TRUE) +  
  labs(title = "일자별 지하수위 가뭄분석",
       x = "일자",
       y = "지하수위(El.m)") +
  scale_y_continuous(limits = c(mina, maxa))  +
  scale_x_date(breaks="2 months", expand = c(0,0), labels = date_format("%Y-%b"))           +
  theme_minimal()+  theme(plot.title=element_text(hjust=0.5))


p

setwd("C:/Users/EKR/Desktop/KRC/04_관측자료분석(자원관리실무)/")
write_xlsx(x=testdata, path = "용인6pivot.xlsx")



