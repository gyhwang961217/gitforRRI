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
library(magick)
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
library(trend)
library(MASS)
library(openxlsx)
options(scipen = 100)

# 01_결측값 찾기----
#특정 연도의 결측값(NA) 개수를 세도록 하는 반복문
setwd("C:/Users/EKR/Desktop/★_평가보고서 작성/" )
getwd()

# 엑셀 파일 목록 가져오기
dtest <- read.table("staion_농촌2.txt",header = T,fileEncoding = "euc-kr")
STA1 <- dtest$STAID

# 결과를 저장할 리스트 초기화
na_counts <- list()  # 관측소별 NA/NULL 값을 저장할 리스트

for (i in seq_along(STA1)) {  # STA1의 길이에 따라 반복
  cat("처리 중:", STA1[i], "\n")
  fname1 <- paste("C:/Users/EKR/Desktop/★_평가보고서 작성/data_resource/", STA1[i], ".xlsx", sep = "")
  
  # 파일 존재 여부 확인
  if (file.exists(fname1)) {
    data <- read_xlsx(fname1, skip = 1)
    
    # 측정일자와 시간 분리
    data <- data %>%
      mutate(
        측정일자 = as.character(측정일자),
        일자 = substr(측정일자, 1, 8),
        시간 = as.numeric(substr(측정일자, 9, 10))
      ) %>%
      filter(grepl("^[0-9]{8}$", 일자)) %>%  # 일자 형식 검증
      mutate(날짜 = ymd(일자)) %>%          # ymd 변환
      filter(!is.na(날짜))                  # NA 제거
    
    # 나머지 처리 계속 진행
    data <- data %>%
      mutate(
        ELm = as.numeric(`수위(해수면기준)`),
        `수온(상부)` = as.numeric(`수온(상부)`),
        `전기전도도(상부)` = as.numeric(`전기전도도(상부)`)
      ) %>%
      filter(
        `수온(상부)` > 0 & `수온(상부)` <= 25,
        `전기전도도(상부)` >= 10 & `전기전도도(상부)` <= 80000
      )
    
    data <- subset(data, data$시간=="12")  #2시는 없는 데이터 많아서..
    
    # 날짜 데이터 생성 및 병합
    start_date <- as.Date("1900-01-01")
    end_date <- as.Date("2024-11-30")
    date_sequence <- seq(from = start_date, to = end_date, by = "day")
    date_df <- data.frame(날짜 = date_sequence)
    start_groundwater <- min(data3$날짜, na.rm = TRUE)
    fdate_df <- subset(date_df, 날짜 >= start_groundwater)
    
    data4 <- merge(fdate_df, data, by = "날짜", all.x = TRUE)
    
    # 특정 연도의 NA/NULL 값 확인
    year <- 2024
    filtered_data <- data4[year(data4$날짜) == year, ]
    na_or_null_count <- sum(is.na(filtered_data$ELm) | is.null(filtered_data$ELm))
    
    # 결과 저장
    na_counts[[STA1[i]]] <- na_or_null_count  # 리스트에 추가
  }
  write_xlsx(data4, paste0("C:/Users/EKR/Desktop/★_평가보고서 작성/testttt/",STA1[i], ".xlsx"))
}

# 반복문 종료 후 결과를 데이터프레임으로 변환
na_counts_df <- data.frame(
  관측소 = names(na_counts),
  NA_NULL_개수 = unlist(na_counts)
)

view(na_counts_df)
# 결과를 엑셀 파일로 저장
write.xlsx(na_counts_df, "C:/Users/EKR/Desktop/★_평가보고서 작성/결측값처리12시.xlsx", rowNames = FALSE)

message("모든 작업이 완료되었습니다!")

#빠진 데이터 확인
dtest <- read.table("staion_농촌.txt",header = T,fileEncoding = "euc-kr")
STA1 <- dtest$STAID

observatory_names <- na_counts_df$관측소
missing_stations <- STA1[!STA1 %in% observatory_names]
view(missing_stations)
#--------------------------