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
library(gridExtra)
options(scipen = 100)

setwd("C:/Users/EKR/Desktop/★_평가보고서 작성/" )
getwd()
#첫 값이 0이거나 이상하면 전체적으로 오류가 날 수 있음. 확인이 필요함
#그러기 위해서, 이동평균을 계산할 필요가 있음.
# 관측소 이름을 메모장에서 읽어오기
dtest <- read.table("staion_농촌2.txt", header = T, fileEncoding = "euc-kr")
STA1 <- dtest$STAID
rain_stations <- dtest$강수  # 강수량 데이터를 가져올 기상 관측소 이름

# 결과를 저장할 리스트 생성
result_list <- list()

# 각 관측소에 대해 데이터 처리 반복
for (i in seq_along(STA1)) {  # STA1의 길이에 따라 반복
  cat("처리 중:", STA1[i], "\n")
  
  # 관측소에 맞는 파일을 불러오기
  file_path <- paste("C:/Users/EKR/Desktop/★_평가보고서 작성/data_resource/", STA1[i], ".xlsx", sep = "")  
  data <- read_xlsx(file_path, skip = 1)  # 파일 불러오기
  
  # 데이터 정리 과정 (이전에 작성한 코드를 반복)
  data <- data[, c(1, 2, 3, 4, 6, 7)]  # 필요한 열만 선택
  
  # 날짜 구분
  data <- data %>%
    mutate(
      측정일자 = as.character(측정일자),
      일자 = substr(측정일자, 1, 8),
      시간 = as.numeric(substr(측정일자, 9, 10))
    ) %>%
    filter(grepl("^[0-9]{8}$", 일자)) %>%  # 일자 형식 검증
    mutate(날짜 = ymd(일자)) %>%          # ymd 변환
    filter(!is.na(날짜))                  # NA 제거
  
  
  data <- subset(data, data$시간=="12")
  
  # 나머지 처리 계속 진행
  data <- data %>%
    mutate(
      `수위(해수면기준)` = as.numeric(`수위(해수면기준)`),
      `수온(상부)` = as.numeric(`수온(상부)`),
      `전기전도도(상부)` = as.numeric(`전기전도도(상부)`),
      연도               = year(날짜)
    ) %>%
    filter(
      `수온(상부)` > 2 & `수온(상부)` <= 25,
      `전기전도도(상부)` >= 10 & `전기전도도(상부)` <= 80000,
      `수위(지표면기준)` > 0
    )
  

  # 이상치 함수 적용
  chadata4 <- data %>%
    arrange(날짜) %>%
    mutate(
      keep = {
        ref <- NA_real_  # ref를 숫자형 NA로 초기화
        sapply(seq_along(`수위(해수면기준)`), function(i) {
          current <- `수위(해수면기준)`[i]
          if (is.na(current)) {
            return(FALSE)  # NA는 FALSE로 처리
          }
          if (is.na(ref) || abs(current - ref) <= 5) {
            ref <<- current  # ref를 갱신
            return(TRUE)     # 조건을 충족하면 TRUE 반환
          } else {
            return(FALSE)    # 조건을 충족하지 않으면 FALSE 반환
          }
        })
      }
    ) %>%
    filter(keep) %>%
    dplyr::select(-keep)
  
  
  data44 <- data %>%
    left_join(chadata4 %>%
                dplyr::select(날짜, `수위(해수면기준)`) %>%
                rename(ELm = `수위(해수면기준)`), by = "날짜")
  
  
  # 수위값 NA인 경우 다른 측정값도 NA로 변환
  data444 <- data44 %>%
    mutate(
      `수온(상부)` = if_else(is.na(ELm), NA_real_, `수온(상부)`),
      `전기전도도(상부)` = if_else(is.na(ELm), NA_real_, `전기전도도(상부)`)
    )
  
  # 중복 데이터 제거
  data5 <- data444 %>%
    distinct(날짜, .keep_all = TRUE)
  
  # 강수량 자료 합치기: 각 관측소에 맞는 기상관측소 이름을 이용
  rain_station <- rain_stations[STA1 == STA1[i]]  # 현재 관측소에 해당하는 강수량 관측소 이름
  aosrain <- read_excel("강수량_2024까지최종.xlsx", col_names = c("지점", "지점명", "날짜", "강수량"), skip = 1)
  aosrain <- aosrain %>%
    mutate(날짜 = ymd(날짜))
  
  rain <- subset(aosrain, aosrain$지점명 == rain_station)
  df <- merge(data5, rain, by = "날짜", all.x = TRUE)
  df <- dplyr::select(.data = df, -지점, -지점명)
  
  # 날짜와 시간 설정
  start_date <- as.Date("1900-01-01")
  end_date <- as.Date("2024-11-30")
  date_sequence <- seq(from = start_date, to = end_date, by = "day")
  date_df <- data.frame(날짜 = date_sequence)
  start_groundwater <- min(data5$날짜)
  fdate_df <- subset(date_df, date_df$날짜 >= start_groundwater)
  
  df1 <- merge(fdate_df, df, by = "날짜", all.x = TRUE)
  
  # 연도별 평균 계산 및 새로운 데이터프레임 생성
  result_df <- data.frame(
    관정명 = STA1[i], # 현재 관측소 이름
    지하수위_전체_평균 = mean(df1 %>% filter(연도 != 2024) %>% pull(ELm), na.rm = TRUE),
    지하수위_2024년_평균 = mean(df1 %>% filter(연도 == 2024) %>% pull(ELm), na.rm = TRUE), 
    지하수위_2024년_최대 = max(df1 %>% filter(연도 == 2024) %>% pull(ELm), na.rm = TRUE),
    지하수위_2024년_최소 = min(df1 %>% filter(연도 == 2024) %>% pull(ELm), na.rm = TRUE),
    지하수위_차이 = mean(df1 %>% filter(연도 != 2024) %>% pull(ELm), na.rm = TRUE) - mean(df1 %>% filter(연도 == 2024) %>% pull(ELm), na.rm = TRUE),
    전기전도도_전체_평균 = mean(df1 %>% filter(연도 != 2024) %>% pull(`전기전도도(상부)`), na.rm = TRUE),
    전기전도도_2024년_평균 = mean(df1 %>% filter(연도 == 2024) %>% pull(`전기전도도(상부)`), na.rm = TRUE)
  )
  
  # 결과 리스트에 추가
  result_list[[STA1[i]]] <- result_df
  
  write_xlsx(df1, paste0("C:/Users/EKR/Desktop/★_평가보고서 작성/농촌_장기관측 추세분석/",STA1[i], ".xlsx"))
}

# 모든 관측소의 결과를 하나로 합치기
final_result <- bind_rows(result_list)
view(final_result)
# 결과를 엑셀 파일로 저장
write_xlsx(final_result, "전체_단계구분_결과1111.xlsx")

#2024년 필터링 문제 확인
#전기전도도 전체평균 대비 현재평균 차이 확인
#변동폭 분석(수위 변동 폭)
#전기전도도, SAR 알칼리도 어쩌고

#해수의 경우, 몰비와 파이퍼 다이아그램