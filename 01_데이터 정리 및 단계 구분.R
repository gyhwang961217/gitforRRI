setwd("C:/Users/EKR/Desktop/★_평가보고서 작성/" )
getwd()

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
  data1 <- data %>%
    mutate(측정일자 = as.character(측정일자)) %>%
    mutate(일자 = substr(측정일자, 1, 8)) %>%
    mutate(시간 = as.numeric(substr(측정일자, 9, 10)))
  
  data2 <- data1 %>%
    mutate(날짜 = ymd(일자)) %>%
    mutate(연도 = year(날짜), 월 = month(날짜), 일 = day(날짜))
  
  data3 <- data2 %>%
    mutate(일별 = format(날짜, "%m-%d"))
  
  data4 <- data3 %>%
    mutate(ELm = `수위(해수면기준)`)
  
  data4 <- subset(data3, data3$시간 == "2")
  
  data4 <- data4 %>%
    mutate(`수위(해수면기준)` = as.numeric(`수위(해수면기준)`)) %>%
    mutate(`수온(상부)` = as.numeric(`수온(상부)`)) %>%
    mutate(`전기전도도(상부)` = as.numeric(`전기전도도(상부)`))
  
  # 이상치 제거
  data14 <- subset(data4, data4$`수온(상부)` > 0 & data4$`수온(상부)` <= 25)
  data14 <- subset(data14, data14$`전기전도도(상부)` >= 0 & data14$`전기전도도(상부)` <= 80000)
  
  # 이상치 함수 적용
  chadata4 <- data14 %>%
    arrange(날짜) %>%
    mutate(
      keep = {
        ref <- NA
        sapply(seq_along(`수위(해수면기준)`), function(i) {
          if (is.na(ref) || abs(`수위(해수면기준)`[i] - ref) <= 2) {
            ref <<- `수위(해수면기준)`[i]
            TRUE
          } else {
            FALSE
          }
        })
      }
    ) %>%
    filter(keep) %>%
    dplyr::select(-keep)
  
  data44 <- data4 %>%
    left_join(chadata4 %>% dplyr::select(날짜, `수위(해수면기준)`) %>%
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
    관정명 = STA1[i],  # 현재 관측소 이름
    전기전도도_2024년_평균 = mean(df1 %>% filter(연도 == 2024) %>% pull(`전기전도도(상부)`), na.rm = TRUE),
    지하수위_전체_평균 = mean(df1$ELm, na.rm = TRUE),
    지하수위_2024년_평균 = mean(df1 %>% filter(연도 == 2024) %>% pull(ELm), na.rm = TRUE),
    전체_평균_2024년_평균 = mean(df1$ELm, na.rm = TRUE) - mean(df1 %>% filter(연도 == 2024) %>% pull(ELm), na.rm = TRUE)
  )
  
  # 결과 리스트에 추가
  result_list[[STA1[i]]] <- result_df
}

# 모든 관측소의 결과를 하나로 합치기
final_result <- bind_rows(result_list)

# 결과를 엑셀 파일로 저장
write_xlsx(final_result, "전체_결과.xlsx")

