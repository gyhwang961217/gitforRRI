# 01_결측값 찾기----
# 작업 경로 설정
setwd("C:/Users/EKR/Desktop/★_평가보고서 작성/")
getwd()

# 엑셀 파일 목록 가져오기
dtest <- read.table("지하수위변동분석_202개관측소.txt", header = TRUE, fileEncoding = "euc-kr")
STA1 <- dtest$STAID
control_stations <- dtest$관리본부

# 결과를 저장할 리스트 생성
result_list <- list()


# 각 관측소에 대해 데이터 처리 반복
for (i in seq_along(STA1)) {  # STA1의 길이에 따라 반복
  cat("처리 중:", STA1[i], "\n")
  
  # 관측소에 맞는 파일을 불러오기
  file_path <- paste("C:/Users/EKR/Desktop/★_평가보고서 작성/data_resource/", STA1[i], ".xlsx", sep = "")  
  data <- read_xlsx(file_path, skip = 1)  # 파일 불러오기
  
  # 데이터 정리 과정
  data <- data[, c(1, 2, 3, 4, 6, 7)] 
  data <- data %>%
    mutate(
      측정일자 = as.character(측정일자),
      날짜 = ymd(substr(측정일자, 1, 8)),
      연도 = year(날짜),
      월 = month(날짜),
      시간 = as.numeric(substr(측정일자, 9, 10))
    ) %>%
    filter(!is.na(날짜), 시간 == 12)  # 정오 데이터 필터링
  
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
  
  # 이상치 제거
  data <- data %>%
    arrange(날짜) %>%
    mutate(
      keep = {
        ref <- NA_real_
        sapply(seq_along(`수위(해수면기준)`), function(j) {
          current <- `수위(해수면기준)`[j]
          if (is.na(current)) return(FALSE)
          if (is.na(ref) || abs(current - ref) <= 5) {
            ref <<- current
            return(TRUE)
          } else {
            return(FALSE)
          }
        })
      }
    ) %>%
    filter(keep) %>%
    dplyr::select(-keep)
  
  # 날짜와 시간 설정
  start_date <- as.Date("1900-01-01")
  end_date <- as.Date("2024-11-30")
  date_sequence <- seq(from = start_date, to = end_date, by = "day")
  date_df <- data.frame(날짜 = date_sequence)
  
  # 데이터 병합
  df <- merge(date_df, data, by = "날짜", all.x = TRUE)
  
  # 전체 기간의 월별 평균값 계산
  overall_monthly_avg <- df %>%
    group_by(월) %>%
    summarise(전체월별평균값 = mean(`수위(해수면기준)`, na.rm = TRUE))
  
  # 2024년의 월별 평균값 계산
  monthly_avg_2024 <- df %>%
    filter(연도 == 2024) %>%
    group_by(월) %>%
    summarise(월별평균값_2024 = mean(`수위(해수면기준)`, na.rm = TRUE))
  
  # 두 데이터 병합 및 차이 계산
  comparison <- overall_monthly_avg %>%
    left_join(monthly_avg_2024, by = "월") %>%
    mutate(차이 = 월별평균값_2024 - 전체월별평균값)
  
  # 관측소 이름 추가
  comparison <- comparison %>%
    mutate(관측소 = STA1[i])
  
  # 결과 저장
  result_list[[STA1[i]]] <- comparison
  
  # 개별 결과를 엑셀 파일로 저장
  output_path <- paste0("C:/Users/EKR/Desktop/★_평가보고서 작성/지하수위변동분석/", STA1[i], ".xlsx")
  write_xlsx(list(
    전체_월별_평균값 = overall_monthly_avg,
    `2024년_월별_평균값` = monthly_avg_2024,
    차이분석 = comparison
  ), output_path)
}

# 모든 관측소의 결과를 하나로 합치기
final_result <- bind_rows(result_list)

# 최종 결과를 엑셀 파일로 저장
write_xlsx(final_result, "C:/Users/EKR/Desktop/★_평가보고서 작성/전체_지하수위변동분석결과.xlsx")
