
install.packages("trend")
library(trend)
library(MASS)

# 작업 디렉토리 설정
setwd("C:/Users/EKR/Desktop/학위논문(2025)/")  #파일별 sheet 1의 level
getwd()

# 엑셀 파일 목록 가져오기
data1 <- read.table("STATION.txt",header = T,fileEncoding = "euc-kr")
view(data1)
STA1 <- data1$STA_ID

# 각 파일에 대해 반복 수행
for (i in seq_along(STA1)) {
  # 엑셀 파일 읽기
  fname1 <- paste("C:/Users/EKR/Desktop/KRC/01_지토학회/07_Sen_test/01_IN/", STA1[i], ".xlsx", sep = "")
  
  # 파일 존재 여부 확인
  if (file.exists(fname1)) {
    data <- read_xlsx(fname1, skip = 1)
    
    # 측정일자와 시간 분리
    data1 <- data %>% 
      mutate(측정일자 = as.character(측정일자)) %>% 
      mutate(측정시간 = substr(측정일자, 9, 10),
             측정일자 = substr(측정일자, 1, 8))
    
    data2 <- data1 %>%
      mutate(날짜 = ymd(측정일자)) %>% 
      mutate(연도 = year(날짜),
             월 = month(날짜),
             일 = day(날짜))
    
    data3 <- data2 %>%
      mutate(일별 = format(날짜, "%m-%d"))  # character 형태로 변환됨
    
    data3 <- data3 %>%
      mutate(`수위(해수면기준)` = as.numeric(`수위(해수면기준)`))
    
    data3 <- subset(data3, data3$측정시간 == "12")
    data4 <- data3 %>% 
      mutate(ELm = as.numeric(`수위(해수면기준)`))
    
    start_groundwater <- min(data3$날짜)
    
    # fdate_df는 정의되어 있어야 합니다.
    fdate_df <- subset(date_df, date_df$날짜 >= start_groundwater)
    
    data4 <- merge(fdate_df, data4, by = "날짜", all.x = TRUE)
    omitlevel <- as.numeric(na.omit(data4$ELm))
    
    # rolling mean 계산
    ml <- rollmean(omitlevel, k = 7, fill = NA, align = "right")
    
    # 유효한 수위 및 날짜 데이터 필터링
    filtered_stage_data <- data4$ELm[!is.na(data4$ELm)]
    filtered_date_data <- data4$날짜[!is.na(data4$ELm)]
    
    # 연도별 평균값 계산
    if (length(filtered_stage_data) > 0 && length(filtered_date_data) > 0) {
      data_summary <- data.frame(Date = filtered_date_data, Stage = filtered_stage_data) %>%
        mutate(Year = format(Date, "%Y")) %>%
        group_by(Year) %>%
        summarize(Average_Stage = mean(Stage, na.rm = TRUE), .groups = 'drop')
      
      # 결과 파일로 저장 (원래 엑셀 파일 이름 사용)
      output_filename <- paste0("C:/Users/EKR/Desktop/KRC/01_지토학회/07_Sen_test/02_OUT/", 
                                STA1[i], "_annual_average.xlsx")
      write_xlsx(data_summary, output_filename)
    } else {
      cat("수위 데이터 또는 날짜 데이터가 유효하지 않습니다:", fname1, "\n")
    }
  } else {
    cat("파일이 존재하지 않습니다:", fname1, "\n")  # 파일이 존재하지 않을 경우 메시지 출력
  }
}






getwd()
data1 <- read.table("STATION.txt",header = T,fileEncoding = "euc-kr")
STA1 <- data1$STA_ID
print(STA1)
view(data1)

#mann-kendall 분석----

# Mann-Kendall 분석----경로 없을 때 건너뛰는 버전.
for (i in seq(length(STA1))) {  # STA1의 길이에 따라 반복
  cat("처리 중:", STA1[i], "\n")
  
  # 파일 열기
  fname1 <- paste("07_Sen_test/02_OUT/", STA1[i], "_annual_average.xlsx", sep = "")
  
  # 파일 존재 여부 확인
  if (!file.exists(fname1)) {
    cat("파일이 존재하지 않습니다. 건너뜁니다:", fname1, "\n")
    next  # 파일이 없으면 현재 반복 건너뛰기
  }
  
  # 데이터 읽기
  data1 <- read_xlsx(fname1, skip = 1)
  
  # Average_Stage 열 확인 및 변환
  if (ncol(data1) >= 2) {  # 데이터가 최소 2개 열 이상인지 확인
    a <- as.numeric(data1[[2]])  # 2번째 열을 Average_Stage로 지정
    
    # 3개 이상의 값이 있는 경우에만 분석
    if (length(a) >= 3) {
      # Mann-Kendall 테스트
      c1 <- mk.test(a, continuity = TRUE)
      
      # Sens slope 계산
      c <- sens.slope(a, conf.level = 0.95)
      
      # 결과 저장
      d <- c$estimates
      e <- c$conf.int
      d1 <- c1$statistic
      e1 <- c1$p.value
      
      # 결과 파일 경로 설정
      ofile1 <- paste("07_Sen_test/03_OUT/", STA1[i], "_P50_ST_slope.TXT", sep = "")
      ofile2 <- paste("07_Sen_test/03_OUT/", STA1[i], "_P50_ST_conf_int.TXT", sep = "")
      ofile3 <- paste("07_Sen_test/03_OUT/", STA1[i], "_P50_MK_Stats.TXT", sep = "")
      ofile4 <- paste("07_Sen_test/03_OUT/", STA1[i], "_P50_MK_P_value.TXT", sep = "")
      
      # 결과 저장
      write.matrix(d, ofile1, sep = "\t")
      write.matrix(e, ofile2, sep = "\t")
      write.matrix(d1, ofile3, sep = "\t")
      write.matrix(e1, ofile4, sep = "\t")
      
    } else {
      cat("Average_Stage 데이터가 3개 미만입니다. 건너뜁니다:", STA1[i], "\n")
      next  # 현재 반복 건너뛰기
    }
  } else {
    cat("데이터에 2개 이상의 열이 없습니다. 건너뜁니다:", STA1[i], "\n")
    next  # 현재 반복 건너뛰기
  }
}


#엑셀로 저장하기----

# 결과를 저장할 빈 데이터프레임 생성
all_results <- data.frame()

# 관측소 목록
for (i in seq(length(STA1))) {
  cat("처리 중:", STA1[i], "\n")
  
  # 각 관측소의 _P50_MK_Stats.txt 파일 경로 설정
  stats_file <- paste("07_Sen_test/03_OUT/", STA1[i], "_P50_MK_Stats.TXT", sep = "")
  pvalue_file <- paste("07_Sen_test/03_OUT/", STA1[i], "_P50_MK_P_value.TXT", sep = "")
  
  # 파일 존재 여부 확인 및 읽기
  if (file.exists(stats_file) && file.exists(pvalue_file)) {
    # _P50_MK_Stats.txt 파일 읽기
    stats_data <- read_delim(stats_file, delim = "\t", col_names = FALSE)
    # _P50_MK_P_value.TXT 파일 읽기
    pvalue_data <- read_delim(pvalue_file, delim = "\t", col_names = FALSE)
    
    # 관측소 이름을 열로 추가
    stats_data$Observatory <- STA1[i]
    pvalue_data$Observatory <- STA1[i]
    
    # 결과 데이터프레임 결합
    combined_data <- cbind(stats_data, P_Value = pvalue_data$X1)  # P-value는 첫 번째 열을 가정
    
    # 모든 결과 데이터를 결합
    all_results <- rbind(all_results, combined_data)
  } else {
    cat("파일이 존재하지 않습니다. 건너뜁니다:", stats_file, "또는", pvalue_file, "\n")
  }
}

# 최종 결과를 Excel 파일로 저장
output_filename <- "C:/Users/EKR/Desktop/KRC/01_지토학회/07_Sen_test/03_OUT/all_MK_Results.xlsx"
write_xlsx(all_results, output_filename)

cat("모든 결과가 성공적으로 Excel 파일로 저장되었습니다:", output_filename, "\n")


# C:/Users/EKR/Desktop/KRC/01_지토학회/07_Sen_test/01_IN
#  C:/Users/EKR/Desktop/KRC/01_지토학회/07_Sen_test/01_IN



