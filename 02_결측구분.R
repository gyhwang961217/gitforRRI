# 01_결측값 찾기----
# 작업 경로 설정
setwd("C:/Users/EKR/Desktop/★_평가보고서 작성/")
getwd()

# 엑셀 파일 목록 가져오기
dtest <- read.table("staion_해수2.txt", header = TRUE, fileEncoding = "euc-kr")
STA1 <- dtest$STAID

na_counts <- list()  # 관측소별 NA/NULL 값을 저장할 리스트

for (i in seq_along(STA1)) {  # STA1의 길이에 따라 반복
  cat("처리 중:", STA1[i], "\n")
  fname1 <- paste("C:/Users/EKR/Desktop/★_평가보고서 작성/해수_장기관측 추세분석/", STA1[i], ".xlsx", sep = "")
  
  # 파일 존재 여부 확인
  if (file.exists(fname1)) {
    data <- read_xlsx(fname1)
  
    # 특정 연도의 NA/NULL 값 확인
    year <- 2024
    filtered_data <- data[as.numeric(format(data$날짜, "%Y")) == year, ] #이상치면 2024도 삭제됨..쩝
    na_or_null_count <- sum(is.na(filtered_data$ELm) | filtered_data$ELm == "")
    
    # 결과 저장
    na_counts[[STA1[i]]] <- na_or_null_count  # 리스트에 추가
  }
  write_xlsx(data, paste0("C:/Users/EKR/Desktop/★_평가보고서 작성/testttt/",STA1[i], ".xlsx"))
}


# 반복문 종료 후 결과를 데이터프레임으로 변환
na_counts_df <- data.frame(
   관측소 = names(na_counts),
  NA_NULL_개수 = unlist(na_counts)
)

view(na_counts_df)
# 결과를 엑셀 파일로 저장

write.xlsx(na_counts_df, "C:/Users/EKR/Desktop/★_평가보고서 작성/결측값처리233.xlsx", rowNames = FALSE)

