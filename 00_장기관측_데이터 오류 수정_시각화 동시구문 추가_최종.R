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
  data <- read_xlsx(file_path, skip = 1) #쓸데없는거 삭제(skip)
  
  #특정 데이터만 선택----
  # 관측소 데이터 불러오기
  data <- data %>%
    dplyr::select(1, 2, 3, 4, 6, 7)  # 필요한 열만 선택
  
  # 날짜 처리
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
      `수위(해수면기준)` = as.numeric(`수위(해수면기준)`),
      `수온(상부)` = as.numeric(`수온(상부)`),
      `전기전도도(상부)` = as.numeric(`전기전도도(상부)`)
    ) %>%
    filter(
      `수온(상부)` > 2 & `수온(상부)` <= 25,
      `전기전도도(상부)` >= 10 & `전기전도도(상부)` <= 80000
    )
  
  data <- subset(data, data$시간=="2")
  
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
          if (is.na(ref) || abs(current - ref) <= 2) {
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
  
  dates <- df1$날짜
  groundwater <- df1$ELm
  TEMP1 <- df1$`수온(상부)`
  #TEMP2 <- df1$`수온(하부)`
  EC1 <- df1$`전기전도도(상부)` 
  #EC2 <- df1$`전기전도도(하부)`
  precipitation <- df1$강수량
  
  
  groundwater <- as.numeric(groundwater)
  TEMP1 <- as.numeric(TEMP1)
  #TEMP2 <- as.numeric(TEMP2)
  EC1 <- as.numeric(EC1)
  #EC2 <- as.numeric(EC2)
  
  # 주축 (지하수위) 그래프 그리기
  graph_dir <- "C:/Users/EKR/Desktop/graph/농촌/앞자름/"
  dir.create(graph_dir, showWarnings = FALSE)  # 그래프 저장 폴더 생성 (없을 경우)
  
  # 그래프 파일 이름 설정
  graph1_path <- file.path(graph_dir, paste0(STA1[i], "_ELm.png"))
  graph2_path <- file.path(graph_dir, paste0(STA1[i], "_EC.png"))
  graph3_path <- file.path(graph_dir, paste0(STA1[i], "_TEMP.png"))
  combined_path <- file.path(graph_dir, paste0(STA1[i], "_combined.png"))
  
  
  png(graph1_path, width = 635, height = 300)
  
  par(mar = c(4, 4, 2, 4) + 0.1,    # 네모박스의 바깥 여백 설정 (상, 좌, 하, 우)
      xaxs = "i",                    # x축 여백 제거
      yaxs = "i")                    # y축 여백 제거
  
  # 그래프 작성하기기
  #a<- min(dates)
  a<-min(dates)+365
  b<- max(dates)
  year_start <- as.Date(paste(format(a, "%Y"), "-01-01", sep=""))
  year_end <- as.Date(paste(format(b, "%Y"), "-01-01", sep=""))
  
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
  png(graph2_path, width = 635, height = 300)
  par(mar = c(4, 4, 2, 4) + 0.1,    # 네모박스의 바깥 여백 설정 (상, 좌, 하, 우)
      xaxs = "i",                    # x축 여백 제거
      yaxs = "i")                    # y축 여백 제거
  
  # EC1, 2
  plot(dates, EC1, lwd=2, type="l", col="black", 
       xaxt="n", ylim=c((min(EC1, na.rm=TRUE)-200), (max(EC1, na.rm=TRUE)+200)), 
       xlab="", ylab="EC (μS/㎝)", xlim=c(year_start, b))
  #lines(dates, EC2, lwd=2, col="red")
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
  png(graph3_path, width = 635, height = 300)
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
  
  # 이미지 불러오기 및 합치기
  graph1 <- image_read(graph1_path)
  graph2 <- image_read(graph2_path)
  graph3 <- image_read(graph3_path)
  combined <- image_append(c(graph1, graph2, graph3), stack = TRUE)
  
  # 결과 이미지 저장
  image_write(combined, combined_path)
}
