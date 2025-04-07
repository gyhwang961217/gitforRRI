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
library(writexl)
options(scipen = 100)

#데이터 입력 
data <- read_xlsx(file.choose())
view(data)

setwd("C:/Users/EKR/Desktop/KRC/07_지토학회(12월호)/")
getwd()

#히스토그램 작성 확인----
par(mar = c(4, 4, 4, 4) + 0.1,    # 네모박스의 바깥 여백 설정 (상, 좌, 하, 우)
    xaxs = "i",                    # x축 여백 제거
    yaxs = "i")                    # y축 여백 제거

breaks <- seq(0, 600, by = 50)
h <- hist(data$표고,
          breaks = breaks,
          main = "",
          xlab = "EL.(m)",
          ylab = "number of wells",
          col = "grey",
          border = "black",
          ylim = c(0, max(h$counts) + 5))

axis(1, at =breaks, labels = breaks)   # y축(2번 축)에 커스텀 눈금 추가


# 각 구간의 빈도를 텍스트로 표시
text(x = h$mids,          # 각 구간의 중간값
     y = h$counts + 0.5,  # 텍스트 위치를 막대 위로 이동
     labels = h$counts,   # 구간 빈도 표시
     col = "black", 
     cex = 0.8)       


hist_data <- data.frame(
  구간_시작 = h$breaks[-length(h$breaks)], # 각 구간의 시작값
  구간_끝 = h$breaks[-1],                 # 각 구간의 끝값
  관측소_개수 = h$counts                   # 각 구간의 빈도수
)




# 엑셀 파일로 저장
write_xlsx(hist_data, "히스토그램_결과.xlsx")

#구간 구분 검토 0. 50단위 구분----
# 고도 분포를 50 단위로 나누기
data <- data %>%
  mutate(Elevation_Group = cut(data$표고, 
                               breaks = seq(0, 600, by = 50), 
                               include.lowest = TRUE, 
                               labels = c("0~50", "50~100", "100~150", "150~200", "200~250", 
                                          "250~300", "300~350", "350~400", "400~450",
                                          "450~500",  "500~550", "550~600")))

view(data)
# 각 그룹별 AP 평균값 계산
grouped_data <- data %>%
  group_by(Elevation_Group) %>%
  summarise(
    Total_AP_diff = mean(전체_상하반기차이, na.rm = TRUE),
    Number = n() # 관측망 개수를 계산하여 새로운 열로 추가
  )

view(grouped_data)
write_xlsx(grouped_data, "전체_고도별 구분결과.xlsx")
# 결과 확인
print(grouped_data)


ggplot(grouped_data, aes(x = Elevation_Group, y = Total_AP_diff)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") + # 테두리 추가
  geom_text(aes(label = sprintf("%.2f", Total_AP_diff)),  # 소숫점 둘째 자리 값 표시
            vjust = -0.5, size = 3) + # 텍스트 위치와 크기 조정
  labs(title = "Elevation vs. AP diff",
       x = "Elevation (m)",
       y = "AP") +
  theme_minimal() +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) # Y축 소숫점 둘째 자리 설정




#구간 구분 검토 1. 분위수 구분----
quantiles <- quantile(data$표고, probs = seq(0, 1, by = 0.25))
data$구간분위 <- cut(data$표고, breaks = quantiles, include.lowest = TRUE, 
                 labels = c("1분위수", "2분위수", "3분위수", "4분위수"))


# 확인
view(data)

#분위별 고도 평균값 계산
grouped_data_quan <- data %>%
  group_by(구간분위) %>%
  summarise(Total_AP_diff_quantile = mean(`전체_상하반기차이`, na.rm = TRUE),
            Number = n())  
view(grouped_data_quan)

write_xlsx(grouped_data_quan, "전체_분위수 구분결과.xlsx")

#전체기간 대상 상, 하반기 AP 차이
#시각화
ggplot(grouped_data_quan, aes(x = 구간분위, y = Total_AP_diff_quantile)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") + # 테두리 추가
  geom_text(aes(label = sprintf("%.2f", Total_AP_diff_quantile)),  # 소숫점 둘째 자리 값 표시
            vjust = -0.5, size = 3) + # 텍스트 위치와 크기 조정
  labs(title = "Elevation vs. AP diff",
       x = "Elevation (m)",
       y = "AP") +
  theme_minimal() +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) 


#구간 구분 검토 2. 클러스터링----
clusters <- kmeans(data$표고, centers = 4)

#클러스터별 범위 확인
cluster_min <- tapply(data$표고, clusters$cluster, min)
cluster_max <- tapply(data$표고, clusters$cluster, max)

# 클러스터별 최솟값과 최댓값을 하나의 데이터프레임으로 확인
cluster_summary <- data.frame(
  Cluster = 1:length(cluster_min),
  Min = cluster_min,
  Max = cluster_max
)

view(cluster_summary)

# 클러스터별 평균값 계산
cluster_means <- tapply(data$표고, clusters$cluster, mean)
# 평균값에 따라 클러스터 번호를 오름차순으로 정렬
sorted_means <- sort(cluster_means)  # 평균값 오름차순 정렬
# 정렬된 평균값에 맞춰 새로운 클러스터 번호 생성
# sorted_means는 평균값 순으로 정렬된 클러스터의 값들이므로, 이를 다시 클러스터 번호에 매핑
cluster_mapping <- match(clusters$cluster, names(sorted_means))
# 클러스터 번호를 재배치하여 '구간클러'에 새로운 번호 할당
data$구간클러 <- factor(cluster_mapping, levels = 1:length(sorted_means))
# 각 클러스터별 평균 고도 확인
b <- aggregate(data$표고, by = list(data$구간클러), FUN = mean)
b
write_xlsx(b, "클러스터 구분기준.xlsx")
#구간별 고도 평균값 계산
grouped_data_clu <- data %>%
  group_by(구간클러) %>%
  summarise(Total_AP_diff_clu = mean(`전체_상하반기차이`, na.rm = TRUE),
            Number = n())

view(grouped_data_clu)
write_xlsx(grouped_data_clu, "전체_클러스터 구분결과.xlsx")
#시각화
ggplot(grouped_data_clu, aes(x = 구간클러, y = Total_AP_diff_clu)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") + # 테두리 추가
  geom_text(aes(label = sprintf("%.2f", Total_AP_diff_clu)),  # 소숫점 둘째 자리 값 표시
            vjust = -0.5, size = 3) + # 텍스트 위치와 크기 조정
  labs(title = "Elevation vs. AP diff",
       x = "Elevation (m)",
       y = "AP") +
  theme_minimal() +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) 





#반복_풍수기----
# 고도 50단위 구분 도면

grouped_data_p <- data %>%
  group_by(Elevation_Group) %>%
  summarise(Total_AP_diff = mean(풍수_상하반기차이, na.rm = TRUE),
            Number = n())

# 결과 확인
print(grouped_data_p)


ggplot(grouped_data_p, aes(x = Elevation_Group, y = Total_AP_diff)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") + # 테두리 추가
  geom_text(aes(label = sprintf("%.2f", Total_AP_diff)),  # 소숫점 둘째 자리 값 표시
            vjust = -0.5, size = 3) + # 텍스트 위치와 크기 조정
  labs(title = "Elevation vs. AP diff",
       x = "Elevation (m)",
       y = "AP") +
  theme_minimal() +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) # Y축 소숫점 둘째 자리 설정

write_xlsx(grouped_data_p, "풍수기_고도별 구분결과.xlsx")


#구간 구분 검토 1. 분위수 구분
#분위별 고도 평균값 계산
grouped_data_quan_p <- data %>%
  group_by(구간분위) %>%
  summarise(Total_AP_diff_quantile = mean(`풍수_상하반기차이`, na.rm = TRUE),
            Number = n())  
view(grouped_data_quan)
#전체기간 대상 상, 하반기 AP 차이


#시각화
ggplot(grouped_data_quan_p, aes(x = 구간분위, y = Total_AP_diff_quantile)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") + # 테두리 추가
  geom_text(aes(label = sprintf("%.2f", Total_AP_diff_quantile)),  # 소숫점 둘째 자리 값 표시
            vjust = -0.5, size = 3) + # 텍스트 위치와 크기 조정
  labs(title = "Elevation vs. AP diff",
       x = "Elevation (m)",
       y = "AP") +
  theme_minimal() +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) 

write_xlsx(grouped_data_quan_p, "풍수기_분위수 구분결과.xlsx")
#구간 구분 검토 2. 클러스터링----
clusters <- kmeans(data$표고, centers = 4)

#클러스터별 범위 확인
cluster_min <- tapply(data$표고, clusters$cluster, min)
cluster_max <- tapply(data$표고, clusters$cluster, max)

# 클러스터별 최솟값과 최댓값을 하나의 데이터프레임으로 확인
#구간별 고도 평균값 계산
grouped_data_clu_p <- data %>%
  group_by(구간클러) %>%
  summarise(Total_AP_diff_clu = mean(`풍수_상하반기차이`, na.rm = TRUE),
            Number = n())

view(grouped_data_clu)
write_xlsx(grouped_data_clu_p, "풍수기_클러스터 구분결과.xlsx")
#시각화
ggplot(grouped_data_clu_p, aes(x = 구간클러, y = Total_AP_diff_clu)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") + # 테두리 추가
  geom_text(aes(label = sprintf("%.2f", Total_AP_diff_clu)),  # 소숫점 둘째 자리 값 표시
            vjust = -0.5, size = 3) + # 텍스트 위치와 크기 조정
  labs(title = "Elevation vs. AP diff",
       x = "Elevation (m)",
       y = "AP") +
  theme_minimal() +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) 




#반복_갈수기----
# 고도 50단위 구분 도면

grouped_data_g <- data %>%
  group_by(Elevation_Group) %>%
  summarise(Total_AP_diff = mean(갈수_상하반기차이, na.rm = TRUE),
            Number = n())

# 결과 확인
print(grouped_data_g)


ggplot(grouped_data_g, aes(x = Elevation_Group, y = Total_AP_diff)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") + # 테두리 추가
  geom_text(aes(label = sprintf("%.2f", Total_AP_diff)),  # 소숫점 둘째 자리 값 표시
            vjust = -0.5, size = 3) + # 텍스트 위치와 크기 조정
  labs(title = "Elevation vs. AP diff",
       x = "Elevation (m)",
       y = "AP") +
  theme_minimal() +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) # Y축 소숫점 둘째 자리 설정

write_xlsx(grouped_data_g, "갈수기_고도별 구분결과.xlsx")


#구간 구분 검토 1. 분위수 구분
#분위별 고도 평균값 계산
grouped_data_quan_g <- data %>%
  group_by(구간분위) %>%
  summarise(Total_AP_diff_quantile = mean(`갈수_상하반기차이`, na.rm = TRUE),
            Number = n())  
view(grouped_data_quan_g)
#전체기간 대상 상, 하반기 AP 차이


#시각화
ggplot(grouped_data_quan_g, aes(x = 구간분위, y = Total_AP_diff_quantile)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") + # 테두리 추가
  geom_text(aes(label = sprintf("%.2f", Total_AP_diff_quantile)),  # 소숫점 둘째 자리 값 표시
            vjust = -0.5, size = 3) + # 텍스트 위치와 크기 조정
  labs(title = "Elevation vs. AP diff",
       x = "Elevation (m)",
       y = "AP") +
  theme_minimal() +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) 

write_xlsx(grouped_data_quan_g, "갈수기_분위수 구분결과.xlsx")
#구간 구분 검토 2. 클러스터링----
#구간별 고도 평균값 계산
grouped_data_clu_g <- data %>%
  group_by(구간클러) %>%
  summarise(Total_AP_diff_clu = mean(`갈수_상하반기차이`, na.rm = TRUE),
            Number = n())

view(grouped_data_clu)
write_xlsx(grouped_data_clu_g, "갈수기 클러스터 구분결과.xlsx")
#시각화
ggplot(grouped_data_clu_g, aes(x = 구간클러, y = Total_AP_diff_clu)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") + # 테두리 추가
  geom_text(aes(label = sprintf("%.2f", Total_AP_diff_clu)),  # 소숫점 둘째 자리 값 표시
            vjust = -0.5, size = 3) + # 텍스트 위치와 크기 조정
  labs(title = "Elevation vs. AP diff",
       x = "Elevation (m)",
       y = "AP") +
  theme_minimal() +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) 

