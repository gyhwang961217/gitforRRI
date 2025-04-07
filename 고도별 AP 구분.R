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

#데이터 입력 
data <- read_xlsx(file.choose())
view(data)

#히스토그램 작성 확인----
breaks <- seq(0, 600, by = 50)
h <- hist(data$표고,
     breaks = breaks,
     main = "관측소 해발고도 분포",
     xlab = "해발고도 (m)",
     ylab = "관측소 수",
     col = "skyblue",
     border = "black")

axis(1, at =breaks, labels = breaks)   # y축(2번 축)에 커스텀 눈금 추가


# 각 구간의 빈도를 텍스트로 표시
text(x = h$mids,          # 각 구간의 중간값
     y = h$counts + 1,    # 막대 위쪽 (빈도 + 1) 위치
     labels = h$counts,   # 구간의 빈도 표시
     col = "black", 
     cex = 0.8)        

view(data)
#구간 구분 검토 1. 분위수 구분----
quantiles <- quantile(data$표고, probs = seq(0, 1, by = 0.25))
data$구간분위 <- cut(data$표고, breaks = quantiles, include.lowest = TRUE, 
                 labels = c("1분위수", "2분위수", "3분위수", "4분위수"))


# 확인
view(data)

#분위별 고도 평균값 계산
mean_by_quartile <- data %>%
  group_by(구간분위) %>%
  summarise(mean_AP = mean(`Total AP`, na.rm = TRUE))

#시각화
ggplot(data, aes(x = 구간분위, y = `Total AP`, fill = 구간분위)) +
  geom_boxplot(alpha = 0.6) +  # 박스플롯 그리기 (이상치는 숨김)
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # 평균값을 빨간색 점으로 표시
  geom_text(data = mean_by_quartile, aes(x = 구간분위, y = mean_AP, label = round(mean_AP, 2)), color = "black", vjust = -0.5) +  
  labs(title = "", 
       x = "고도 구간(분위수)", 
       y = "AP") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(1, 12), breaks = seq(1, 12, by = 1))

# geom_boxplot(outlier.shape = NA, alpha = 0.6) +  # 박스플롯 그리기 (이상치는 숨김)


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


#구간별 고도 평균값 계산
mean_by_cluster <- data %>%
  group_by(구간클러) %>%
  summarise(mean_AP = mean(`Total AP`, na.rm = TRUE))

#시각화
ggplot(data, aes(x = 구간클러, y = `Total AP`, fill = 구간클러)) +
  geom_boxplot(alpha = 0.6) +  # 박스플롯 그리기 (이상치는 숨김)
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # 평균값을 빨간색 점으로 표시
  geom_text(data = mean_by_cluster, aes(x = 구간클러, y = mean_AP, label = round(mean_AP, 2)), color = "black", vjust = -0.5) +  
  labs(title = "", 
       x = "고도 구간(클러스터링)", 
       y = "AP") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(1, 12), breaks = seq(1, 12, by = 1))
