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

# 로우데이터 자료 정리하기----
data <- read_xlsx(file.choose()) 
view(data)


# 데이터 열 이름 확인 (열 이름 확인 후 수정)
colnames(data)

# 데이터 변환: 열 이름을 사용해 깊이와 전기전도도 짝 지어서 처리
long_data <- data %>%
  dplyr::select(
    Depth1 = `D(2015.07)`, EC1 = `EC...2`,
    Depth2 = `D(2016.09)`, EC2 = `EC...4`,   # 첫 번째 연도
    Depth3 = `D(2017.03)`, EC3 = `EC...6`,
    Depth4 = `D(2018.09)`, EC4 = `EC...8`,
    Depth5 = `D(2019.04)`, EC5 = `EC...10`,
    Depth6 = `D(2020.09)`, EC6 = `EC...12`    # 세 번째 연도
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "Set"),
    names_pattern = "(Depth|EC)(\\d+)"
  ) %>%
  mutate(Set = as.factor(Set))

view(long_data)


long_data <- long_data %>%
  mutate(
    Set = factor(Set, labels = c("2015-07","2016-09", "2017-03", "2018-09","2019-04","2020-09"))
  )


# 그래프 생성
#선 색이 많을 때(자동으로 색상 지정)----

#ggplot(long_data, aes(x = EC, y = Depth, color = Set, group = Set)) +
#scale_color_maual 삭제

#선 색 수동으로 조정----

ggplot(long_data, aes(x = EC, y = Depth, color = Set, group = Set)) +
  geom_path(size = 0.8) +
  geom_point(size = 2) +
  scale_y_reverse(
    limits = c(70, 0),             # y축 범위 설정
    breaks = seq(0, 70, by = 10),  # y축 숫자 10 단위
    minor_breaks = seq(0, 70, by = 5)  # y축 눈금 5 단위
  ) +
  scale_x_continuous(
    position = "top",              # x축을 위쪽으로 이동
    minor_breaks = NULL            # 필요시 추가 점선 설정
  ) +
  scale_color_manual(
    values = c("skyblue", "blue", "red", "brown","black", "darkgrey"),  # 원하는 색깔로 지정
    labels = c("2015-07","2016-09", "2017-03", "2018-09","2019-04","2020-09")  # 범례 라벨 설정
  ) +
  labs(
    x = "EC (μS/cm)",              # x축 레이블
    y = "Depth (m)",               # y축 레이블
    color = "Year"                 # 범례 제목 변경
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey70", linetype = "dotted"), # x축 점선 추가
    panel.grid.minor.x = element_blank(),                                   # 보조선 제거
    panel.grid.major.y = element_line(color = "grey70", linetype = "solid"), # y축 주요선
    axis.title.x.top = element_text(size = 12, margin = margin(b = 10)),     # 위쪽 x축 제목 스타일
    axis.text.x.top = element_text(size = 10),                              # 위쪽 x축 숫자 스타일
    legend.position = c(0.8, 0.8),  # 범례 위치: 그래프 내부 조정
    legend.background = element_rect(color = "black", fill = "white"), # 범례 테두리 추가
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    aspect.ratio = 2                   # 세로로 긴 그래프 비율
  )
