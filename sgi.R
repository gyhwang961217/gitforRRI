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
options(scipen = 100)

# 로우데이터 SGI 자료 정리하기----
data <- read_xlsx(file.choose(), skip=1) #쓸데없는거 삭제(skip)
view(data)

#is.data.frame(data)
#str(data)

#특정 데이터만 선택
data <- data[,c(1,2,3)]
view(data)

#날짜 구분
data1 <- data %>% 
  mutate(측정일자           = as.character(측정일자)) %>% 
  mutate(측정일자           = substr(측정일자, 1, 8))

data2 <- data1 %>%
  mutate(날짜           = ymd(측정일자)) %>% 
  mutate(연도             = year(날짜),
         월           = month(날짜),
         일           = day(날짜))

data3<- data2 %>%
  mutate(일별     = format(날짜, "%m-%d"))


# 자료 형식 정리하기----
pivot_data <- data3 %>%
  dplyr::select(일별, 연도, `수위(해수면기준)`) %>%
  pivot_wider(names_from = 연도, values_from = `수위(해수면기준)`) #오류뜨면 중복값 등 검수

view(pivot_data)

pivot_Data<- pivot_data[, -ncol(pivot_data)]

pivot_Data_end <- pivot_Data %>%
  arrange(일별)

#2월 29일 지워버리기
pivot_Data_end <- pivot_Data_end[-60,]
view(pivot_Data_end)
str(pivot_Data_end)

#필요시 사용
#pivot_Data_end <- pivot_Data_end[, names(pivot_Data_end) != "NA"]
#view(pivot_Data_end)


# 커널 밀도 함수 추정 및 cdf 구하기

#함수 정의
calculate_cdf <- 
  function(row) {
   gw_levels <- 
      as.numeric(row[-1])  #첫번째 열(일자) 제외 숫자변환
   density_est <- 
     density(gw_levels, na.rm = TRUE) #KDE 수행
   density_cdf <- 
     approxfun(density_est$x, 
               cumsum(density_est$y)/
                 sum(density_est$y))
   last_value <-
     gw_levels[length(gw_levels)]
   return(density_cdf(last_value))
  }

#is.data.frame(pivot_Data_end)
#str(pivot_Data_end)
#sapply(pivot_Data_end, class)
#오류가 나는 이유는 날짜 등에 중복값이 있기 때문이므로, 삭제 후 다시 분석할 것.

pivot_Data_end$CDF <- apply(pivot_Data_end, 1, calculate_cdf)
view(pivot_Data_end)

# SGI 수치 구하기
pivot_Data_end$SGI <- qnorm(pivot_Data_end$CDF)
view(pivot_Data_end)


setwd("C:/Users/EKR/Desktop/할이리/충남_240902/SGI/")
write_xlsx(x=pivot_Data_end, path = "홍성2.xlsx")


#검증하기(시각화)
data_test <- pivot_Data_end %>%
  dplyr::select(일별, 지하수위 = '2023', SGI) %>%
  pivot_longer(cols=c(지하수위, SGI),
               names_to = "Type",
               values_to = "Value")
view(data_test)

ggplot(data_test, aes(x =
                        as.Date(일별, format= "%m-%d"),
                      y=Value, color = Type)) +
  geom_line() +
  labs(title = "Groundwater level and SGI", 
       x = "날짜", y= "값")+
  theme_minimal()

#상관분석
cortest <- read_xlsx(file.choose())
view(cortest)
cor_result <- cor.test(cortest$SGI,cortest$SPI)
x<- cortest$SGI
y<-cortest$SPI
cor_result
# 상관계수와 p-value 추출
cor_coef <- round(cor_result$estimate, 2)
p_value <- round(cor_result$p.value, 4)

# 산점도 시각화
ggplot(data = data.frame(x, y), aes(x = x, y = y)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # 회귀선 추가
  labs(title = "Scatter Plot with SGI-SPI Correlation",
       subtitle = paste("Correlation:", cor_coef, "| p-value:", p_value),
       x = "SGI", y = "SPI") +
  theme_minimal()
