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
library(zoo)
options(scipen = 100)

#관측데이터 불러오기----
cw1<-read_xlsx(file.choose(),skip = 1)
view(cw1)
cw1 <- cw1 %>% 
  mutate(측정일자           = as.numeric(측정일자),
         '수위(해수면기준)' = as.numeric(`수위(해수면기준)`),
         '전기전도도(상부)' = as.numeric(`전기전도도(상부)`)) %>% 
  mutate(측정일             = 측정일자/1000000,
         측정시간           = ymd_hm(측정일자/100)) %>% 
  mutate(측정일             = ymd(측정일),
         측정시간           = hour(측정시간))

cw12 <- cw1     %>%
  dplyr::select(관정명, `수위(해수면기준)`, `전기전도도(상부)`,측정일)

head(cw12)
view(cw12)
# 강수량 자료 합치기----
#aosrain <- read_excel(file.choose(), 
#                     col_names = c("지점", "지점명", "측정일", "강수량"), skip=1)
#aosrain <-  aosrain %>% 
#  mutate(측정일    = ymd(측정일))
#head(aosrain)
#  강우자료 많을 시 추출 
rain<-subset(aosrain, aosrain$지점명=="경주시")
dfcw1 <- merge(cw12,rain,by="측정일",all=TRUE)

dfcw1 <- dfcw1  %>%
  dplyr::select(-지점, -지점명)
#na값 뱉어내기
dfcw1 <- na.omit(dfcw1)
head(dfcw1)


# 지하수위-강수량 교차상관 분석----
#이동평균 계산
dfcw1$gw_moving <- rollmean(dfcw1$`수위(해수면기준)`, k = 5, fill = NA, align = "right")
dfcw1<- na.omit(dfcw1)
#view(dfcw1)
#교차상관분석
result <- ccf(dfcw1$gw_moving, dfcw1$강수량, lag.max = 300, plot = TRUE) #음수: 1이 먼저 변화, 2 변화
abline(v=0)
d<-data.frame(result$lag, result$acf)
plot(d, ylim=c(-1,1),xlab="lag(day)", ylab="cross-correlation",main="근덕2",type="l")
abline(h=seq(-1, 1, by=0.1), col="gray", lty=2)
abline(v=0, col="gray", lty=2)
#그래프 저장(580*600)
#교차상관계수 및 지연시간 찾기
cor_values <- result$acf
lag_values <- result$lag

positive_lag_indices <- which(lag_values >=0)
positive_lags <- lag_values[positive_lag_indices]
positive_cors <- cor_values[positive_lag_indices]


max_cor_index <- which.max(positive_cors)
max_cor <- positive_cors[max_cor_index]
max_lag <- positive_lags[max_cor_index]

cat("가장 큰 교차상관계수:", max_cor, "\n")
cat("해당 지연시간:", max_lag, "일\n")

view(d)
# EC-강수량 교차상관 분석----
#이동평균 계산
dfcw1$ec_moving <- rollmean(dfcw1$`전기전도도(상부)`, k = 5, fill = NA, align = "right")
dfcw1<- na.omit(dfcw1)
#교차상관분석
result <- ccf(dfcw1$ec_moving, dfcw1$강수량,lag.max = 300, plot = TRUE)  #음수: 1이 먼저 변화, 2 변화
d<-data.frame(result$lag, result$acf)
plot(d, ylim=c(-1,1),xlab="lag(day)", ylab="cross-correlation",main="선도2",type="l")
abline(h=seq(-1, 1, by=0.1), col="gray", lty=2)
abline(v=0, col="gray", lty=2)
#그래프 저장(높이:600)
#교차상관계수 및 지연시간 찾기
cor_values <- result$acf
lag_values <- result$lag

positive_lag_indices <- which(lag_values >=0)
positive_lags <- lag_values[positive_lag_indices]
positive_cors <- cor_values[positive_lag_indices]


max_cor_index <- which.max(positive_cors)
max_cor <- positive_cors[max_cor_index]
max_lag <- positive_lags[max_cor_index]

cat("가장 큰 교차상관계수:", max_cor, "\n")
cat("해당 지연시간:", max_lag, "일\n")

view(d)

# EC-지하수위위 교차상관 분석----
result <- ccf(dfcw1$ec_moving, dfcw1$gw_moving, lag.max = 300, plot = TRUE)  #음수: 1이 먼저 변화, 2 변화
d<-data.frame(result$lag, result$acf)
plot(d, ylim=c(-1,1),xlab="lag(day)", ylab="cross-correlation",main="나리2",type="l")
abline(h=seq(-1, 1, by=0.1), col="gray", lty=2)
abline(v=0, col="gray", lty=2)
#그래프 저장(높이:600)
#교차상관계수 및 지연시간 찾기
cor_values <- result$acf
lag_values <- result$lag

positive_lag_indices <- which(lag_values >=0)
positive_lags <- lag_values[positive_lag_indices]
positive_cors <- cor_values[positive_lag_indices]

max_cor_index <- which.max(positive_cors)
max_cor <- positive_cors[max_cor_index]
max_lag <- positive_lags[max_cor_index]

cat("가장 큰 교차상관계수:", max_cor, "\n")
cat("해당 지연시간:", max_lag, "일\n")

view(d)
