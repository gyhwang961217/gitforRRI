# 패키지 설치----
install.packages("tidyverse")
install.packages("scales")
install.packages("readxl")
library(tidyverse)
library(scales)
library(readxl)
options(scipen = 100)

# 데이터 불러오기----
cw1<-read_xlsx(file.choose(),skip = 1)
view(cw1)
is.data.frame(cw1)
str(cw1) # 데이터 형식 확인

# 시간 추출하기(오전 2시 데이터)----
cw1 <- cw1 %>% 
  mutate(측정일자           = as.numeric(측정일자),
         '수위(해수면기준)' = as.numeric(`수위(해수면기준)`),
         '전기전도도(상부)' = as.numeric(`전기전도도(상부)`)) %>% 
  mutate(측정일             = 측정일자/1000000,
         측정시간           = ymd_hm(측정일자/100)) %>% 
  mutate(측정일             = ymd(측정일),
         측정시간           = hour(측정시간))

cw1  <- select(.data = cw1, -`수온(하부)`, `전기전도도(하부)`,-`수위(지표면기준)`,-수심)
cw1  <- select(.data = cw1,-'전기전도도(하부)')
cw12 <- subset(cw1, cw1$측정시간==2)

view(cw12)
# 강수량 자료 합치기----
aosrain <- read_excel(file.choose(), 
                      col_names = c("지점", "지점명", "측정일", "강수량"), skip=1)
str(aosrain)
aosrain <-  aosrain %>% 
  mutate(측정일    = ymd(측정일))
view(aosrain)
#  강우자료 많을 시 추출 
rain<-subset(aosrain, aosrain$지점명=="보은")
dfcw1 <- merge(cw12,rain,by="측정일",all=TRUE)
dfcw1 <- select(.data = dfcw1, -지점, -지점명)
view(dfcw1)

#통계량 확인----
summary(dfcw1$`수위(해수면기준)`)

# 이상치 제외(+-50%, 조정 가능)----
avg<-mean(na.omit(dfcw1$`수위(해수면기준)`))
dfcw1 <- dfcw1 %>% 
  mutate(el_edt   = ifelse(`수위(해수면기준)`>=0.03*avg & `수위(해수면기준)`<=3*avg,
                           `수위(해수면기준)`, NA))
summary(dfcw1$el_edt)

avg2<- mean(na.omit(dfcw1$el_edt))
# 시각화----
p<- ggplot(data = dfcw1, mapping = aes(x = dfcw1$측정일)) +
  geom_line(aes(y = dfcw1$el_edt), linewidth = 0.5)     +
  geom_hline(yintercept = avg2, linetype = 'dashed')    +
  ggtitle("강릉2(2015-2023)")                           +
  scale_x_date(breaks="2 months", expand = c(0,0), labels = date_format("%Y-%b"))           +
  scale_y_continuous(limits = c(-0.5,1.5))                                                  +
  theme(axis.text.x = element_text(angle = 60), plot.title=element_text(hjust=0.5))         +
  xlab("Date")                                          +
  ylab("El.m")

p

pp <- p + geom_bar(aes(y=강수량*0.005), stat = "identity", colour="grey", alpha=0.5)
pp <- pp    +
  scale_y_continuous(sec.axis = 
                       sec_axis(~./0.005, name= "precipitation", breaks = seq(0, 400, by = 50)))
pp                         

view(dfcw1)
view(nonadf)
# 교차상관분석----
nonadf<-na.omit(dfcw1)
result<-ccf(nonadf$`수위(해수면기준)`,nonadf$강수량,lag.max = 50)
summary(result)
head(result)
summary(result$acf)
view(result)
view(result$acf)
dmax<-max(d$result.acf)


# 교차상관 계수와 지연 시간 추출
ccf_values <- data.frame(
  lag = result$lag,
  correlation = result$acf
)

# 최대 교차상관계수와 해당 지연 시간 찾기
max_ccf <- ccf_values %>%
  filter(correlation == max(correlation)) %>%
  slice(1)

print(max_ccf)








