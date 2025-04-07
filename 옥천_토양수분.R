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
options(scipen = 100)

# 데이터 불러오기----
cw1<-read_xlsx(file.choose())
view(cw1)
is.data.frame(cw1)
str(cw1) # 데이터 형식 확인
str(dfcw1)
# 시간 추출하기(오전 2시 데이터)----
cw1 <- cw1 %>% 
  mutate(날짜           = ymd(날짜),
         ELm = as.numeric(`지하수위(옥천3)`),
         Soil = as.numeric(`토양수분측정값(보정)`)) %>% 
  mutate(연도             = year(날짜),
         월           = month(날짜))


cw1  <- select(.data = cw1, -'토양수분측정값(보정)', -'지하수위(옥천3)')
view(cw1)

# 강수량 자료 합치기----
aosrain <- read_xlsx(file.choose(), 
                      col_names = c("지점", "지점명", "날짜", "강수량"), skip=1)
view(aosrain)

#  강우자료 많을 시 추출 rain<-subset(aosrain, aosrain$지점명=="강릉")
dfcw1 <- merge(cw1,aosrain,by="날짜",all=TRUE)
view(dfcw1)
dfcw1 <- select(.data = dfcw1, -지점, -지점명)
str(dfcw1)

# 이상치 제외(+-50%, 조정 가능)----
avg<-mean(na.omit(dfcw1$`수위(해수면기준)`))
dfcw1 <- dfcw1 %>% 
  mutate(el_edt   = ifelse(`수위(해수면기준)`>=0.03*avg & `수위(해수면기준)`<=3*avg,
                           `수위(해수면기준)`, NA))
summary(dfcw1$el_edt)

avg2<- mean(na.omit(dfcw1$`지하수위(옥천3)`))
view(dfcw1)

# 시각화----

p1<- ggplot(data= dfcw1, aes(x= 날짜, y= ELm))                                         +
     geom_line(color='black')                                                           +
     ggtitle("Groundwater")                                                            +
     theme(axis.text.x = element_text(angle = 60), plot.title=element_text(hjust=0.5)) +
     scale_x_date(breaks="6 months", expand = c(0,0), labels = date_format("%Y-%b"))   +
     xlab("Date")                                                                      +
     ylab("EL.m")

p1

p2<- ggplot(data= dfcw1, aes(x= 날짜, y= Soil))                                     +
  geom_line(color='brown')                                                          +
  ggtitle("Soil moisture")                                                          +
  theme(axis.text.x = element_text(angle = 60), plot.title=element_text(hjust=0.5)) +
  scale_x_date(breaks="6 months", expand = c(0,0), labels = date_format("%Y-%b"))   +
  xlab("Date")                                                                      +
  ylab("%")

p2

p3<- ggplot(data= dfcw1, aes(x= 날짜, y= -강수량))                                   +
  geom_line(color='Blue')                                                          +
  ggtitle("Precipitation")                                                          +
  theme(axis.text.x = element_text(angle = 60), plot.title=element_text(hjust=0.5)) +
  scale_x_date(breaks="6 months", expand = c(0,0), labels = date_format("%Y-%b"))   +
  xlab("Date")                                                                      +
  ylab("mm")

p3

com_p <- ggarrange(p3, p2, p1, ncol = 1, nrow = 3, align = "v")

com_p

ggsave("total2.png", plot = com_p, width = 20, height = 10, dpi = 1200)
#특정 시기 데이터 추출----
start_date<- as.Date("2018-05-01")
end_date<- as.Date("2018-08-31")
p1<- ggplot(data= dfcw1, aes(x= 날짜, y= ELm))                                       +
  geom_line(color='black')                                                           +
  ggtitle("Groundwater")                                                             +
  theme(axis.text.x = element_text(angle = 60), plot.title=element_text(hjust=0.5))  +
  scale_x_date(breaks="15 days", expand = c(0,0), labels = date_format("%Y-%m-%d"))  +
  coord_cartesian(xlim = c(start_date, end_date))                                    +
  geom_vline(xintercept = as.numeric(as.Date("2018-06-26")), linetype = 'dashed')    +
  geom_vline(xintercept = as.numeric(as.Date("2018-07-01")), linetype = 'dashed')    +
  ylim(min(dfcw1$ELm, na.rm = TRUE), max(dfcw1$ELm, na.rm = TRUE))                   +
  xlab("Date")                                                                       +
  ylab("EL.m")

p1

p2<- ggplot(data= dfcw1, aes(x= 날짜, y= Soil))                                      +
  geom_line(color='brown')                                                           +
  ggtitle("Soil moisture")                                                           +
  theme(axis.text.x = element_text(angle = 60), plot.title=element_text(hjust=0.5))  +
  scale_x_date(breaks="15 days", expand = c(0,0), labels = date_format("%Y-%m-%d"))  +
  coord_cartesian(xlim = c(start_date, end_date))                                    +
  geom_vline(xintercept = as.numeric(as.Date("2018-06-26")), linetype = 'dashed')    +
  geom_vline(xintercept = as.numeric(as.Date("2018-07-01")), linetype = 'dashed')    +
  ylim(min(dfcw1$Soil, na.rm = TRUE), max(dfcw1$Soil, na.rm = TRUE))                 +
  xlab("Date")                                                                       +
  ylab("%")

p2

p3<- ggplot(data= dfcw1, aes(x= 날짜, y= -강수량))                                   +
  geom_bar(fill='Blue', stat="identity")                                             +
  ggtitle("Precipitation")                                                           +
  theme(axis.text.x = element_text(angle = 60), plot.title=element_text(hjust=0.5))  +
  scale_x_date(breaks="15 days", expand = c(0,0), labels = date_format("%Y-%m-%d"))  +
  coord_cartesian(xlim = c(start_date, end_date))                                    +
  #geom_text(aes(label = 날짜), vjust = -0.5, size = 3)                               +
  ylim(min(-dfcw1$강수량, na.rm = TRUE), max(-dfcw1$강수량, na.rm = TRUE))           +
  geom_vline(xintercept = as.numeric(as.Date("2018-06-26")), linetype = 'dashed')    +
  geom_vline(xintercept = as.numeric(as.Date("2018-07-01")), linetype = 'dashed')    +
  xlab("Date")                                                                       +
  ylab("mm")

p3

com_p <- ggarrange(p3, p2, p1, ncol = 1, nrow = 3, align = "v")
com_p

ggsave("2018_2.png", plot = com_p, width = 10, height = 15, dpi = 1200)

# 특정 시기 분석(2)
start_date<- as.Date("2019-04-14")
end_date<- as.Date("2019-07-31")
p1<- ggplot(data= dfcw1, aes(x= 날짜, y= ELm))                                       +
  geom_line(color='black')                                                           +
  ggtitle("Groundwater")                                                             +
  theme(axis.text.x = element_text(angle = 60), plot.title=element_text(hjust=0.5))  +
  scale_x_date(breaks="15 days", expand = c(0,0), labels = date_format("%Y-%m-%d"))  +
  geom_vline(xintercept = as.numeric(as.Date("2019-05-18")), linetype = 'dashed')    +
  geom_vline(xintercept = as.numeric(as.Date("2019-06-06")), linetype = 'dashed')    +
  coord_cartesian(xlim = c(start_date, end_date))                                    +
  ylim(min(dfcw1$ELm, na.rm = TRUE), max(dfcw1$ELm, na.rm = TRUE))                   +
  xlab("Date")                                                                       +
  ylab("EL.m")

p1

p2<- ggplot(data= dfcw1, aes(x= 날짜, y= Soil))                                      +
  geom_line(color='brown')                                                           +
  ggtitle("Soil moisture")                                                           +
  theme(axis.text.x = element_text(angle = 60), plot.title=element_text(hjust=0.5))  +
  scale_x_date(breaks="15 days", expand = c(0,0), labels = date_format("%Y-%m-%d"))  +
  coord_cartesian(xlim = c(start_date, end_date))                                    +
  geom_vline(xintercept = as.numeric(as.Date("2019-05-18")), linetype = 'dashed')    +
  geom_vline(xintercept = as.numeric(as.Date("2019-06-06")), linetype = 'dashed')    +
  ylim(min(dfcw1$Soil, na.rm = TRUE), max(dfcw1$Soil, na.rm = TRUE))                 +
  xlab("Date")                                                                       +
  ylab("%")

p2

p3<- ggplot(data= dfcw1, aes(x= 날짜, y= -강수량))                                   +
  geom_bar(fill='Blue', stat="identity")                                             +
  ggtitle("Precipitation")                                                           +
  theme(axis.text.x = element_text(angle = 60), plot.title=element_text(hjust=0.5))  +
  scale_x_date(breaks="15 days", expand = c(0,0), labels = date_format("%Y-%m-%d"))  +
  coord_cartesian(xlim = c(start_date, end_date))                                    +
  ylim(min(-dfcw1$강수량, na.rm = TRUE), max(-dfcw1$강수량, na.rm = TRUE))           +
  geom_vline(xintercept = as.numeric(as.Date("2019-05-18")), linetype = 'dashed')    +
  geom_vline(xintercept = as.numeric(as.Date("2019-06-06")), linetype = 'dashed')    +
  #geom_text(aes(label = 날짜), vjust = -0.5, size = 3)                               +
  xlab("Date")                                                                       +
  ylab("mm")

p3

com_p <- ggarrange(p3, p2, p1, ncol = 1, nrow = 3, align = "v")
com_p

ggsave("tota3.png", plot = com_p, width = 10, height = 15, dpi = 1200)

# 상관성 분석----

view(dfcw1)
dfcw2  <- select(.data = dfcw1, -No,-관측지점,-주소, -연도, -월)
view(dfcw2)


# 특정 기간 한정산점도도----

dftest <- subset(dfcw1, dfcw1$연도==2018)
dftest<- subset(dftest, dfcw1$월>=5 & dfcw1$월<=8)
view(dftest)
testplot<- ggplot(data= dftest, mapping = aes(x= dftest$날짜)) +
  geom_line(aes(y= dfcw3333$`지하수위(옥천3)`))+
  ggtitle("옥천3(2017-2023)")        

dftest %>% 
  ggplot2::ggplot(mapping = aes(x = ELm, y = Soil))    + 
  ggplot2::geom_point()


cor.test(dftest$ELm, dftest$Soil, method="spearman")
cor.test(dftest$Soil, dftest$ELm, method="spearman")

# 특정기간(2)
dftest2 <- subset(dfcw1, dfcw1$연도==2019)
dftest2 <- subset(dftest2, dfcw1$월>=4 & dfcw1$월<=7)
view(dftest2)
       

dftest2 %>% 
  ggplot2::ggplot(mapping = aes(x = ELm, y = Soil))    + 
  ggplot2::geom_point()


cor.test(dftest2$ELm, dftest2$Soil, method="spearman")
cor.test(dftest2$Soil, dftest2$ELm, method="spearman")

# 강수량과의 상관성
# 자기상관
non<- na.omit(dfcw1)
# na값을 0으로 대체
dfcw1$강수량[is.na(dfcw1$강수량)] <-0 
dfcw1$강수량[is.na(dfcw1$ELm)] <-0 
dfcw1$강수량[is.na(dfcw1$Soil)] <-0 

view(dfcw1)
a<- acf(dfcw1$ELm, lag.max=200, type=c("correlation"), plot=TRUE)
plot(a$lag, a$acf, ylim=c(-1,1), xlab="lag(Day)", ylab="Auto-correlation", main="옥천3", type="l")
abline(h=0, lty=2, lwd=1.5)

# 교차상관(강수량)

raincor<- ccf(dfcw1$강수량, dfcw1$ELm, lax.max=5, plot=TRUE)
plot(raincor$lag, raincor$acf, ylim=c(-1,1), xlab="lag(Day)", ylab="Cross-correlation", main="옥천3", type="l") 

testrain<- data.frame(raincor$lag, raincor$acf)
plot(testrain, ylim=c(-1,1), xlab="lag(Day)", ylab="Cross-correlation", main="옥천3", type="l")
abline(h=0, lty=2, lwd=1.5)
abline(v=0, lty=2, lwd=1.5)
max(testrain$raincor.acf)
view(testrain)

cor.test(dfcw1$ELm, dfcw1$강수량, method="spearman")
cor.test(dfcw1$강수량, dfcw1$ELm, method="spearman")
cor(dfcw1$ELm, dfcw1$강수량, method="spearman")
# 상관분석 H0: 상관성이 없다. 상관성이 없음!
view(dfcw1)

# 정규성 검정
shapiro.test(dfcw2$ELm)
shapiro.test(dfcw2$Soil)
shapiro.test(dfcw2$강수량)

dfcw2 %>% 
  purrr::keep(is.numeric) -> dfcw3 
view(dfcw3)

cor.test(dfcw3$강수량, dfcw3$ELm, method="spearman")
cor.test(dfcw3$강수량, dfcw3$Soil, method="spearman")
cor.test(dfcw3$Soil, dfcw3$ELm, method="spearman")

psych::corr.test(dfcw3, method = "spearman")
corr(dfcw3)

cor(dfcw3, method = "spearman") %>% 
  round(digits        = 3)

dfcw3 %>% 
  GGally::ggpairs()    -> cortest


cortest

dfcw3  <- select(.data = dfcw1, -날짜, -No,-관측지점,-주소, -강수량, -측정년도, -측정월)
view(dfcw3)
dfcw3  <- select(.data = dfcw3, -강수량)

dfcw3 %>% 
  GGally::ggpairs()    -> dfcw4
dfcw4
cor(dfcw1$`토양수분측정값(보정)`, dfcw1$`지하수위(옥천3)`, method="pearson")
dfcw1


dfcw2 <- dfcw1 %>% 
  mutate(측정년도             = year(날짜))
view(dfcw3)
dfcw3 <- subset(dfcw2, dfcw2$측정년도==2022)

dfcw33 <- dfcw2 %>% 
  mutate(측정월             = month(날짜))

view(dfcw33)

dfcw333 <- subset(dfcw33, dfcw33$측정년도==2022)
dfcw3333<- subset(dfcw333, dfcw333$측정월>=4 & dfcw333$측정월<=8)
view(dfcw3333)
g1<- ggplot(data= dfcw3333, mapping = aes(x= dfcw3333$날짜)) +
  geom_line(aes(y= dfcw3333$`지하수위(옥천3)`))+
  ggtitle("옥천3(2017-2023)")        

g1

g2<- g1+ geom_line(aes(y=dfcw3333$`토양수분측정값(보정)`*0.05), stat= "identity", colour = "blue", alpha= 0.5)
g3<- g2+scale_y_continuous(sec.axis=sec_axis(~./0.05, name= "토양수분"))
g3
g4<- g3+ scale_y_break(c(2, 106.5))
g4


dfcw4  <- select(.data = dfcw3333, -날짜, -No,-관측지점,-주소, -강수량, -측정년도, -측정월)
view(dfcw4)
dfcw4 %>% 
  GGally::ggpairs()    -> dfcw5
dfcw5


# 강수량과의 분석----

# 강수량 자료 합치기----
aosrain <- read_xlsx(file.choose(), 
                     col_names = c("지점", "지점명", "날짜", "강수량"), skip=1)
view(aosrain)
aosrain <-  aosrain %>% 
  subset(지점명=="보은")


head(aosrain)
#  강우자료 많을 시 추출 rain<-subset(aosrain, aosrain$지점명=="강릉")
dfgw1 <- merge(cw1,aosrain,by="날짜",all=TRUE)
dfgw1 <- select(.data = dfcw1, -지점, -지점명)
head(dfgw1)
view(dfgw1)

dfgw1 <- dfgw1 %>% 
  mutate(날짜           = ymd(날짜))
str(dfgw1)
#통계량 확인----
summary(dfcw1$`수위(해수면기준)`)

# 이상치 제외(+-50%, 조정 가능)----
avg<-mean(na.omit(dfcw1$`수위(해수면기준)`))
dfcw1 <- dfcw1 %>% 
  mutate(el_edt   = ifelse(`수위(해수면기준)`>=0.03*avg & `수위(해수면기준)`<=3*avg,
                           `수위(해수면기준)`, NA))
summary(dfcw1$el_edt)

avg2<- mean(na.omit(dfcw1$`지하수위(옥천3)`))
view(dfcw1)
# 시각화----

avg<-mean(na.omit(dfcw1$`수위(해수면기준)`))
dfcw1 <- dfcw1 %>% 
  mutate(el_edt   = ifelse(`수위(해수면기준)`>=0.03*avg & `수위(해수면기준)`<=3*avg,
                           `수위(해수면기준)`, NA))
summary(dfcw1$el_edt)

avg2<- mean(na.omit(dfgw1$`지하수위(옥천3)`))

# 시각화----
a<- ggplot(data = dfgw1, mapping = aes(x = 날짜))      +
  geom_line(aes(y = `지하수위(옥천3)`))                  +
  geom_hline(yintercept = avg2, linetype = 'dashed')           +
  ggtitle("옥천3(2015-2023)")                                  +
  scale_x_date(breaks="2 months", expand = c(0,0), labels = date_format("%Y-%b"))           +                   
  theme(axis.text.x = element_text(angle = 60), plot.title=element_text(hjust=0.5))         +
  xlab("Date")                                          +
  ylab("El.m")

a

aa <- a + geom_bar(aes(y=강수량*0.01), stat = "identity", colour="grey", alpha=0.5)
aa
aa <- aa    +
  scale_y_continuous(sec.axis = 
                       sec_axis(~./0.01, name= "precipitation", breaks = seq(0, 400, by = 50)))
aa                         

a4<- aa+ scale_y_break(c(2.25, 106.5))
a4

ㅠby


p1 <- ggplot(data= dfcw1, mapping = aes(x= dfcw1$날짜, y= dfcw1$ELm))                +
  geom_line(aes(y= dfcw1$ELm))                                                      +
  ggtitle("옥천3(2017-2023)")                                                       +
  theme(axis.text.x = element_text(angle = 60), plot.title=element_text(hjust=0.5)) +
  scale_x_date(breaks="6 months", expand = c(0,0), labels = date_format("%Y-%b"))   +
  xlab("Date")                                                                      +
  ylab("EL.m")
