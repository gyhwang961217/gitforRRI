
df2019 <- subset(dfcw1, dfcw1$연도==2019)
df2019 <- subset(df2019, dfcw1$월>=4 & dfcw1$월<=7)

cor.test(df2019$강수량, df2019$ELm, method = "spearman")
view(df2019)

df2019 %>% 
  ggplot2::ggplot(mapping = aes(x = 강수량, y = ELm))    + 
  ggplot2::geom_point()

# 결측 제거
at<- data.frame(df2019$강수량, df2019$ELm)
at<- na.omit(at)
view(at)
# 지하수위와 강수량 위치 중요.

raincor<- ccf(at$df2019.ELm, at$df2019.강수량, plot=TRUE)
max(raincor$acf)

bt <- data.frame(raincor$lag, raincor$acf)

view(bt)
am<- ggplot(data=bt, mapping=aes(x=bt$raincor.lag)) +
     geom_bar(aes(y=bt$raincor.acf), stat= "identity", fill='blue', alpha=0.5) +
     geom_line(aes(y=bt$raincor.acf)) +
  ggtitle("Cross-Correlation")                                                         +
  theme(axis.text.x = element_text(angle = 60), plot.title=element_text(hjust=0.5))  +
  geom_vline(xintercept = 6, linetype = 'dashed')    +
  geom_hline(yintercept = 0.29, linetype = 'dashed')    +
  xlab("lag(Day)") +
  ylab("Cross-correlation")

am


plot(raincor$lag, raincor$acf, ylim=c(-1,1), xlab="lag(Day)", ylab="Cross-correlation", main="옥천3", type="l") 


testplot<- ggplot(data= dftest, mapping = aes(x= dftest$날짜)) +
  geom_bar(aes(y= dfcw3333$`지하수위(옥천3)`))+
  ggtitle("옥천3(2017-2023)")   



df2018 <- subset(dfcw1, dfcw1$연도==2018)
df2018 <- subset(df2018, dfcw1$월>=5 & dfcw1$월<=8)
