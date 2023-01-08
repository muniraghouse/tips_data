library(kableExtra)
library(plotly)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(GGally)
library(corrplot)
td=read.csv("C:/Users/Administrator/Desktop/INTERN/csv datasets/tips.csv",header = T)
str(td)
td_fact=c("sex","smoker","day","time","size")
td=td %>% mutate(across(.cols = all_of(td_fact),.fns = as.factor))
###UIvariate============
ggplot(td,aes(x=tip))+
  geom_histogram(color="green",fill="pink")
ggplot(td,aes(x=total_bill))+
  geom_histogram(color="brown",fill="green")
###Total bill vs tips============
ggplot(td,aes(x=total_bill,y=tip))+
  geom_point(size=3,color='red')
cc=cor.test(td$tip,td$total_bill)
cf=rbind(cbind(cc$p.value,cc$conf.int[1],cc$conf.int[2],cc$estimate,cc$null.value))
cf1=data.frame(cf)
colnames(cf1)=c("p-value","Confidence interval-LL","Confidence interval-HL","sample estimate")
cf1
##box plotfor smoker
ggplot(td,aes(x=smoker,y=tip))+
  geom_boxplot()
##density plot for 
ggplot(td, aes(tip,fill=sex)) + 
  geom_density() + 
  xlab("Tips")+ 
  ylab("Sex")+
  facet_grid(~smoker)
td %>% group_by(smoker,sex) %>% summarise(count=n(),
                                          Average=mean(tip),
                                          minimum=min(tip),
                                          maximum=min(tip),
                                          standrad=sd(tip))
ss=t.test(tip~smoker,data = td)
sf=rbind(cbind(ss$p.value,ss$conf.int[1],ss$conf.int[2],ss$estimate,ss$null.value))
sf1=data.frame(sf)
colnames(sf1)=c("p-value","Confidence interval-LL","Confidence interval-HL","sample estimate")
sf1

##bar plot for sex and tip using facet====
ggplot(td, aes(tip,fill=sex)) + 
  geom_density() + 
  xlab("Tips")+ 
  ylab("Sex")+
  facet_grid(~day)

td %>% group_by(day,sex) %>% summarise(count=n(),
                                          Average=mean(tip),
                                          minimum=min(tip),
                                          maximum=min(tip),
                                          standrad=sd(tip))
s1=t.test(tip~sex,data = td)
sd=rbind(cbind(s1$p.value,s1$conf.int[1],s1$conf.int[2],s1$estimate,s1$null.value))
sd1=data.frame(sd)
colnames(sd1)=c("p-value","Confidence interval-LL","Confidence interval-HL","sample estimate")
sd1
t=aov(tip~day,data=td)
summary(t)
###time vs tip bar plot=============
ggplot(data=td) + geom_bar(aes(x=time,fill = tip))
td %>% group_by(time) %>% summarise(count=n(),
                                          Average=mean(tip),
                                          minimum=min(tip),
                                          maximum=min(tip),
                                          standrad=sd(tip))
####pie for time and tips
ggplot(td,
          aes(x = factor(""), fill = time)) +
  geom_bar() +
  coord_polar(theta = "y") +
  scale_x_discrete("")+
  labs(title="Distribution of time",
       x="time", 
       y="tip")+scale_fill_brewer(palette = "Set1")

td %>% group_by(time) %>% summarise(count=n(),
                                          Average=mean(tip),
                                          minimum=min(tip),
                                          maximum=min(tip),
                                          standrad=sd(tip))
c1=t.test(tip~time,data = td)
df=rbind(cbind(c1$p.value,c1$conf.int[1],c1$conf.int[2],c1$estimate,c1$null.value))
df1=data.frame(df)
colnames(df1)=c("p-value","Confidence interval-LL","Confidence interval-HL","sample estimate")
df1
####service vs tips==========
ggplot(td,aes(x=size,y=tip))+
  geom_boxplot(fill="pink",color="brown")

a1=aov(tip~size,td)
summary(a1)
a1_t2=TukeyHSD(a1)
a1_t12=data.frame(a1_t2$size)
a1_t12
