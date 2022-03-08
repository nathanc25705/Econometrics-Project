library("stargazer")
library("haven")
library("tidyverse")
library("dplyr")
library("summarytools")
library("vtable")
data<- as.data.frame(read_dta("data_giulietti_etal.dta"))

###Question 1, Table of summary statistics###

# convert columns to type numeric #
data$complexity<-as.numeric(data$complexity)
data$race<-as.numeric(data$race)
data$sender<-as.numeric(data$sender)
data$recipient<-as.numeric(data$recipient)

sumtable(data,vars=c("reply","complexity","cordial_reply","length_reply",
                     "delay_reply","race","sender","recipient"),
         summ=c("notNA(x)","mean(x)","sd(x)","median(x)","min(x)","max(x)",
                "countNA(x)"),
         summ.names=c(" Number of Obs.","Mean","SD","Median","Min","Max",
                      "Number of Missing Obs."),out="return" )

###Question 2, Frequency Table###

freqtable<-data%>%
  count(recipient) %>% 
  mutate(prop = prop.table(n))
rownames(freqtable)<-c("School District","Library","Sheriff","Treasurer",
                       "Job Centre Veterans Rep.","County Clerk")
colnames(freqtable)<-c("Recipient Number","  No. Emails",
                       "  Prop. Emails")
freqtable


###Question 3, Cross Tabulation###

Table3<-ctable(data$recipient,data$sender,prop="c",totals=FALSE,style="simple",dnn=c("Recipient","Sender"))
Table3

###Question 4, Data manipulation with Tidyverse###

dataq4<-data %>% 
  select(race,cordial_reply,length_reply,delay_reply,reply)%>%
  gather(key = Variable, value = value, -race)%>%
  group_by(race, Variable) %>% 
  summarise(value = list(value))%>%
  spread(race,value)%>%
  group_by(Variable)%>%
  mutate(Mean_White=mean(unlist(`0`),na.rm=TRUE,digits=3),
         Mean_Black=mean(unlist(`1`),na.rm=TRUE,digits=3),
         Differences=Mean_White-Mean_Black,
         P_value = t.test(unlist(`0`), unlist(`1`))$p.value
  )
select(dataq4,-`0`,-`1`)


###Question 5, Simple regression with Stargazer###

reg1<-lm(reply~race,data=data)
stargazer(reg1,type="text")


###Question 6, Data Manipulation and Visualization with Tidyverse and ggplot

# Changing race from 0 and 1 to white and black #
data$race=factor(data$race,levels=c(1,0),labels=c("Black","White"))
dataq6 <- data %>%
  select(race,reply,cordial_reply,length_reply)%>%
  group_by(race)%>%
  summarise(mean_reply = mean(reply,na.rm=TRUE), 
            mean_cr= mean(cordial_reply,na.rm=TRUE),
            mean_lr= mean(length_reply,na.rm=TRUE),
            sd_reply = sd(reply,na.rm=TRUE),
            sd_cr = sd(cordial_reply,na.rm=TRUE),
            sd_lr = sd(length_reply,na.rm=TRUE))

plot1 <-ggplot(dataq6, aes(race,mean_reply)) +
  geom_col()  

plot1 + labs(y="Mean Reply ± s.d.", x = "Race") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_bar(stat="identity",fill="skyblue1")+
  geom_errorbar(aes(ymin = mean_reply - sd_reply, ymax = mean_reply + sd_reply),
                width=0.2)+
  theme_classic()+
  ggtitle("Mean Reply and Variation by Race")



plot2 <-ggplot(dataq6, aes(race,mean_cr)) +
  geom_col()  

plot2 + labs(y="Mean Cordial Reply ± s.d.", x = "Race") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_bar(stat="identity",fill="skyblue1")+
  geom_errorbar(aes(ymin = mean_cr - sd_cr, ymax = mean_cr + sd_cr),
                width=0.2)+
  theme_classic()+
  ggtitle("Mean Cordial Reply and Variation by Race")


plot3 <-ggplot(dataq6, aes(race,mean_lr)) +
  geom_col()  

plot3 + labs(y="Mean Reply Length ± s.d.", x = "Race") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_bar(stat="identity",fill="skyblue1")+
  geom_errorbar(aes(ymin = mean_lr - sd_lr, ymax = mean_lr + sd_lr),
                width=0.2)+
  theme_classic()+
  ggtitle("Mean Reply Length and Variation by Race")


###Question 7, Simple regression with Stargazer###

data<- as.data.frame(read_dta("data_giulietti_etal.dta"))
reg2<-lm(complexity~race,data=data)
stargazer(reg2,type="text")




####Part C, Monte Carlo Simulation and Visualization~~~~

dgp1<- function(n){
  x<-rnorm(n,100,15)
  u<-rnorm(n,0,8)
  b1=2
  y<-(b1*x)+u
  df<-data.frame(x,y)
  reg<-lm(y~x,data=df)
  b1<-coefficients(reg)[2]
  summary<-summary(reg)
  se<-summary$coefficients[2,2]
  vector=c(b1,se)
  return(vector)
}
set.seed(1234)
data<-(replicate(1000,dgp1(100)))
b1<-data[1,]
se<-data[2,]
ggplot() + geom_histogram(aes(b1),color="darkblue", fill="lightblue")+theme_classic()+
  geom_vline(aes(xintercept=mean(b1)),
             color="orange", linetype="dashed", size=1)+
  xlab(expression("Estimate" ~ hat(beta) [1]))+
  ggtitle(expression("Sampling Distribtion of" ~ widehat(beta) [1] ~ "(n=1000)"))

#Number of confidence intervals that include 2#
confint_upper<-c(b1+1.96*se)
confint_lower<-c(b1-1.96*se)
observations<-0
for (n in 1:length(confint_upper)) {
  if((confint_upper[n]>2 & confint_lower[n]<2)==TRUE){
    observations<-observations+1}}
#Proportion of confidence intervals that include the mean 2#
observations/length(confint_upper)