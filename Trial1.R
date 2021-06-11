
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("cowplot")
#install.packages("readr")
#install.packages( "lavaan")
#install.packages("smooth", "Hmisc")
#install.packages("plyr")
#install.packages("scales")

source("R_rainclouds.R")
source("summarySE.R")
source("simulateData.R")

library("plyr")
library("smooth","Hmisc")
library("cowplot")
library("readr")
library("ggplot2")
library("dplyr")
library("scales")
library("patchwork")
library("hrbrthemes")



riskdata<- read.csv("credit_risk_dataset.csv")
riskdata1<-na.omit(riskdata)
crd<-subset(riskdata1,riskdata1$person_age<90)
View(crd)
dim(crd)
summary(crd)


plot<-hist(crd$person_age)
plot$breaks<-c("20-25","25-30","30-35","35-40","40-45","45-50","50-55","55-60",
               "60-65","65-70","70-75","75-80","80-85","85<")
data.frame(plot$breaks,c(plot$counts,0))
### 
plot1<-ggplot(data=crd, aes(person_age,fill=as.factor(loan_status)))+
  geom_histogram(
    breaks=seq(0,90,by=5),
    col="black",
    alpha=0.5)+
  labs(
    title="Histogram of age data",
      x="age",
      y="frequency")+
  facet_wrap(~loan_intent)
plot1a<-ggplot_build(plot1)
plot1a

sp1a<-split(plot1a$data[[1]],plot1a$data[[1]]$PANEL)
sp1adf<-list()
h<-list()
for (i in 1:6){
  sp1adf[[i]]<-data.frame(x=unique(sp1a[[i]]$x),y=(subset(sp1a[[i]],sp1a[[i]]$group==2)$count/(subset(sp1a[[i]],sp1a[[i]]$group==2)$count+subset(sp1a[[i]],sp1a[[i]]$group==1)$count)))
  
  h[[i]]<-ggplot(omitted<-na.omit(sp1adf[[i]]),aes(x=x,y=y))+
    geom_line()
}
plot1b<-plot_grid(plotlist = h,
                  labels = c("DEBTCONSOLIDATION","               EDUCATION","    HOMEIMPROVEMENT",
                             "            MEDICAL","             PERSONAL","            VENTURE"),
                  label_size = 8,
                  hjust = -1,
                  vjust = 0.8
                  )
plot1b
nrow(crd)
###
plot2<- ggplot(data=crd, aes(x=loan_status ,y=person_age,fill=as.factor(loan_status)))+
  geom_flat_violin(position = position_nudge(x=0.1,y=0),adjust=1)+
  geom_point(position = position_jitter(width=0.1),size=0.1,alpha=0.1)+
  geom_boxplot(aes(x=as.numeric(loan_status)+0.1,y=person_age)
               ,width=0.1,outlier.shape = NA,alpha=0.5)+
  facet_wrap(~loan_intent)+
  coord_flip()+
  labs(
    title= "Raincloud plot age data",
    x="Count grouped acc. to Loan Status",
    y="Age"
    )

plot2  


###


crd
crd1<-crd[,c(1,2,5,7,8,9)]
crd1$FOIR<-(crd1$loan_amnt+crd1$loan_amnt*(crd1$loan_int_rate/100))/crd1$person_income*100

plot3<-ggplot(crd1, aes(x=FOIR,fill=as.factor(loan_status)))+
  geom_histogram(color="black",
                 alpha=0.7
              )+
  labs(
    title = "Histogram of the Fixed Obligation to Income Ratios.",
    x="FOIR(in%)",
    y="frequency"
  )
plot3a<-ggplot_build(plot3)  
plot3a

nrow(plot3a$data[[1]][plot3a$data[[1]]$group==2,])
p3a<-plot3a$data[[1]]
p3a$perc_count<-rep(p3a[p3a$group==2,]$count*100/(p3a[p3a$group==2,]$count+p3a[p3a$group==1,]$count),2)
p3a<-na.omit(p3a)
nrow(p3a)

plot3b<-ggplot(p3a,aes(x=x,y=perc_count))+
  geom_line(
    size=1.2,
    alpha=0.6,
    lty="dotdash"
  )+
  labs(
    title="Line Plot describing FOIR vs %bad-loans",
    x="FOIR(in %)",
    y="% bad loans"
  )+
  scale_x_continuous(breaks = seq(0,100,10))+
  scale_y_continuous(labels=percent)
plot3b


####

plot1c<-ggplot(data=riskdata1, aes(person_age,fill=as.factor(loan_status)))+
  geom_bar(
    col="black",
    alpha=0.5,
    width=5)+
  labs(
    title="Histogram of age data",
    x="age",
    y="frequency")
plot1c<-ggplot_build(plot1c)
plot1c

###
plot1c$data[[1]]
sp1c<-split(plot1c$data[[1]],as.factor(plot1c$data[[1]]$group))
sp1c[[2]]$bg<-sp1c[[2]]$count/(sp1c[[1]]$count)
sp1c[[2]]
ggplot(sp1c[[2]],aes(x=x))+
  geom_col(aes(y=sp1c[[2]]$count))+
  
  geom_line(aes(y=bg))

ggplot()+
  geom_col(data=sp1c[[1]],aes(x=x,y=count),fill="blue",col="black")+
  geom_col(data=sp1c[[2]],aes(x=x,y=count),fill="red",col="black")+
  geom_line(data=sp1c[[2]],aes(x=x,y=bg*10000),col="green")+
  scale_y_continuous(
    name="Population",
    sec.axis=sec_axis(trans = ~./10000, name="bad rates")
  )
####
plot1d<-ggplot()+
  geom_bar(data=riskdata1
           ,aes(person_age,fill=as.factor(loan_status))
            ,width = 3,
           stat = "count",
           position = "stack")+
  scale_x_continuous(n.breaks = 5,
                    limits = c(15,80))
plot1d<-ggplot_build(plot1d)
plot1d
total<-sum(plot1d$data[[1]]$count)
total
plot1d$data[[1]]$count<-plot1d$data[[1]]$count*100/total
length(rep(plot1d$data[[1]][plot1d$data[[1]]$group==2,plot1d$data[[1]]$count],2))#/
length(plot1d$data[[1]]$count)
ggplot()+
  geom_bar(data=plot1d$data[[1]],
           aes(x=x,y=count),
           stat = "identity")


(plot1d$data[[1]]["group"==1,])


hi<-crd
hello<-group_by(hi, G=cut(person_age,breaks=quantile(person_age,probs=seq(0,1,0.1))))
badhello<-hello[hello$loan_status==1,]
goodhello<-hello[hello$loan_status==0,]
yo<-ggplot()+
  geom_bar(data=hello,aes(G))
yo<-ggplot_build(yo)
yo1<-(yo$data[[1]][-11,])
badyo<-ggplot()+
  geom_bar(data=badhello,aes(G))
badyo<-ggplot_build(badyo)
badyo1<-badyo$data[[1]][-11,]
goodyo<-ggplot()+
  geom_bar(data=goodhello,aes(G))
goodyo<-ggplot_build(goodyo)
goodyo1<-goodyo$data[[1]][-11,]
yo1$bdl_perc<-badyo1$count*100/yo1$count
yo1$gdl_perc<-goodyo1$count*100/yo1$count
yo1$woe<-log(yo1$gdl_perc/yo1$bdl_perc)
yo1$G<-levels(as.factor(hello$G))
yo1$ppn_perc<-yo1$count*100/sum(yo1$count)
yo1

ggplot()+
  geom_bar(data=yo1,
           aes(x=G,y=ppn_perc),
           stat="identity")+
  geom_line(data=yo1,
            aes(x,y=bdl_perc))+
  geom_point(data=yo1,
            aes(x,y=bdl_perc))

