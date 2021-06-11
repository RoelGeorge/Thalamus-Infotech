#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("shinythemes")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("cowplot")
#install.packages("scales")
#install.packages("readr")
#install.packages( "lavaan")
#install.packages("smooth", "Hmisc")
#install.packages("plyr")

library("cowplot")
library("dplyr")
library("shiny")
library("shinydashboard")
library("shinythemes")
library("ggplot2")
library("scales")
library("plyr")
library("smooth","Hmisc")
library("readr")

source("R_rainclouds.R")
source("summarySE.R")
source("simulateData.R")


ui<-fluidPage(theme=shinytheme("darkly"),
  titlePanel(title ="App1" ),
  sidebarPanel(width = 3),
  mainPanel(
  wellPanel(fluidRow(
    column(6,plotOutput("plot1a")),
    column(5,plotOutput("plot1b"))
  )),
  wellPanel(fluidRow(
    column(6,plotOutput("plot2"))
  )),
  wellPanel(fluidRow(
    column(6,plotOutput("plot3a")),

    column(5,plotOutput("plot3b"))
  )))
 
)


server<- function(input,output){

  output$plot1a<-renderPlot({
    riskdata= read.csv("credit_risk_dataset.csv")
    riskdata1=na.omit(riskdata)
    crd=subset(riskdata1,riskdata1$person_age<90)
    
    plot1<-ggplot(data=crd, aes(person_age,fill=as.factor(loan_status)))+
      geom_histogram(
        
        breaks=seq(15,95,by=5),
        col="black",
        alpha=0.5)+
      labs(
        title="Histogram of age data",
        x="age",
        y="frequency")+
      facet_wrap(~loan_intent)
    
    plot1a<- ggplot_build(plot1)
    plot1a
  })
  
  output$plot1b<-renderPlot({
    sp1a<-split(plot1a$data[[1]],plot1a$data[[1]]$PANEL)
    sp1adf<-list()
    h<-list()
    for (i in 1:6){
      sp1adf[[i]]<-data.frame(x=unique(sp1a[[i]]$x),y=(subset(sp1a[[i]],sp1a[[i]]$group==2)$count/(subset(sp1a[[i]],sp1a[[i]]$group==2)$count+subset(sp1a[[i]],sp1a[[i]]$group==1)$count)))
      
      h[[i]]<-ggplot(omitted<-na.omit(sp1adf[[i]]),aes(x=x,y=y))+
        geom_line()
    }
    plot1b<-plot_grid(plotlist = h,
                      labels = c("DEBTCONSOLIDATION","          EDUCATION","HOMEIMPROVEMENT",
                                 "       MEDICAL","       PERSONAL","                VENTURE"),
                      label_size = 8,
                      hjust =-0.6,
                      vjust = 1.8
    )
    plot1b
    
  })
  
  output$plot2<- renderPlot({
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
    
  })
  
  output$plot3a<-renderPlot({
    crd1<-crd[,c(1,2,5,7,8,9)]
    crd1$FOIR<-(crd1$loan_amnt+crd1$loan_amnt*(crd1$loan_int_rate/100))/crd1$person_income*100
    
    plot3<-ggplot(crd1, aes(x=FOIR,fill=as.factor(loan_status)))+
      geom_histogram(color="black",
                     alpha=0.7,
      )+
      labs(
        title = "Histogram of the Fixed Obligation to Income Ratios.",
        x="FOIR(in%)",
        y="frequency"
      )
    plot3a<-ggplot_build(plot3)  
    plot3a
  })
  
  output$plot3b<-renderPlot({
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
      scale_y_continuous()
    plot3b
  })
  
  
}



shinyApp(ui=ui, server=server)









