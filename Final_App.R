"""
	@script-author: Jyothi Tom, Shekhina Neha, Gokul S, Aishwarya Lakshmi, Srija S, Robin Wilson
	@script-description: RShiny app to predict the risk of Auto Insurance Fraud   
 	@script-details: Written in RStudio
"""

library(shiny)
library(shinydashboard)
library(faraway)
library(ggplot2)
library(dplyr)
#********************************************************************************************************************
#Remove columns with null values

insurance <- read.csv("C:\\Users\\jyoth\\Desktop\\SEM 2\\Bhogle Sir's\\R Proj\\Insurance\\Final\\cleaned_data.csv")
null_cols = unique(names(insurance)[which(insurance == '?', arr.ind=T)[, "col"]])
insurance[,c(null_cols,"policy_number")]<-NULL

ins123<-insurance

ins123[((ins123$age>=19) & (ins123$age<=28)),"age_grp"]<-"19-28"
ins123[(ins123$age>=29) & (ins123$age<=38),"age_grp"]<-"29-38"
ins123[(ins123$age>=39) & (ins123$age<=48),"age_grp"]<-"39-48"
ins123[(ins123$age>=49) & (ins123$age<=58),"age_grp"]<-"49-58"
ins123[(ins123$age>=59) & (ins123$age<=68),"age_grp"]<-"59-68"

ins123[(round(ins123$months_as_customer/12,0)>=0 & round(ins123$months_as_customer/12,0)<=9),"custyears"]<-"0-9"
ins123[(round(ins123$months_as_customer/12,0)>=10 & round(ins123$months_as_customer/12,0)<=19),"custyears"]<-"10-19"
ins123[(round(ins123$months_as_customer/12,0)>=20 & round(ins123$months_as_customer/12,0)<=29),"custyears"]<-"20-29"
ins123[(round(ins123$months_as_customer/12,0)>=30 & round(ins123$months_as_customer/12,0)<=40),"custyears"]<-"30-40"

#***********************************************************************************************************************
# Plot Functions
#***********************************************************************************************************************
cols<-c("blue","red","purple","deepskyblue","orange","darkgreen","darkturquoise","gold","firebrick4","violetred")


bar_plot <- function(x1,y1,main){
  cont<-table(ins123[,x1],ins123[,y1])
  dat<-data.frame(p=round(cont[,2]/rowSums(cont)*100,2))
  plt <- ggplot(data=dat, aes(x=row.names(dat), y=p)) +
    geom_bar(stat="identity", fill=sample(cols,1))  +
    geom_text(aes(label=p), vjust=-0.3, size=3.0)+
    labs(x=strsplit(main,"vs")[[1]][2],y=c(strsplit(main,"vs")[[1]][1], "(in %)"),title=main)#+
  return(plt)
}

#------------------------------------------------------------------------------------------------------------------------

line_plot<- function(x1,y1,main){
  cont<-table(ins123[,x1],ins123[,y1])#;cont
  dat<-data.frame(y=round(cont[,2]/rowSums(cont)*100,2))
  plt<-ggplot(data=dat, aes(x=as.integer(row.names(dat)), y=round(cont[,2]/rowSums(cont)*100,2),group=1)) +
    geom_line(linetype="solid",color=sample(cols,1),size=1) +  
    ylim(0,100)+
    labs(x=strsplit(main,"vs")[[1]][2],y=strsplit(main,"vs")[[1]][1],title=main)+
    theme_minimal()
  return(plt)
}

#-------------------------------------------------------------------------------------------------------------------------
#Multiple Bar

mult_bar<-function(x1,y1,main){
  cont<-data.frame(a=ins123[,x1],b=ins123[,y1])
  dat<-cont %>%
    group_by(a,b) %>%
    summarise(n=n())
  plt <- ggplot(data=dat, aes(x=dat$a, y=n, fill=dat$b)) +
    geom_bar(stat="identity", position=position_dodge())  +
    geom_text(aes(label=n), vjust=-0.4, size=3.0) +
    
    labs(x=strsplit(main,"vs")[[1]][2], y=strsplit(main,"vs")[[1]][1], title=main,fill=strsplit(main,"vs")[[1]][1])+
    theme(legend.position = "right")
  return(plt)
}

#***********************************************************************************************************************

ui<-dashboardPage(skin = "red",
                  dashboardHeader(title="HMIC(Hakuna Matata Insurance Company)",titleWidth=2000),
                  dashboardSidebar(
                    sidebarMenu(
                      menuItem("Home", tabName = "home", icon = icon("Home")),
                      menuItem("FRAUD DETECTION", tabName = "fraud_detection", icon = icon("dashboard")),
                      menuItem("Insights", tabName = "charts")
                    )
                  ),
                  dashboardBody(
                    
                    tabItems(
                      #HOMEPAGE
                      tabItem(tabName = "home",
                              #h3("HMIC(HAKUNA MATATA INSURANCE COMPANY"),
                              tags$img(
                                src = "https://mk0bfsieletsonlt96u6.kinstacdn.com/wp-content/uploads/2018/12/insurance-plan.jpg",
                                height = "80%",
                                width="100%")
                          
                      ),
                      #FRAUD DETECTION
                      tabItem(tabName = "fraud_detection",
                              fluidRow(
                                column(5,selectInput("select1", label = h1("Hobbies"), 
                                                     choices = list("Camping" = 1, "Chess" = 2, "Cross-fit" = 3,"Others"= 4), 
                                                     selected = 1)),
                                column(7,selectInput("select2", label = h1("Incident-Severity"), 
                                                     choices = list("Major Damage"=1,"Minor Damage"=2,"Others"=3), 
                                                     selected = 1))),
                              
                              fluidRow(actionButton(inputId = "click",label = "Submit")),
                              
                              
                              fluidRow(imageOutput(outputId = "res")) #, height="20%",width="20%"))
                      ),

                      #PLOTS
                      tabItem(tabName="charts",
                              mainPanel(
                                tabsetPanel(type="tabs",
                                            tabPanel(id="tab1", title="Fraud Cases", 
                                                     selectInput("ch", label = h3("Select Attribute"), 
                                                                 choices = list("----"=0,"Age" = 1, "Model Year" = 2, "Occupation" = 3, "Severity of Incident"=4, "Number of Witnesses"=5,"Years as Customer"=6),
                                                                 selected=0),
                                                    
                                                     plotOutput("f_plots")
                                            ),
                                            tabPanel(id="tab2", title="Cases",
                                                     fluidRow(plotOutput("hobbies")),
                                                     fluidRow(box(plotOutput("car_models")),
                                                              box(plotOutput("inc_state")))
                                            ))
                              )
                              
                      )
                    )
                  )
)

#*************************************************SERVER********************************************************

server<-function(input,output){
  #Loading the saved LR model
  lr <- readRDS("C:\\Users\\jyoth\\Desktop\\SEM 2\\Bhogle Sir\\R Proj\\Insurance\\Final\\LR.rda")
  
  result <- eventReactive(input$click, {
    camping=0
    chess=0
    cross_fit=0
    major_damage=0
    
    if (input$select1 == "1") {
      camping=1
    } else if (input$select1 == "2") {
      chess=1
    } else if (input$select1 == "3") {
      cross_fit=1
    } 
    if (input$select2 == "1"){major_damage=1}
    
    features <- data.frame(camping,chess,cross_fit,major_damage)
    
    prob <- predict(lr, features, type = "response")
    pred <- ifelse(prob > 0.5, "YES", "NO")
    pred = pred[[1]]
    if(pred=="NO")
    { result<- "C:\\Users\\jyoth\\Desktop\\SEM 2\\Bhogle Sir\\R Proj\\Insurance\\Final\\safe.png" }
    else if(pred=="YES")
    { result<- "C:\\Users\\jyoth\\Desktop\\SEM 2\\Bhogle Sir\\R Proj\\Insurance\\Final\\fraud.jpg"}
    return(result)    #Returning the predictions
  })
  
  output$res <- renderImage({
    filename <- normalizePath(file.path(result()))
    list(src=filename,alt="Oopsie", width=400, height=300)

  }, deleteFile = FALSE)

  graphs<- eventReactive(input$ch, {
    if(input$ch==1){
      bar_plot("age_grp","fraud_reported","Fraud Cases vs Age Groups")
    }
    else if(input$ch==2){
      line_plot("auto_year","fraud_reported","Fraud Cases vs Model Year")
    }
    else if(input$ch==3){
      bar_plot("insured_occupation","fraud_reported","Fraud Cases vs Occupation of Insured")
    }
    else  if(input$ch==4){
      bar_plot("incident_severity","fraud_reported", "Fraud Cases vs Severity of Incident")
    }
    else if(input$ch==5){
      bar_plot("witnesses","fraud_reported","Fraud Cases vs No. of Witness")
    }
    else if(input$ch==6){
      bar_plot("custyears","fraud_reported","Fraudulent Cases vs Years")
    }
  })
  output$f_plots<-renderPlot(
    {graphs()}
  )
  output$hobbies <- renderPlot(mult_bar("insured_hobbies","fraud_reported","Fraud Cases vs Hobbies of the Insured"))
  ins123$car_model<- paste0(ins123$auto_make," ",ins123$auto_model)
  output$car_models <- renderPlot( barplot(head(sort(table(ins123$car_model,ins123$fraud_reported)[,1],decreasing=TRUE)),col="blue", xlab="Car Models", ylab="Non-fraudulent Cases"))
  output$inc_state <- renderPlot(barplot(table(ins123$incident_state),col="orange",xlab="Location of Accident",ylab="Count of Accidents"))
  
}    

shinyApp(ui,server)
