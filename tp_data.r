library(kableExtra)
library(shiny)
library(shinydashboard)
library(shinyalert)
library(DT)
library(data.table)
library(plotly)
library(dashboardthemes)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(GGally)
library(corrplot)
td=read.csv("C:/Users/Administrator/Desktop/INTERN/csv datasets/tips.csv",header = T)
str(td)
td_fact=c("sex","smoker","day","time","size")
td=td %>% mutate(across(.cols = all_of(td_fact),.fns = as.factor))
td_nv=td %>% select(where(is.numeric))
cc=cor.test(td$tip,td$total_bill)
cf=rbind(cbind(cc$p.value,cc$conf.int[1],cc$conf.int[2],cc$estimate,cc$null.value))
cf1=data.frame(cf)
colnames(cf1)=c("p-value","Confidence interval-LL","Confidence interval-HL","sample estimate")
cf1
ss=t.test(tip~smoker,data = td)
sf=rbind(cbind(ss$p.value,ss$conf.int[1],ss$conf.int[2],ss$estimate,ss$null.value))
sf1=data.frame(sf)
colnames(sf1)=c("p-value","Confidence interval-LL","Confidence interval-HL","sample estimate")
sf1
a=aov(tip~day,td)
summary(a)
a1_t=TukeyHSD(a)
a1_t1=data.frame(a1_t$day)
a1_t1
c1=t.test(tip~time,data = td)
df=rbind(cbind(c1$p.value,c1$conf.int[1],c1$conf.int[2],c1$estimate,c1$null.value))
df1=data.frame(df)
colnames(df1)=c("p-value","Confidence interval-LL","Confidence interval-HL","sample estimate")
df1
a1=aov(tip~size,td)
summary(a1)
a1_t2=TukeyHSD(a1)
a1_t12=data.frame(a1_t2$size)
a1_t12

ui  <-  dashboardPage(skin = "blue",
                      dashboardHeader(title = "Tips dataset",
                                      tags$li(class="dropdown",tags$a(href="https://newmarkhotels.com/accommodation/mystik-lifestyle-boutique-hotel?gclid=Cj0KCQiAzeSdBhC4ARIsACj36uF4UisXLvYsunMTWh-WyvtdrSw5wkYY8E2OFIy8M2nEWekkJ9RNagQaAmZ9EALw_wcB",icon("youtube"), "Video Link",target="_blank",style = "font-weight: 700; color: #4d3a7d;")),
                                      tags$li(class="dropdown",tags$a(href="https://muniraghouse.shinyapps.io/Datasets/",icon("shinyio"), "My profile",target="_blank",style = "font-weight: 700; color: green;"))),
                      dashboardSidebar(
                        sidebarMenu(id = "tab1", selected = "Home",
                                    menuItem("home", tabName = "Home", icon = icon("home")),
                                    menuItem("Tips info", tabName = "Tips", icon = icon("address-card")))),
                      dashboardBody(
                        shinyDashboardThemes(
                          theme = "blue_gradient"
                        ),
                        tabItems(
                          tabItem("Home",
                                  fluidRow(column(12, h1("About"))),
                                  fluidRow(column(12,h3("The Tips dataset is a data frame with 244 rows and 7 variables 
                                           which represents some tipping data where one waiter recorded information about 
                                           each tip he received over a period of a few months working in one restaurant. 
                                           In all the waiter recorded 244 tips. The data was reported in a collection of case studies
                                           for business statistics (Bryant & Smith 1995).[4] The waiter collected several variables: 
                                           The tip in dollars, the bill in dollars, the sex of the bill payer, whether there were smokers in the party,
                                           the day of the week, 
                                              the time of day and the size of the party."))),
                                  img(src = "m.png", height =300, width = 600,
                                      style="display: block; margin-left: auto; margin-right: auto;"),
                                  fluidRow(column(12, DTOutput("InputData")))
                          ),
                          tabItem("Tips",
                                  tabsetPanel(type = "tabs", selected = "total_bill", id = "intabset1",
                                              tabPanel("Bill", value = "total_bill",
                                                       h1("plots for total bill vs tips"),
                                                       fluidRow(column(8, plotlyOutput("tt")),
                                                                fluidRow(column(4,sliderInput(
                                                                  "Slider21", "Select the Total bill group",
                                                                  min = floor(min(td$total_bill)),
                                                                  max = ceiling(max(td$total_bill)),
                                                                  value = c(min = min(td$total_bill), max = max(td$total_bill))),
                                                                  fluidRow(sliderInput(
                                                                    "Slider22", "Select the tip group",
                                                                    min = floor(min(td$tip)),
                                                                    max = ceiling(max(td$tip)),
                                                                    value = c(min = min(td$tip), max = max(td$tip))))),
                                                                  fluidRow(h1(" ")),
                                                                  fluidRow(column(12,offset=0,dataTableOutput("bo"))),
                                                                  fluidRow(column(1,offset = 6,downloadButton("download21", label = "Download")))),
                                                                fluidRow(h1("Testing"),
                                                                         DT::dataTableOutput("mytable")),
                                                       )
                                                       ),
                                              tabPanel("sex", value = "total_bill",
                                                       h1("facet plot for sex,smoker vs tips"),
                                                       fluidRow(column(8, plotlyOutput("ss")),
                                                                column(4, fluidRow(selectizeInput(
                                                                  "select1", "Please select the choice of sex", multiple = TRUE,
                                                                  choices = sort(unique(td$sex)),
                                                                  selected = sort(unique(td$sex))
                                                                )),
                                                                fluidRow(sliderInput(
                                                                  "Slider31", "Please select the tips group",
                                                                  min = floor(min(td$tip)),
                                                                  max = ceiling(max(td$tip)),
                                                                  value = c(min = min(td$tip), max = max(td$tip))
                                                                )))),
                                                       fluidRow(h1("")),
                                                       fluidRow(column(11,offset=1,fluidRow(radioButtons("radio1","Select numerical summary",
                                                                                                         choices = c("Average","Minimum","Maximum","standrad deviation","All")))),column(6,uiOutput("us1"))),
                                                       fluidRow(column(12,offset=0,dataTableOutput("sso"))),
                                                       fluidRow(column(1,offset = 6,downloadButton("download32", label = "Download"))),
                                                       fluidRow(h1("Testing"),
                                                                DT::dataTableOutput("mytable1"))
                                              ),
                                              tabPanel("Day", value = "day",
                                                       h1("Plot for day"),
                                                       fluidRow(column(8, plotlyOutput("sd")),
                                                                column(4, fluidRow(selectizeInput(
                                                                  "select11", "Please select the choice of day", multiple = TRUE,
                                                                  choices = sort(unique(td$day)),
                                                                  selected = sort(unique(td$day))
                                                                )),
                                                                fluidRow(sliderInput(
                                                                  "Slider311", "Please select the tips group",
                                                                  min = floor(min(td$tip)),
                                                                  max = ceiling(max(td$tip)),
                                                                  value = c(min = min(td$tip), max = max(td$tip))
                                                                )))),
                                                       fluidRow(h1("")),
                                                       fluidRow(column(11,offset=1,fluidRow(radioButtons("radio2","Select numerical summary",
                                                                                                         choices = c("Average","Minimum","Maximum","All")))),column(6,uiOutput("us2"))),
                                                       fluidRow(column(12,offset=0,dataTableOutput("do"))),
                                                       fluidRow(column(1,offset = 6,downloadButton("download33", label = "Download"))),
                                                       fluidRow(h1("Testing"),
                                                                DT::dataTableOutput("mytable2"))
                                                       
                        ),
                        tabPanel("Timing", value = "time",
                                 h1("bar plot for time vs tips"),
                                 fluidRow(column(8, plotlyOutput("ty")),
                                          column(4, fluidRow(selectizeInput(
                                            "select4", "Please select the choice of time", multiple = TRUE,
                                            choices = sort(unique(td$time)),
                                            selected = sort(unique(td$time))
                                          )),
                                          fluidRow(sliderInput(
                                            "Slider5", "Please select the tips group",
                                            min = floor(min(td$tip)),
                                            max = ceiling(max(td$tip)),
                                            value = c(min = min(td$tip), max = max(td$tip))
                                          )))),
                                 fluidRow(h1("")),
                                 fluidRow(column(11,offset=1,fluidRow(radioButtons("radio3","Select numerical summary",
                                                                                   choices = c("Average","Minimum","Maximum","All")))),column(6,uiOutput("us3"))),
                                 fluidRow(column(12,offset=0,dataTableOutput("tyo"))),
                                 fluidRow(h1("Testing"),
                                          DT::dataTableOutput("mytable4")),
                                 fluidRow(column(1,offset = 6,downloadButton("download34", label = "Download")))
                                 ),
                        tabPanel("Persons", value = "size",
                                 h1("Boxplot for size vs tips"),
                                 fluidRow(column(8, plotlyOutput("sy")),
                                          column(4, fluidRow(selectizeInput(
                                            "select5", "Please select the choice of size", multiple = TRUE,
                                            choices = sort(unique(td$size)),
                                            selected = sort(unique(td$size))
                                          )),
                                          fluidRow(sliderInput(
                                            "Slider6", "Please select the tips group",
                                            min = floor(min(td$tip)),
                                            max = ceiling(max(td$tip)),
                                            value = c(min = min(td$tip), max = max(td$tip))
                                          )))),
                                 fluidRow(h1("")),
                                 fluidRow(column(11,offset=1,fluidRow(radioButtons("radio4","Select numerical summary",
                                                                                   choices = c("Average","Minimum","Maximum","All")))),column(6,uiOutput("us4"))),
                                 fluidRow(column(12,offset=0,dataTableOutput("syo"))),
                                 fluidRow(column(1,offset = 6,downloadButton("download35", label = "Download"))),
                                 fluidRow(h1("Testing"),
                                          DT::dataTableOutput("mytable5"))
                                 
                                 )
                                 )))))
server <- function(input, output,session) {
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as ", session$user),
        subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
    }
  })
  output$tt <- renderPlotly({
    s11 <- td[which(td$total_bill %in% (input$Slider21[1]:input$Slider21[2])),]
    s1 <- s11[which(s11$tip > input$Slider22[1] & s11$tip <= input$Slider22[2]),]
    ggplot(s1,aes(x=total_bill,y=tip))+
      geom_point(size=3,color='red')
  })
  output$bo <- renderDataTable({
    df = as.data.frame(td %>% summarise(across(.cols=names(td_nv),.fns = ~summary(.x))))
    datatable(df, options = list(searching = FALSE))
  }) 
  output$download1 <- downloadHandler(
    filename = function() {
      paste("total_bill", ".csv", sep = "")
    },
    content = function(file) {
      df = as.data.frame(td %>% summarise(across(.cols=names(td_nv),.fns = ~summary(.x))))
      datatable(df, options = list(searching = FALSE))
    })
  output$ss <- renderPlotly({
    pdata0 <- td[which(td$sex %in% input$select1),]
    pdata61 <- pdata0[which(pdata0$tip %in% (input$Slider31[1]:input$Slider31[2])),]
    ggplot(pdata61, aes(tip,fill=sex)) + 
      geom_density() + 
      xlab("Tips")+ 
      ylab("Sex")+
      facet_grid(~smoker)
  })
  output$us1 <- renderUI({
    if(input$radio1 == "Average"){
      df = as.data.frame(td %>% 
                           group_by(sex,smoker) %>% 
                           summarise(countt=n(),
                                     Average=round(mean(tip))
                                     
                           ),
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
      
    }
    else if(input$radio1 == "Minimum"){
      df = as.data.frame(td %>% 
                           group_by(sex,smoker) %>% 
                           summarise(countt=n(),
                                     minimum=round(min(tip))
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio1 == "Maximum"){
      df = as.data.frame(td %>% 
                           group_by(sex,smoker) %>% 
                           summarise(countt=n(),
                                     maximum=round(min(tip))
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio1 == "standrad deviation"){
      df = as.data.frame(td %>% 
                           group_by(sex,smoker) %>% 
                           summarise(countt=n(),
                                     standrad=round(sd(tip))
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    else if(input$radio1 == "All"){
      df = as.data.frame(td %>% 
                           group_by(sex,smoker) %>% 
                           summarise(countt=n(),
                                     Average=round(mean(tip)),
                                     minimum=round(min(tip)),
                                     maximum=round(min(tip)),
                                     standrad=round(sd(tip))
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
  })
  output$us2 <- renderUI({
    if(input$radio2 == "Average"){
      df = as.data.frame(td %>% 
                           group_by(day) %>% 
                           summarise(countt=n(),
                                     Average=round(mean(tip))
                                     
                           ),
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
      
    }
    else if(input$radio2 == "Minimum"){
      df = as.data.frame(td %>% 
                           group_by(day) %>% 
                           summarise(countt=n(),
                                     minimum=round(min(tip))
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio2 == "Maximum"){
      df = as.data.frame(td %>% 
                           group_by(day) %>% 
                           summarise(countt=n(),
                                     maximum=round(min(tip))
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio2 == "All"){
      df = as.data.frame(td %>% 
                           group_by(day) %>% 
                           summarise(countt=n(),
                                     Average=round(mean(tip)),
                                     minimum=round(min(tip)),
                                     maximum=round(min(tip)),
                                     standrad=round(sd(tip))
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
  })
  output$us3 <- renderUI({
    if(input$radio3 == "Average"){
      df = as.data.frame(td %>% 
                           group_by(time,smoker) %>% 
                           summarise(countt=n(),
                                     Average=round(mean(tip))
                                     
                           ),
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
      
    }
    else if(input$radio3 == "Minimum"){
      df = as.data.frame(td %>% 
                           group_by(time,smoker) %>% 
                           summarise(countt=n(),
                                     minimum=round(min(tip))
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio3 == "Maximum"){
      df = as.data.frame(td %>% 
                           group_by(time,smoker) %>% 
                           summarise(countt=n(),
                                     maximum=round(min(tip))
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio3 == "standrad deviation"){
      df = as.data.frame(td %>% 
                           group_by(time,smoker) %>% 
                           summarise(countt=n(),
                                     standrad=round(sd(tip))
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
    else if(input$radio3 == "All"){
      df = as.data.frame(td %>% 
                           group_by(time,smoker) %>% 
                           summarise(countt=n(),
                                     Average=round(mean(tip)),
                                     minimum=round(min(tip)),
                                     maximum=round(min(tip)),
                                     standrad=round(sd(tip))
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
  })
  output$us4 <- renderUI({
    if(input$radio4 == "Average"){
      df = as.data.frame(td %>% 
                           group_by(size) %>% 
                           summarise(countt=n(),
                                     Average=round(mean(tip))
                                     
                           ),
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
      
    }
    else if(input$radio4 == "Minimum"){
      df = as.data.frame(td %>% 
                           group_by(size) %>% 
                           summarise(countt=n(),
                                     minimum=round(min(tip))
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "Maximum"){
      df = as.data.frame(td %>% 
                           group_by(size) %>% 
                           summarise(countt=n(),
                                     maximum=round(min(tip))
                                     
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE))
    }
    else if(input$radio4 == "All"){
      df = as.data.frame(td %>% 
                           group_by(size) %>% 
                           summarise(countt=n(),
                                     Average=round(mean(tip)),
                                     minimum=round(min(tip)),
                                     maximum=round(min(tip)),
                                     standrad=round(sd(tip))
                           ),
                         
                         row.names = NULL)
      datatable(df, options = list(searching = TRUE,paging = FALSE,Search.col= TRUE))
    }
  })
  output$mytable = DT::renderDataTable({
    cf1
  }) 
  
  output$mytable1 = DT::renderDataTable({
    sf1
  })
  output$mytable2 = DT::renderDataTable({
    a1_t1
  })
  output$mytable4 = DT::renderDataTable({
    df1
  })
  output$mytable5 = DT::renderDataTable({
    a1_t12
  })
  output$sd <- renderPlotly({
    pdata11 <- td[which(td$day %in% input$select11),]
    pdata1 <- pdata11[which(pdata11$tip %in% (input$Slider311[1]:input$Slider311[2])),]
    ggplot(pdata1,aes(x=day,y=tip))+
      geom_boxplot(color="brown",fill="green")
  })
  output$ty <- renderPlotly({
    pdata12 <- td[which(td$time %in% input$select4),]
    pdata21 <- pdata12[which(pdata12$tip > input$Slider5[1] & pdata12$tip <= input$Slider5[2]),]
    ggplot(pdata21, aes(tip,fill=smoker)) + 
      geom_histogram() + 
      xlab("tip")+ 
      ylab("smoker")+ 
      facet_grid(~time)+
      labs(title = "Facet for tip vs time")
  })
  
  output$sy <- renderPlotly({
    pdata22 <- td[which(td$size %in% input$select5),]
    pdata12 <- pdata22[which(pdata22$tip %in% (input$Slider6[1]:input$Slider6[2])),]
    ggplot(pdata12,aes(x=size,y=tip))+
      geom_boxplot(fill="pink",color="brown")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)