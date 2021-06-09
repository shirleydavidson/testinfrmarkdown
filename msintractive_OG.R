library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyscreenshot)
library(dplyr)
#library(DT, lib.loc = "\\\\saohlnfs003/Desktop$/cya028/Documents/R/R-3.6.2/library")


#load("//saohlnfs003/Desktop$/cya028/Documents/med-supp-shiny/2021/final_rates.Rdata")

ui<-
  fluidPage(#themeSelector(),
    theme = shinytheme("superhero"),
    #setBackgroundColor("#2d3e50"),
    absolutePanel(right = 0, left = 50, up = 150, width = '94.5%',
                  h2("Montana Medicare Supplement Insurance", tags$style(HTML("h2{font-size: 45px}"))),
                  h4("2021-2022 Rate Comparison Guide", align = "left", tags$style(HTML("h4{font-size: 35px}"))),
                  mainPanel(width = 12),
                  fluidRow(
                    column(2,
                           pickerInput("Company",
                                       "Company:",
                                       choices = unique(as.character(final_rates$Company)),
                                       multiple = TRUE,
                                       selected = unique(as.character(final_rates$Company)),
                                       choicesOpt = list(style = rep_len("font-size: 18px", 21)),
                                       options = list('actions-box' = TRUE))),
                    column(2,
                           pickerInput("Plan",
                                       "Plan:",
                                       choices = c("A","B","C","D","F", "F HD", "G" ,"G HD","K","L","M","N" ),
                                       multiple = TRUE,
                                       selected = "A",
                                       choicesOpt = list(style = rep_len("font-size: 18px", 12)),
                                       options = list('actions-box' = TRUE))),
                    column(2,
                           pickerInput("Age",
                                       "Age:",
                                       choices = unique(as.character(final_rates$Age)),
                                       multiple = TRUE,
                                       selected = 65,
                                       choicesOpt = list(style = rep_len("font-size: 18px", 36)),
                                       options = list('actions-box' = TRUE))),
                    column(2,
                           pickerInput("Tobacco", 
                                       "Tobacco Usage:",
                                       choices = unique(as.character(final_rates$Tobacco)), 
                                       selected = "No",
                                       choicesOpt = list(style = rep_len("font-size: 18px", 2)),
                                       options = list('action-box' = TRUE))),
                    column(2,
                           pickerInput("Time",
                                       "Time:",
                                       choices = unique(as.character(final_rates$Time)), 
                                       selected = "Monthly",
                                       choicesOpt = list(style = rep_len("font-size: 18px", 2)),
                                       options = list('action-box' = TRUE))),
                    column(2,
                           pickerInput("Discount", 
                                       "Discount:",
                                       choices = c("Yes","No"), 
                                       selected = "No",
                                       choicesOpt = list(style = rep_len("font-size: 18px", 2)),
                                       options = list('action-box' = TRUE))),
                    column(2,downloadBttn(label = "Download", outputId = "report")),
                    column(2, actionBttn("go", "Screenshot", color = "primary")),
                  ),
                  
                  fluidRow(column(12,
                                  fluidRow(column(9, helpText("Click ", tags$a(href = "https://www.medicare.gov/supplements-other-insurance/how-to-compare-medigap-policies", "here", target = "blank"), " to see a plan benefit comparison.", style = 'font-size: 21px')),
                                           fluidRow(column(3,helpText(strong("Rate Increase History"), align = 'center', style = "font-size: 20px")))))),
                  fluidRow(column(12,
                                  fluidRow(column(9),
                                           fluidRow(column(3, helpText("Blank spaces in the 2016-2021 columns where rate increase history does not exist", align  = 'center', style = 'font-size: 21px')))))),
                  
                  fluidRow(DT::dataTableOutput("data1"), style = "padding-bottom:20px", tags$head(tags$style(HTML("thead{font-size:21px}")))),
                  
                  fluidRow(column(12, helpText(strong("BlueCross BlueShield and Globe Life do not have tobacco/nontobacco distinction."), align = 'left', style = "font-size: 21px; color: #df691a"),
                                  helpText(strong("Only companies that chose to respond are included in this rate guide."), align = 'left', style = "font-size: 21px; color: #df691a"),
                                  helpText(strong("Rates are subject to change."), align = "left", style = "font-size: 21px; color: #df691a"),
                                  helpText(strong("Companies that do not have a discount description and have a discount 0% do not offer a household discount."), align = 'left', style = "font-size: 21px; color: #df691a"), br(),)),
                  
                  fluidRow(DT::dataTableOutput("data2"), style = "padding-bottom:20px", tags$head(tags$style(HTML("thead{font-size:21px}")))),
                  
                  fluidRow(column(12,helpText(strong("Office of the Montana State Auditor" ,style = "font-size: 21px"), br(),
                                              strong("Commissioner of Securities & Insurance",style = "font-size: 21px"), br(), 
                                              strong("840 Helena Avenue, Helena, MT 59601",style = "font-size: 21px"), br(),
                                              strong("Phone:(406) 444-7834" ,"TDD: (406) 444-3246",style = "font-size: 21px"), br(),
                                              strong("Website:", tags$a(href="https://www.csimt.gov", "csimt.gov", target = "_blank"),   "Fax: (406) 444-3497",style = "font-size: 21px"), align = 'center' )))
    ) ,tags$head(tags$style(HTML("label{font-size: 25px}"))),
    tags$head(tags$style(HTML("[selected]{background-color: #df691a}"))),
    tags$body(tags$style(HTML("body{font-size: 21px; background-color: #2b3e50}"))) 
  )

#####
server<-function(input,output, session){
  
  output$data1<-DT::renderDataTable(DT::datatable({
    data<-final_rates
    data<-data[data$Company %in% input$Company & data$Rate >0,]
    data<-data[data$Plan %in% input$Plan,]
    data<-data[data$Age %in% input$Age,]
    data<-data[data$Tobacco %in% input$Tobacco,]
    data<-data[data$Time %in% input$Time,]
    data<-data[data$HH %in% input$Discount,]
    data[,c("Company", "Age", "Plan", "Type", "Rate", "2016", "2017", "2018","2019", "2020", "2021", "Phone", "links" )]
  },rownames = FALSE,
  options = list(pageLength = 10,
                 lengthChange = TRUE,
                 bFilter = FALSE,
                 oderClasses = TRUE,
                 scrollX = TRUE,
                 scrollY = TRUE,
                 width = "20%",
                 columnDefs = list(list(targets = 5, width = '40'),
                                   list(targets = 12, width = "100"))),
  style = "bootstrap4",
  fillContainer = FALSE,
  escape = FALSE,
  selection = "none",
  #class = "hover",
  colnames = c("Company", "Age", "Plan", "Type", "Rate", "2016", "2017", "2018","2019", "2020", "2021", "Phone", "Website"))%>%
    DT::formatStyle( c("Company", "Phone", "links", "Age", "Plan", "Type", "Rate", "2016", "2017", "2018","2019", "2020", "2021"), color = "white", fontSize = '21px')%>%  
    DT::formatCurrency('Rate', digits = 0)
  )
  
  #####
  tbl1<-final_rates
  tbl1<-tbl1[,c("Company", "Phone", "Website", "Age", "Plan", "Type","Tobacco", "Time", "HH", "Rate", "Description", "Percent", "2016", "2017", "2018","2019", "2020", "2021" )]
  names(tbl1)[11]<-"Discount Description"
  names(tbl1)[12]<-"Discount Percent"
  tbl2<-reactive({
    tbl1<-tbl1[tbl1$Company %in% input$Company,]
    tbl1<-tbl1[tbl1$Rate >0,]
    tbl1<-tbl1[tbl1$Plan %in% input$Plan,]
    tbl1<-tbl1[tbl1$Age %in% input$Age,]
    tbl1<-tbl1[tbl1$Tobacco %in% input$Tobacco,]
    tbl1<-tbl1[tbl1$Time %in% input$Time,]
    tbl1<-tbl1[tbl1$HH %in% input$Discount,]  
    tbl1[, c("Company", "Age", "Plan", "Type", "Rate","Discount Percent", "Discount Description" , "2016", "2017", "2018","2019", "2020", "2021","Phone", "Website")]
  })
  
  #####
  output$data2<-DT::renderDataTable(DT::datatable({
    data<-discount
    data<-data[data$Company %in% input$Company,]
    data[,c("Company","Percent","Description")]
  },rownames = FALSE,
  options = list(pageLength = 10,
                 lengthChange = TRUE,
                 bFilter = FALSE,
                 oderClasses = TRUE,
                 scrollX = TRUE,
                 scrollY = TRUE,
                 width = "20%",
                 columnDefs = list(list(targets = 1, width = '125'))),
  fillContainer = FALSE,
  selection = "none",
  colnames = c("Company", "Discount %", "Discount Description"),
  style ="bootstrap4")%>%
    DT::formatStyle(c("Company", "Percent", "Description"), color = "white", fontSize = "21px")%>% 
    DT::formatPercentage("Percent")) 
  
  ##### 
  output$report<-downloadHandler(
    filename = function(){
      paste("Med_Sup", Sys.Date(), '.csv', sep = " ")},
    content = function(file){write.csv(tbl2(),file,row.names = FALSE)}
  )
  
  observeEvent(input$go, {
    screenshot()
  })
  
}
shinyApp(ui = ui, server = server)

