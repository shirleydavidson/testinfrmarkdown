library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyscreenshot)
library(dplyr)
#library(DT, lib.loc = "\\\\saohlnfs003/Desktop$/cya028/Documents/R/R-3.6.2/library")


#load("//saohlnfs003/Desktop$/cya028/Documents/med-supp-shiny/2021/final_rates.Rdata")
b64 <- base64enc::dataURI(file="CSIBadge.png", mime="image/png")
b642 <- base64enc::dataURI(file="CSI Auditor Logo.png", mime="image/png")
ui<-
  fluidPage(
    theme = shinytheme("paper"),
    absolutePanel(right = 0, left = 50, up = 150, width = '94.5%',
                  h1( img(src=b64,height = 150, width = 150), "Montana Medicare Supplement Insurance", align= "center", tags$style(HTML("h1{color: black}"))),
                  h4("2021-2022 Rate Comparison Guide", align = "center", tags$style(HTML("h4{font-size: 35px; color: black}"))),
                  mainPanel(width = 12),
                  fluidRow(
                    column(2, align="left", tags$style(HTML(".selected {background-color:#0394fc }")), 
                           pickerInput("Company",
                                       "Company:", 
                                       choices = unique(as.character(final_rates$Company)),
                                       multiple = TRUE,
                                       selected = unique(as.character(final_rates$Company)),
                                       choicesOpt = list(style = rep_len("font-size: 18px; color: black", 21)),
                                       options = list('actions-box' = TRUE))),
                    column(2, align="left",
                           pickerInput("Plan",
                                       "Plan:", 
                                       choices = c("A","B","C","D","F", "F HD", "G" ,"G HD","K","L","M","N" ),
                                       multiple = TRUE,
                                       selected = "A",
                                       choicesOpt = list(style = rep_len("font-size: 18px; color: black", 12)),
                                       options = list('actions-box' = TRUE))),
                    column(2, align="left", 
                           pickerInput("Type",
                                       "Type:",
                                       choices = unique(as.character(final_rates$Type)),
                                       multiple = TRUE,
                                       selected = "AA",
                                       choicesOpt = list(style = rep_len("font-size: 16px; color: black", 21)),
                                       options = list('actions-box' = TRUE))),
                    column(2, align="left",
                           pickerInput("Age",
                                       "Age:",
                                       choices = unique(as.character(final_rates$Age)),
                                       multiple = TRUE,
                                       selected = 65,
                                       choicesOpt = list(style = rep_len("font-size: 18px; color: black", 36 )),
                                       options = list('actions-box' = TRUE), 
                                       )),
                    
                    column(2, align="left",
                           pickerInput("Tobacco", 
                                       "Tobacco Usage:",
                                       choices = unique(as.character(final_rates$Tobacco)), 
                                       selected = "No",
                                       choicesOpt = list(style = rep_len("font-size: 18px; color: black", 2)),
                                       options = list('action-box' = TRUE))),
                    
                    column(2, align="left",
                           pickerInput("Time",
                                       "Time:",
                                       choices = unique(as.character(final_rates$Time)), 
                                       selected = "Monthly",
                                       choicesOpt = list(style = rep_len("font-size: 18px; color: black", 2)),
                                       options = list('action-box' = TRUE))),
                    
                    # column(2, align="left",
                    #        pickerInput("Discount",
                    #                    "Discount:",
                    #                    choices = c("Yes","No"),
                    #                    selected = "No",
                    #                    choicesOpt = list(style = rep_len("font-size: 18px; color: black", 2)),
                    #                    options = list('action-box' = TRUE))),

                    column(2,downloadBttn(label = "Download", outputId = "report")),
                    column(2, actionBttn("go", "Screenshot", color = "primary"))
                  ),
                  
                  fluidRow(column(12,
                                  fluidRow(column(9, helpText("Click ", 
                                                              tags$a(href = "https://www.medicare.gov/supplements-other-insurance/how-to-compare-medigap-policies", 
                                                                     "here", target = "blank"), " to see a plan benefit comparison.", style = 'font-size: 21px; color: black')),
                                           fluidRow(column(3,helpText(strong("Rate Increase History"), align = 'center', style = "font-size: 20px; color: black")))))),
                  fluidRow(column(12,
                                  fluidRow(column(9),
                                           fluidRow(column(3, helpText("Blank spaces in the 2018-2021 columns where rate increase history does not exist", 
                                                                       align  = 'center', style = 'font-size: 21px; color: black')))))),
                  
                  fluidRow(DT::dataTableOutput("data1"), style = "padding-bottom:20px", tags$head(tags$style(HTML("thead{font-size:21px}")))),
                  
                  fluidRow(column(12, helpText(strong("BlueCross BlueShield and Globe Life do not have tobacco/nontobacco distinction."), align = 'left', style = "font-size: 21px; color: black"),
                                  helpText(strong("Only companies that chose to respond are included in this rate guide."), align = 'left', style = "font-size: 21px; color; color: black"),
                                  helpText(strong("Rates are subject to change."), align = "left", style = "font-size: 21px; color: black"),
                                  helpText(strong("12 Companies offer a household discount."), align = 'left', style = "font-size: 21px; color: black"), br(),) #orange color #df691a
                    
                          ),
                  h4("Compare Companies Side By Side", align = "center", tags$style(HTML("h4{font-size: 35px; color: black}"))),
                  
                  fluidRow(column(2, align="left", 
                                  pickerInput("Compares",
                                              "Choose 5 Companies:",
                                              choices = unique(as.character(final_rates$Company)),
                                              multiple = TRUE,
                                              selected = unique(as.character(final_rates$Company[1:5])),
                                              choicesOpt = list(style = rep_len("font-size: 16px; color: black", 21)),
                                              options = list("max-options" = 5,'actions-box' = TRUE)))),

                  fluidRow(DT::dataTableOutput("data2"), style = "padding-bottom:20px", 
                            tags$head(tags$style(HTML("thead{font-size:21px}"))), columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                
                  
                  h4(img(src=b642,height = 150, width = 250), align= "center")
                  
                  # fluidRow(column(12,helpText(strong("Office of the Montana State Auditor" ,style = "font-size: 21px"), br(),
                  #                             strong("Commissioner of Securities & Insurance",style = "font-size: 21px"), br(), 
                  #                             strong("840 Helena Avenue, Helena, MT 59601",style = "font-size: 21px"), br(),
                  #                             strong("Phone:(406) 444-7834" ,"TDD: (406) 444-3246",style = "font-size: 21px"), br(),
                  #                             strong("Website:", tags$a(href="https://www.csimt.gov", "csimt.gov", target = "_blank"),   
                  #                                    "Fax: (406) 444-3497",style = "font-size: 21px"), align = 'center' )))
    ),
                  
                  

                  tags$head(tags$style(HTML("label{font-size: 25px}"))),
                  tags$head(tags$style(HTML("[selected]{background-color: #df691a}"))),
                  tags$body(tags$style(HTML("body{font-size: 21px; background-color: white}")))) # #00a4b3 light blue #2b3e50 the dark blue
  

#####
server<-function(input,output, session){
  
  output$data1<-DT::renderDataTable(DT::datatable({
    data<-final_rates
    data<-data[data$Company %in% input$Company & data$Rate >0,]
    data<-data[data$Plan %in% input$Plan,]
    data<-data[data$Age %in% input$Age,]
    data<-data[data$Tobacco %in% input$Tobacco,]
    data<-data[data$Time %in% input$Time,]
    data<-data[data$Type %in% input$Type,]
    # data<-data[data$HH %in% input$Discount,]
    data[,c("Company", "Rate","Age", "Plan", "Type",   "2018","2019", "2020", "2021", "Phone", "links" )]
  },rownames = FALSE,
  options = list(pageLength = 10,
                 lengthChange = TRUE,
                 bFilter = FALSE,
                 oderClasses = TRUE,
                 scrollX = TRUE,
                 scrollY = TRUE,
                 width = "20%",
                 columnDefs = list(list(targets = 5, width = '40'),
                                   list(targets = 10, width = "100"))),
  style = "bootstrap4",
  fillContainer = FALSE,
  escape = FALSE,
  selection = "none",
  #class = "hover",
  colnames = c("Company", "Rate", "Age", "Plan", "Type",   "2018","2019", "2020", "2021", "Phone", "Website"))%>%
    DT::formatStyle( c("Company","Rate", "Age", "Plan", "Type",   "2018","2019", "2020", "2021", "Phone", "links"), color = "black", fontSize = '21px')%>%
    # DT::formatStyle("Rate", backgroundColor =  'lightblue') %>% 
    DT::formatCurrency('Rate', digits = 0)
  )
  
  

  #####
  output$data2<-DT::renderDataTable(DT::datatable({
    data<-final_rates
    data<-data[data$Company %in% input$Compares,]
    data<-data[data$Plan %in% input$Plan,]
    data<-data[data$Age %in% input$Age,]
    data<-data[data$Tobacco %in% input$Tobacco,]
    data<-data[data$Time %in% input$Time,]
    data<-data[data$Type %in% input$Type,]
    # data<-data[data$HH %in% input$Discount,]
    data[,c("Company", "Rate", "disRate", "Percent","Description")]
  },rownames = FALSE,
  options = list(pageLength = 10,
                 lengthChange = FALSE,
                 bFilter = FALSE,
                 oderClasses = TRUE,
                 scrollX = TRUE,
                 scrollY = TRUE,
                 width = "20%",
                 columnDefs = list(list(targets = 1, width = '125'))),
  fillContainer = FALSE,
  selection = "none",
  colnames = c("Company","Rate", "Rate After Discount","Discount %", "Discount Description"),
  style ="bootstrap4")%>%
    DT::formatStyle(c("Company", "Rate", "disRate", "Percent", "Description"), color = "black", fontSize = "21px")%>% 
    DT::formatPercentage("Percent") %>% 
    DT::formatCurrency(c('Rate', "disRate"), digits = 0)) 
  
  ##### 
  output$report<-downloadHandler(
    filename = function(){
      paste("Med_Sup", Sys.Date(), '.csv', sep = " ")},
    content = function(file){write.csv(tbl2(),file,row.names = FALSE)}
  )
  
  observeEvent(input$go, screenshot()
  )       #option=list(columnDefs=list(list(targets=3:5, class="dt-right")))
  
}
shinyApp(ui = ui, server = server)

