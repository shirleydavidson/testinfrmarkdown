library(shiny)
library(dplyr)
## Create item pricing data
set.seed(1234)
init_items = function() {
  item.id=1:1000
  ensemble.id=rep(1:100,each=10)
  cost=round(runif(1000,10,100), 2)
  profit=round(cost*runif(1000,0.01,0.15), 2)
  price=cost+profit
  
  data.frame(item.id, ensemble.id, cost, price, profit)
}
items = init_items()

## Create ensemble pricing data
init_ensembles = function(items) {
  items %>% group_by(ensemble.id) %>% summarize_each(funs(sum), cost, price, profit)
}
ensembles = init_ensembles(items)

## Attach dependencies
## https://github.com/timelyportfolio/functionplotR/issues/1#issuecomment-224369431
getdeps <- function() {
  htmltools::attachDependencies(
    htmltools::tagList(),
    c(
      htmlwidgets:::getDependency("datatables","DT")
    )
  )
}

# Define UI for application
ui <- shinyUI(fluidPage(
  tabsetPanel(#id="Linked Table Test",
    tabPanel("Page 1", DT::dataTableOutput("page1")),
    tabPanel("Page 2", inputPanel(
      numericInput("ensemble.id", label = "Ensemble ID:", 0, min(ensembles$ensemble.id), max(ensembles$ensemble.id))
    ),
    textOutput("page2"), DT::dataTableOutput("table2"),getdeps())
  )
))

# Define server logic
server <- shinyServer(function(input, output, session) {
  output$page1 <- DT::renderDataTable(ensembles, rownames = FALSE,
                                      callback=JS(
                                        'table.on("click.dt", "tr", function() {
    tabs = $(".tabbable .nav.nav-tabs li a");
    var data=table.row(this).data();
    document.getElementById("ensemble.id").value=data[0];
    Shiny.onInputChange("ensemble.id",data[0]);
    $(tabs[1]).click();
    table.row(this).deselect();
    })'                     
                                      ))
  
  
  output$table2 <- DT::renderDataTable(items %>% filter(ensemble.id==input$ensemble.id) %>% select(-ensemble.id), rownames = FALSE)
  
  output$page2 <- renderText({
    print(input$ensemble.id)
    paste0("Detailed pricing information for ensemble #",input$ensemble.id,":")
  })
})


# Run the application
shinyApp(ui = ui, server = server)