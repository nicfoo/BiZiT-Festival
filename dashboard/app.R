#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#setwd("C:/Users/ZhunHung/Downloads/BIZIT Festival/dashboard")
library(shiny)
library(DT)

require(shinydashboard)
source("Text V2.R")

#Install shinydashboard


header <- dashboardHeader(title = "Singtel", dropdownMenuOutput("msgMenu"), dropdownMenuOutput("messageMenu"))
sidebar <- dashboardSidebar(disable = TRUE)
demoanswer <- ""

# Define UI for application that draws a histogram
frow1 <- fluidRow(
  valueBoxOutput("positive")
  ,valueBoxOutput("neutral")
  ,valueBoxOutput("negative")
)

frow2 <- #fluidRow(
  tabsetPanel(
    tabPanel("Facebook Comments", dataTableOutput("testRF")),
    tabPanel("Demo", 
             textAreaInput("caption", "Enter your question", "How do I check my data bill?", width = "1000px"),
             verbatimTextOutput("value")
    )
  
)

frow3 <- fluidRow (
  infoBoxOutput(
    "comment"
    ,width = 4
  ),
  infoBoxOutput(
    "suggestedreply"
    ,width = 6
  ),
  #actionButton("reply", "Reply")
  htmlOutput('mySite'),
  shiny::actionButton("deleteRows", "Mark as Completed",
                      icon=icon("check"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  
)

body <- dashboardBody(frow1, frow2, frow3)



ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {
  values <- reactiveValues(dfWorking = dashsample)
  
  
  output$messageMenu <- renderMenu({
    
    dropdownMenu(type = "tasks", badgeStatus = "success",
                 taskItem(value = 60, color = "green",
                          "Daily Cases Quota"
                 ),
                 taskItem(value = 17, color = "aqua",
                          "Unresolved cases"
                 )
    )
    
  })
  
  output$msgMenu <- renderMenu({
    dropdownMenu(type = "notifications",
                 notificationItem(
                   text = "25 new posts today",
                   icon("users")
                 ),
                 notificationItem(
                   text = "You have 4 follow-up cases due today",
                   icon = icon("exclamation-triangle"),
                   status = "warning"
                 )
    )
  })
  
  output$value <- renderText({ 
    if(length(process(input$caption)$text)!=0){
      predictTest(input$caption, mat, model)
      } else {
        "Please enter a valid question"
      }
    })
  
  output$mySite <- renderUI({
    a("Reply", href=paste0("https://www.facebook.com/singtel/posts/", substring(dashsample[input$testRF_rows_selected,1], 13),
                           target = "_blank")
    )
  })
  
   output$testRF <- renderDataTable(
     values$dfWorking
     ,selection='single'
     ,filter = 'top'
     ,options = list(
       pageLength = 5
     )
   )
   output$comment = renderInfoBox({
     infoBox(
       title = "Comment"
       ,values$dfWorking[input$testRF_rows_selected,5]
       ,color = "blue"
       ,icon=icon("facebook")
     )
     
   })
   
   output$suggestedreply = renderInfoBox({
     infoBox(
       title = "Suggested Reply"
       ,if(length(input$testRF_rows_selected) != 0) {predictTest(as.character(values$dfWorking[input$testRF_rows_selected,5]), mat, model)}
       else {" "}
       ,color = "blue"
       ,icon=icon("lightbulb-o")
     )
     
   })
   
   output$positive = renderValueBox({
     valueBox(
       formatC(nrow(dashsample)-nrow(values$dfWorking), format="d", big.mark = ',')
       ,"Cases closed this month"
       ,color = "green"
       ,icon = icon("thumbs-up")
     )
   })
   output$neutral = renderValueBox({
     valueBox(
       formatC(nrow(values$dfWorking), format="d", big.mark = ',')
       ,"Unresolved cases"
       ,color = "yellow"
       ,icon =icon("inbox")
     )
   })
   output$negative = renderValueBox({
     valueBox(
       formatC(round(nrow(values$dfWorking[values$dfWorking$Sentiment=="Negative",])), format="d", big.mark = ',')
       ,"Negative Cases"
       ,color = "red"
       ,icon = icon("frown-o")
     )
   })
   observeEvent(input$deleteRows,{
     
     if (!is.null(input$testRF_rows_selected)) {
       
       values$dfWorking <- values$dfWorking[-as.numeric(input$testRF_rows_selected),]
     }
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

