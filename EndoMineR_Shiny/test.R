library(shiny)
library(DT)
library(shinydashboard)
library(here)
library(radiant)
library(radiant.basics)
library(radiant.data)
library(radiant.design)



ui <- dashboardPage(
  dashboardHeader(),

  dashboardSidebar(DT::dataTableOutput("test")),
  dashboardBody()
)

server <- shinyServer(function(input, output, session) {
  dat <- data.frame(
    country = c('China', 'Belgium'),
    flag = c('<img src="http://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Flag_of_the_People%27s_Republic_of_China.svg/200px-Flag_of_the_People%27s_Republic_of_China.svg.png" height="52"></img>',
             '<img src="img1213.jpg" height="52"></img>'
    )
  )
  output$test <- DT::renderDataTable({ DT::datatable(dat, escape = F) })
})

shinyApp(ui=ui, server=server)

