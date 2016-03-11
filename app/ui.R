library(shiny)
library(shinydashboard)
library(dygraphs)
library(TTR)
library(DT)


dashboardPage(
  
  dashboardHeader(title = 'MBS Trends', titleWidth = 250),
  dashboardSidebar(width = 250,
    sidebarMenu(id = 'sidebarmenu',
      menuItem('Graphical analysis', tabName = 'graphical', icon = icon('line-chart')),
      menuItem('Data view', tabName = 'tabular', icon = icon('table')),
      selectInput('classes', label = 'Select asset class:', choices = list('FHLMC' = 'FHLMC', 'FNMA' = 'FNMA', 'GNMA' = 'GNMA')),
      selectInput('family', label = 'Select mortgage family:', choices = list('Single Family 15Y' = 'Single Family 15Y', 'Single Family 30Y' = 'Single Family 30Y')),
      dateRangeInput('dates', 'Select date range:', start = min(master.data$date), min = min(master.data$date), end = max(master.data$date), max = max(master.data$date)),
      checkboxGroupInput('coupon', label = 'Select coupon values:', choices = levels(master.data$coupon), selected = levels(master.data$coupon)[1]),
      
      conditionalPanel("input.sidebarmenu == 'graphical'",
      checkboxInput('smoother', label = 'Show moving average values:', value = FALSE),
      
      conditionalPanel(
        condition = 'input.smoother == true',
        sliderInput('smootherScale', 'Select moving average days:', min = 1, max = 30, value = 1)
      ))
    )
  ),
  
  dashboardBody(
    fluidRow(
      tabItems(
        tabItem(tabName = 'graphical',
          tabBox(title = 'Metrics', width = 12,
             tabPanel(id = 'avgPrice', title = 'Average Price', height = '650px',
                      dygraphOutput('avgPrice.graph', height = '600px')),
             tabPanel(id = 'wgtPrice', title = 'Weighted Price', height = '650px',
                      dygraphOutput('wgtPrice.graph', height = '600px')),
             tabPanel(id = 'tradesVol', title = 'Volume of Trades', height = '650px',
                      dygraphOutput('tradeVol.graph', height = '600px')),
             tabPanel(id = 'tradesNum', title = 'Number of Trades', height = '650px',
                      dygraphOutput('tradeNum.graph', height = '600px'))
             )
    ),
        tabItem(tabName = 'tabular',
            tabBox(title = 'Data', width = 12,
            tabPanel(id = 'dataView', title = 'Data view', height = '650px',
            dataTableOutput('tabularView')
            )
  )
      )
    ))
  )
)