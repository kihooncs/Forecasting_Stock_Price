library(shiny)
library(dplyr)
library(rvest)
library(highcharter)
library(shinythemes)

tbl <-
  read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies') %>% html_nodes(css = 'table')
tbl <- tbl[1] %>% html_table() %>% as.data.frame()
tbl$Ticker.symbol <- gsub(pattern = '\\.', '-', tbl$Ticker.symbol)
SM500security <- tbl$Security
SM500symbol <- tbl$Ticker.symbol
SM500Industries <- unique(tbl$GICS.Sector)
indicators.names <- c(
  "Simple Moving Average",
  "Exponential Moving Average",
  "Bollinger Bands",
  "Commodity Channel Index",
  "Chande Momentum Oscillator",
  "Moving Average Convergence Divergence"
)
indicators.func <- c("SMA", "EMA", "BBands", "CCI", "CMO", "MACD")
shinyUI (navbarPage(
  id = "navbar",
  ## theme of the shiny app
  theme = shinytheme("readable"),
  # Application title
  "Stock Trends: Visualize & Predict",
  tabPanel(
    "Stock Trends Graphed",
    sidebarPanel(
      ## select box for company symbol(has placeholder)
      selectizeInput(
        'symbol',
        label = h3('Company'),
        choices = SM500symbol,
        options = list(
          placeholder = 'Please select a company below',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      ## select box for plot type, selected one is candlestick
      selectInput(
        "plottype",
        label = h3("Plot Types"),
        choices = c(
          "candlestick",
          "line",
          "area",
          "spline",
          "ohlc",
          "column",
          "columnrange"
        ),
        selected = 3
      ),
      ## select box for indicator that add to the plot
      selectInput(
        "indicator",
        label = h3("Indicator Options"),
        choices = indicators.names,
        multiple = TRUE
      ),
      ## select box for the company that add to the plot
      selectInput(
        "comparision",
        label = h3("Company to Compare Against"),
        choices = SM500symbol,
        multiple = TRUE
      ),
      ## button to plot based on the user input
      actionButton("go", "Plot")
    ),
    ## stock plot
    mainPanel(highchartOutput("stockplot"))
  ),
  tabPanel(
    "Stock Historical Data",
    sidebarPanel(
      ## select for the industries of stocks, selected is all
      selectInput(
        "industry",
        label = h3("Industry"),
        choices = c("All", SM500Industries),
        selected = 1
      ),
      ## symbol of company, which is connecting with industry part
      uiOutput("compAbbr")
    ),
    ## data table for the selected stock
    mainPanel(dataTableOutput('selectedstock'))
  ),
  tabPanel("Predict Stocks",
           sidebarPanel(
             ## selected box of the symbols of companies(has placeholder)
             selectizeInput(
               'predictstock',
               'Company Symbols',
               choices = SM500symbol,
               options = list(
                 placeholder = 'Please select a company below',
                 onInitialize = I('function() { this.setValue(""); }')
               )
             ),
             ## numeric input of the number of days to predict
             numericInput(
               "periods",
               "period that you want to predict(recommand 365 days)",
               value = 365
             ),
             ## action button to do the plot
             actionButton("go2", "Plot predicted data"),
             ## download button to download the csv file of predicted data of the stock
             downloadButton("downloadData", "Download the predicted data")
           ),
           ## plot the predicted stock trend
           mainPanel(
             plotOutput("predictstockplot", height = "800px")
           )),
  tabPanel("About Us",
           h2("Introductions"),
           p("Greetings! We are Sangho Bak, Yao Dou, Jeffrey Jing, and David Lee, also known as team \"50/50\", from INFO 201 BD.
              We're proud and honored to bring you Stock Trends: Visualize and Predict, an interactive program that allows users
              to analyze stock trends and compare two different stocks from the S&P 500. All data is exportable and downloadable,
              and is and will always be free to use."),
           h3 ("About Us"),
           p("We came upon the name 50/50 when we realized that our group consisted of two koreans and two chinese people. However,
              we have high hopes that together we'll combine for a solid 100%. Sangho Bak is a Junior who is currently a student in
              the Foster School of Business, is the oldest member of the group. Jeffrey Jing and David Lee are both Sophomores, who
              are actively seeking to major in an engineering/technology related field. Yao Dou, the youngest member of our group,
              is a brilliant Freshman who is only sixteen! Yao is a significant contributor in making ST:VP, and is actively seeking
              to become a computer science major. The entire team has no doubts of his abilities, and are all rooting for his future
              success. Together, this ragtag team proudly brings to you Stock Trends: Visualize and Predict. ")
           )
))