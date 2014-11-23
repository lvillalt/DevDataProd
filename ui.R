library(markdown)

shinyUI(fluidPage(
  headerPanel("Time Series Decomposition Analysis"),
  sidebarPanel(
    radioButtons("ptyp", "Plot:",
                 choices=c("Data", "TS")),
    tags$hr(),
    radioButtons("frq", "Sample Time Series:",
                 choices=c("Freq1.rds",
                   "Freq2.rds",
                   "Freq3.rds",
                   "Freq4.rds",              
                   "Uploaded")),
    fileInput('file1', 'Choose file to upload',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv'
              )
    ),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ','),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 '"'),
    tags$hr(),
    p('To upload a *.csv file, and have it plot,',
      'ensure that it includes a SINGLE numeric COLUMN,',
      'and that there is a capital letter X in the header.',
      'Analysis for uploaded files assumes MONTHLY, rather ',
      'than daily frequency.'
    )    

  ),
  mainPanel(
    
    tabsetPanel(type = "tabs", 
                tabPanel("Documentation", includeHTML("Documentation.html")),
 #               tabPanel("Documentation", includeMarkdown("Documentation.md")),
                tabPanel("Trend", plotOutput("trendPlot")),
                tabPanel("Seasonal", plotOutput("seasonPlot")),
                tabPanel("Random", plotOutput("randomPlot")),
                tabPanel("StructTS", plotOutput("structTSPlot")), 
                tabPanel("Loess", plotOutput("stlPlot")),
                tabPanel("Summary", verbatimTextOutput("summary")), 
                tabPanel("Sample Table", dataTableOutput(outputId="table")),
                tabPanel("Uploaded Table", tableOutput('contents'))
    )
      
  )
))
