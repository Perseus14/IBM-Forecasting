### Get Data Page 1
ui_login <- function(...) {
  args <- list(...)
  list(ui = fluidPage(  
    useShinyjs(),
    #css hex color - http://www.w3schools.com/cssref/css_colors.asp
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;} #login_link {float:right;} #login_login {color: #006600;} #login_application {color: #0000ff;}"))),
    tags$head(tags$style(HTML("#demo{background-color: blue;}.input_msg {color: #FF9999;} .input_success {color: #339900;} .input_fail {color: #CC0033;} "))),
    tags$script('$(document).keyup(function(event) { if (event.keyCode == 13) { if($("#login_application").length) { $("#login_application").click(); } else { $("#login_login").click();}}});'),
    tags$script('$("#login_username").focus();'),
    # div(id = "login_link",
    #     actionButton("login_leave", "Leave", icon = icon("close"), width = "100px")
    # ),
    column(12, align="center", h3("Forecasting Tool")),

           wellPanel(
             br(),
             br(),
             textInput("acc", "Account", "IBM"),
             div(class = "input_msg", textOutput("login_username_msg")),
            selectInput("ind", "Choose industry",
                                  list('Select'=c("Aerospace & Defense",
  "Automotive", "Banking", "Chemicals & Petroleum", "Energy & utilities",
  "Financial services", "Insurance", "Healthcare", "Media & entertainment",
  "Retail", "Telecommunication", "Travel & transport", "Others"))
                      )
              #,textOutput("result")
                      
                ),
             sidebarPanel(
               fileInput('file1', 'Choose Excel File',
                         accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv','text/xlsx','.xlsx')),
               tags$hr(),
               #checkboxInput('header', 'Header', TRUE),
               #radioButtons('sep', 'Separator',
               #              c(Comma=',',
               #              Semicolon=';',
               #              Tab='\t'),
               #             'Comma'),
             #br(),
             #if (is.null(filedata())) return()
             uiOutput('ui.action')
             #actionButton("login_login", "Submit", icon = icon("sign-in"), width = "100px")
            
             ),
             mainPanel(
               tableOutput('contents')
             ),
             
             
             br()
             # div(class = "input_fail", textOutput("login_fail")),
             # uiOutput("login_more")
           )
    )
    
    
}

# ui_logout <- function(...) {
#   args <- list(...)
#   fluidRow(
#     useShinyjs(),
#     tags$head(tags$style(HTML(".container-fluid {margin: 150px;} #logout_message {text-align: center;}"))),
#     column(5, offset = 4,
#            wellPanel(
#              div(id = "logout_message",
#                  h4(args$message)
#                  )
#            )
#     )
#   )
# }

### Trend Page 2
ui_application <- function(...) {
  args <- list(...)
  shinyUI(fluidPage(theme = "bootstrap.css"
                    ,column(12, align="center", h2("Input Data Distribution"))
                  ,fluidRow(
                    column(4, h3("Data Details"),wellPanel(htmlOutput("text"))) 
                      ,column(4, h3("Quality Metrics"),wellPanel(htmlOutput("text1"))) 
                      #column(5,wellPanel(tableOutput('contents')))
                      ,column(7,h3("Plot of Trend"),plotOutput("trendplot"))
                      #,column(12,verbatimTextOutput("summary"))
                    ),
                    div(id = "login_link",
                        actionButton("application_back", "Back", icon = icon("refresh"), width = "100px"),
                        actionButton("application_next", "Next", icon = icon("sign-out"), width = "100px")
                    )
                    
  ))
}

###Input Data Distribution plot Page 3
ui_algoPlot <- function(...) {
  args <- list(...)
  list(ui = fluidPage(  
    useShinyjs(),
    #css hex color - http://www.w3schools.com/cssref/css_colors.asp
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;} #login_link {float:right;} #login_login {color: #006600;} #login_application {color: #0000ff;}"))),
    tags$head(tags$style(HTML(".input_msg {color: #FF9999;} .input_success {color: #339900;} .input_fail {color: #CC0033;} .run_forecast{background-color:orange;}"))),
    tags$script('$(document).keyup(function(event) { if (event.keyCode == 13) { if($("#login_application").length) { $("#login_application").click(); } else { $("#login_login").click();}}});'),
    tags$script('$("#login_username").focus();'),
    column(12, align="center", offset = 0, h2("Input Data Distribution")),
    br(),
    br(),
    fluidRow(
      column(6,plotOutput("serviceLine_Tick"))
      ,column(6,plotOutput("severity_Tick")),br(),br(),br()
      ,column(10,plotOutput("heat_wave"))
      ,column(8,h3("Table of Count of Severity and Serviceline"),dataTableOutput("table"))
    ),
    mainPanel(plotOutput("plot")),
    div(id = "login_link",
        actionButton("input_dis_next", "Next", icon = icon("sign-out"), width = "100px")
    )
    
    
  )
  )
}



### Choose preference Page 4 
ui_selectalgo <- function(...) {
  args <- list(...)
  
  list(ui = fluidPage(
    useShinyjs(),
    #css hex color - http://www.w3schools.com/cssref/css_colors.asp
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;} #login_link {float:right;} #login_login {color: #006600;} #login_application {color: #0000ff;}"))),
    tags$head(tags$style(HTML(".input_msg {color: #FF9999;} .input_success {color: #339900;} .input_fail {color: #CC0033;} .run_forecast{background-color:orange;}"))),
    tags$script('$(document).keyup(function(event) { if (event.keyCode == 13) { if($("#login_application").length) { $("#login_application").click(); } else { $("#login_login").click();}}});'),
    tags$script('$("#login_username").focus();'),
    column(12, offset = 1,align="center", h4("Forecasting Calculation Options")),
    br(),
    column(6,align="center",offset = 4,
           wellPanel(
             br(),
             br(),
             radioButtons("Preference", "Choose one:",
                          c(
                            'Recommended'= TRUE,
                            'Preference'= FALSE)
             ),
             selectInput("Prefoption", "Choose algorithm",
                         list('Select'=c("Croston","Simple Moving Average", "Holt Winter","ARIMA"))
             ),
             textInput("horizon", "Forecast Horizon in Weeks","12"),
             #div(id="submit",style="display:inline-block",submitButton("Run Forecast"), style="float:bottom"),
             
             br(),
             actionButton("demo", "Accuracy check"),#to click definitely before forecast
             bsModal("largeModalID","Comparison of Error for Different Algorithms", "demo", size = "large",lapply(1:length(servicelist), function(i) {
  
               #add titles to tables
                 #title=servicelist[i] 
                 #column(i*4, h1(servicelist[i]))
               div(style = 'overflow-x: scroll', tableOutput(paste("table", i, sep="")))
                 #,print(i)
                 #uiOutput("plots"),print(i)
               
             })),
             br(),
             br(),
             actionButton("run_forecast", "Run Forecast", width = "100px")
             
          
           )
    )
    
  )
  )
}


###Forecast plot Page 5
ui_forecastPlot <- function(...) {
  args <- list(...)
  list(ui = fluidPage(  
    useShinyjs(),
    #css hex color - http://www.w3schools.com/cssref/css_colors.asp
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;} #login_link {float:right;} #login_login {color: #006600;} #login_application {color: #0000ff;}"))),
    tags$head(tags$style(HTML(".input_msg {color: #FF9999;} .input_success {color: #339900;} .input_fail {color: #CC0033;} .run_forecast{background-color:orange;}"))),
    tags$script('$(document).keyup(function(event) { if (event.keyCode == 13) { if($("#login_application").length) { $("#login_application").click(); } else { $("#login_login").click();}}});'),
    tags$script('$("#login_username").focus();'),
    column(12, align="center", offset = 0, h2("Forecast Data")),
    mainPanel(
      do.call(tabsetPanel, c(id='t',lapply(1:(length(servicelist)+1), function(i) {
        if(i<=length(servicelist))
        {
          tabPanel(
            
            
            
            title=servicelist[i],
            column(12,plotOutput(paste("plot", i, sep=""), width = "100%"))
            # ,column(6, h4("Summary"),wellPanel()
            
            #,print(i)
            #uiOutput("plots"),print(i)
          )
        }
        else if(i>length(servicelist) && isRecommended == TRUE)
        {
          tabPanel(
            title="Summary",
            column(6,tableOutput("winner"))
          )
        }
      })))
    ),
    sidebarPanel(
      downloadButton('downloadData', 'Download Forecast Value')
    )
  )
  )
}  



ui <- (htmlOutput("page"))


