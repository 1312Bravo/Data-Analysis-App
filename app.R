# https://towardsdatascience.com/how-to-build-a-data-analysis-app-in-r-shiny-143bee9338f7
# https://rstudio.github.io/shinydashboard/structure.html
# https://mastering-shiny.org/index.html
# https://www.rstudio.com/resources/cheatsheets/

# https://www.statology.org/r-guides/
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# http://www.sthda.com/english/articles/32-r-graphics-essentials/131-plot-two-continuous-variables-scatter-graph-and-alternatives/

library(shiny)
library(shinydashboard)
library(fresh)
# library(icons)
# library(fontawesome)
library(shinyWidgets)
library(htmltools)
library(data.table)
library(shinyjs)
library(DT)
library(ggplot2)
library(ggExtra)
library(dplyr)
library(knitr)
library(kableExtra)
library(skimr)
library(DataExplorer)

# 0. Libraries & Source ----

source("fresh-themes.R")
source("Analysis-Plot-Functions.R")
source("Analysis-Statistics-Functions.R")
pokemon <- read.csv("pokemon.csv")

not_sel <- "Not Selected"

# options(shiny.error = function() {
#   stop("An error has occurred")
# })

# 1. UI ----
ui <- dashboardPage(title = "Data Analyser", 
                    
                    
                    ## 1.1 Header ----      
                    dashboardHeader(title = "Data Analyser"),
                    
                    ## 1.2 Side Bar ----
                    dashboardSidebar(
                      sidebarMenu(
                        
                        menuItem("Data Import", tabName = "data_import", icon = icon("upload")),
                        menuItem("Data Look", tabName = "data_look", icon = icon("book-reader")),
                        menuItem("Data Manipulation", tabName = "manipulation", icon = icon("wrench")),
                        menuItem("Analysis", tabName = "analysis", icon = icon("address-card"))
                        
                      ) # sidebarMenu
                    ), # Side Bar
                    
                    dashboardBody(
                                  # 3. CSS tags ----  
                                  # Import CSS $& Themes
                                  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
                                  use_theme(mytheme_2), use_theme(mytheme), useShinyjs(),
                                  # Datatable Look
                                  tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #fcba03 !important;}')),
                                  # Errors
                                  tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
                                  #tags$style(type='text/css', '#metadata, #summary {white-space: pre-wrap;}'),
                                  # Notifications
                                  tags$style(succNotifyTag("newcol_succ")), tags$style(errNotifyTag("newcol_err")),
                                  tags$style(succNotifyTag("colname_succ")), tags$style(errNotifyTag("colname_err")),
                                  tags$style(succNotifyTag("collclas_succ")), tags$style(errNotifyTag("collclas_err")),
                                  
                                  tabItems(
                                    
                                    ## 1.3 Data Import ----
                                    tabItem(tabName = "data_import",
                                            fluidRow(
                                              
                                              ### 1.3.1 Import ----
                                              box(width=4, title = "Inputs", solidHeader = TRUE,
                                                  fileInput(inputId = "csv_input", 
                                                            label = "Select CSV File to Import",
                                                            accept = ".csv",
                                                            buttonLabel = list(icon("searchengin"), "Browse..."),
                                                            placeholder = "Data.csv")
                                              ), # side bar
                                              
                                              tabBox(width=8, 
                                                     
                                                     ### 1.3.2 Data Introduce ---- 
                                                     tabPanel(title = "Introduce", solidHeader = TRUE,
                                                              conditionalPanel(condition = "output.csv_import_ready",
                                                                               fluidRow(
                                                                                 column(width = 4, verbatimTextOutput(outputId = "introduce")),
                                                                                 column(width = 4, verbatimTextOutput(outputId = "missing")),
                                                                                 column(width = 4, verbatimTextOutput(outputId = "class"))
                                                                               ))),
                                                     ### 1.3.3 Metadata ----
                                                     tabPanel(title = 'Metadata', solidHeader = TRUE,
                                                              conditionalPanel(condition = "output.csv_import_ready",
                                                                               verbatimTextOutput(outputId = "metadata"))),
                                                     ### 1.3.4 Summary ----
                                                     tabPanel(title = 'Summary', solidHeader = TRUE,
                                                              conditionalPanel(condition = "output.csv_import_ready",
                                                              verbatimTextOutput(outputId = "summary")))
                                                     
                                              ) # main panel
                                              
                                            )
                                    ), # Data import
                                    
                                    ## 1.6 Data Look ----
                                    
                                    tabItem(tabName = "data_look",
                                            conditionalPanel(condition="output.csv_import_ready",
                                                             fluidRow(div(class = 'mainPanel', dataTableOutput(outputId = "file_input_show"))))
                                            ),
                                    
                                    ## 1.5 Data manipulation ----
                                    
                                    tabItem(tabName = "manipulation",
                                            
                                            fluidRow(
                                              #### 1.5.1 Col Name Select ----
                                              box(width = 3, title = "Change Column Name", solidHeader = TRUE,
                                                  div(class = "center-select-3", 
                                                      selectInput(inputId = "change_col_name",
                                                              label = "Old Column Name",
                                                              choices = c(not_sel),
                                                              width = selectBarWidth)),
                                                  div(class = "center-select-3", 
                                                      textInput(inputId = 'new_col_name', 
                                                            label = 'New Column Name',
                                                            placeholder = not_sel,
                                                            width = selectBarWidth)),
                                                  actionButton(inputId = "rename_col",
                                                               label = "Rename",
                                                               width = buttonWidth)
                                              ), 
                                              
                                              ### 1.5.3 New Column ----
                                              box(width = 6, title = "Make New Column From Two Existing", solidHeader = TRUE,
                                                  fluidRow(
                                                    column(width = 6, div(class = "center-select-3",
                                                           selectInput(inputId = "new_col_1",
                                                                       label = "Column One",
                                                                       choices = c(not_sel),
                                                                       width = selectBarWidth))),
                                                    column(width = 6, div(class = "center-select-3",
                                                           selectInput(inputId = "new_col_2",
                                                                       label = "Column Two",
                                                                       choices = c(not_sel),
                                                                       width = selectBarWidth)))
                                                  ),
                                                  div(class = "center-radio-6-4",
                                                    radioGroupButtons(inputId = "new_col_operation",
                                                                      label = "Choose an operation",
                                                                      choices = c(`<i class="fas fa-plus"></i>` = "plus",
                                                                                  `<i class="fas fa-minus"></i>` = "minus",
                                                                                  `<i class="fas fa-times"></i>` = "times",
                                                                                  `<i class="fas fa-divide"></i>` = "divide"),
                                                                      justified = TRUE,
                                                                      width = "350px"),
                                                  ),
                                                  fluidRow(
                                                    column(width = 6, div(class = "center-select-3", 
                                                                          textInput(inputId = 'newnew_col_name', 
                                                                                    label = 'Column Name',
                                                                                    placeholder = not_sel,
                                                                                    width = selectBarWidth))),
                                                    column(width = 6, div(class = "center-select-3",
                                                                          actionButton(inputId = "makenew_col",
                                                                                       label = "Compute",
                                                                                       width = buttonWidth)))
                                                  )),
                                              
                                              ### 1.5.3 Col ClassSelect -----
                                              box(width = 3, title = "Change Column Class",
                                                  div(class = "center-select-3", 
                                                      selectInput(inputId = "change_col_class",
                                                              label = "Column Name",
                                                              choices = c(not_sel),
                                                              width = selectBarWidth)),
                                                  div(class = "center-select-3", 
                                                      selectInput(inputId = "new_col_class",
                                                              label = "New Column Class",
                                                              choices = c("Not selected" = "ns", "Factor" = "factor", "Numeric" = "numeric", "Integer" = "integer", "Character" = "character"),
                                                              width = selectBarWidth)),
                                                  actionButton(inputId = "reclass_col",
                                                               label = "Change class",
                                                               width = buttonWidth)
                                              )
                                            )
                                    ),
                                    
                                    ## 1.4 Analysis ----
                                    tabItem(tabName = "analysis",
                                            fluidRow(
                                              
                                              ### 1.4.1 Selectors ----
                                              box(width = 3, title = 'Inputs',
                                                  div(class = "center-select-3",
                                                      selectInput(inputId = "num_var_1", 
                                                              label = list("Numerical Variable ", icon("hand-pointer")),
                                                              choices = c(not_sel),
                                                              width = selectBarWidth)),
                                                  div(class = "center-select-3", 
                                                      selectInput(inputId = "num_var_2", 
                                                              label = list("Numerical Variable ", icon("hand-peace")),
                                                              choices = c(not_sel),
                                                              width = selectBarWidth)),
                                                  div(class = "center-select-3",
                                                      selectInput(inputId = "fact_var", 
                                                              label = list("Factor Variable ", icon("hand-spock")),
                                                              choices = c(not_sel),
                                                              width = selectBarWidth)),
                                                  actionButton(inputId = 'clear_button', 
                                                               label = 'Clear',
                                                               icon = icon("snowplow"),
                                                               width = buttonWidth)
                                                  
                                              ), # inputs
                                              
                                              tabBox(width = 9,
                                                     
                                                     
                                                     ### 1.4.2 Plots ----
                                                     tabPanel(title = "Plot", 
                                                              
                                                              #### 1.4.2.1 One Numeric ----
                                                              conditionalPanel(condition = num_var_one, 
                                                                               radioGroupButtons(inputId = "num_var_one",
                                                                                                 label = "Choose a graph",
                                                                                                 choices = c(`<i class="fas fa-signal"></i>` = "histogram",
                                                                                                             `<i class="fas fa-chart-area"></i>` = "density",
                                                                                                             `<i class="fas fa-box-open"></i>` = "box"),
                                                                                                 justified = TRUE,
                                                                                                 width = radioButtonWidth),
                                                                               conditionalPanel(condition = "input.num_var_one == 'histogram'",
                                                                                                div(class = "plot-border", plotOutput(outputId = "num_var_one_hist"))),
                                                                               conditionalPanel(condition = "input.num_var_one == 'density'",
                                                                                                div(class = "plot-border", plotOutput(outputId = "num_var_one_density"))),
                                                                               conditionalPanel(condition = "input.num_var_one == 'box'",
                                                                                                div(class = "plot-border", plotOutput(outputId = "num_var_one_box"))),
                                                                               
                                                              ), # One Numeric
                                                              #### 1.4.2.2 Two Numeric ----
                                                              conditionalPanel(condition = num_var_1_num_var_2,
                                                                               radioGroupButtons(inputId = "num_var_two",
                                                                                                 label = "Choose a graph",
                                                                                                 choices = c(`<i class="fas fa-braille"></i>` = "scatter",
                                                                                                             `<i class="fas fa-braille"></i><span>  </span><i class="fas fa-chart-area"></i>` = "density",
                                                                                                             `<i class="fas fa-braille"></i><span>  </span><i class="fas fa-box-open"></i>` = "box"),
                                                                                                 justified = TRUE,
                                                                                                 width = radioButtonWidth),
                                                                               conditionalPanel(condition = "input.num_var_two == 'scatter'",
                                                                                                div(class = "plot-border", plotOutput(outputId = "num_var_two_scatter"))),
                                                                               conditionalPanel(condition = "input.num_var_two == 'density'",
                                                                                                div(class = "plot-border", plotOutput(outputId = "num_var_two_density"))),
                                                                               conditionalPanel(condition = "input.num_var_two == 'box'",
                                                                                                div(class = "plot-border", plotOutput(outputId = "num_var_two_box"))),
                                                              ), # Two numeric
                                                              
                                                              #### 1.4.2.3 Numeric & Factor ----
                                                              conditionalPanel(condition = num_var_fact_var,
                                                                               radioGroupButtons(inputId = "num_var_fact_var",
                                                                                                 label = "Choose a graph",
                                                                                                 choices = c(`<i class="fas fa-signal"></i>` = "histogram",
                                                                                                             `<i class="fas fa-chart-area"></i>` = "density",
                                                                                                             `<i class="fas fa-box-open"></i>` = "box"),
                                                                                                 justified = TRUE,
                                                                                                 width = radioButtonWidth),
                                                                               conditionalPanel(condition = "input.num_var_fact_var == 'histogram'",
                                                                                                div(class = "plot-border", plotOutput(outputId = "num_var_fact_var_histogram"))),
                                                                               conditionalPanel(condition = "input.num_var_fact_var == 'density'",
                                                                                                div(class = "plot-border", plotOutput(outputId = "num_var_fact_var_density"))),
                                                                               conditionalPanel(condition = "input.num_var_fact_var == 'box'",
                                                                                                div(class = "plot-border", plotOutput(outputId = "num_var_fact_var_box"))),
                                                              ), # Numeric & Factor
                                                              
                                                              #### 1.4.2.4 Factor ----
                                                              conditionalPanel(condition = fact_var,
                                                                               radioGroupButtons(inputId = "only_fact_var",
                                                                                                 label = "Choose a graph",
                                                                                                 choices = c(`<i class="fas fa-chart-bar"></i>` = "bar",
                                                                                                             `<i class="fas fa-chart-pie"></i>` = "pie"),
                                                                                                 justified = TRUE,
                                                                                                 width = radioButtonWidth),
                                                                               conditionalPanel(condition = "input.only_fact_var == 'bar'",
                                                                                                div(class = "plot-border", plotOutput(outputId = "only_fact_var_bar"))),
                                                                               conditionalPanel(condition = "input.only_fact_var == 'pie'",
                                                                                                div(class = "plot-border", plotOutput(outputId = "only_fact_var_pie"))),
                                                              ), # Factor
                                                              
                                                              #### 1.4.2.5 Two Numeric & Factor ----
                                                              conditionalPanel(condition = num_var_1_num_var_2_fact_var,
                                                                               radioGroupButtons(inputId = "two_num_var_fact_var",
                                                                                                 label = "Choose a graph",
                                                                                                 choices = c(`<i class="fas fa-braille"></i>` = "scatter",
                                                                                                             `<i class="fas fa-braille"></i><span>  </span><i class="fas fa-chart-area"></i>` = "density",
                                                                                                             `<i class="fas fa-braille"></i><span>  </span><i class="fas fa-box-open"></i>` = "box"),
                                                                                                 justified = TRUE,
                                                                                                 width = radioButtonWidth),
                                                                               conditionalPanel(condition = "input.two_num_var_fact_var == 'scatter'",
                                                                                                div(class = "plot-border", plotOutput(outputId = "two_num_var_fact_var_scatter"))),
                                                                               conditionalPanel(condition = "input.two_num_var_fact_var == 'density'",
                                                                                                div(class = "plot-border", plotOutput(outputId = "two_num_var_fact_var_density"))),
                                                                               conditionalPanel(condition = "input.two_num_var_fact_var == 'box'",
                                                                                                div(class = "plot-border", plotOutput(outputId = "two_num_var_fact_var_box"))))
                                                              
                                                     ), # Plot
                                                     
                                                     ### 1.4.3 Statistics ----
                                                     tabPanel(title = "Statistics", 
                                                              
                                                              #### 1.4.3.1 One Numeric ----
                                                              conditionalPanel(condition = num_var_one,
                                                                               fluidRow(
                                                                                 column(width = 12, align = "center", 
                                                                                        div(class = "stat-border", tableOutput(outputId = "num_var_only_summary_table")))
                                                                               )),
                                                              #### 1.4.3.2 Two Numeric ----
                                                              conditionalPanel(condition = num_var_1_num_var_2,
                                                                               fluidRow(
                                                                                 column(width = 6, align = "center",
                                                                                        div(class = "stat-border", tableOutput(outputId = "num_var_1_summary_table"))),
                                                                                 column(width = 6, align = "center",
                                                                                        div(class = "stat-border", tableOutput(outputId = "num_var_2_summary_table")))
                                                                               )),
                                                              #### 1.4.3.3 Numeric & Factor ----
                                                              conditionalPanel(condition = num_var_fact_var,
                                                                               fluidRow(
                                                                                 column(width = 6, align = "center",
                                                                                        div(class = "stat-border", tableOutput(outputId = "num_var_summary_table"))),
                                                                                 column(width = 6, align = "center",
                                                                                        div(class = "stat-border", tableOutput(outputId = "fact_var_summary_table")))
                                                                               )),
                                                              #### 1.4.3.4 Factor ----
                                                              conditionalPanel(condition = fact_var,
                                                                               fluidRow(
                                                                                 column(width = 12, align = "center",
                                                                                        div(class = "stat-border", tableOutput(outputId = "fact_var_only_summary_table")))
                                                                               )),
                                                              #### 1.4.3.5 Two Numeric & Factor ----
                                                              conditionalPanel(condition = num_var_1_num_var_2_fact_var,
                                                                               fluidRow(
                                                                                 column(width = 4, align = "center",
                                                                                        div(class = "stat-border", tableOutput(outputId = "num_var_11_summary_table"))),
                                                                                 column(width = 4, align = "center",
                                                                                        div(class = "stat-border", tableOutput(outputId = "num_var_12_summary_table"))),
                                                                                 column(width = 4, align = "center",
                                                                                        div(class = "stat-border", tableOutput(outputId = "fact_var_add_summary_table")))
                                                                               )),
                                                     ) # Statistics
                                                     
                                              )
                                              
                                            )) # analysis
                                    
                                  ) # tabItems
                    ) # Body
                    
) # ui

# 2. Server ----
server <- function(input, output, session){
  
  ## 2.1 Data Import ----
  
  output$file_input_name <- renderText({
    if(is.null(input$csv_input)){"No file uploaded yet"}
    else(gsub("\\..*", "", input$csv_input$name))
  })
  
  ### 2.1.1 Data input ----
  
  globals <- reactiveValues(path=NULL)
  
  observeEvent(input$csv_input, {
    req(input$csv_input)
    data <- fread(input$csv_input$datapath)
    colnames(data) <- gsub(" ", "_", colnames(data))
    globals$data_input = data
  })
  
  output$csv_import_ready <- reactive({
    return(!is.null(input$csv_input))
  })
  outputOptions(output, "csv_import_ready", suspendWhenHidden = FALSE)
  
  ### 2.1.2 Introduce ----
  
  print_introduce <- eventReactive(input$csv_input, {
    data <- t(introduce(globals$data_input))
    data <- as.data.frame(data)
    rownames(data) <- c("Rows", "Columns", "Discrete Columns", "Continious Columns", "All Missing Columns",
                        "Total Missing Values", "Complete Rows", "Total Observations", "Memory Usage")
    colnames(data) <- c("Value")
    data})
  
  output$introduce <- renderPrint({print_introduce()})
  
  print_missing <- eventReactive(input$csv_input, {
    data <- profile_missing(globals$data_input)
    data <- as.data.frame(data)
    rownames(data) <- data$feature
    data <- data[,-1]
    colnames(data) <- c("No. Missing", "% Missing")
    data})
  
  output$missing <- renderPrint({print_missing()})
  
  print_class <- eventReactive(input$csv_input, {
    data <- t(as.data.frame(lapply(globals$data_input, class)))
    colnames(data) <- "Class"
    data})
  
  output$class <- renderPrint({print_class()})
  
  
  ### 2.1.3 Metadata ----
  
  print_metadata <- eventReactive(input$csv_input, {skim(globals$data_input)})
  output$metadata <- renderPrint({print_metadata()})
  
  ### 2.1.4 Summary ----
  
  output$summary <- renderPrint({summary(globals$data_input)})
  
  ## 2.2.4 Data Look ----
  
  output$file_input_show <- renderDataTable(
    globals$data_input,
    rownames = FALSE,
    class = 'row-border',
    options = list(scrollX = TRUE,
                   pageLength = 14,
                   # colReorder = TRUE,
                   columnDefs = list(list(className = 'dt-center', targets = "_all")),
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#fcba03', 'color': '#fff'});",
                     "}")
    )
  )
  
  ## 2.3 Manipulation ----
  
  ### 2.3.1 Column Name ----
  
  observeEvent(input$rename_col, {
    if(input$change_col_name != not_sel){
      names(globals$data_input)[names(globals$data_input) == input$change_col_name] <- input$new_col_name
    }
    
    if(input$new_col_name != "" & input$change_col_name != not_sel){succNotification("colname_succ")}
    else{errNotification("colname_err")}
  })
  
  observeEvent(input$rename_col, {
    shinyjs::reset('change_col_name')
    shinyjs::reset('new_col_name')
  })
  
  ### 2.3.3 New Column ---- 
  
  observeEvent(input$makenew_col, {
    data <- globals$data_input
    num1 <- which(names(pokemon) == input$new_col_1)
    num2 <- which(names(pokemon) == input$new_col_2)
    
    if(input$new_col_operation == "plus"){data[, input$newnew_col_name] <- pokemon[, num1] + pokemon[, num2]}
    if(input$new_col_operation == "minus"){data[, input$newnew_col_name] <- pokemon[, num1] - pokemon[, num2]}
    if(input$new_col_operation == "times"){data[, input$newnew_col_name] <- pokemon[, num1] * pokemon[, num2]}
    if(input$new_col_operation == "divide"){data[, input$newnew_col_name] <- pokemon[, num1] / pokemon[, num2]}
    globals$data_input <- data
    
    if(input$new_col_1 != not_sel & input$new_col_2 != not_sel & input$newnew_col_name != ""){succNotification("newcol_succ")}
    else{errNotification("newcol_err")}
  })
  
  observeEvent(input$makenew_col, {
    shinyjs::reset('new_col_1')
    shinyjs::reset('new_col_2')
    shinyjs::reset('new_col_operation')
    shinyjs::reset('newnew_col_name')
  })
  
  ### 2.3.3 Column Class ----
  
  observeEvent(input$reclass_col, {
    # if(input$new_col_class == "Factor"){globals$data_input[,input$change_col_class] <- as.factor(as.character(globals$data_input[,input$change_col_class]))}
    # if(input$new_col_class == "Numeric"){globals$data_input[,input$change_col_class] <- as.numeric(globals$data_input[,input$change_col_class])}
    # if(input$new_col_class == "Integer"){globals$data_input[,input$change_col_class] <- as.integer(globals$data_input[,input$change_col_class])}
    # if(input$new_col_class == "Character"){globals$data_input[,input$change_col_class] <- as.character(globals$data_input[,input$change_col_class])}
    # if(input$new_col_class == "Date"){globals$data_input[,input$change_col_class] <- as.Date(globals$data_input[,input$change_col_class])}
    if(input$new_col_class != "ns"){
      globals$data_input <- eval(parse(text = paste0('globals$data_input %>% mutate(',
                                                     input$change_col_class, ' = as.', input$new_col_class, '(', input$change_col_class, '))')))
    }
    if(input$new_col_class != "ns" & input$change_col_class != not_sel){succNotification("collclas_succ")}
    else{errNotification("collclas_err")}
    })
  
  observeEvent(input$reclass_col, {
    shinyjs::reset('change_col_class')
    shinyjs::reset('new_col_class')
    })
  
  ## 2.2 Analysis ----
  
  ### 2.2.1 Select inputs ----
  
  observeEvent(globals$data_input, {
    choices <- c(not_sel, names(globals$data_input))
    freezeReactiveValue(input, "num_var_1")
    freezeReactiveValue(input, "fact_var")
    freezeReactiveValue(input, "num_var_2")
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
    
    # Change column name
    updateSelectInput(inputId = "change_col_name", choices = choices)
    # Change column class
    updateSelectInput(inputId = "change_col_class", choices = choices)
    # Make new column
    updateSelectInput(inputId = "new_col_1", choices = choices)
    updateSelectInput(inputId = "new_col_2", choices = choices)
  })
  
  observe({
    choices <- c(not_sel, names(globals$data_input))
    
    # Select variables
    if(!is.null(input$num_var_1) | !is.null(input$fact_var)){
      updateSelectInput(session, inputId = "num_var_2",
                        choices = c(not_sel, choices[!(choices %in% c(input$num_var_1, input$fact_var))]),
                        selected = isolate(input$num_var_2))
    }
    
    if(!is.null(input$num_var_1) | !is.null(input$num_var_2)){
      updateSelectInput(session, inputId = "fact_var",
                        choices = c(not_sel, choices[!(choices %in% c(input$num_var_1, input$num_var_2))]),
                        selected = isolate(input$fact_var))
    }
    
    if(!is.null(input$num_var_2) | !is.null(input$fact_var)){
      updateSelectInput(session, inputId = "num_var_1",
                        choices = c(not_sel, choices[!(choices %in% c(input$num_var_2, input$fact_var))]),
                        selected = isolate(input$num_var_1))
    }
    
    # Make new column
    if(!is.null(input$new_col_1)){
      updateSelectInput(session, inputId = "new_col_2",
                        choices = c(not_sel, choices[!(choices %in% c(input$new_col_1))]),
                        selected = isolate(input$new_col_2))
    }
    if(!is.null(input$new_col_2)){
      updateSelectInput(session, inputId = "new_col_1",
                        choices = c(not_sel, choices[!(choices %in% c(input$new_col_2))]),
                        selected = isolate(input$new_col_1))
    }
  })
  
  observeEvent(input$clear_button, {
    shinyjs::reset('num_var_1')
    shinyjs::reset('num_var_2')
    shinyjs::reset('fact_var')
    
  })
  
  num_var_1 <- reactive({input$num_var_1})
  num_var_2 <- reactive({input$num_var_2})
  fact_var <- reactive({input$fact_var})
  
  ### 2.2.2 Plot ----
  
  #### 2.2.2.1 One numeric ----
  
  # Histogram
  num_var_one_hist <- reactive({#input$run_button, {
    draw_num_var_one_hist(globals$data_input, num_var_1(), num_var_2())
  })
  output$num_var_one_hist <- renderPlot(num_var_one_hist())
  
  # Density plot
  num_var_one_density <- reactive({#input$run_button, {
    draw_num_var_one_density(globals$data_input, num_var_1(), num_var_2())
  })
  output$num_var_one_density <- renderPlot(num_var_one_density())
  
  # Box plot
  num_var_one_box <- reactive({#input$run_button, {
    draw_num_var_one_box(globals$data_input, num_var_1(), num_var_2())
  })
  output$num_var_one_box <- renderPlot(num_var_one_box())
  
  #### 2.2.2.2 Two numeric ----
  
  # Scatter plot
  num_var_two_scatter <- reactive({#input$run_button, {
    draw_num_var_two_scatter(globals$data_input, num_var_1(), num_var_2())
  })
  output$num_var_two_scatter <- renderPlot(num_var_two_scatter())
  
  # Density plot
  num_var_two_density <- reactive({#input$run_button, {
    draw_num_var_two_density(globals$data_input, num_var_1(), num_var_2())
  })
  output$num_var_two_density <- renderPlot(num_var_two_density())
  
  # Box plot
  num_var_two_box <- reactive({#input$run_button, {
    draw_num_var_two_box(globals$data_input, num_var_1(), num_var_2())
  })
  output$num_var_two_box <- renderPlot(num_var_two_box())
  
  #### 2.2.2.3 Numeric & Factor ---- 
  
  # Histogram
  num_var_fact_var_hist <- reactive({#input$run_button, {
    draw_num_var_fact_var_hist(globals$data_input, num_var_1(), num_var_2(), fact_var())
  })
  output$num_var_fact_var_histogram <- renderPlot(num_var_fact_var_hist())
  
  # Density plot
  num_var_fact_var_density <- reactive({#input$run_button, {
    draw_num_var_fact_var_density(globals$data_input, num_var_1(), num_var_2(), fact_var())
  })
  output$num_var_fact_var_density <- renderPlot(num_var_fact_var_density())
  
  # Box plot
  num_var_fact_var_box <- reactive({#input$run_button, {
    draw_num_var_fact_var_box(globals$data_input, num_var_1(), num_var_2(), fact_var())
  })
  output$num_var_fact_var_box <- renderPlot(num_var_fact_var_box())
  
  #### 2.2.2.4 Factor ---- 
  
  # Barplot
  only_fact_var_bar <- reactive({#input$run_button, {
    draw_only_fact_var_bar(globals$data_input, fact_var())
  })
  output$only_fact_var_bar <- renderPlot(only_fact_var_bar())
  
  # Pieplot
  only_fact_var_pie <- reactive({#input$run_button, {
    draw_only_fact_var_pie(globals$data_input, fact_var())
  })
  output$only_fact_var_pie <- renderPlot(only_fact_var_pie())
  
  #### 2.2.2.5 Two Numeric & Factor ----
  
  # Scatter plot
  two_num_var_fact_var_scatter <- reactive({#input$run_button, {
    draw_two_num_var_fact_var_scatter(globals$data_input, num_var_1(), num_var_2(), fact_var())
  })
  output$two_num_var_fact_var_scatter <- renderPlot(two_num_var_fact_var_scatter())
  
  # Density plot
  two_num_var_fact_var_density <- reactive({#input$run_button, {
    draw_two_num_var_fact_var_density(globals$data_input, num_var_1(), num_var_2(), fact_var())
  })
  output$two_num_var_fact_var_density <- renderPlot(two_num_var_fact_var_density())
  
  # Box plot
  two_num_var_fact_var_box <- reactive({#input$run_button, {
    draw_two_num_var_fact_var_box(globals$data_input, num_var_1(), num_var_2(), fact_var())
  })
  output$two_num_var_fact_var_box <- renderPlot(two_num_var_fact_var_box())
  
  ### 2.2.3 Statistics ----
  
  #### 2.2.3.1 One Numeric ----
  output$num_var_only_summary_table <- renderText(kablanje(num_var_only_summary_table(globals$data_input, num_var_1(), num_var_2())))
  
  #### 2.2.3.2 Two Numeric ----
  output$num_var_1_summary_table <- renderText(kablanje(numerical_stat(globals$data_input, num_var_1())))
  output$num_var_2_summary_table <- renderText(kablanje(numerical_stat(globals$data_input, num_var_2())))
  
  #### 2.2.3.3 Numeric & Factor ----
  output$num_var_summary_table <- renderText(kablanje(num_var_only_summary_table(globals$data_input, num_var_1(), num_var_2())))
  output$fact_var_summary_table <- renderText(kablanje(factor_stat(globals$data_input, fact_var())))
  
  #### 2.2.3.4 Factor ----
  output$fact_var_only_summary_table <- renderText(kablanje(factor_stat(globals$data_input, fact_var())))
  
  #### 2.2.3.5 Two Numeric &Factor ----
  output$num_var_11_summary_table <- renderText(kablanje(numerical_stat(globals$data_input, num_var_1())))
  output$num_var_12_summary_table <- renderText(kablanje(numerical_stat(globals$data_input, num_var_2())))
  output$fact_var_add_summary_table <- renderText(kablanje(factor_stat(globals$data_input, fact_var())))
  
} # Server










shinyApp(ui, server)



