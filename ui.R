# ui <- fluidPage(
#   # App title ----
#   titlePanel("3DTS"),
#   
#   # Sidebar layout with input and output definitions ----
#   sidebarLayout(
#     
#     # Sidebar panel for inputs ----
#     sidebarPanel(
#       
#       # Input: Drug 1
#       textInput("drug1", h3("Drug 1"), 
#                 value = "tocilizumab"),
#       textInput("drug2", h3("Drug 2"), 
#                 value = "atorvastatin")
#       # actionButton("goButton", "Check")
#       
#     ),
#     
#     # Main panel for displaying outputs ----
#     mainPanel(
#       
#       # Output: Histogram ----
#       # plotOutput(outputId = "networkPlot")
#       forceNetworkOutput("plot1", width = "1200px",height = "800px"),
#       tableOutput('table')
#       
#       
#     )
#   )
# )
library(shiny)
library(igraph)
library(networkD3)
library(shinydashboard)
library(shiny)
library(DT)
library(viridis)
library(shinyjs)
library(V8)
ui <- dashboardPage(
  dashboardHeader(
    title = "3-Drug Safety Tool",
    # titleWidth = 200,
    disable = F,
   dropdownMenu(type = "messages",badgeStatus = NULL,headerText = " ",
                 messageItem(href = "https://pharmacoepidemiology.ethz.ch/",
                   from = "Contact",
                   message = div(style = "padding-right:70px;align: center;",HTML(paste0(
                     "<br>",
                     "Pharmacoepidemiology Group",
                     "<br>",
                     "ETH Zurich.",
                     "<br>",
                     "<br>",
                     "For further information contact",
                     "<br>",
                     "Adrian Martinez de la Torre:",
                     "<br>",
                     "adrian.martinez@pharma.ethz.ch"
                     
                   )))
                 ))
      
  ),
  dashboardSidebar(
  
    div(style = "text-align: justify; width:95%; padding-left: 20px;",HTML(paste0(
      "<br>",
      "   The 3DST is an interactive platform for 3-way drug-drug interactions.",
      "<br>",
      "<br>",
      "The user must specify two drugs of interest and the tool will display the drugs that interact with each drug of interest.",
      "<br>",
      "<br>"
    ))),
   HTML(paste0(
    "<br>",
    "<a href='https://pharmacoepidemiology.ethz.ch/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='ethlogo.png' width = '186'></a>"
    # "<font color=\"#D5C0A1\"><b>",
    # "<font size= 0.01px><b>",
    # "Developed by the Pharmacoepidemiology Group of ETH Zürich ©",
    # "<font color=\"#FFFFFF\"><b>",
    # "<font size= 2pt><b>",
    # "<br>",
    # "<br>"
      )),
   div(style = "padding-left:25px;",HTML(paste0(
     "<font color=\"#D5C0A1\"><b>",
     "<font size= 0.01px><b>",
     "Developed by the Pharmacoepidemiology Group of ETH Zürich ©",
     "<font color=\"#FFFFFF\"><b>",
     "<font size= 2pt><b>",
     "<br>",
     "<br>"
   )
   )
   )
   ,
    # actionButton("instructions", "Instructions", icon = icon("info-circle")),
    div(style="width:100%;text-align: center; margin-left: 40px;",actionButton("instructions", "Instructions", icon = icon("info-circle"))),
    sidebarMenu(
      textInput("drug1", h3("Drug 1"), 
                value = "lopinavir-ritonavir,kaletra",
                placeholder = "Enter Drug 1"
                    
                # value = "azithromycin"
      ),
      textInput("drug2", h3("Drug 2"), 
                # value = "hydroxychloroquine"
                value = "tocilizumab",
                placeholder = "Enter Drug 2"
                # value = "simvastatin"
      ),
      div(style="width:100%;text-align: center; margin-left: 65px;", actionButton("go", "Search")),
      # radioButtons("filter_interaction", h3("Interactions Filter"),
      #              choices = list("Only interactions with Drug 1" = 1, 
      #                             "Only interactions with Drug 2" = 2,
      #                             "All interactions" = 3),selected = 3),
      menuItem("Filters",
               # radioButtons("both_interaction", h3("Show Drugs that interact with"),
               #              choices = list(
               #                "One Drug of interest" = "all_drugs",
               #                "Both drugs of interest" = "interest")
               #              ,selected = "all_drugs"),
      # radioButtons("drugs_covid", h3("COVID-19"),
      #              choices = list(
      #                "All Drugs" = "all",
      #                "Only drugs that are likely to be used in COVID-19" = "covid")
      #              ,selected = "all"),
      radioButtons("var_color", h3("Color of nodes"),
                   choices = list(
                     "ATC Code" = "Level1",
                     "Degree of Interaction" = "interaction")
                   ,selected = "interaction"),
      radioButtons("colors_covid", h3("Color of links"),
                   choices = list(
                     "No color" = "all_same",
                     "Major interactions in red" = "red_col")
                   ,selected = "all_same")

      # actionButton("reset_button", "Reset Page")
      )
    )
  ),
  dashboardBody(    tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tags$footer("Copyright 2020. All rights reserved. Pharmacoepidemiology group, ETH Zurich.©",align = "center",
              style = "
position:absolute;
text-align:center;
padding:0px;
bottom: 0;
margin:0px;
overflow-x: hidden;
width:100%;
height:15px; /* Height of the footer */
color: white;
background-color: grey;
z-index: 1000;
font-size:x-small;"
  ),
    tabsetPanel(
      id = "tabs",
      tabPanel(
        title = "Interaction Plot",
        value = "page1",
        fluidRow(
          valueBoxOutput("drug1_interactions"),
          valueBoxOutput("drug2_interactions"),
          valueBoxOutput("both_interactions")
        ),
        fluidRow(),
        fluidRow(
          column(
            width = 7,
            forceNetworkOutput("plot1", width = "1400px",height = "1400px")),
          column(width = 5,
                 # h2(div(style = "color:#1F407A",HTML(paste0("Medications that interact with both drugs of interest")))),
                 span(textOutput("text_title"), style="color:#dd4b39;"),
                 
            DTOutput("interact_with_2_table")
            # tableOutput('table')
          )
        )
      ),
      tabPanel(
        title = "Interaction Table",
        value = "page2",
        fluidRow(
        column(
        width = 8,
        div(  DTOutput('table'), style = "font-size: 85%; width: 85%"),
        downloadButton("downloadData", "Download")
        # div(  tableOutput('table'), style = "font-size: 30%; width: 35%")
        )
        )

      ),
      tabPanel(
        title = "COVID-19",
        value = "page3",
        fluidRow(
          column(
            width = 8,
            div(  DTOutput('covid_table'), style = "font-size: 85%; width: 85%"),
            downloadButton("downloadCovidData", "Download")
            # div(  tableOutput('table'), style = "font-size: 30%; width: 35%")
          )
        )
        
      )
    )
  )
)