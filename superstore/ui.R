# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(shinythemes)
library(readxl)
library(writexl)
library(tidyverse)
library(factoextra)
library(cluster)
library(data.table)
library(scales)

css <- '.nav-tabs>li>a {
  font-family: "Lucida Sans", sans-serif;
  color: orange;font-size: large;
}'


shinyUI(fluidPage(theme = shinytheme("united"),
                  tags$head(tags$style(HTML(css))),
                  h1("Super Store Clustering Dashboard"),
                  tabsetPanel(
                      id="inTabset",
                      tabPanel("IMPORT DATA",
                               mainPanel(
                                   hr(),
                                   # h4("    "),
                                   fileInput('file1', 'Choose xlsx/xls file',
                                             accept = c(".xlsx",".xls",".txt")),
                                   
                                   DT::dataTableOutput('contents'),
                                   hr(),
                                   actionButton("switch_tab", "Continue Analysis")),
                               
                      )
                      ,
                      tabPanel("INPUT DATASET REVIEW",
                               
                               mainPanel(
                                   width = 12, hr(),
                                   fluidRow(
                                       column(6,selectizeInput("category","Category",
                                                               c("Furniture","Technology","Office Supplies"),
                                                               multiple = TRUE)),
                                       column(6,selectizeInput("segment","Segment",
                                                               c("Consumer","Corporate","Home Office"),
                                                               multiple = TRUE ))),
                                   fluidRow(column(6,uiOutput("start_year_filter"))),
                                   plotOutput("plot"),
                                   column(12,offset = 8,downloadButton("download", "Download dataset")),
                                   hr(),
                                   hr(),
                                   actionButton("switch_tab2", "Start Analysis") 
                                   
                               )   
                               
                               
                      ),
                      tabPanel("K-MEANS CLUSTERING ANALYSIS",
                               
                               mainPanel(width = 12,
                                         hr(),
                                         sliderInput("nclus",
                                                     "Number of Clusters:",
                                                     min = 1,
                                                     max = 8,
                                                     value = 3),
                                         plotOutput("kmeans_plot"),
                                         hr(),
                                         verbatimTextOutput("text"),
                                         fluidRow(column(12,h3("Summary"))),
                                         tableOutput('summary')
                                         
                               )
                               
                      )
                      
                  )
)
)
