library(shiny)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(rgdal)
library(maptools)
library(rgeos)
library(sp)
library(plotly)
library(data.table)
library(htmltools)
library(DT)
library(shinyjs)


ui<-fluidPage(
    tags$head(HTML("<title>FAF Comparison </title>")),
    useShinyjs(),
    br(),
    span(style = "font-weight: 600; font-size: 25px; width: 100%;
         color: #022DB7;", "Freight Analysis Framework FAF4 vs. FAF5, Year 2017"),
    
    br(),br(),
    fluidRow(
      column(8, leafletOutput("Zone", height = "550px")%>% withSpinner(color="#0dc5c1")),
      column(4, 
             span("Select "), span( style="color:green", "Origin"), span(" and "), span( style="color:red", "Destination"), 
             span(" from the map:"),
             br(),br(),
             htmlOutput("od_info")%>% withSpinner(color="#0dc5c1"),
             hr(),
             htmlOutput("od_total")%>% withSpinner(color="#0dc5c1"),
             hr(),
             htmlOutput("od_total_5")%>% withSpinner(color="#0dc5c1")
             
      )
    ),
    br(),br(),
    fluidRow(
      column(9, div(DT::dataTableOutput("od_vol"),  width = "100%", style = "font-size:100%"))
    ),
    fluidRow(
      column(5, plotlyOutput("od_ton_chart", width = "100%", height = "350px")%>% withSpinner(color="#0dc5c1")),
      column(4, plotlyOutput("od_ton_pie", width = "100%", height = "250px")%>% withSpinner(color="#0dc5c1")),
      column(3, plotlyOutput("od_ton_pie_5", width = "100%", height = "250px")%>% withSpinner(color="#0dc5c1"))
    ),
    hr(),
    fluidRow(
      column(5, plotlyOutput("od_value_chart", width = "100%", height = "350px")%>% withSpinner(color="#0dc5c1")),
      column(4, plotlyOutput("od_value_pie", width = "100%", height = "250px")%>% withSpinner(color="#0dc5c1")),
      column(3, plotlyOutput("od_value_pie_5", width = "100%", height = "250px")%>% withSpinner(color="#0dc5c1"))
    )
               
  )
