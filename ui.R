#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(plyr)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(pivottabler)
library(readr)


shinyUI(dashboardPage(
  title="Solving your first world problem - What to Eat?!",
  skin="black", 
  dashboardHeader(title="My Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName="home", icon=icon("home")),
      menuItem("Getting Start", tabName="start", icon=icon("plus-square")),
      menuItem("Nearby Restaurants", tabName="query", icon=icon("compass")),
      menuItem("Locations", tabName="map", icon=icon("map-marker-alt")),
      menuItem("Charts", tabName="analysis", icon=icon("chart-bar")),
      menuItem("Source", tabName="data", icon=icon("file"))
      )
    ),
  dashboardBody(
    tags$head(tags$style(HTML("
                              #btnGeo{
                              background: none;
                              color: #04009A;
                              border:2px solid;
                              padding: 1em 2em;
                              font-size: 1em;
                              transition: all 0.25s;
                                }

                              #btnGeo:hover{
                                  border-color: #ffa260;
                                  color: white;
                                  box-shadow: 0 0.5em 0.5em -0.4em #f1ff5c;
                                  transform: translateY(-0.25em);
                                  cursor: pointer;
                              }"
                              ))),
    shinyjs::useShinyjs(),
    tabItems(
      
      # Index
      tabItem(tabName= "home",
              fluidPage(
                titlePanel(h1(strong("Brought to you by the 3R"), align="center")),
                helpText(h3(strong("Randomized Restaurant Recommender"), align="center")),
                ),
              fluidRow(
                img(src="banner.png", width="80%", style="display: block; margin-left: auto; margin-right: auto;")
                )
              ),
      # Getting Start (Main Function: Aliah) (Master button: Davion)
      tabItem(tabName= "start",
              fluidPage(
                titlePanel(h1(id="startHeader1", strong("Let's Get Started!"), align="center")),
                helpText(h3(id="startHeader2", strong("Click the below button to start finding restaurants !"), align="center"))
                ),
              br(),
              fluidRow(
                fluidRow( 
                         align="center", 
                         geoloc::button_geoloc("btnGeo", 
                                               "Get My Location")
                ), # Master Trigger
                br(),
                headerPanel(h1("Pick Me A Restaurant !", id="startHeader3", align = "center")),
                br(),br(),br(),br(),br(),br(),
                
                fluidRow(                        
                  column(12, align = "center",
                         actionBttn(
                           inputId = "rules",
                           label = "Recommend Me!",
                           style = "jelly",
                           color = "warning",
                           size = "lg"
                         )
                  )
                ),
                br(),br(),br(),br(),br(),br(),
                fluidRow(
                  column(1), 
                  column(10, align = "center",
                         withSpinner(htmlOutput("location_output"), image = "https://u01.appmifile.com/images/2019/09/10/b3788a8e-24d2-41b3-91c4-131968dab219.gif",  
                                     image.height = "400px", image.width = "400px")
              )))
              ),
      # Nearby Restaurant (Sarmin)
      tabItem(tabName= "query",
              fluidPage(
                titlePanel(h1(strong("Nearby Restaurants"), align="center")),
                helpText(h3(strong("Here are the options !"), align="center"))
                ),
              fluidRow(
                img(src="nearby_banner.png", width="100%")
              ),
              fluidRow(column(12, align="left",
                br(),
                actionButton("nearbyShow", label="Show Me the List !"),
                br(),br(),
                column(8,dataTableOutput('table')),
                br()
              ))
              ),
      # Locations (Davion)
      tabItem(tabName= "map",
              fluidPage(
                titlePanel(h1(strong("The Treasure Map!"), align="center")),
                helpText(h3(strong("Let the hunts begin !"), align="center"))
                ),
              fluidRow(
                column(12,
                       br(),
                       actionButton("btnLocate", label="Locate Me!"),
                       actionButton("btnMarkNearby", label="Show Me Around!"),
                       br(), br(),
                       leafletOutput("mapRestaurant"))
              )
              ),
      # Results Analysis (Syida)
      tabItem(tabName= "analysis",
              fluidPage(
                titlePanel(h1(strong("Your Personal Food Analysis"), align="center")),
                helpText(h3(strong("What sort of restaurants are surrounding you !"), align="center"))
                ),
              br(),
              fluidRow(
                column(8, actionButton("btnMark", label="Get Data")),
                br(),br(),
                column(width = 5, selectInput("features","Features", choices = c("Rating","Open Restaurant","5 Stars Restaurants", "4 Stars Restaurants","3 Stars Restaurants","2 Stars Restaurants","1 or No Star Restaurants"))),
                #plotlyOutput("plot"),
                br(),br(),
                plotOutput('syidaJSON')#,
                #verbatimTextOutput("syidaJSON")
              )
              ),
      # Data (Davion)
      tabItem(tabName= "data",
              fluidPage(
                titlePanel(h1(strong("The Magical JSON Data Source"), align="left")),
                helpText(h3(strong("This is the data based on where you are located !"), align="left"))
                ),
              fluidRow(column(12, 
                              align="left",
                br(),
                p("This web application (built by rshiny) get its location from the", 
                  a(href="https://github.com/ColinFay/geoloc", "geoloc"),
                  " package by Colin Fay."), 
                p("Based on the detected geocode from your device, this application will use ",
                  a(href="https://developers.google.com/maps/documentation/places/web-service/overview", "Google Places API"), 
                  " services to check for the top 20 nearest restaurants around you!"),
                br(),
                p("Here's the returned JSON result from Google Places API.")
              )),
              fluidRow(
                verbatimTextOutput("placesJSON"),
                # verbatimTextOutput("placeJSON") # temporarily show place api
              )
              )
      )
  )
))
