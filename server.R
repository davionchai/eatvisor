#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(jsonlite)
library(rjson)
library(httr)
library(uuid)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(breakDown)
library(readr)
library(viridis)
library (hrbrthemes)


shinyServer(function(input, output, session) {
    # Global var
    place_url <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
    subplace_url <- "https://maps.googleapis.com/maps/api/place/details/json"
    photo_url <- "https://maps.googleapis.com/maps/api/place/photo"
    place_key <- "" #Insert your google place API keys here
    # disable button at init
    shinyjs::hide("btnLocate")
    shinyjs::hide("btnMarkNearby")
    shinyjs::hide("startHeader3")
    shinyjs::hide("rules")
    shinyjs::hide("nearbyShow")
    shinyjs::hide("btnMark")
    shinyjs::hide("features")
    shinyjs::hide("syidaJSON")
    # hide("btnRetry")
    # Session var
    my_lat <- NULL
    my_long <- NULL
    # res_place <- NULL
    res_places <- NULL
    markNearby_activated <- FALSE
    # ----------------Testing Section Start----------------
    # query data
    # observeEvent(triggers$init, {
    #     req(input$btnGeo_lon)
    #     req(input$btnGeo_lat)
    # output$placeJSON <- renderText({
    #     res_place <<- getPlace(temp_place)
    #     })
    # })

    # # display photo
    # observeEvent(input$btnPhoto, {
    #     output$photo <- getPhoto(temp_photo)
    # })
    # ----------------Testing Section End----------------
    
    # ----------------Davion's Location Section Start----------------
    
    
    # get user location
    observeEvent(input$btnGeo, {
        output$mapRestaurant <- renderLeaflet({
            req(input$btnGeo_lon)
            req(input$btnGeo_lat)
            my_lat <<- input$btnGeo_lat
            my_long <<- input$btnGeo_lon
            icons <- awesomeIcons(
                icon='ios-close',
                iconColor="white",
                library='ion',
                markerColor="red"
            )
            leaflet() %>%
                addTiles() %>%
                setView(as.numeric(my_long), as.numeric(my_lat), zoom = 17) %>%
                addAwesomeMarkers(as.numeric(my_long), 
                                  as.numeric(my_lat), 
                                  icon=icons,
                                  labelOptions = labelOptions(noHide = T, direction = "bottom"),
                                  label = "Found you here!")
        })
        outputOptions(output, "mapRestaurant", suspendWhenHidden = FALSE)
        output$placesJSON <- renderText({
            res_places <<- getPlaces(input$btnGeo_lat, input$btnGeo_lon)
        })
        outputOptions(output, "placesJSON", suspendWhenHidden = FALSE)
        
        shinyjs::show("btnLocate")
        shinyjs::show("btnMarkNearby")
        shinyjs::show("startHeader3")
        shinyjs::show("rules")
        shinyjs::show("nearbyShow")
        shinyjs::show("btnMark")
        shinyjs::show("features")
        shinyjs::show("syidaJSON")
        # show("btnRetry")
        shinyjs::hide("btnGeo")
        shinyjs::hide("startHeader1")
        shinyjs::hide("startHeader2")
        # Sys.sleep(3)
        # triggers$init <<- TRUE
    })
    # relocate user markdown in map
    observeEvent(input$btnLocate, {
        leafletProxy("mapRestaurant", session) %>%
            setView(as.numeric(my_long), as.numeric(my_lat), zoom = 17)
    })
    # after getting nearby data, mark nearby data (deprecated)
    # observeEvent(input$btnMark, {
    #     res_places.json <- fromJSON(res_places)
    #     df_geo <- cbind.data.frame(name=res_places.json$results$name, lon=res_places.json$results$geometry$location$lng, lat=res_places.json$results$geometry$location$lat)
    #     print(df_geo[, c("name", "lat", "lon")])
    #     leafletProxy("mapRestaurant", session) %>%
    #     setView(as.numeric(my_long), as.numeric(my_lat), zoom = 17) %>%
    #     addMarkers(
    #         data=df_geo,
    #         lng=~lon,
    #         lat=~lat,
    #         label=~name)
    # })
    # Loop data and get the details
    observeEvent(input$btnMarkNearby, {
        if (markNearby_activated==FALSE) {
            markNearby_activated <<- TRUE
            res_places.json <- jsonlite::fromJSON(res_places)
            df_reference <- cbind.data.frame(place_id=res_places.json$results$place_id)
            for(i in rownames(df_reference)){
               place_data.json <- fromJSON(getPlace(df_reference[i, "place_id"]))
               place_data.display <- data.frame(cbind(name=place_data.json$result$name,
                                           address=place_data.json$result$vicinity,
                                           status=place_data.json$result$business_status,
                                           rating=place_data.json$result$rating,
                                           lon=place_data.json$result$geometry$location$lng, 
                                           lat=place_data.json$result$geometry$location$lat,
                                           url=place_data.json$result$url
                                           ))
               content <- paste(sep = "<br/>",
                                paste0("<b><a href='", place_data.display[1, "url"], "', target='_blank'>", place_data.display[1, "name"], "</a></b>"),
                                paste0("Operating Status: ", place_data.display[1, "status"]),
                                paste0("Rating: ", checkRating(place_data.display[1, "rating"])),
                                place_data.display[1, "address"]
               )
               leafletProxy("mapRestaurant", session) %>%
                   setView(as.numeric(my_long), as.numeric(my_lat), zoom = 17) %>%
                   addMarkers(
                       lng=as.numeric(place_data.display[1, "lon"]),
                       lat=as.numeric(place_data.display[1, "lat"]),
                       label=place_data.display[1, "name"],
                       popup=content)
            }
        }
        else {
            leafletProxy("mapRestaurant", session) %>%
                setView(as.numeric(my_long), as.numeric(my_lat), zoom = 17)
        }
    })
    # function check rating exist
    checkRating <- function(score) {
        if (is.null(score)) {
            return("No Rating")
        } 
        else {
            return(score)
        }
    }
    
    
    # ----------------Davion's Location Section End----------------
    
    # ----------------Sarmin's Nearby Section Start----------------
    
    
    observeEvent(input$nearbyShow, {
        
        # get nearby json
        res_places.json <- jsonlite::fromJSON(res_places)
        results <- res_places.json[['results']]
        filter_df <- data.frame(cbind(names=results$name,
                                      business_status=results$business_status,
                                      ratings=results$rating, 
                                      user_ratings_totals=results$user_ratings_total, 
                                      vicinitys=results$vicinity))
        colnames(filter_df) <- c('Name','Business_Status','Ratings','User_Ratings_Totals','Address')
        output$table <- renderDataTable(filter_df)
    })
    
    
    # ----------------Sarmin's Nearby Section End----------------
    
    # ----------------Aliah's 'Start' Section Start----------------
    
    
    show_result <- eventReactive(input$rules, {
        json_data <- jsonlite::fromJSON(res_places)
        json_data2 <- data.frame(cbind(name=json_data$result$name,
                                       address=json_data$result$vicinity,
                                       status=json_data$result$business_status,
                                       rating=json_data$result$rating,
                                       open = json_data$result$opening_hours))
        json_data2$open <- gsub("TRUE", "Open", json_data2$open)
        json_data2$rating <- gsub(0, "No Rating", json_data2$rating)
        json_data2$open <- gsub("FALSE", "Closed", json_data2$open)
        json_data2$open[is.na(json_data2$open)] <- "unavailable"
        df <- copy(json_data2)
        Sys.sleep(1.5)
        show_result <- df[sample(nrow(df),1),]
        return(show_result)
    }
    )
    
    
    output$location_output <- renderText({
        
        outcome_text <- HTML(paste(
            
            "<div class='card'>",
            "<img src='https://cdn.iconscout.com/icon/free/png-256/restaurant-1495593-1267764.png' alt='John' style='width:20%'>",
            "<br><br>",
            "<p class='title'><font size='15'><b>", show_result()[,"name"],"</b></font></p>",
            "<p><font size='5'>",show_result()[,"address"],"</font></p>",
            "<p><font size='5'> Store is ",show_result()[,"open"], "! </font></p>",
            "<p><font size='5'>",show_result()[,"rating"],"&nbsp;<img src='https://icons.iconarchive.com/icons/oxygen-icons.org/oxygen/256/Actions-rating-icon.png' alt='John' width='40' height='40'></font></p>",
            "</div>"
            
        )
        )
        
        return(outcome_text)
        
    }
    )
    
    
    # ----------------Aliah's 'Start' Section End----------------
    
    # ----------------Syida's 'Analysis' Section Start----------------
    
    
    observeEvent(input$btnMark, {
        res_places.json <- jsonlite::fromJSON(res_places)   
        df <- cbind.data.frame(name=res_places.json$result$name, 
                               #status=res_places.json$result$business_status, 
                               rating=res_places.json$result$rating, 
                               Open_now =res_places.json$result$opening_hours$open_now)
        
        
        if(input$features %in% c("Rating")){
            # keep only the open restaurants
            #df <- df %>% filter(Open_now == T)
            
            # Replacing rating values into categorical values
            df$rating[df$rating > 4.9] <-  "5 Stars"
            df$rating[df$rating >= 4 & df$rating < 5] <- "4 Stars"
            df$rating[df$rating >= 3 & df$rating < 4] <- "3 Stars"
            df$rating[df$rating >= 2 & df$rating < 3] <- "2 Stars"
            df$rating[df$rating <= 1] <- "1 Star"
            df$rating[df$rating  == "NA" ] <- "No Star"
            df <- df %>% group_by(rating) %>%
                summarise(Total = n())
            
            output$syidaJSON <- renderPlot({
                ggplot(df, aes(x = as.factor(rating), y = Total)) +
                    geom_bar(stat = "identity", fill = "blue") + 
                    #scale_fill_brewer(pallette="Dark2") +
                    geom_text(aes(label = Total),
                              hjust = 1.5,
                              color = "white",
                              size = 7) +
                    ggtitle("Restaurant Rating Analysis") +
                    theme(plot.title = element_text(hjust = 0.5))+
                    theme(axis.text = element_text(size=20),
                          axis.title = element_text(size = 20),
                          plot.title = element_text(size = 20),
                          axis.line = element_line(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank()) +
                    xlab("Rating Level") +
                    ylab("No. of Restaurant") +
                    coord_flip()
            })
        }
        
        
        
        if(input$features %in% c("Open Restaurant")){
            # keep only the open restaurants
            df <- df %>% filter(Open_now == T)
            
            #df <- df %>% filter(rating >= 4.0)
            
            output$syidaJSON <- renderPlot({
                ggplot(df, aes(x =name, y = rating)) +
                    geom_bar(aes(x = reorder(name,rating),y = rating), 
                             stat = "identity", fill = "Pink") +
                    #scale_fill_brewer(pallette="Dark2") +
                    geom_text(aes(label = rating),
                              hjust = 1.5,
                              color = "black",
                              size = 7) +
                    ggtitle("List of Restaurant") +
                    theme(plot.title = element_text(hjust = 0.5))+
                    theme(axis.text = element_text(size=15),
                          axis.title = element_text(size = 15),
                          plot.title = element_text(size = 15),
                          axis.line = element_line(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank()) +
                    xlab("Restaurants Name ") +
                    ylab("Rating Level") +
                    coord_flip()
                
            })
        }
        
        if(input$features %in% c("5 Stars Restaurants")){
            # keep only the open restaurants
            df <- df %>% filter(rating > 4.9)
            
            output$syidaJSON <- renderPlot({
                ggplot(df, aes(x =name, y = rating)) +
                    geom_bar(stat = "identity", fill = "Purple") +
                    geom_text(aes(label = rating),
                              hjust = 1.5,
                              color = "white",
                              size = 7) +
                    ggtitle("List of Restaurant") +
                    theme(plot.title = element_text(hjust = 0.5))+
                    theme(axis.text = element_text(size=15),
                          axis.title = element_text(size = 15),
                          plot.title = element_text(size = 15),
                          axis.line = element_line(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank()) +
                    xlab("Restaurants Name ") +
                    ylab("Rating Level") +
                    coord_flip()
                
            })
        }
        if(input$features %in% c("4 Stars Restaurants")){
            # keep only the open restaurants
            df <- df %>% filter(rating >= 4 & df$rating < 5)
            
            output$syidaJSON <- renderPlot({
                ggplot(df, aes(x = name, y = rating)) +
                    geom_bar(aes(x = reorder(name,rating),y = rating), 
                             stat = "identity", fill = "Purple") +
                    geom_text(aes(label = rating),
                              hjust = 1.5,
                              color = "white",
                              size = 7) +
                    ggtitle("List of Restaurant") +
                    theme(plot.title = element_text(hjust = 0.5))+
                    theme(axis.text = element_text(size=15),
                          axis.title = element_text(size = 15),
                          plot.title = element_text(size = 15),
                          axis.line = element_line(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank()) +
                    xlab("Restaurants Name ") +
                    ylab("Rating Level") +
                    coord_flip()
                
            })
        }
        if(input$features %in% c("3 Stars Restaurants")){
            # keep only the open restaurants
            df <- df %>% filter(rating >= 3 & df$rating < 4)
            
            output$syidaJSON <- renderPlot({
                ggplot(df, aes(x = name, y = rating)) +
                    geom_bar(aes(x = reorder(name,rating),y = rating), 
                             stat = "identity", fill = "Purple") +
                    geom_text(aes(label = rating),
                              hjust = 1.5,
                              color = "white",
                              size = 7) +
                    ggtitle("List of Restaurant") +
                    theme(plot.title = element_text(hjust = 0.5))+
                    theme(axis.text = element_text(size=15),
                          axis.title = element_text(size = 15),
                          plot.title = element_text(size = 15),
                          axis.line = element_line(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank()) +
                    xlab("Restaurants Name ") +
                    ylab("Rating Level") +
                    coord_flip()
                
            })
        }
        if(input$features %in% c("2 Stars Restaurants")){
            # keep only the open restaurants
            df <- df %>% filter(rating >= 2 & df$rating < 3)
            
            output$syidaJSON <- renderPlot({
                ggplot(df, aes(x = name, y = rating)) +
                    geom_bar(aes(x = reorder(name,rating),y = rating), 
                             stat = "identity", fill = "Purple") +
                    geom_text(aes(label = rating),
                              hjust = 1.5,
                              color = "white",
                              size = 7) +
                    ggtitle("List of Restaurant") +
                    theme(plot.title = element_text(hjust = 0.5))+
                    theme(axis.text = element_text(size=15),
                          axis.title = element_text(size = 15),
                          plot.title = element_text(size = 15),
                          axis.line = element_line(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank()) +
                    xlab("Restaurants Name ") +
                    ylab("Rating Level") +
                    coord_flip()
                
            })
        }
        if(input$features %in% c("1 Stars Restaurants")){
            # keep only the open restaurants
            df <- df %>% filter(rating >= 1 & df$rating < 2)
            
            output$syidaJSON <- renderPlot({
                ggplot(df, aes(x = name, y = rating)) +
                    geom_bar(aes(x = reorder(name,rating),y = rating), 
                             stat = "identity", fill = "Purple") +
                    geom_text(aes(label = rating),
                              hjust = 1.5,
                              color = "white",
                              size = 7) +
                    ggtitle("List of Restaurant") +
                    theme(plot.title = element_text(hjust = 0.5))+
                    theme(axis.text = element_text(size=15),
                          axis.title = element_text(size = 15),
                          plot.title = element_text(size = 15),
                          axis.line = element_line(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank()) +
                    xlab("Restaurants Name ") +
                    ylab("Rating Level") +
                    coord_flip()
                
            })
        }
        if(input$features %in% c("1 or No Star Restaurants")){
            # keep only the open restaurants
            df <- df %>% filter(df$rating <= 1)
            
            output$syidaJSON <- renderPlot({
                ggplot(df, aes(x = name, y = rating)) +
                    geom_bar(aes(x = reorder(name,rating),y = rating), 
                             stat = "identity", fill = "Purple") +
                    geom_text(aes(label = rating),
                              hjust = 1.5,
                              color = "white",
                              size = 7) +
                    ggtitle("List of Restaurant") +
                    theme(plot.title = element_text(hjust = 0.5))+
                    theme(axis.text = element_text(size=15),
                          axis.title = element_text(size = 15),
                          plot.title = element_text(size = 15),
                          axis.line = element_line(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank()) +
                    xlab("Restaurants Name ") +
                    ylab("Rating Level") +
                    coord_flip()
                
            })
        }
    })
    
    
    # ----------------Syida's 'Analysis' Section End----------------
    
    # ----------------Worker Section Start----------------
    # worker unit to query nearby places
    getPlaces <- function(my_lat, my_long) {
        res <- httr::GET(url=place_url,
                         query=list(
                             location=paste0(my_lat, ",", my_long),
                             # radius="4000",
                             rankby="distance",
                             type="restaurant",
                             # keyword="chicken",
                             key=place_key
                         )
        )
        # print(paste0("Nearby query: ", res$status_code))
        PlacesDataframe.content <- content(res, "text", encoding="UTF-8")
        return(PlacesDataframe.content)
    }
    # worker unit to query place data based on place id
    getPlace <- function(place_id) {
        res <- httr::GET(url=subplace_url,
                         query=list(
                             place_id=place_id,
                             key=place_key
                         ))
        # print(paste0("Place query: ", res$status_code))
        PlaceDataframe.content <- content(res, "text", encoding="UTF-8")
        return(PlaceDataframe.content)
    }
    # worker unit to query place photo based on photo reference (only applicable with dynamic server)
    getPhoto <- function(photo_ref) {
        res <- httr::GET(url=photo_url,
                         query=list(
                             photoreference=photo_ref,
                             maxwidth="400",
                             key=place_key
                         ))
        # print(paste0("Photo query: ", res$status_code))
        raw_photo <- content(res, 'raw')
        # set width and height
        width <- 400
        height <- 400
        # generate random uuid for naming
        photo_uuid <- uuid::UUIDgenerate()
        # create temp folder to host image
        temp_png <- paste0("www/temp/", photo_uuid, ".png")
        writeBin(raw_photo, temp_png)
        return(renderImage({
            list(
                src=temp_png,
                contentType = "image/png",
                width=width,
                height=height,
                alt="img alternative text"
                )}, 
            deleteFile=TRUE)
        )
    }
    # ----------------Worker Section End----------------
})
