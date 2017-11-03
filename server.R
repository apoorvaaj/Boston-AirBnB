library(shiny)
library(ggmap)
library(ggplot2)
#library("tm")
#library("SnowballC")
#library("wordcloud")
#library("RColorBrewer")

shinyServer(
  function(input,output,session)
  {
    output$listingplot <- renderPlot(
      {
        if(input$Attributes == "Location")
        {
          newlistings <- subset(listings,
                                listings$neighbourhood_cleansed == input$InputLocation,
                                select = c("latitude",
                                           "longitude",
                                           "property_type",
                                           "transformed_price",
                                           "neighbourhood_cleansed"))
          NLdataframe <- data.frame(newlistings)
          bostonmap <- get_map(location = c(lon = -71.0589, 
                                            lat = 42.3601), 
                               zoom = 15, 
                               maptype = "roadmap",
                               source = "google")
          displaymap <- ggmap(bostonmap,extent = "normal", maprange = TRUE) + geom_point(data= NLdataframe,
                                                      aes(x=longitude, 
                                                          y=latitude),
                                                      colour = "blue")
          displaymap
          #plot(newlistings$longitude,newlistings$latitude,col="blue")
          #hist(newlistings$transformed_price)
        }
        else if(input$Attributes == "Property Type")
        {
          newlistings <- subset(listings,
                                listings$property_type == input$InputPropertyType,
                                select = c("latitude",
                                           "longitude",
                                           "property_type",
                                           "transformed_price",
                                           "neighbourhood_cleansed"))
          NLdataframe <- data.frame(newlistings)
          bostonmap <- get_map(location = c(lon = -71.0589, 
                                            lat = 42.3601), 
                               zoom = 15, 
                               maptype = "roadmap",
                               source = "google")
          displaymap <- ggmap(bostonmap,extent = "normal", maprange = TRUE) + geom_point(data= NLdataframe,
                                                      aes(x=longitude, 
                                                          y=latitude),
                                                      colour = "red")
          displaymap
          #plot(newlistings$longitude,newlistings$latitude,col="red")
          #hist(newlistings$transformed_price)
        }
        else if(input$Attributes == "Rate")
        {
          newlistings <- subset(listings,
                                listings$transformed_price <= input$InputRate,
                                select = c("latitude",
                                           "longitude",
                                           "property_type",
                                           "transformed_price",
                                           "neighbourhood_cleansed"))
          NLdataframe <- data.frame(newlistings)
          bostonmap <- get_map(location = c(lon = -71.0589, 
                                            lat = 42.3601), 
                               zoom = 15, 
                               maptype = "roadmap",
                               source = "google")
          displaymap <- ggmap(bostonmap, extent = "normal", maprange = TRUE) + geom_point(data= NLdataframe,
                                                      aes(x=longitude, 
                                                          y=latitude),
                                                      colour = "green")
          displaymap
          #plot(newlistings$longitude,newlistings$latitude,col="green")
          #hist(newlistings$transformed_price)
        }
      }
    )
  output$summarytable <- renderPrint(
    {
      if(input$Attributes == "Location")
      {
        newlistings <- subset(listings,
                              listings$neighbourhood_cleansed == input$InputLocation,
                              select = c("transformed_price",
                                         "bedrooms",
                                         "bathrooms",
                                         "number_of_reviews"))
        #plot(newlistings$longitude,newlistings$latitude,col="blue")
        summary(newlistings)
      }
      else if(input$Attributes == "Property Type")
      {
        newlistings <- subset(listings,
                              listings$property_type == input$InputPropertyType,
                              select = c("transformed_price",
                                         "bedrooms",
                                         "bathrooms",
                                         "number_of_reviews"))
        #plot(newlistings$l ongitude,newlistings$latitude,col="red")
        summary(newlistings)
      }
      else if(input$Attributes == "Rate")
      {
        newlistings <- subset(listings,
                              listings$transformed_price <= input$InputRate,
                              select = c("transformed_price",
                                         "bedrooms",
                                         "bathrooms",
                                         "number_of_reviews"))
        #plot(newlistings$longitude,newlistings$latitude,col="green")
        summary(newlistings)
      }
    }
  )
  output$ReviewOut <- renderPrint(
  {
    if(input$Attributes == "Location")
    {
      reviewdf <- subset(listings,
                            listings$neighbourhood_cleansed == input$InputLocation,
                            select = c("summary"))
      data.frame(reviewdf)
    }
    else if(input$Attributes == "Property Type")
    {
      reviewdf <- subset(listings,
                            listings$property_type == input$InputPropertyType,
                            select = c("summary"))
      data.frame(reviewdf)
    }
    else if(input$Attributes == "Rate")
    {
      reviewdf <- subset(listings,
                            listings$transformed_price <= input$InputRate,
                            select = c("summary"))
      data.frame(reviewdf)
    }
  }
)
  }
)

