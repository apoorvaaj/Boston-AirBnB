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
          plot(newlistings$longitude,newlistings$latitude,col="blue")
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
          plot(newlistings$longitude,newlistings$latitude,col="red")
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
          plot(newlistings$longitude,newlistings$latitude,col="green")
          #hist(newlistings$transformed_price)
        }
      }
    )
  }
)

