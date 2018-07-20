shinyServer(function(input, output, session){

    data = reactive({ # Check if species selection changes
      if(input$species == 'American Bittern'){ # Check which species
        if(is.null(american_bittern)){ # Has the dataframe been loaded from csv yet?
          american_bittern = read.csv('./data/american_bittern.csv', stringsAsFactors = FALSE) # If not, load data
        }
        return(american_bittern) # set temp data to currently selected species data
      }
      
      if(input$species == 'Bald Eagle'){ # Same as above
        if(is.null(bald_eagle)){
          bald_eagle = read.csv('./data/bald_eagle.csv', stringsAsFactors = FALSE)
        }
        return(bald_eagle)
      }
      
      if(input$species == 'Belted Kingfisher'){
        if(is.null(belted_kingfisher)){
          belted_kingfisher = read.csv('./data/belted_kingfisher.csv', stringsAsFactors = FALSE)
        }
        return(belted_kingfisher)
      }
      
      if(input$species == 'Blue Jay'){
        if(is.null(blue_jay)){
          blue_jay = read.csv('./data/blue_jay.csv', stringsAsFactors = FALSE)
        }
        return(blue_jay)
      }
      
      if(input$species == 'Golden Eagle'){
        if(is.null(golden_eagle)){
          golden_eagle = read.csv('./data/golden_eagle.csv', stringsAsFactors = FALSE)
        }
        return(golden_eagle)
      }
      
      if(input$species == 'Marsh Wren'){
        if(is.null(marsh_wren)){
          marsh_wren = read.csv('./data/marsh_wren.csv', stringsAsFactors = FALSE)
        }
        return(marsh_wren)
      }
      
      if(input$species == 'Peregrine Falcon'){
        if(is.null(peregrine_falcon)){
          peregrine_falcon = read.csv('./data/peregrine_falcon.csv', stringsAsFactors = FALSE)
        }
        return(peregrine_falcon)
      }
      
      if(input$species == 'Ruby-throated Hummingbird'){
        if(is.null(rt_hummingbird)){
          rt_hummingbird = read.csv('./data/rt_hummingbird.csv', stringsAsFactors = FALSE)
        }
        return(rt_hummingbird)
      }
      
      if(input$species == 'Short-eared Owl'){
        if(is.null(shorteared_owl)){
          shorteared_owl = read.csv('./data/shorteared_owl.csv', stringsAsFactors = FALSE)
        }
        return(shorteared_owl)
      }
      
      if(input$species == 'Spotted Sandpiper'){
        if(is.null(spotted_sandpiper)){
          spotted_sandpiper = read.csv('./data/spotted_sandpiper.csv', stringsAsFactors = FALSE)
        }
        return(spotted_sandpiper)
      }
    })
    
    image_src = reactive({ # Changes the image source to display different images depending on input$species
      if(input$species == 'American Bittern'){ # Check which species
        return('american_bittern.jpg') # return species specific image source
      }
      
      if(input$species == 'Bald Eagle'){ # Same as above
        return('bald_eagle.jpg')
      }
      
      if(input$species == 'Belted Kingfisher'){
        return('belted_kingfisher.jpg')
      }
      
      if(input$species == 'Blue Jay'){
        return('blue_jay.jpg')
      }
      
      if(input$species == 'Golden Eagle'){
        return('golden_eagle.jpg')
      }
      
      if(input$species == 'Marsh Wren'){
        return('marsh_wren.jpg')
      }
      
      if(input$species == 'Peregrine Falcon'){
        return('peregrine_falcon.jpg')
      }
      
      if(input$species == 'Ruby-throated Hummingbird'){
        return('rt_hummingbird.jpg')
      }
      
      if(input$species == 'Short-eared Owl'){
        return('shorteared_owl.jpg')
      }
      
      if(input$species == 'Spotted Sandpiper'){
        return('spotted_sandpiper.jpg')
      }
    })
    
    # render ggplot of observation data
    output$map = renderPlot({
      # Plot the ggplot map
      ggplot(to_mapdata(filter(data(), month(date) == input$month)), aes(x=long, y=lat, group=group)) +
        scale_fill_continuous(high = "#003666", low = "#cee8ff") +
        geom_polygon(aes(fill = legend)) + coord_map() +
        labs(title=paste(input$species, 'Range in', month.name[input$month]),x="",y="") + theme_void() +
        theme(legend.position="bottom") +
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    # render bird image
    output$image = renderUI({
      img(src = image_src(),
          width = 500,
          style ="display: block; margin-left: auto; margin-right: auto;")
    })
    
    # show data using DataTable
    output$table = DT::renderDataTable({
        datatable(to_mapdata(filter(data(), month(date) == input$month), choice=2), rownames=FALSE)
    })
    
    output$hist = renderPlot({
      # plot the histogram
      ggplot(to_histdata(filter(data(), month(date) == input$month)), aes(x = time)) +
      geom_histogram(aes(y=value), stat = 'identity', binwidth = 100) +
      scale_x_discrete(breaks=time_seq, labels=time_seq) + labs(x='Time', y='Observations') +
      ggtitle(paste('Optimal Viewing Times for', input$species, 'in', month.name[input$month])) +
      theme_economist()
    })
})