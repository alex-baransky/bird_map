shinyServer(function(input, output, session){
    
    # Check if species selection changes
    current_data = reactive({
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
    
    # Changes the image source to display different images depending on input$species
    image_src = reactive({ 
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
    
    # If breed checkbox is checked, uncheck when selecting a different species
    observeEvent(input$species, {
      if(input$breed){
        updateCheckboxInput(session, "breed", value = FALSE)
      }
    })
    
    # Text for play button help explanation and data/images sources
    output$menu_text = renderUI({
      HTML('<br>Move both slider endpoints to January (1) and press \
      "play" to show an animated series of monthly bird sightings for 2016.<br/>\
      <br>Image source: Audobon Guide to North American Birds website. Observation data from the 2017 \
      eBird Basic Dataset. Breeding season range estimates from The Cornell Lab of Ornithology Birds \
      of North America website.<br/>')})
    
    # render ggplot of observation data
    output$map = renderPlot({
      ggplot(to_mapdata(filter_months(current_data(), input$month)), aes(x=long, y=lat, group=group)) +
        scale_fill_continuous(high = "#003666", low = "#cee8ff", name = "log(Observations)") +
        geom_polygon(aes(fill = legend)) + coord_map() +
        labs(title = paste(input$species, 'Sightings by County', title_months(input$month)), x = "", y = "") + theme_void() +
        theme(legend.position="bottom") +
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    # render bird image
    output$image = renderUI({
      img(src = image_src(),
          width = 500,
          style ="display: block; margin-left: auto; margin-right: auto;")
    })
    
    # show county observations data as table
    output$county_table = DT::renderDataTable({
        datatable(to_mapdata(filter_months(current_data(), input$month), choice=2), rownames=FALSE)
    })
    
    # show time observations data as table
    output$time_table = DT::renderDataTable({
      datatable(to_histdata(filter_months(current_data(), input$month), choice=2), rownames=FALSE)
    })
    
    output$hist = renderPlot({
      # plot the histogram
      suppressWarnings( # ignore warning messages about unknown parameters (binwidth, bins, pad) because x-axis is categorical and not continuous
        ggplot(to_histdata(filter_months(current_data(), input$month)), aes(x = time)) +
        geom_histogram(aes(y = value), stat = 'identity') +
        scale_x_discrete(breaks = time_seq, labels = time_seq) + labs(x = 'Time', y = 'Observations') +
        ggtitle(paste('Optimal Viewing Times for', input$species, title_months(input$month))) +
        theme_economist()
      )
    })
    
    # If checkbox input is checked, set month range equal to the breeding season estimate for the selected species
    observeEvent(input$breed,{
      if (input$breed){
        updateNumericInput(session, "month", value = breed_range(input$species))
      }
      else{
        updateNumericInput(session, "month", value = c(1,12))
      }
    })
})