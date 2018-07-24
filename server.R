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
      
      if(input$species == 'Pileated Woodpecker'){
        if(is.null(pileated_woodpecker)){
          pileated_woodpecker = read.csv('./data/pileated_woodpecker.csv', stringsAsFactors = FALSE)
        }
        return(pileated_woodpecker)
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
      
      if(input$species == 'Pileated Woodpecker'){
        return('pileated_woodpecker.jpg')
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
    
    # The following are used for intro page text display
    output$intro_header = renderUI({
      h1('Welcome to my Bird Range Shiny App!')
    })
    
    output$intro_author = renderUI({
      h4('Coded by: Alex Baransky <alex.baransky@gmail.com>')
    })
    
    output$intro_body1 = renderUI({
      p('This app uses subsets of the eBird database to produce a density map of bird sightings by county.
        My aim was to create a tool that could display a visual representation of a species\' range
        and how it changes over the course of a year, similar to this range map for the Belted Kingfisher:')
    })
    
    output$intro_body2 = renderUI({
      p('The tabs on the left allow the inspection of different aspects of the data. The "Map" tab shows a 
        map of the United States with each county colored to its corresponding observation density. The "Time Histogram"
        tab displays a histogram of sighting frequency by time of day. The "Data Table" tab shows the bird observation data
        grouped by county and by time.')
    })
    
    output$intro_body3 = renderUI({
      p('To start, select a species from the dropdown menu. The map may take a few seconds to load. Check the box under species
        select if you only want to see the range of sightings during that species\' breeding season. Using the slider, you can
        select a range of months to ivestigate, or you can select a single month by moving both slider endpoints to the same month.
        You can also press the "play" button under the slider to cycle through the remaining months of the year and get an idea of
        the change in observation range of the species. Each map will take a few seconds to load.')
    })
    
    output$intro_body4 = renderUI({
      p('I hope you enjoy using my app!')
    })
    
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
        ggtitle(paste('Observation Time Frequency for', input$species, title_months(input$month))) +
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