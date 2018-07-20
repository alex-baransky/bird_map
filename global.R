library(DT)
library(shiny)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(data.table)

# intitialize all species dataset'holders'
american_bittern = NULL
bald_eagle = NULL
belted_kingfisher = NULL
blue_jay = NULL
golden_eagle = NULL
marsh_wren = NULL
peregrine_falcon = NULL
rt_hummingbird = NULL
shorteared_owl = NULL
spotted_sandpiper = NULL

# make x axis labels, variable for time histogram
time_seq = c('00:00', '02:00', '04:00', '06:00', '08:00', '10:00', '12:00', '14:00', '16:00', '18:00', '20:00', '22:00')

# create a dataframe of all the times (HH:MM) in a day and set values for each equal to 0
h = c(c('00', '01', '02', '03', '04', '05', '06', '07', '08', '09'), as.character(10:23))
m = c(c('00', '01', '02', '03', '04', '05', '06', '07', '08', '09'), as.character(10:59))
time_df = as.vector(sapply(h, function(x) sapply(m, function(y) paste0(x, ':', y)))) %>%
  data.frame(0)
colnames(time_df) = c('time', 'value')
time_df$time = as.character(time_df$time)

# selectizeInput species selections
species = c('American Bittern', 'Bald Eagle', 'Belted Kingfisher', 'Blue Jay',
            'Golden Eagle', 'Marsh Wren', 'Peregrine Falcon', 'Ruby-throated Hummingbird',
            'Short-eared Owl', 'Spotted Sandpiper')
species = species[order(species)]

# function that takes in a dataset and returns observation values grouped by location
to_mapdata = function(data, choice=1){ # Option to choose return of map.df data (1) or obs data (2)
  
  # data.table use for matching up observations from species datasets (on state,county)
  map.county = data.table(map_data('county')) %>%
    mutate(location = paste(region, subregion, sep = ','))
  
  # group by location and take log of value
  obs = data %>%
    group_by(location) %>% 
    summarise(observations = n(), value = log(n())) %>% 
    data.table()
  
  # left join with total map dataframe (map.county) and replace NAs (counties with no observations) with 0s
  map.df = left_join(map.county, obs, by = 'location') %>%
    mutate(value = ifelse(is.na(value), 0, value))
  
  # make a legend column that splits the values into groups for discrete color mapping 
  map.df$legend = cut(map.df$value, breaks = 10)
  levels(map.df$legend) = 1:10
  map.df$legend = as.numeric(map.df$legend)
  
  if(choice == 1){ # return map.df data for graphing the map
    return(map.df)
  }
  if(choice == 2){ # return obs data for showing the data table
    return(obs %>% select(location, observations, `log(observations)` = value))
  }
}

# function that taks in a dataset and returns observation values by time observed (e.g. 15:30:00)
to_histdata = function(data){
  
  # group by unique times
  temp = data %>%
    group_by(time) %>% 
    summarise(value = n()) %>% 
    filter(time != '')
  
  # join the bird obs dataframe with full time dataframe (with all obs = 0) and
  # then replace NAs with 0s
  full_times = left_join(time_df, temp, by = 'time') %>%
    select(time, value = value.y) %>%
    mutate(value = ifelse(is.na(value), 0, value))
  
  return(full_times)
}