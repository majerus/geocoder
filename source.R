# load and install packages
pkg <- c("httr", "rjson", "dplyr", "stringr", "devtools", "leaflet")

new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}

library(httr)
library(rjson)
library(dplyr)
library(stringr)
library(devtools)

if (!require("leaflet")) devtools::install_github("rstudio/leaflet")
library(leaflet)

# read in data
data <- read.csv('filepath/filename.csv')

# create location var 
data$location <- paste(str_trim(as.character(data$street)), 
                       str_trim(as.character(data$city)),
                       str_trim(as.character(data$state)),
                       str_trim(as.character(data$zip)), sep=' ')

# sample data for testing 
sample <-
 data %>% sample_n(50)

# create geocode function with tryCatch for errors
geo.dsk <- function(addr){ 
  require(httr)
  require(rjson)
  
  out <- tryCatch({
  
  url      <- "http://www.datasciencetoolkit.org/maps/api/geocode/json"
  response <- GET(url,query=list(sensor="FALSE",address=addr))
  json <- fromJSON(content(response,type="text"))
  loc  <- json['results'][[1]][[1]]$geometry$location
  return(c(address=addr,long=loc$lng, lat= loc$lat))
  }, 
    
  error = function(cond) {
            message(paste("Address not geocoded:", addr))
            message("Here's the original error message:")
            message(cond)
            # Choose a return value in case of error
            return(NA)
        },  

 warning = function(cond) {
            message(paste("Address caused a warning:", addr))
            message("Here's the original warning message:")
            message(cond)
            # Choose a return value in case of warning
            return(NULL)
        },

 finally = {
            message(paste("Processed Address:", addr))
            message("One down...")
        }
    )    
    return(out)
}

# geocode data and join with sampled data
system.time(result <- do.call(rbind,lapply(as.character(sample$location),geo.dsk)))
result <- data.frame(result)
result <- cbind(result, sample)

# create map
leaflet(result) %>%
addTiles() %>%
setView(-93.65, 42.0285, zoom = 3) %>%
addCircles(result$long, result$lat) 


# write out data in csv
write.csv(result,'filepath/newfilename.csv')