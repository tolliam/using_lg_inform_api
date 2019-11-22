# Libaries required
library(tidyverse)
library(jsonlite)
library(httr)
library(digest)

setwd("")

# PART 1 Method A (EASY): 
# Go to this link to make query: https://developertools.esd.org.uk/data?value.valueType=raw&period=latest&columnGrouping=metricType&columnGrouping=period&columnGrouping=valueType&rowGrouping=area
# Click on the download button tab on left hand side
# At the bottom of the "permanent url request" box you will see a url - click to the right to this to copy link
# Paste request between quotation marks below
url <- ""

# PART 1 Method B (COMPLEX): Build query

# Paste api key and secret information here
key <- "" 
secret <- ""

# Define function to generate signature based on url and application key
constr_url <- function(url){
  url <- paste(url, "ApplicationKey=", key, sep="") # Useful function for combining information in to srintg
  sig1 <- hmac(secret, url, algo="sha1", raw=TRUE, serialize = FALSE)
  signature <- base64_enc(sig1)
  signature <- paste("&Signature=", signature, sep="")
  url <- paste(url, signature, sep="")
}

# Bring up a table of LG Inform metrics   
url_metrics <- "http://webservices.esd.org.uk/metricTypes?"
url_metrics <- constr_url(url_metrics)
get_metric <- GET(url_metrics)
resp <- content(get_metric, "text")
metrics <- fromJSON(resp) %>% data.frame()
View(metrics)

# Get area_types types
url_areas <- "http://webservices.esd.org.uk/areaTypes?"
url_areas <- constr_url(url_areas)
get_metric <- GET(url_areas)
resp <- content(get_metric, "text")
area_types <- fromJSON(resp) %>% data.frame()
View(area_types)

# Get area groupings
url_areas <- "http://webservices.esd.org.uk/comparisonGroups?"
url_areas <- constr_url(url_areas)
get_metric <- GET(url_areas)
resp <- content(get_metric, "text")
area_groupings <- fromJSON(resp) %>% data.frame()
View(area_groupings)

# Get periods
url_periods <- "http://webservices.esd.org.uk/periods?"
url_periods <- constr_url(url_periods)
get_metric <- GET(url_periods)
resp <- content(get_metric, "text")
periods <- fromJSON(resp) %>% data.frame()
View(periods)

# Construct query

# Add area here use one of the "identifiers" from area_groupings
# Or use other examples "E92000001:MLSOA" All MSOAs in England
# AllLaInCountry_England
area <- list("AllLaInCountry_England", "E92000001", "E12000007")
area <- paste(area, collapse=",")

# Add metric type in to the list below
metricTypes <- list("", "")
metricTypes <- paste(metricTypes, collapse=",")

# Choose a time period based on periods tab 
# Or choose "latest","latest:2", (last 2 available years) "latest:3" (last 3 available years)

period <- "latest:2"

rest_of_query <- "&columnGrouping=valueType&rowGrouping=metricType&rowGrouping=period&rowGrouping=area&"
url_main_query <- "http://webservices.esd.org.uk/data?metricType="
url_main_query <- paste(url_main_query, metricTypes, "&", "area=", area, "&", "period=", period, rest_of_query, sep="")
url_main_query <- constr_url(url_main_query)

# PART 2: RUN QUERY
get_metric <- GET(url_main_query)
resp <- content(get_metric, "text")
result <- fromJSON(resp)
result <- result$rows
result <- unnest_longer(result,values,indices_include = FALSE) # This is needed as a few different value options are present
result <- do.call(data.frame, result)
result <- result[c("area.identifier", "area.label", "metricType.label", "period.label", "values.value")]
View(result)

write.csv(result, "result.csv")

