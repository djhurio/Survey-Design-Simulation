### Geocoding in R with Google Maps
### http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps

library(RCurl)
library(RJSONIO)


### The Google Geocoding API
### http://code.google.com/apis/maps/documentation/geocoding/

construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

gGeoCode <- function(address) {
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  lat <- x$results[[1]]$geometry$location$lat
  lng <- x$results[[1]]$geometry$location$lng
  return(c(lat, lng))
}

gGeoCode("Rīga")
gGeoCode("Viesīte")


### The Google Directions API
### http://code.google.com/apis/maps/documentation/directions/

construct.directions.url <- function(origin, destination, return.call = "json", sensor = "false") {
  root <- "http://maps.googleapis.com/maps/api/directions/"
  u <- paste(root, return.call, "?origin=", origin, "&destination=", destination, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

# construct.directions.url("Rīga", "Viesīte")

gDirections <- function(origin, destination) {
  u <- construct.directions.url(origin, destination)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  x$routes[[1]]$legs[[1]]$distance$value
}

gDirections("Rīga", "Viesīte")
gDirections("Lāpstu iela 15, Rīga", "Skolas iela 19, Rīga")



### Nominatim Search Service
### http://open.mapquestapi.com/nominatim/

open.geocode.url <- function(q, format = "json", limit = "1") {
  root <- "http://open.mapquestapi.com/nominatim/v1/search"
  u <- paste(root, "?format=", format, "&q=", q, "&limit=", limit, sep = "")
  return(URLencode(u))
}

open.geocode.url("Rīga")


oGeoCode <- function(q) {
  u <- open.geocode.url(q)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  lat <- x[[1]]$lat
  lon <- x[[1]]$lon
  return(paste(lat, lon, sep = ","))
  x
}

oGeoCode("Rīga")
oGeoCode("Viesīte")




### Open Directions Service Developer's Guide
### http://open.mapquestapi.com/directions/


oGeoCode("Rīga")

open.directions.url <- function(from, to, outFormat = "json", unit = "k") {
  root <- "http://open.mapquestapi.com/directions/v0/route"
  u <- paste(root, "?outFormat=", outFormat, "&from=", from, "&to=", to, "&unit=", unit, sep = "")
  return(URLencode(u))
}

open.directions.url(oGeoCode("Rīga"), oGeoCode("Viesīte"))

oDirections <- function(from, to) {
  u <- open.directions.url(from, to)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  x$route$legs[[1]]$distance
}

oDirections(oGeoCode("Rīga"), oGeoCode("Viesīte"))
oDirections(oGeoCode("Rīga"), oGeoCode("Liepāja"))
oDirections(oGeoCode("Daugavpils"), oGeoCode("Ventspils"))

oDirections(oGeoCode("15, Lāpstu iela, Rīga, Latvija"), oGeoCode("Elizabetes iela 21, Rīga"))
gDirections("Lāpstu iela 15, Rīga", "Elizabetes iela 21, Rīga")

