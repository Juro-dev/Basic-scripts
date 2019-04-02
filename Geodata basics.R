# Importing data ####
rm(list = ls())
setwd("~/Berlin resto")

# Importing OSM xml file ####
q <- opq ("Berlin Germany") %>%
  add_osm_feature(key = "amenity", value = "restaurant")
osmdata_xml(q, "list.xml", quiet = TRUE)

xml1 <- read_xml("list.xml")

allnodes <- xml_find_all(xml1, "//node")
allattrs <- xml_attrs(allnodes)
df <- data.frame(id = xml_attr(allnodes, "id"), lat = xml_attr(allnodes, "lat"), lon = xml_attr(allnodes, "lon"))

'%ni%' <- Negate('%in%')

keyval <- lapply(allnodes, function(i) {
  id <- xml_attr(i, 'id')
  v <- xml_attr(xml_children(i), 'v')
  k <- xml_attr(xml_children(i), 'k')
  return (paste(id, k,v))       
})

keyval <- unlist(keyval)                  

for (i in keyval) {
  i <- trimws(i)
  
  id <-  str_split_fixed(i, " ", 3)[1]
  key <- str_split_fixed(i, " ", 3)[2]
  val <- str_split_fixed(i, " ", 3)[3]
  
  if (key != "") {
    if (key %ni% colnames(df)) df[,key] <- ' '            
    df[df$id == id, key ] <- val
    
  }
}

df1 <- df
df1$lat <- as.numeric(as.character(df1$lat))
df1$lon <- as.numeric(as.character(df1$lon))


dfchin <- df1 %>% 
  filter(df1[,"cuisine"] == "chinese")

dfind <- df1 %>% 
  filter(df1[,"cuisine"] == "indian")

dfthai <- df1 %>% 
  filter(df1[,"cuisine"] == "thai")

dfger <- df1 %>% 
  filter(df1[,"cuisine"] == "german")

dftw <- df1 %>% 
  filter(df1[,"cuisine"] == "taiwanese")

dfgreek <- df1 %>% 
  filter(df1[,"cuisine"] == "greek")

dfsudan <- df1 %>% 
  filter(df1[,"cuisine"] == "sudanese")

dfspanish <- df1 %>% 
  filter(df1[,"cuisine"] == "spanish")

dfnew <- rbind(dfchin, dfind, dfsudan, dfger, dfspanish, dfgreek)

# Visualization GPS coordinates ####

bbox <- osmdata::getbb ("berlin, germany")

berlin <- get_map(location = bbox, source = "osm")
berlinMap <-  ggmap(berlin)

berlinMap1 <- berlinMap + geom_point(data = df1, aes(x = lon, y = lat), size = 1)

berlinMap2 <- berlinMap + geom_point(data = dfnew, aes(x = lon, y = lat), size = 1) +
  ggtitle("Different Cuisines") +
  facet_wrap(~cuisine)

leaflet() %>%
  addTiles() %>%
  setView(lng = 13.4, lat = 52.5166667, zoom = 11) %>%
  addMarkers(data = dfnew, lng = dfnew$lon, lat = dfnew$lat, popup = paste(dfnew$name, dfnew$cuisine, dfnew$website))