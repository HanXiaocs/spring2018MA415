library(ggmap)
library(tidyverse)

# simple mapping with ggmap

gctest <- geocode("Bude North Cornwall Cricket Club, Bude")
gctest2 <- geocode('Summerleaze Beach, Bude')
gctest3 <- geocode('Crooklets Beach, Bude')
gctest4 <- geocode('Crooklets Inn, Bude')
from <- "Crooklets Inn, Bude"
to1 <- "Crooklets Beach, Bude"
to2 <- "Bude North Cornwall Cricket Club"
to3 <- "Summerleaze Beach, Bude"
route_df1 <- route(from, to1, structure = "route")
route_df2 <- route(from, to2, structure = "route")
route_df3 <- route(from, to3, structure = "route")
#Name <- c('Bude North Cornwall Cricket Club','Summerleaze Beach','Crooklets Beach','?')

maptest <- get_map(gctest,zoom = 15,maptype = "roadmap")
#ggmap(maptest)+geom_point(aes(x=lon,y=lat),data = gctest4,size=3,color='red')+geom_point(aes(x=lon,y=lat),
#                                                                                         data = gctest, color = 'red', size = 3)
ggmap(maptest)+geom_point(aes(x=lon,y=lat),
            data = gctest, color = 'red', size = 3)+geom_point(aes(x=lon,y=lat),
             data = gctest2, color = 'red', size = 3)+geom_point(aes(x=lon,y=lat),
            data = gctest3, color = 'red', size = 3)+geom_point(aes(x=lon,y=lat), 
            data = gctest4, color = 'red', size = 3)+
  geom_text(data = gctest, aes(x=lon,y=lat,label = 'Bude North Cornwall Cricket Club'),
            size=3)+
  geom_text(data = gctest2, aes(x=lon,y=lat,label = 'Summerleaze Beach'),
            size=3)+
  geom_text(data = gctest3, aes(x=lon,y=lat,label = 'Crooklets Beach'),
            size=3)+
   geom_text(data = gctest4, aes(x=lon,y=lat,label = 'Crooklets Inn'),
                              size=3)+
  geom_path(aes(x = lon, y = lat), size = 1.5,
    data = route_df1, lineend = "round")+
    geom_path(aes(x = lon, y = lat), size = 1.5,
    data = route_df2, lineend = "round",color = 'blue')+geom_path(
    aes(x = lon, y = lat), colour = "red", size = 1.5,
    data = route_df3, lineend = "round"
  )
  
maptest2 <- get_map(gctest,zoom = 15,maptype = "watercolor")
ggmap(maptest2)+geom_point(aes(x=lon,y=lat),
                          data = gctest, color = 'red', size = 3)+geom_point(aes(x=lon,y=lat),
                                                                             data = gctest2, color = 'red', size = 3)+geom_point(aes(x=lon,y=lat),
                                                                                                                                 data = gctest3, color = 'red', size = 3)+geom_point(aes(x=lon,y=lat), 
                                                                                                                                                                                     data = gctest4, color = 'red', size = 3)+
  geom_text(data = gctest, aes(x=lon,y=lat,label = 'Bude North Cornwall Cricket Club'),
            size=3)+
  geom_text(data = gctest2, aes(x=lon,y=lat,label = 'Summerleaze Beach'),
            size=3)+
  geom_text(data = gctest3, aes(x=lon,y=lat,label = 'Crooklets Beach'),
            size=3)+
  geom_text(data = gctest4, aes(x=lon,y=lat,label = 'Crooklets Inn'),
            size=3)+
  geom_path(aes(x = lon, y = lat), size = 1.5,
            data = route_df1, lineend = "round")+
  geom_path(aes(x = lon, y = lat), size = 1.5,
            data = route_df2, lineend = "round",color = 'blue')+geom_path(
              aes(x = lon, y = lat), colour = "red", size = 1.5,
              data = route_df3, lineend = "round"
            )
  



# grab a center/zoom map and compute its bounding box
gc <- geocode("white house, washington dc")
map <- get_map(gc)
bb <- attr(map, "bb")
bbox <- bb2bbox(bb)


ggmap(map) +
  geom_point(
    aes(x = lon, y = lat),
    data = gc, color = "red", size = 3
  )

map <- get_map(gc, zoom=16)

ggmap(map) +
  geom_point(
    aes(x = lon, y = lat),
    data = gc, color = "blue", size = 3
  )



#set the location
map1 <- get_googlemap(location = "massachusetts", source = "google")
ggmap(map1, extent = "normal"   )

#set the center
map2 <- get_map(location = c(-71.2, 42.4))
ggmap(map2)


#set the center
map2 <- get_googlemap(center = c(-71.2, 42.4),zoom = 8)
ggmap(map2)

# map2 <- get_navermap(center = c(-71.2, 42.4),zoom = 8,)

# map2 <- get_cloudmademap(center = c(-71.2, 42.4),zoom = 8)


#set the zoom
map3 <- get_map("Boston University", zoom= 17)
ggmap(map3)

map3 <- getgoogle_map("Boston University", zoom= 17)
ggmap(map3)



map3 <- get_map("Boston University", maptype = "hybrid",zoom= 17)
ggmap(map3)


map3 <- get_map("Boston University", maptype = "watercolor",zoom= 17)
ggmap(map3)




from <- "Boston University"
to <- "Burlington, Vermont"
route_df <- route(from, to, structure = "route")

map10 <- get_map("Boston, MA", zoom = 8)
ggmap(map10) +  
  geom_path(
  aes(x = lon, y = lat), colour = "red", size = 1.5,
  data = route_df, lineend = "round"
)





###########################################
## maps library


# library(maps)
# map.ma <- map_data(state = "Massachusetts")

map.ma <- map('state', fill = TRUE, col = palette())


data(stateMapEnv)
data(state.carto.center)


map('usa')

# NYC
a <- map.where("state", -73.8, 41)
a

b <- map.where("world",-73.8, 41)
b



# Auckland
map.where("nz", 174.6, -36.92)
# find both in the world
map.where(x = c(174.6, -73.8), y = c(-36.92, 41))
# with a map object:
m = map("state", "new york", fill = TRUE, plot = FALSE)
map.where(m, -74, 41.2)

####################################################################


map.europe <- map_data("world")



map.europe <- as.data.frame(map.europe)

ggplot() +
  geom_polygon(data = map.europe, aes(x = long, y = lat, group = group))



  geom_point(data = df.euro_cities, 
             aes(x = lon, y = lat, size = population), 
             color = "red", alpha = .3) +
  coord_cartesian(xlim = c(-9,45), ylim = c(32,70))




