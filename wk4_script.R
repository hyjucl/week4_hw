library(sf)
library(here)
library(tidyverse)
library(dplyr)
library(janitor)

diff_ineq <- read_csv(here::here("Data", 
                              "Gender_Inequality_Index_edit.csv"))

shape <- st_read(here::here("Data",
                            "World_Countries_(Generalized)",
                            "World_Countries__Generalized_.shp"))

shape %>%
  st_geometry() %>%
  plot()


diff_only <- diff_ineq %>%
  dplyr::select(Country, yr_10, yr_19)%>%
  group_by(Country)%>%
  summarise(diff_two = yr_10 - yr_19)%>%
  mutate(diff_two)



diff_only_join <- shape %>%
  merge(.,
        diff_only,
        by.x="COUNTRY", #需要改一下 x是shp的第一个
        by.y="Country") #y是csv的第一个


library(tmap)
library(tmaptools)

tmap_mode("plot") #plot也可以换成view
# change the fill to your column name if different
diff_only_join %>%
  qtm(.,fill = "diff_two")




bbox_diff <- diff_only_join %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "esri", zoom = NULL)

tm_shape(bbox_diff)+
  tm_rgb()+
  
  tm_shape(shape) + 
  tm_polygons("diff_two", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              #title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "County to state percent difference in meeting science standards", 
            legend.position = c("right", "bottom"))



