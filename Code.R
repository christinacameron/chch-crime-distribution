# load required packages
library(sf)
library(tidyverse)
library(kableExtra)
library(plotly)
library(tmap)

# import data
area_units <- st_read("Data/statsnzarea-unit-2017-generalised-version-SHP")
crime <- read.csv("Data/Police_victimisation_data.csv")

# remove full stops from crime column Area.Unit
crime$`Area.Unit` <- str_replace(crime$`Area.Unit`, "\\.", "")

# match crime column Area.Unit to area_units column AU2017_NAM
crime_sf <- right_join(crime, area_units, by = c("Area.Unit" = "AU2017_NAM")) %>%
  select(c(-ï..Hour, -Map.Detail.Name, -Region, -Territorial.Authority, -Mb2014, 
           -REGC2014.label, -Ta2014.Nam, -Victimisations, -AREA_SQ_KM, 
           -LAND_AREA_, -Shape_Leng, # remove irrelevant variables
           `Day` = Occurrence.Day.Of.Week, # rename remaining ones
           `ANZSOC division` = ANZSOC.Division,
           `ANZSOC group` = ANZSOC.Group,
           `Area unit` = Area.Unit,
           `Hour` = Occurrence.Hour.Of.Day,
           `Date` = Year.Month,
           `Victimisation count` = Number.of.Victimisations)) %>% 
  na.omit() %>% # remove NAs
  add_count(`Area unit`, `ANZSOC division`, name = "Division count") %>% # add division count
  add_count(`Area unit`, `ANZSOC group`, name = "Group count") %>% # add group count
  add_count(`Area unit`, name = "All crimes") %>% # add total count
  mutate(`ANZSOC division` = str_to_sentence(`ANZSOC division`), # fix title casing
         `ANZSOC group` = str_to_sentence(`ANZSOC group`),
         `Division %` = round(`Division count`/`All crimes`*100,2), # add percentages
         `Group %` = round(`Group count`/`All crimes`*100,2),
         `Area unit` = fct_rev(`Area unit`)) %>% # put into correct order for the charts
  st_as_sf() # convert to sf

# fix case of acronym
crime_sf$`ANZSOC group` <- str_replace(crime_sf$`ANZSOC group`, "n.e.c.", "N.E.C.")

# change from character to factor and reorder for the charts
crime_sf$`ANZSOC division` <- factor(crime_sf$`ANZSOC division`,
                                     levels = names(sort(table(crime_sf$`ANZSOC division`), 
                                                         decreasing = TRUE)))
crime_sf$`ANZSOC group` <- factor(crime_sf$`ANZSOC group`, 
                                  levels = names(sort(table(crime_sf$`ANZSOC group`), 
                                                      decreasing = TRUE)))

# counts of each crime division per area unit
crime_div <- crime_sf %>% 
  select(c(`Area unit`, `ANZSOC division`)) %>%
  count(`Area unit`, `ANZSOC division`) %>% # change the columns to ANZSOC divisions
  pivot_wider(names_from = `ANZSOC division`, values_from = n) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% # change NAs to 0 for summing
  mutate(`All crimes` = rowSums(.[3:8]), # add total count
         across(`Theft and related offences`:`All crimes`, 
                ~na_if(., 0))) %>% # change 0s back to NA
  rename_with(str_to_sentence) %>% # fix title casing
  st_as_sf() # convert to sf

# make a palette
palette <- c("#7fc97f", "#4475ae", "#9abda4", "#7d449d", "#b6b1c9", "#cd1588", 
             "#d1b3bb", "#e31864", "#edbb99", "#ce3f37", "#fdcb89", "#b35c20", 
             "#fee791", "#8c6143", "#f2f59a", "#666666", "#9bb54a")



### <<< CLEANING/PREP | EDA >>>



# interactive stacked bar chart with area units and crime divisions
stack_bar_div <- ggplot(crime_sf) + 
  geom_bar(aes(x = `Area unit`, fill = `ANZSOC division`, # variables
               text = paste("Division count:", `Division count`, # custom hover text
                            "<br>Total count:", `All crimes`))) + 
  labs(title = "Crime divisions by area unit", # title
       x = "", y = "Crime occurrences") + # axis labels
  scale_fill_discrete(type = palette) + # colour palette
  coord_flip() # flip x and y axes
ggplotly(stack_bar_div, width = 1000, height = 2500, # dimensions
         tooltip = list("Area unit", "ANZSOC division", "text")) %>% # hover text
  layout(title = list(x = 0, y = 1, yanchor = "top"), # title position
         legend = list(orientation = "v", x = 0.645, y = 0.97, # legend position
                    xanchor = "center", yanchor = "top"))

# interactive stacked bar chart with area units and crime groups
stack_bar_group <- ggplot(crime_sf) + 
  geom_bar(aes(x = `Area unit`, fill = `ANZSOC group`, # variables
               text = paste("Count:", `Group count`, # custom hover text
                            "<br>Total count:", `All crimes`))) + 
  labs(title = "Crime groups by area unit", # title
       x = "", y = "Crime occurrences") + # axis labels
  scale_fill_discrete(type = palette) + # colour palette
  coord_flip() # flip x and y axes
ggplotly(stack_bar_group, width = 1000, height = 2500, # dimensions
         tooltip = list("Area unit", "ANZSOC group", "text")) %>% # hover text
  layout(title = list(x = 0, y = 1, yanchor = "top"), # title position
         legend = list(orientation = "v", x = 0.74, y = 0.992, # legend position
                    xanchor = "center", yanchor = "top"))

# interactive mosaic chart with area units and crime divisions
mosaic_div <- ggplot(crime_sf) + 
  geom_bar(aes(x = `Area unit`, fill = `ANZSOC division`, # variables
               text = paste("Count:", `Division count`, # custom hover text
                            "<br>Percentage of all crime:", `Division %`)),
           position = position_fill()) + # convert to mosaic chart
  labs(title = "Crime divisions by area unit", # title
       x = "", y = "Proportion") + # axis labels
  scale_fill_discrete(type = palette) + # colour palette
  coord_flip() # flip x and y axes
ggplotly(mosaic_div, width = 1000, height = 3000, # dimensions
         tooltip = list("Area unit", "ANZSOC division", "text")) %>% # hover text
  layout(title = list(x = 0, y = 1, yanchor = "top"), # title position
         margin = list(t = 110), # margin size
         legend = list(orientation = "h", x = 0.4, y = 1.03, # legend position
                    xanchor = "center", yanchor = "top"))

# interactive mosaic chart with area units and crime groups  
mosaic_group <- ggplot(crime_sf) + 
  geom_bar(aes(x = `Area unit`, fill = `ANZSOC group`, # variables
               text = paste("Count:", `Group count`, # custom hover text
                            "<br>Percentage of all crime:", `Group %`)),
           position = position_fill()) + # convert to mosaic chart
  labs(title = "Crime groups by area unit", # title
       x = "", y = "Proportion") + # axis labels
  scale_fill_discrete(type = palette) + # colour palette
  coord_flip() # flip x and y axes
ggplotly(mosaic_group, width = 1000, height = 3000, # dimensions
         tooltip = list("Area unit", "ANZSOC group", "text")) %>% # hover text
  layout(title = list(x = 0, y = 1, yanchor = "top"), # title position
         margin = list(t = 200), # margin size
         legend = list(orientation = "h", x = 0.4, y = 1.065, # legend position
                    xanchor = "center", yanchor = "top"))

# where each crime group is most common
most_common_group <- crime_sf %>%                                
  arrange(desc(`Group count`)) %>% # order by descending group count
  group_by(`ANZSOC group`) %>% # group by ANZSOC group
  slice(1) # take only the highest count for each group
most_common_group %>% # format as a table
  select(c(`ANZSOC group`, `Area unit`, `Group count`, `All crimes`)) %>%
  st_drop_geometry() %>% # remove geometry column
  kbl() %>%
  kable_styling() %>%
  row_spec(0, bold = T)

# where each crime division is most common
most_common_div <- crime_sf %>%                                
  arrange(desc(`Division count`)) %>% # order by descending division count
  group_by(`ANZSOC division`) %>% # group by ANZSOC division
  slice(1) # take only the highest count for each division
most_common_div %>% # format as a table
  select(c(`ANZSOC division`, `Area unit`, `Division count`, `All crimes`)) %>%
  st_drop_geometry() %>% # remove geometry column
  kbl() %>%
  kable_styling() %>%
  row_spec(0, bold = T)



### <<< EDA | MAP >>>



# interactive map
tmap_mode("view") # change to interactive mode
tm_shape(crime_div) +
  tm_polygons(c("Theft and related offences", # map layers
                "Unlawful entry with intent/burglary, break and enter", 
                "Acts intended to cause injury", 
                "Robbery, extortion and related offences", 
                "Sexual assault and related offences", 
                "Abduction, harassment and other related offences against a person", 
                "All crimes"),
              palette = "Reds", # colour palette
              style = c("jenks", "jenks", "jenks", # breaks style for each layer
                        "jenks", "jenks", "cat", "jenks"),
              textNA = "NA", # NA label in legend
              legend.format = c(digits = 0), # whole numbers in legend
              popup.format = c(digits = 0), # whole numbers in popups
              id = "Area unit") + # hover text
  tm_facets(as.layers = TRUE) + # display as layers instead of facets
  tm_shape(most_common_group, name = "Most common markers") + # add markers
  tm_markers(jitter = 0.2, # space out markers in the same area unit
             clustering = FALSE, # no clustering
             id = "ANZSOC group", # hover text
             popup.vars = c("Area unit:" = "Area unit", # customise popup text
                            "ANZSOC division:" = "ANZSOC division",
                            "Group count" = "Group count")) + 
  tm_scale_bar(position = c("left", "bottom")) + # scale bar
  tm_layout(title = "Crime distribution in Christchurch") # main title