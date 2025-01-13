# Measures Against Covid-19 in Greece - A small analysis

# Measures During the Pandemic Years

#Libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(plotly)
library(collapsibleTree)
library(paletteer)
library(lisa)
library(ggrepel)
library(patchwork)
library(GGally)

data <- read_xlsx("phsm_database_04032024_greece.xlsx", col_names = TRUE)

data$`Start of measure` <- as.Date.POSIXct(data$`Start of measure`)
data$`End of measure` <- as.Date.POSIXct(data$`End of measure`)
data$`Date of access` <- as.Date.POSIXct(data$`Date of access`)

# Number of Measures' Record during the years

freq_of_measures_based_on_year_entry <- data %>%
  dplyr::select(`Entry year`)%>%
  count(`Entry year`)

freq_of_measures_based_on_year_entry$`Entry year` <- as.factor(freq_of_measures_based_on_year_entry$`Entry year`)

g0 <- ggplot(freq_of_measures_based_on_year_entry, aes(x= `Entry year`, y=n, fill=`Entry year`)) +
  geom_bar(stat = 'identity', position = 'dodge')+
  ggtitle("Records of Measures by Year") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  xlab("Year") +
  ylab("Number of Records") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.3, colour = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_hc()+
  scale_fill_paletteer_d("ggthemes::Classic_Blue_Red_12")
g0

data_for_timeline_of_measure_started <- data %>%
  dplyr::select(`Start of measure`) %>%
  count(`Start of measure`)

data_for_timeline_of_measure_ended <- data %>%
  dplyr::select(`End of measure`) %>%
  count(`End of measure`)

g5 <- ggplot(data_for_timeline_of_measure_started, aes(x=`Start of measure`, y=n))+
  geom_line(stat = 'identity',color='darkred') +
  ggtitle("Total Number of Measures Started by Day") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  xlab("Date") +
  theme_hc() +
  scale_fill_paletteer_d("ggthemes::Classic_Blue_Red_12") 

ggplotly(g5)

measure_category_by_year <- data %>%
  group_by(`Entry year`) %>%
  count(`Measure Category`)

g1 <- ggplot(measure_category_by_year, aes(y=n, x=`Entry year` , fill=`Measure Category`))+
  geom_bar(stat = 'identity', position = 'dodge')+
  ggtitle("Category of Measures by Year") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  xlab("Year") +
  ylab("Number of Measures") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.3, colour = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_hc()+
  scale_fill_paletteer_d("ggthemes::Classic_Blue_Red_12")
g1

changes <- data %>%
  dplyr::select(`Measure Category`,`Change in measure`)

changes$`Change in measure`[changes$`Change in measure` == 0] <- 'Change of measure is not applicable' 
changes$`Change in measure`[changes$`Change in measure` == 1] <- 'A measure or type of measure is put in place for the first time'
changes$`Change in measure`[changes$`Change in measure` == 2] <- 'Measure is either re-introduced, extended or strengthened/intensified'
changes$`Change in measure`[changes$`Change in measure` == 3] <- 'A measure is eased or lifted'

 first_measures_changes_distribution <- changes %>%
    group_by(`Measure Category`) %>%
    dplyr::filter(`Change in measure` =='A measure or type of measure is put in place for the first time')%>%
    count(`Change in measure`)
  
  g11 <- ggplot(first_measures_changes_distribution, aes(y= `Measure Category`, x = n, fill = `Measure Category`))+
    geom_bar(stat='identity', position = 'dodge')+
    ggtitle("Measures that Intorduced for First Time in Each Category") +
    theme(
      plot.title = element_text(hjust = 0.5)
    )+
    xlab("Count") +
    ylab("Measure Category") +
    geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = -0.3, colour = "black", size = 6)+
    theme_hc()+
    scale_fill_paletteer_d("ggthemes::Classic_Blue_Red_12")

  g11

first_measures_changes <- changes %>%
    group_by(`Measure Category`) %>%
    dplyr::filter(`Change in measure` !='A measure or type of measure is put in place for the first time')
  
g12 <- ggplot(first_measures_changes, aes(y= `Measure Category`, fill = `Change in measure`))+
    geom_bar(stat='count', position = 'dodge')+
    ggtitle("Distribution of Measures that Introduced for the First Time based on Type of Changes") +
    theme(
      plot.title = element_text(hjust = 0.5)
    )+
    xlab("Count") +
    ylab("Measure Category") +
    geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), hjust = -0.3, colour = "black", size = 6)+
    theme_hc()+
    scale_fill_paletteer_d("ggthemes::Classic_Blue_Red_12")
g12

measures_category_subcategory <- data %>%
  dplyr::select(`Measure Category`, `Measure Subcategory`, Measure)%>%
  group_by(`Measure Category`)


g2_tree_plot_measures_categories <- collapsibleTree(measures_category_subcategory, c("Measure Category", "Measure Subcategory", "Measure"), root = "Category of Measures")
g2_tree_plot_measures_categories


# Local Measures and Cities

local_measures_for_map <- read_xlsx("local_measures_for_map_4.xlsx")


local_measures_for_map$`Region that measure adapted`[local_measures_for_map$`Region that measure adapted` == "Eordea"] <- 'Eordaea'

# Add long and lat based on cities

library(leaflet)
library(leaflet.extras)
library(tidygeocoder)


# Wordcloud for cities
library(ggwordcloud)

set.seed(123)
cities <- local_measures_for_map %>%
  dplyr::select(`Region that measure adapted`) %>%
  count(`Region that measure adapted`)

ggwordcloud(cities$`Region that measure adapted`, cities$n)

The Map

local_measures_for_map_stats <- local_measures_for_map %>%
  dplyr::select(`Region that measure adapted`, `Level of enforcement`, `Starting Year`, `Ending Year`)

local_measures_for_map_stats$`Starting Year` <- substr(local_measures_for_map_stats$`Starting Year`,1,4)
local_measures_for_map_stats$`Ending Year` <- substr(local_measures_for_map_stats$`Ending Year`,1,4)  

local_measures_for_map_stats_final <- local_measures_for_map_stats %>%   
  group_by(`Region that measure adapted`) %>%
  count(`Region that measure adapted`,`Level of enforcement`, `Starting Year`, `Ending Year`)

local_measures_for_map_stats_final <- local_measures_for_map_stats_final %>%
  mutate(popup_info=paste(`Region that measure adapted`, "<br/>", "Count of Measures:", n, "<br/>", "Start:", `Starting Year`, "<br/>", "End:", `Ending Year`))

local_measures_for_map_stats_final <- local_measures_for_map_stats_final %>%
  geocode(`Region that measure adapted`, method = "osm", lat = latitude, long = longitude)

local_measures_for_map_stats_final <- local_measures_for_map_stats_final[!is.na(local_measures_for_map_stats_final$`Level of enforcement`), ]

local_measures_for_map_stats_final$color <- ifelse(local_measures_for_map_stats_final$`Level of enforcement` == "Measure is a requirement", "darkred",
                                                   ifelse(local_measures_for_map_stats_final$`Level of enforcement` == "Not applicable to the measure", "lightblue", "darkgreen"))

leaflet(local_measures_for_map_stats_final) %>%
  addTiles() %>%
  addAwesomeMarkers(
    data = subset(local_measures_for_map_stats_final, `Level of enforcement` == "Measure is a requirement"),
    ~longitude, ~latitude,
    icon = awesomeIcons(icon = "map-marker", markerColor = "darkred", library = "glyphicon"),
    group = "Measure is a requirement",
    popup = ~popup_info
  ) %>%
  addAwesomeMarkers(
    data = subset(local_measures_for_map_stats_final, `Level of enforcement` == "Not applicable to the measure"),
    ~longitude, ~latitude,
    icon = awesomeIcons(icon = "map-marker", markerColor = "lightblue", library = "glyphicon"),
    group = "Not applicable to the measure",
    popup = ~popup_info
  ) %>%
  addAwesomeMarkers(
    data = subset(local_measures_for_map_stats_final, `Level of enforcement` == "Measure is a recommendation"),
    ~longitude, ~latitude,
    icon = awesomeIcons(icon = "map-marker", markerColor = "darkgreen", library = "glyphicon"),
    group = "Measure is a recommendation",
    popup = ~popup_info
  ) %>%
  addControl(html = 'Count of Local Measures in Greece based on the Level of Enforcement', position = 'bottomleft') %>%
  addLayersControl(
    overlayGroups = c("Measure is a requirement", "Not applicable to the measure", "Measure is a recommendation"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Scope of Measures

data$`Scope of measure`[data$`Scope of measure` == 0] <- 'Local Measure'
data$`Scope of measure`[data$`Scope of measure` == 1] <- 'Nationwide Measure'
data$`Level of enforcement`[data$`Level of enforcement` == 0] <- 'Not applicable to the measure'
data$`Level of enforcement`[data$`Level of enforcement` == 1] <- 'Measure is a recommendation'
data$`Level of enforcement`[data$`Level of enforcement` == 2] <- 'Measure is a requirement'


scope_of_measure <- data %>%
  dplyr::select(`Scope of measure`) %>%
  count(`Scope of measure`)

g3 <- ggplot(scope_of_measure, aes(y=n, x=`Scope of measure` , fill=`Scope of measure`))+
  geom_bar(stat = 'identity', position = 'dodge')+
  ggtitle("Scope of Measures") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  xlab("Scope Category") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.3, colour = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_hc() +
  scale_fill_paletteer_d("ggthemes::Classic_Blue_Red_12")

scope_of_measure_by_level_of_enforcement <- data %>%
  dplyr::select(`Scope of measure`, `Level of enforcement`) %>%
  group_by(`Scope of measure`) %>%
  count(`Level of enforcement`)

g3

g4 <- ggplot(scope_of_measure_by_level_of_enforcement, aes(y=n, x=`Scope of measure` , fill=`Level of enforcement`))+
  geom_bar(stat = 'identity', position = 'dodge')+
  ggtitle("Scope of Measures by Level of Enforcement") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  xlab("Scope Category") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.3, colour = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_hc() +
  scale_fill_paletteer_d("ggthemes::Classic_Blue_Red_12") 

g4
