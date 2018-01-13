library("tmap")
library("leaflet")
library("chron")
library("rio")
library("scales")
library("shiny")
library("rsconnect")

setwd("C:/Learning/map/")
datafile <- "data.xlsx"
azdata <- rio::import(datafile)
azdata <- azdata[,c("County","Hillary","Trump" )]
azdata$Total <- azdata$Trump + azdata$Hillary
azdata$HillaryPct <- round(((azdata$Hillary / azdata$Total)),3)
azdata$TrumpPct <- round(((azdata$Trump / azdata$Total)),3)

usshapefile <- "cb_2014_us_county_5m/cb_2014_us_county_5m.shp"
usgeo <- read_shape(file=usshapefile)
azgeo <- usgeo[usgeo@data$STATEFP=="04",]

azgeo@data$NAME <- as.character(azgeo@data$NAME)
azgeo <- azgeo[order(azgeo@data$NAME),]
azdata <- azdata[order(azdata$County),]
azmap <- append_data(azgeo, azdata, key.shp = "NAME", key.data="County")

azmap$winner <- ifelse (azdata$Hillary> azdata$Trump ,'Hillary',ifelse(azdata$Trump> azdata$Hillary,'Trump',NA))
minpct <- min(c(azmap$`HillaryPct`, azmap$`TrumpPct`))
maxpct <- max(c(azmap$`HillaryPct`, azmap$`TrumpPct`))

hillaryPalette <- colorNumeric(palette = "Blues", domain = c(minpct, maxpct))
trumpPalette <- colorNumeric(palette = "Reds", domain = c(minpct, maxpct))
winnerPalette <- colorFactor(palette=c("#5c22d1","#d12522"), domain = azmap$winner)

azpopup <- paste0("County: ", azmap@data$NAME,
                  " Winner: ", azmap@data$winner,
                  ", Hillary: ", percent(azmap@data$`HillaryPct`),
                  ", Trump: ", percent(azmap@data$`TrumpPct`)
)

ui <- fluidPage(
  leafletOutput("map")
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(azmap) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke=TRUE,
                  weight=1,
                  smoothFactor = 0.2,
                  fillOpacity = .75,
                  popup=azpopup, 
                  color= ~winnerPalette(azmap@data$winner),
                  group="Winners"
      ) %>% 
      addLegend(position="bottomleft", colors=c("#5c22d1", "#d12522"), labels=c("Hillary", "Trump"))  %>%
      
      addPolygons(stroke=TRUE,
                  weight=1,
                  smoothFactor = 0.2, 
                  fillOpacity = .75, 
                  popup=azpopup, 
                  color= ~hillaryPalette(azmap@data$`HillaryPct`),
                  group="Hillary"
      ) %>%
      
      addPolygons(stroke=TRUE,
                  weight=1,
                  smoothFactor = 0.2, 
                  fillOpacity = .75, 
                  popup=azpopup, 
                  color= ~trumpPalette(azmap@data$`TrumpPct`),
                  group="Trump"
      ) %>%
      
      
      addLayersControl(
        baseGroups=c("Winners", "Hillary", "Trump"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}
shinyApp(server = server, ui = ui)
