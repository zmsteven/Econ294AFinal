library(rsconnect)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(foreign)
library(tidyr)
library(stringr)
library(maps)
library(ggmap)
library(shinythemes)
##Needs to be relative reference
myd<- read.csv("https://raw.githubusercontent.com/zmsteven/Econ294AFinal/master/DATA/WB_PPI_China.csv",stringsAsFactors = FALSE)
myds <- myd[,-c(1,3:6,8,9,12,13,15,18:23,25:27,31:45)]

for(i in 1:ncol(myds)){
  myds[,i] <- ifelse(myds[,i]=="Not Applicable"|myds[,i]=="Not Available"|myds[,i]=="N/A"|myds[,i]==""|myds[,i]=="N/A, N/A",NA,(myds[,i]))
}

myds$Subtype.of.PPI <- ifelse(myds$Subtype.of.PPI=="Build, operate, and transfer","BOT",myds$Subtype.of.PPI)
myds$Subtype.of.PPI <- ifelse(myds$Subtype.of.PPI=="Build, rehabilitate, operate, and transfer","BROT",myds$Subtype.of.PPI)
myds$Subtype.of.PPI <- ifelse(myds$Subtype.of.PPI=="Lease contract","Lease",myds$Subtype.of.PPI)
myds$Subtype.of.PPI <- ifelse(myds$Subtype.of.PPI=="Rehabilitate, operate, and transfer","ROT",myds$Subtype.of.PPI)
myds$Subtype.of.PPI <- ifelse(myds$Subtype.of.PPI=="Build, own, and operate","BOO",myds$Subtype.of.PPI)
myds$Subtype.of.PPI <- ifelse(myds$Subtype.of.PPI=="Management contract","Management",myds$Subtype.of.PPI)
myds$Subtype.of.PPI <- ifelse(myds$Subtype.of.PPI=="Rehabilitate, lease or rent, and transfer","RLT",myds$Subtype.of.PPI)

myds<- myds %>% 
  separate(Location, paste0("Location",1:2), sep = c(", ")) %>% 
  separate(Location1, paste0("Location2.",1:2), sep = c("/")) %>% 
  separate(Location2.1, paste0("Location3.",1:2), sep = c(" and ")) %>% 
  separate(Location3.1, paste0("Location4.",1:2), sep = c(","))

myds <- subset(myds,select = -c(Location4.2:Location2))
key <- "AIzaSyBh0r4AjoZNI7Jtr-wZJdr5xmXK1GNYa4w"
register_google(key)

myds_coord <- cbind(myds, t(sapply(myds$Location4.1,geocode,USE.NAMES = F)))
myds_coord$lon <- as.numeric(myds_coord$lon)
myds_coord$lat <- as.numeric(myds_coord$lat)

myds_coord <- filter(myds_coord,lon>=80&lon<=135)
myds_coord <- filter(myds_coord,lat>20)
which(grepl(91.89325, myds_coord$lon))
myds_coord <- myds_coord[-105,]
myds_coord <- myds_coord[complete.cases(myds_coord$ContractPeriod), ]
PPI.names <- names(table(myds$Subtype.of.PPI))
status.names <- names(table(myds$Project.status))
################################################################################################################################

ui = fluidPage(theme = shinytheme("cerulean"),titlePanel("Water Infrastructure Projects in China"),h4("Zachary Stevens"),
               
               leafletOutput("Map",height = 1000),
               absolutePanel(id="controls",class="panel panel-default",fixed = TRUE,draggable = TRUE,top = 100,left = "auto",
                             right = 20,bottom = "auto",width = 330,height = "auto", h2("Project filter"),
                             selectInput("PPI.type","Subtype of PPI",choices=PPI.names,selected = "BOT"),
                             selectInput("Project.stat","Project Status",choices=status.names,selected = "Active"),
                             sliderInput("Period","Contract Period(Years)",min = 11,max = 50,value = 30,step = 1)))


##############################################################################################################  
server = function(input, output) {
  
  observe({
    output$Map <- renderLeaflet({
      leaflet() %>% 
        addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                 attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
        setView(lng = 103.2105,lat = 35.6012,zoom = 4)
    })
    
    type <- input$PPI.type
    status <- input$Project.stat
    period <- input$Period
    
    projects <- myds_coord %>% 
      filter(myds_coord$Subtype.of.PPI %in% type & 
               myds_coord$Project.status %in% status &
               myds_coord$ContractPeriod %in% period)
    
    leafletProxy("Map") %>% clearMarkers() %>% 
      addMarkers(lng = projects$lon,
                 lat = projects$lat,
                 popup=paste0("<strong>",projects$Project.name,"</strong><br/>",
                              "Investment ($US Thousands): ",projects$TotalInvestment,
                              "</strong><br/>",projects$CapacityType,": ",projects$Capacity,
                              "</strong><br/>","Investment Year: ",projects$InvestmentYear
                 ))
  })
}


# Run the application 
shinyApp(ui = ui, server = server,options = list(height=1000))