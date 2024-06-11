## install 'webshot' package
library(devtools)
install_github("wch/webshot")

## load packages
library(leaflet)
library(htmlwidgets)
library(webshot)

## create map
m <- leaflet() %>% addTiles()

## save html to png
saveWidget(m, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "Rplot.png",
        cliprect = "viewport")