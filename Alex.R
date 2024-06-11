source("Script.R")

install.packages("sf")
library(sf)

data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=','))
View(data$fk_stadedev)

# x <- st_transform(data$X, crs = st_crs(data$X), dst= 4326)
sfc = st_sfc(st_point(c(1720320.1079,1721095.6459)), st_point(c(8294619.3561,8293514.7374)), crs = 3949)
x =st_transform(sfc, crs = 3949, dst = 4326)
# st_transform(sfc, 4326)
x
x[1][1]

for(i in length(data$X)){

    data$X[i],data$Y[i] <- st_transform(st_sfc(st_point(c(data$X[i], data$Y[i]), crs = 3949)), crs = 3949, dst = 4326)

}

View(data)
sfc <- st_sfc()