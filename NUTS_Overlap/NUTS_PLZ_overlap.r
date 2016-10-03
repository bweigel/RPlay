library(rgdal)
library(rgeos)
library(dplyr)
library(ggplot2)
library(maptools)
library(mapproj)
# Read Shapefile with 3-digit-PLZ-regions
dsn <- "/home/mori/Data/GIS/PLZ/3stellig/plz-3stellig.shp"
layer <- ogrListLayers(dsn)[1]
PLZ.DE <- readOGR(dsn, layer)

# make a dataframe with all the centroids of the PLz regions for subsetting later
PLZ.centers <- as.data.frame(gCentroid(PLZ.DE, byid = T, id=PLZ.DE$id)@coords )
PLZ.centers$id <- PLZ.DE$plz
PLZ.centers <- mutate(PLZ.centers, id = paste(id, "XX", sep=""))

# helper function to subset PLZ regions according to their location inside another region
subsetPLZ <- function(points,df){
    point.in.polygon(points$x, points$y, df$long, df$lat) %>% as.logical()
}

# Read shapefile with NUTS-1/2/3 regions of Europe
dsn <- "/home/mori/Data/GIS/NUTS 2013/data/NUTS_RG_10M_2013.shp"
layer <- ogrListLayers(dsn)[1]
EU  <- readOGR(dsn, layer)
# subset spatial object to Germany
DE <- EU[grepl("(DE)", EU$NUTS_ID),]
# plot to see if the subset is correct
plot(DE)

# make dataframes for plotting in gglot
DE.pl0 <- fortify(DE[DE$STAT_LEVL_ == 0,], region = "NUTS_ID")
DE.pl1 <- fortify(DE[DE$STAT_LEVL_ == 1,], region = "NUTS_ID")
DE.pl2 <- fortify(DE[DE$STAT_LEVL_ == 2,], region = "NUTS_ID")
DE.pl3 <- fortify(DE[DE$STAT_LEVL_ == 3,], region = "NUTS_ID")
PLZ.DE <- fortify(PLZ.DE, region="plz") %>% mutate(id = paste(id, "XX", sep=""))
rm(DE)

# show overlap of NUTS regions and PLZ-regions
# subset to Saxony-Anhalt
ggplot() +
  theme_minimal() +
  geom_polygon(data=DE.pl3 %>% filter(grepl("(DEE)", id)), 
               aes(x=long, y=lat, group=group), color=alpha("red", 0.2), fill=NA, size=0.75) +
  geom_polygon(data=DE.pl1 %>% filter(id == "DEE"), 
               aes(x=long, y=lat, group=group), color=alpha("black", 0.9), fill=NA, size=1) +
  # subsetting here is a little crude, since PLZ dont differentiate between states
  geom_polygon(data=PLZ.DE %>% filter(grepl("([03]..XX)", id)), 
               aes(x=long, y=lat, group=group), color=alpha("black", 0.5), fill=NA, size=0.5) +
  coord_map()


###############################################################################
####################### make simple subset ####################################
###############################################################################
dic <- PLZ.centers[subsetPLZ(PLZ.centers[,1:2], DE.pl1 %>% filter(id == "DEE")),]$id
PLZ.subset.ids <- !(match(PLZ.DE$id, dic) %>% is.na())
set_A <- PLZ.DE %>% filter(PLZ.subset.ids)
set_B <- DE.pl3 %>% filter(grepl("(DEE)", id)) 

ggplot() +
  theme_minimal() +
  geom_polygon(data=set_A, 
               aes(x=long, y=lat, group=group), color=alpha("red", 0.2), fill=NA, size=0.75) +
  geom_polygon(data=set_B, 
               aes(x=long, y=lat, group=group), color=alpha("black", 0.9), fill=NA, size=1) +
  coord_map()

set_A <- set_A %>% select(long, lat, id)
set_B <- set_B %>% select(long, lat, id)

set_A <- split(set_A, f = set_A$id)
set_B <- split(set_B, f = set_B$id)

###############################################################################
###############################################################################
###############################################################################
## two different sets of polygons 1,2,...,n and A,B,...,p
## 
## for(X = 1 to n):
##    for(Y = A to p):
##      PIP(X,Y) > pip_X := vector(bools)         (check which points of X lie within Y)
##        if(pip_X)                               (if any point of X lies within Y ..)
##          PIP(Y,X) > pip_Y := vector(bools)     (.. check which points of Y lie within X)
##          new_poly_X_in_Y := c(Y[pip_Y], X[pip_X]) (make new polygon from points that lie within each other)
##

x<-sapply(set_A, function(A){
  sapply(set_B, function(B){
    A_in_B <- point.in.polygon(A$long, A$lat, B$long, B$lat) %>% as.logical
    if(sum(A_in_B)) {
      B_in_A <- point.in.polygon(B$long, B$lat, A$long, A$lat) %>% as.logical
      poly_out <- data.frame(long = c(A$long[A_in_B], B$long[B_in_A]),
                             lat = c(A$lat[A_in_B], B$lat[B_in_A]),
                             A = unique(A$id),
                             B = unique(B$id),
                             from = c(rep(unique(A$id), sum(A_in_B)),
                                      rep(unique(B$id), sum(B_in_A))),
                             id = c(row.names(A)[A_in_B],
                                    row.names(B)[B_in_A]))
      poly_out
    }
  })
})

# filter out cells that are not null
x <- x[apply(x, c(1,2), function(x) !is.null(unlist(x)))]

# the problem is, that the points taken from different polygons aren't in order
id <- 4
ggplot() +
  geom_polygon(data=x[[id]], aes(x=long, y=lat, group=A), fill="grey20", alpha=0.2) +
  geom_point(data=x[[id]], aes(x=long, y=lat, group=A, color=from), size=3) +
  geom_polygon(data=x[[id]] %>% filter(from == "063XX"), aes(x=long, y=lat, group=A), fill=NA, color="black", linetype=2) +
  coord_map()

#######################################################################################
####################### calculate overlapping areas via MC     ########################
#######################################################################################

set_A <- PLZ.DE %>% filter(PLZ.subset.ids)
set_B <- DE.pl3 %>% filter(grepl("(DEE)", id)) 

minmax_xy <- c(xmin = min(set_A$long, set_B$long), xmax = max(set_A$long, set_B$long),
               ymin = min(set_A$lat, set_B$lat), ymax = max(set_A$lat, set_B$lat))

N=10000
points <- data.frame(x=runif(N, minmax_xy["xmin"],minmax_xy["xmax"]),
                     y=runif(N, minmax_xy["ymin"],minmax_xy["ymax"]))

# calculate how many points were rained on each polygon of each set of polygons
pip_A <- sapply(split(set_A, f = set_A$id), function(A){
  PinPolyA <- point.in.polygon(points$x, points$y, A$long, A$lat) %>% as.logical
  lapply(split(set_B, f = set_B$id), function(B){
    PinPolyB <- point.in.polygon(points$x, points$y, B$long, B$lat) %>% as.logical
    out <- PinPolyB + PinPolyA  # add the booleans together:
                                # points that lie within both polygons get a 2, 
                                # points within just one of them get a 1 and point not within are false (0)
    # set the points within Polygon B to 0 (we're just interested in how A is split up between B's)
    out[!PinPolyA] <- 0        
    out
  })
})

# calculate the ratio of Polygon A (PLZ) in Polygon B (NUTS 3)
mat <- apply(pip_A, c(1,2), function(x){
  x <- unlist(x)
  sum(x == 2)/sum(x > 0)
})

# select only columns (PLZ) which have overlaps
mat <- mat[,colSums(mat) > 0]
knitr::kable(mat, digits=2)
colSums(mat)


# plot one combination to check if it worked
pip_A.df <- as.data.frame(pip_A)
df1 <- cbind(points, key=pip_A.df$`064XX`$DEE09)

ggplot() +
  theme_minimal() +
  geom_point(data=df1, aes(x=x,y=y,color=as.factor(key)), alpha=0.8, size=0.8) + # classified point according to MC
  geom_polygon(data=set_A, 
               aes(x=long, y=lat, group=group), fill=NA, color=alpha("grey30", 1), size=1) +
  geom_polygon(data=set_B, 
               aes(x=long, y=lat, group=group), color=alpha("grey50", 1), fill=NA, size=2) +
  coord_map()


#######################################################################################
#######################################################################################
#######################################################################################

# calculatre distance between two two dimensional points
dist0 <- function(x1, y1, x2, y2){
  sqrt(abs(x1-x2)^2+abs(y1-y2)^2)
}

a<-sapply(x, function(df){
  df <- split(df, f = df$from)
  if(length(df) > 1){
    apply(df[[2]],1, function(B){
      print(B[1])
      tmp_dist <- dist0(df[[1]]$long, df[[1]]$lat, as.numeric(B[1]), as.numeric(B[2]))
      #id <- df[[1]]$id[which(tmp_dist == min(tmp_dist))]
      #id
      print(min(tmp_dist))
      tmp_dist
    })
  }
})

