#' ---
#' title: Spatial data manipulation for Eurostat package
#' author: Markus Kainu
#' date: "started Mar 19 2016 - Last updated: **`r Sys.time()`**"
#' output: 
#'   html_document: 
#'     toc: true
#'     toc_float: true
#'     number_sections: yes
#' ---
#'
#'
#' ***
#' 
#' <b>Please email <a href="mailto:markuskainu@gmail.com?Subject=Spatial data manipulation for Eurostat package" target="_top">Markus Kainu</a> 
#' if any questions!</b>
#' 
#' source code: [analysis.R](./analysis.R)
#' 
#' ***

#+ setup, include = F
library(knitr)
knitr::opts_chunk$set(list(echo=TRUE,
                           eval=TRUE,
                           cache=FALSE,
                           warning=FALSE,
                           message=FALSE))

# create folders
dir.create("./zip/", recursive = TRUE)
dir.create("./rawdata/", recursive = TRUE)
dir.create("./rdata/", recursive = TRUE)


#' # Download and manipulate spatial data
#' 
#' We are: 
#' 
#' 1. downloading 2013 polygon shapefiles from [European Commission/Eurostat/GISCO](http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts)
#' in four different resolution 1:60 mln, 1:20 mln, 1:10 mln, 1:3 mln and 1:1 mln.
#' 2. fortifying the `SpatialPolygonDataFrame`-objects into regular `data.frame`-objects using `ggplot2::fortify()`
#' 3. saving the `data.frame` objects into RData-files with their respective names to be used in 
#' `eurostat::merge_with_shape()`-function
#'
#' ***
#' 
#' ## Conditions for data use 
#' 
#' from [http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units](http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units)
#' 
#' **Administrative units / Statistical units**
#' 
#' In addition to the general copyright and licence policy applicable to the whole Eurostat website, the following specific provisions apply to the datasets you are downloading. The download and usage of these data is subject to the acceptance of the following clauses:
#' 
#' 1. The Commission agrees to grant the non-exclusive and not transferable right to use and process the Eurostat/GISCO geographical data downloaded from this page (the "data").
#' 2. The permission to use the data is granted on condition that:
#' 
#'     1. the data will not be used for commercial purposes;
#'     2. the source will be acknowledged. A copyright notice, as specified below, will have to be visible on any printed or electronic publication using the data downloaded from this page.
#'     
#' **Copyright notice**
#' 
#' When data downloaded from this page is used in any printed or electronic publication, in addition to any other provisions applicable to the whole Eurostat website, data source will have to be acknowledged in the legend of the map and in the introductory page of the publication with the following copyright notice:
#' - `EN: © EuroGeographics for the administrative boundaries`
#' - `FR: © EuroGeographics pour les limites administratives`
#' - `DE: © EuroGeographics bezüglich der Verwaltungsgrenzen`
#' 
#' For publications in languages other than English, French or German, the translation of the copyright notice in the language of the publication shall be used.
#' 
#' If you intend to use the data commercially, please contact [EuroGeographics](http://www.eurogeographics.org/) for information regarding their licence agreements
#' 
#' ***
#'  
#'    
#+ load_and_fortify
library(rgdal)
library(ggplot2)
library(maptools)
library(rgeos)
library(dplyr)
# Load the GISCO shapefile
if (!file.exists("./rdata/NUTS_2013_60M_SH.RData")){
  
  if (!file.exists("./rawdata/NUTS_2013_60M_SH/data/NUTS_RG_60M_2013.shp")){
    download.file("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2013_60M_SH.zip",
                  destfile="./zip/NUTS_2013_60M_SH.zip")
    unzip("./zip/NUTS_2013_60M_SH.zip", exdir = "./rawdata/")
    NUTS_2013_60M_SH_SPDF <- readOGR(dsn = "./rawdata/NUTS_2013_60M_SH/data", layer = "NUTS_RG_60M_2013", verbose=FALSE)
    save(NUTS_2013_60M_SH_SPDF, file="./rdata/NUTS_2013_60M_SH_SPDF.RData")
  } else load("./rdata/NUTS_2013_60M_SH_SPDF.RData")
  shape <- NUTS_2013_60M_SH_SPDF
  shape$id <- rownames(shape@data)
  map.points <- fortify(shape, region = "id")
  map.df <- merge(map.points, shape, by = "id")
  map.df <- arrange(map.df, order)
  NUTS_2013_60M_SH_DF <- map.df
  save(NUTS_2013_60M_SH_DF, file="./rdata/NUTS_2013_60M_SH_DF.RData")
  
  if (!file.exists("./rawdata/NUTS_2013_20M_SH/data/NUTS_RG_20M_2013.shp")){
    download.file("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2013_20M_SH.zip",
                  destfile="./zip/NUTS_2013_20M_SH.zip")
    unzip("./zip/NUTS_2013_20M_SH.zip", exdir = "./rawdata/")
    NUTS_2013_20M_SH_SPDF <- readOGR(dsn = "./rawdata/NUTS_2013_20M_SH/data", layer = "NUTS_RG_20M_2013", verbose=FALSE)
    save(NUTS_2013_20M_SH_SPDF, file="./rdata/NUTS_2013_20M_SH_SPDF.RData")
  } else load("./rdata/NUTS_2013_20M_SH_SPDF.RData")
  shape <- NUTS_2013_20M_SH_SPDF
  shape$id <- rownames(shape@data)
  map.points <- fortify(shape, region = "id")
  map.df <- merge(map.points, shape, by = "id")
  map.df <- arrange(map.df, order)
  NUTS_2013_20M_SH_DF <- map.df
  save(NUTS_2013_20M_SH_DF, file="./rdata/NUTS_2013_20M_SH_DF.RData")
  
  if (!file.exists("./rawdata/NUTS_2013_10M_SH/data/NUTS_RG_10M_2013.shp")){
    download.file("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2013_10M_SH.zip",
                  destfile="./zip/NUTS_2013_10M_SH.zip")
    unzip("./zip/NUTS_2013_10M_SH.zip", exdir = "./rawdata/")
    NUTS_2013_10M_SH_SPDF <- readOGR(dsn = "./rawdata/NUTS_2013_10M_SH/data", layer = "NUTS_RG_10M_2013", verbose=FALSE)
    save(NUTS_2013_10M_SH_SPDF, file="./rdata/NUTS_2013_10M_SH_SPDF.RData")
  } else load("./rdata/NUTS_2013_10M_SH_SPDF.RData")
  shape <- NUTS_2013_10M_SH_SPDF
  shape$id <- rownames(shape@data)
  map.points <- fortify(shape, region = "id")
  map.df <- merge(map.points, shape, by = "id")
  map.df <- arrange(map.df, order)
  NUTS_2013_10M_SH_DF <- map.df
  save(NUTS_2013_10M_SH_DF, file="./rdata/NUTS_2013_10M_SH_DF.RData")
  
  if (!file.exists("./rawdata/NUTS_2013_03M_SH/data/NUTS_RG_03M_2013.shp")){
    download.file("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2013_03M_SH.zip",
                  destfile="./zip/NUTS_2013_03M_SH.zip")
    unzip("./zip/NUTS_2013_03M_SH.zip", exdir = "./rawdata/")
    NUTS_2013_03M_SH_SPDF <- readOGR(dsn = "./rawdata/NUTS_2013_03M_SH/data", layer = "NUTS_RG_03M_2013", verbose=FALSE)
    save(NUTS_2013_03M_SH_SPDF, file="./rdata/NUTS_2013_03M_SH_SPDF.RData")
  } else load("./rdata/NUTS_2013_03M_SH_SPDF.RData")
  shape <- NUTS_2013_03M_SH_SPDF
  shape$id <- rownames(shape@data)
  map.points <- fortify(shape, region = "id")
  map.df <- merge(map.points, shape, by = "id")
  map.df <- arrange(map.df, order)
  NUTS_2013_03M_SH_DF <- map.df
  save(NUTS_2013_03M_SH_DF, file="./rdata/NUTS_2013_03M_SH_DF.RData")

  if (!file.exists("./rawdata/NUTS_2013_01M_SH/data/NUTS_RG_01M_2013.shp")){
    download.file("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2013_01M_SH.zip",
                  destfile="./zip/NUTS_2013_01M_SH.zip")
    unzip("./zip/NUTS_2013_01M_SH.zip", exdir = "./rawdata/")
    NUTS_2013_01M_SH_SPDF <- readOGR(dsn = "./rawdata/NUTS_2013_01M_SH/data", layer = "NUTS_RG_01M_2013", verbose=FALSE)
    save(NUTS_2013_01M_SH_SPDF, file="./rdata/NUTS_2013_01M_SH_SPDF.RData")
  } else load("./rdata/NUTS_2013_01M_SH_SPDF.RData")
  shape <- NUTS_2013_01M_SH_SPDF
  shape$id <- rownames(shape@data)
  map.points <- fortify(shape, region = "id")
  map.df <- merge(map.points, shape, by = "id")
  map.df <- arrange(map.df, order)
  NUTS_2013_01M_SH_DF <- map.df
  save(NUTS_2013_01M_SH_DF, file="./rdata/NUTS_2013_01M_SH_DF.RData")

}

#' As a result we got five RData files of following sizes

#+ print_filesizes
f <- as.data.frame(list.files("./rdata/", full.names = T), stringsAsFactors = FALSE)
names(f) <- "filename"
for (rown in 1:nrow(f)){
  f[rown,"size(Mb)"] <- round(file.size(f[["filename"]][[rown]])/1000000,1)
}
print(f)

#' Then I used [`rsync`](https://en.wikipedia.org/wiki/Rsync) for uploading the files
#' to my server at [http://koti.kapsi.fi/~muuankarski/ropengov/eurostat_geodata/rdata/](http://koti.kapsi.fi/~muuankarski/ropengov/eurostat_geodata/rdata/)
#' where the function can access the for time being.
#' 
#' # Example: Poland at different resolution at different NUTS-levels
#' 
#' Additional info:
#' 
#' - [Eurostat](http://ec.europa.eu/eurostat/web/nuts/overview)
#' - [wikipedia "Nomenclature of Territorial Units for Statistics"](https://en.wikipedia.org/wiki/Nomenclature_of_Territorial_Units_for_Statistics)
#' 
#+ example_poland, out.width=c('250px','250px','250px','250px','250px','250px','250px','250px','250px','250px','250px','250px','250px','250px','250px'), fig.show = "hold", fig.height=8, fig.width=6

resolutions <- c("60","20","10","03","01")
nuts_levels <- c(1,2,3)

for (res in resolutions){
  load(url(paste0("http://koti.kapsi.fi/~muuankarski/ropengov/eurostat_geodata/rdata/NUTS_2013_",
                  res,
                  "M_SH.RData")))
  map.df <- get(paste0("NUTS_2013_",res,"M_SH"))
  for (lev in nuts_levels){
    m <- ggplot(data=map.df[grepl("PL", map.df$NUTS_ID) & nchar(as.character(map.df$NUTS_ID)) == lev+1,], 
           aes(x=long,y=lat,group=group)) +
      theme(title=element_text(size=20)) +
      geom_polygon(color="black",fill=NA) +
      labs(title=paste0("Poland at NUTS-",lev," level \n at 1:",res," mln resolution"))
    print(m)
  }
}




#' # Function to attribute the Eurostat data with spatial data
#' 
#' 

#+ function
merge_with_geo <- function(data,geocol="geo",res=60){
  load(url(paste0("http://koti.kapsi.fi/~muuankarski/ropengov/eurostat_geodata/rdata/NUTS_2013_",
                  res,
                  "M_SH.RData")))
  map.df <- get(paste0("NUTS_2013_",res,"M_SH"))
  d <- merge(data,map.df,by.x=geocol,by="NUTS_ID",all.x=TRUE)
  d <- d[order(d$order),] 
  return(d)
}

#' ## Quick demo1 - whole Europe at country level reso 1:60mln

#+ merge_demo
# Data from Eurostat
dat <- eurostat::get_eurostat(id = "tsdtr420", time_format = "num")
# subset to have only a single row per geo
dat <- dat %>% filter(sex == "T", time == 2014)

# merge with geodata
ff <- merge_with_geo(data=dat,geocol="geo",res = "60")

# plot map
ggplot(data=ff, 
       aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=values),color="white")

#' ## Quick demo2 - Poland at NUTS3 level reso 1:1mln

#+ merge_demo2
# Data from Eurosrat
dat <- eurostat::get_eurostat("tgs00026", time_format = "raw")
# subset to have only a single row per geo
dat <- dat %>% filter(time == 2010, grepl("PL",geo), nchar(as.character(geo)) == 4)

# merge with geodata
ff <- merge_with_geo(data=dat,geocol="geo",res = "01")

# plot map
ggplot(data=ff, 
       aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=values),color="white")


#' # Function to classify the plotting variable
#' 
#+ classification_function

categorise_for_map <- function(x, n=5,method="jenks",manual=FALSE,manual_breaks = NULL,decimals=0) {
  
  library(stringr)
  library(classInt)
  if (manual) {
    levs <- as.data.frame(levels(cut(x, 
                                     breaks=manual_breaks,
                                     include.lowest=T,
                                     dig.lab=5)))
  } else {
    levs <- as.data.frame(levels(cut(x, 
                                     breaks=data.frame(classIntervals(x,n=n,method=method)[2])[,1],
                                     include.lowest=T,
                                     dig.lab=5)))
  } 
  names(levs) <- "orig"
  levs$mod <- str_replace_all(levs$orig, "\\[", "")
  levs$mod <- str_replace_all(levs$mod, "\\]", "")
  levs$mod <- str_replace_all(levs$mod, "\\(", "")
  levs$lower <- gsub(",.*$","", levs$mod)
  levs$upper <- gsub(".*,","", levs$mod)
  
  levs$lower <- factor(levs$lower)
  levs$lower <- round(as.numeric(levels(levs$lower))[levs$lower],decimals)
  levs$lower <- prettyNum(levs$lower, big.mark=" ")
  
  levs$upper <- factor(levs$upper)
  levs$upper <- round(as.numeric(levels(levs$upper))[levs$upper],decimals)
  levs$upper <- prettyNum(levs$upper, big.mark=" ")
  
  levs$labs <- paste(levs$lower,levs$upper, sep=" ~< ")
  
  labs <- as.character(c(levs$labs))
  if (manual) {
    y <- cut(x, breaks = manual_breaks,
             include.lowest=T,
             dig.lab=5, labels = labs)
    rm(manual_breaks)
  } else {
    y <- cut(x, breaks = data.frame(classIntervals(x,n=n,method=method)[2])[,1],
             include.lowest=T,
             dig.lab=5, labels = labs)
  }
  y <- as.character(y)
  #if (is.na(y)) {
  y[is.na(y)] <- "No Data"
  y <- factor(y, levels=c("No Data",labs[1:n]))
  y
}

#' ## Quick demo1 - whole Europe at country level reso 1:60mln

#+ classify_demo
# Data from Eurostat
dat <- eurostat::get_eurostat(id = "tsdtr420", time_format = "num")
# subset to have only a single row per geo
dat <- dat %>% filter(sex == "T", time == 2014)

# categorise
dat$cat <- categorise_for_map(dat$values)

# merge with geodata
ff <- merge_with_geo(data=dat,geocol="geo",res = "60")

# plot map
ggplot(data=ff, 
       aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=cat),color="white") +
  scale_fill_brewer(palette ="Oranges")

#' ## Quick demo2 - Poland at NUTS3 level reso 1:1mln

#+ classify_demo2
# Data from Eurostat
dat <- eurostat::get_eurostat("tgs00026", time_format = "raw")
# subset to have only a single row per geo
dat <- dat %>% filter(time == 2010, grepl("PL",geo), nchar(as.character(geo)) == 4)

# categorise
dat$cat <- categorise_for_map(dat$values)

# merge with geodata
ff <- merge_with_geo(data=dat,geocol="geo",res = "01")

# plot map
ggplot(data=ff, 
       aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=cat),color="white") +
  scale_fill_brewer(palette ="Oranges")


#' # Reproducing the map in the paper

#+ map_repro, fig.width=15, fig.height=15
library(eurostat)
# Downloading and manipulating the tabular data
df <- get_eurostat("tgs00026", time_format = "raw")

# subsetting to year 2005 and NUTS-3 level
df <- df %>% filter(time == 2005, nchar(as.character(geo)) == 4)

# categorise
df$cat <- categorise_for_map(df$values)

# merge with geodata
plot_map <- merge_with_geo(data=df,geocol="geo",res = "60")

# plot map
p <- ggplot(data=plot_map, aes(long,lat,group=group))
p <- p + geom_polygon(aes(fill = cat),colour="white",size=.8)
p <- p + scale_fill_manual(values=c("dim grey",RColorBrewer::brewer.pal(n = 5, name = "Oranges"))) 
p <- p + coord_map(project="orthographic", xlim=c(-22,34), ylim=c(35,70))
p <- p + labs(title = paste0("Disposable household incomes in 2005"))
p <- p +  theme(legend.position = c(0.03,0.40), 
                legend.justification=c(0,0),
                legend.key.size=unit(6,'mm'),
                legend.direction = "vertical",
                legend.background=element_rect(colour=NA, fill=alpha("white", 2/3)),
                legend.text=element_text(size=12), 
                legend.title=element_text(size=12), 
                title=element_text(size=16), 
                panel.background = element_blank(), 
                plot.background = element_blank(),
                panel.grid.minor = element_line(colour = 'Grey80', size = .5, linetype = 'solid'),
                panel.grid.major = element_line(colour = 'Grey80', size = .5, linetype = 'solid'),
                axis.text = element_blank(), 
                axis.title = element_blank(), 
                axis.ticks = element_blank(), 
                plot.margin = unit(c(-3,-1.5, -3, -1.5), "cm"))
p <- p + guides(fill = guide_legend(title = "EUR per Year",
                                    title.position = "top", 
                                    title.hjust=0))
p

#' # Poland with labels
#' 
#+ map_poland_fancy, fig.width=7, fig.height=7
library(eurostat)
# Downloading and manipulating the tabular data
df <- get_eurostat("tgs00026", time_format = "raw")

# subsetting to year 2005 and NUTS-3 level
df <- df %>% filter(time == 2005, nchar(as.character(geo)) == 4, grepl("PL",geo))

df_lab <- label_eurostat(df)
names(df_lab) <- paste0("lab",names(df_lab))

df2 <- cbind(df,df_lab)

# categorise
df2$cat <- categorise_for_map(df2$values)

# merge with geodata
plot_map <- merge_with_geo(data=df2,geocol="geo",res = "60")

cnames <- stats:::aggregate.formula(cbind(long, lat) ~ labgeo, data=plot_map, mean) # region names
cnames <- merge(cnames,df2,by="labgeo")

# plot map
p <- ggplot(data=plot_map, aes(long,lat,group=group))
p <- p + geom_polygon(aes(fill = cat),colour="white",size=.8)
p <- p + scale_fill_manual(values=RColorBrewer::brewer.pal(n = 5, name = "Oranges"))
p <- p + geom_label(data=cnames, aes(long, lat, label = paste(labgeo,"\n",values,"€"), group=labgeo,fill=cat), 
                    size=3.5, color="white", fontface="bold", lineheight=.8, show.legend=FALSE)
p <- p + coord_map(project="orthographic")
p <- p + labs(title = paste0("Disposable household incomes in 2005"))
p <- p +  theme(legend.position = c(0.1,0.03), 
                legend.justification=c(0,0),
                legend.key.size=unit(6,'mm'),
                legend.direction = "vertical",
                legend.background=element_rect(colour=NA, fill=alpha("white", 2/3)),
                legend.text=element_text(size=12), 
                legend.title=element_text(size=12), 
                title=element_text(size=16), 
                panel.background = element_blank(), 
                plot.background = element_blank(),
                panel.grid.minor = element_line(colour = 'Grey80', size = .5, linetype = 'solid'),
                panel.grid.major = element_line(colour = 'Grey80', size = .5, linetype = 'solid'),
                axis.text = element_blank(), 
                axis.title = element_blank(), 
                axis.ticks = element_blank(), 
                plot.margin = unit(c(-3,-1.5, -3, -1.5), "cm"))
p <- p + guides(fill = guide_legend(title = "EUR per Year",
                                    title.position = "top", 
                                    title.hjust=0))
p