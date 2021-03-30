#######downloading and using a subset of data from NOAA sea surface temperatures in R
# from  https://cran.r-project.org/web/packages/heatwaveR/vignettes/OISST_preparation.html
#The packages we will need to install 
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("tidync")
install.packages("doParallel")
install.packages("rerddap")
install.packages("readxl")
install.packages("xlsx")
install.packages("viridis")
install.packages("hrbrthemes")
install.packages("maps")
install.packages("ggmap")
install.packages(c("leaflet", "sp"))
install.packages("RColorBrewer")


# The packages we will use
library(dplyr) # A staple for modern data management in R
library(tidyr)
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing
library("readxl")
library("xlsx")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(maps)
library(ggmap)
library(RColorBrewer)

library(sp)
library(leaflet)

####PNCC Set setup to 1 if the file is in localmachine and there is no need to get from the cloud

dirname <- "~/Desktop/NOAA/"
dirname <- "c:/users/bencasado/desktop/noaa/"
#pnccdatafile <- "~/Desktop/NOAA/PNCCdatafile.x;"
pnccdatafile <- "C:/users/bencasado/Desktop/NOAA/paola_workingdata_7.xlsx"
#pnccdata <- read.csv(pnccdatafile) 
pnccdata <- read_excel(pnccdatafile) 

#=====================================================
#Plot Code #1
#=====================================================

# xlsx files
pnccdata <- read_excel(pnccdatafile)

pnccdata %>%
  ggplot( aes(x=name, y=mp_conc, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Concentration x Names") +
  xlab("")



#=====================================================
#Scatter Plot
#=====================================================
ggplot(pnccdata, aes(x=name, y=Sepal.Width, color=Species)) + 
  geom_point(size=6) +
  theme_ipsum()



#scatter plot with different categories by color, works with 'numerical' variables
myscatterplot<-ggplot(data = mpdata, aes(x=name, y=mp_conc, colour = grp_name)) + geom_point()+ geom_smooth(method="lm")
myscatterplot + xlim(0,1) + ylim(0,2.0)
myscatterplot + labs(title = "Names vs Concentration", x = "Names", y ="Plastic conc (mg/wweight)")
boxplot(mpdata$mp_conc, main="Concentration", sub=paste("Outlier rows: ", boxplot.stats(mpdata$mp_conc)$out))  # box plot for 'distance'

#another way to do it
scatter.smooth(x=mpdata$Perc_sampl_plastic, y=mpdata$mp_conc, main="Box Plots")  # scatterplot

#=====================================================
#Scatter Plot Code
#=====================================================

# xlsx files
mpdata <- read_excel(pnccdatafile) 

#scatter plot with different categories by color, works with 'numerical' variables
myscatterplot<-ggplot(data = mpdata, aes(x=Perc_sampl_plastic, y=mp_conc, colour = grp_name)) + geom_point()+ geom_smooth(method="lm")
myscatterplot + xlim(0,1) + ylim(0,2.0)
myscatterplot + labs(title = "Plastic conc. vs. Proportion of samples contaminated", x = "% harvested samples with plastic", y ="Plastic conc (mg/wweight)")

#another way to do it
scatter.smooth(x=mpdata$Perc_sampl_plastic, y=mpdata$mp_conc, main="Box Plots")  # scatterplot



#=====================================================
#Box Plots
#=====================================================

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(mpdata$Perc_sampl_plastic, main="Plastic", sub=paste("Outlier rows: ", boxplot.stats(mpdata$Perc_sampl_plastic)$out))  # box plot for 'speed'
boxplot(mpdata$mp_conc, main="Concentration", sub=paste("Outlier rows: ", boxplot.stats(mpdata$mp_conc)$out))  # box plot for 'distance'


#=====================================================
#Map Plot
#=====================================================
pnccdata <- read_excel(pnccdatafile) 
mapdata <- as.data.frame(pnccdata)

world_map <- map_data("world")
mapdata %>%
  # need to swap longitude & latitude
  # changing the names of longitude & latitude to x & y just for clarity
  rename(x = Lat, y = Long) %>%
  # somehow blue points have y values flipped
  mutate(y = ifelse(subspecies == "blue", y * -1, y)) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), data = world_map, fill = "grey21", color = "grey21") +
  geom_point(aes(x = x, y = y, color = subspecies)) +
  scale_color_identity() +
  coord_fixed() +
  xlab("") +
  ylab("")



#=====================================================
#Map Plot better and cleaner
#=====================================================

df<-mapdata
coordinates(df) <- ~Long+Lat
countries <- df[Country]


leaflet(df) %>% addMarkers() %>% addTiles()

#=====================================================
#Map Australia
#=====================================================
map <- get_stamenmap( bbox = c(left = 110, bottom = -40, right = 160, top = -10), zoom = 4, maptype = "watercolor")
ggmap(map) + 
  theme_void() + 
  theme(
    plot.title = element_text(colour = "orange"), 
    panel.border = element_rect(colour = "grey", fill=NA, size=2)
  )


#=====================================================
#Map South America
#=====================================================
map <- get_stamenmap( bbox = c(left = -80, bottom = -54, right = -35, top = 10), zoom = 4, maptype = "watercolor")
ggmap(map) + 
  theme_void() + 
  theme(
    plot.title = element_text(colour = "orange"), 
    panel.border = element_rect(colour = "grey", fill=NA, size=2)
  )



#=====================================================
#HeatMap
#=====================================================


df<-select(mapdata,Long,Lat,mp_conc)

map_bounds <- c(left = 180, bottom = -90, right = -180, top = 90)
coords.map <- get_stamenmap(map_bounds, zoom = 7, maptype = "toner-lite")
coords.map <- ggmap(coords.map, extent="device", legend="none")
coords.map <- coords.map + stat_density2d(data=df,  aes(x=Long, y=Lat, fill=..level.., alpha=..level..), geom="polygon")
coords.map <- coords.map +   scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")))


coords.map <- coords.map + geom_point(data=df,  aes(x=Long, y=Lat), fill="red", shape=23, alpha=0.8)


coords.map <- coords.map + theme_bw()
print(coords.map)
ggsave(filename="./coords.png")




#w <- which(d$lon =='14.875' & d$lat =='38.625') ## finds this particular lon &lat in dataframe
#z2 <- d[w,]
#z3 <-rbind(z,z2)
#going to figure out a comand to automatically ensure the data doesnt duplicate 


# plot ST temp at location x lat y long (THIS IS NOT PLOTTING)
g <- ggplot(data = d3, aes(x=year, y=mean_temp, group = month))
g 
  geom_point() 
  geom_line(aes(colour = month), linetype = 1) 
  ylim(10,30) 
  labs(title = "SST at -56.625 long, -35.125 lat ", #these are not the same coordinates being extracted either 
       x = "month", y ="Sea Surface Temperature (˚C)")

#visualize the data that was downloaded
OISST_data %>% 
  filter(t == "2011-01-01") %>% 
  ggplot(aes(x = lon, y = lat)) 
  geom_tile(aes(fill = temp)) 
  # borders() + # Activate this line to see the global map
  scale_fill_viridis_c() 
  coord_quickmap(expand = F) 
  labs(x = NULL, y = NULL, fill = "SST (°C)") 
  theme(legend.position = "bottom") 
  
  g <- ggplot(data = mpdata, aes(x=Perc_sampl_plastic, y=mp_conc, colour = name))
  g +
    geom_point() +
    xlim(0,1) +ylim(0,2.0) +
    labs(title = "Plastic conc. vs. Proportion of samples contaminated", 
         x = "% harvested samples with plastic", y ="Plastic conc (mg/wweight)")
  
  #############################################################################################################
  install.packages("readxl")
  library("readxl")
  
  # xlsx files
  mpdata <- read_excel(countryfilename) 
  w2 <- which(mpdata$name =='fish') ## finds this particular name
  z2 <- mpdata[w2,]
  z2 
  
  
  #scatter plot with different categories by color, works with 'numerical' variables
  g <- ggplot(data = mpdata, aes(x=Perc_sampl_plastic, y=mp_conc, colour = name))
  g +
    geom_point() +
    xlim(0,1) +ylim(0,2.0) +
    labs(title = "Plastic conc. vs. Proportion of samples contaminated", 
         x = "% harvested samples with plastic", y ="Plastic conc (mg/wweight)")
  
  
  
  
  
   #bar graphs with different categories by color, works for 'categorical' variabales with 'numerical ones'
  b <- ggplot(data = mpdata, aes(x=name, y=Plast_size, colour = mp_type))
  b
    geom_bar((aes(fill=mp_type)), stat="identity", na.rm = TRUE, width=0.5) 
    scale_fill_hue() 
    ylim(0,2.0) 
    labs(title = "Plastic size vs. plastic type", 
         x = "Type of seafood", y ="Plastic size (mm?)")
  

