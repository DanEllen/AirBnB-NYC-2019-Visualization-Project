#Loading AirBnb NYC 2019 Data
airbnb_data = read.csv(file = './AB_NYC_2019.csv')

#Understanding the data
names(airbnb_data)
str(airbnb_data)
summary(airbnb_data)

#Objective.	Map with options to see density (heat map) based on price,
# number of listings, and reviews per month. 

#Leaflet is one of the most popular open-source JavaScript libraries for 
#interactive maps. This leaflet R package makes it easy to integrate and control
#Leaflet maps in R. #KernSmooth provides the function bkde2d which computes 
#a 2D Binned Kernel Density Estimate. #MASS provides the function kde2d which
#gives a two-dimensional kernel density estimation with an axis-aligned 
#bivariate normal kernel, evaluated on a square grid.



install.packages("leaflet")
intall.packages('KernSmooth') 
intall.packages('MASS') 
library(leaflet)
library(KernSmooth)
library(MASS)

x=kde2d$x1
y=kde2d$x2
z=kde2d$fhat
CL=contourLines(x , y , z)

map = leaflet() %>% addTiles()



bkde2D()

