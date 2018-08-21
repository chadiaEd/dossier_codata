# Documentation ---
# Description: first steps with R
# Author: Chadia ED-Driouch
# Date: Aug 7, 2018
# Objects ----
name <- "Chadia"
num <- 600258776
logi <- TRUE
typeof(logi)


# Data Structures ----
# Vector have one type 
# matrix 2 dimensionalvector all same type
# data frame can hold a different data type

# load packages ----
library
library(tidyverse)
?tidyverse

# Explore dataset ----

?download.file

download.file( url= "https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/data/gapminder-FiveYearData.csv", destfile = "Data/gapminder.csv")
# Explore dataset

?read_csv

gapminder <- read_csv(file = "Data/gapminder.csv")


#tasting new functions

# str structure

?str

str(gapminder)

head
head(gapminder)

tail(gapminder)

dim(gapminder)

ncol(gapminder)

# view for less than 100thousands rows
View(gapminder)

# subseting  ----
gapminder$country

class(gapminder$country)

length(gapminder$country)

?unique
unique(gapminder$country)

length(unique(gapminder$country))

#cuolumn
gapminder[1]
gapminder[gapminder$country =="Morocco",]
which(gapminder$country =="Morocco")



