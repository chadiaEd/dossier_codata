# Documentation ---
# Description: first steps with R
# Author: Chadia ED-Driouch
# Date: Aug 7, 2018
# Objects ----

# Load the packages ----
library(tidyverse)
# Load gapminder dataset
gapminder <- read_csv(file="Data/gapminder.csv")
# Explore the dataset ----
glimpse(gapminder)

# Make some plots ----

plot(gapminder$gdpPercap , gapminder$lifeExp)

?ggplot2

ggplot(data = gapminder, aes(x= gdpPercap,y=lifeExp)) + 
  geom_point()

??geom
# A scatter plot of year vs pop

ggplot(data = gapminder, aes(x= year,y=pop))+geom_point()

# adding shape
ggplot(data = gapminder, aes(x= year,y=pop, shape = continent )) +
  geom_point()

# adding shape
ggplot(data = gapminder, aes(x= year,y=pop, colour = continent )) +
  geom_point()
#unique year 
unique_years <- unique(gapminder$year)

#scale 

# custom scale in the x axis
ggplot(data = gapminder, aes(x= year,y=pop, colour = continent )) +
  geom_point()+
  scale_x_continuous(breaks = unique_years)



ggplot(data = gapminder, aes(x = year, y = pop, colour = continent)) +
  geom_point() +
  scale_x_continuous(breaks = unique_years) +
  scale_y_continuous(breaks = c(0, 100000000, 200000000, 500000000, 1000000000),
                     labels = c(0, "100 mi", "200 mi", "500 mi", "1 billion"))

# creating a historgram of lifeExp

ggplot(data = gapminder, aes(x=lifeExp, fill= continent)) + 
  geom_histogram(bins = 12)


# creating a histogram of each continent lifeExp
ggplot(data = gapminder, aes(x=lifeExp, fill= continent) )+
  geom_histogram(bins = 12) +
  facet_wrap(~ continent)+
  theme_grey()

# Create a line plot of year and lifeExp coloured by continent
ggplot(data = gapminder, aes(x=year, y= lifeExp,color = continent , by =country) )+
  geom_line() 

#scatter plot gdpPercap vs lifeExp with geom_smooth()
ggplot(data = gapminder, aes(x=gdpPercap, y= lifeExp,color = continent ) )+
  geom_point()+
  geom_smooth(method = "lm", size = 2 , colour = "black")+ scale_x_log10()+
  labs(x= "GDP per Capita", y = "Life Expectation")

# Create a line plot for year vs lifeExp colour by 
# continent
#plot only the countries that start with  letter A

starts_with <- substr(gapminder$country, start = 1, stop=1)
my_countries <- gapminder[starts_with %in% c("A"),]
ggplot(data = my_countries, aes(x=year, y= lifeExp,color = continent  ) ) +
  geom_line()+
  facet_wrap(~country )

F1 <- function(fl){
  starts_with <- substr(gapminder$country, start = 1, stop=1)
my_countries <- gapminder[starts_with %in% c("A"),]
ggplot(data = my_countries, aes(x=year, y= lifeExp,color = continent  ) ) +
  geom_line()+
  facet_wrap(~country )
names <- paste(fl, collapse = "_")
#gsave(filename =  paste("figures/Countries_by_letter_" , names, ".png"))
write.csv(x = my_countries, path = "Data/countriesRW.csv")
}

F1("C") 
  
# new method

N_gapminder  <- gapminder[startsWith(gapminder$country, 'A'),]

ggplot(data = N_gapminder , aes(x=year, y= lifeExp,color = continent) )+
  geom_line()+
  facet_wrap(~country )
ggsave(filename =  "Figures/LifeExp_Countries_R_Y.png")



# Save data ----

starts_with <- substr(gapminder$country, start = 1, stop=1)
my_countries <- gapminder[starts_with %in% c("A"),]

write.csv(x = my_countries, file = "Data/countriesRW.csv")
my_countries
View(my_countries)
write.csv()

# R conditionals

  gapminder %>%                 
  filter(continent == "Africa" ) %>%                   
  select(year, country, lifeExp)

#another option
  
  
# select African countries for lifeExp, country and year,
  
  
Morocco_pop <- gapminder[gapminder$country == "Morocco", ] %>% select( year, pop) 
ggplot(data = Morocco_pop , aes(x=year, y=pop ) )+
  geom_line()+
ggsave(filename =  "Figures/Morocco_pop.png")

nrow(af)
write_csv(af, path = "data/african-countries.csv")







