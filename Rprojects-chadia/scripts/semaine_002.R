library(ggplot2)
install.packages("drat")
drat::addRepo("jr-packages")
install.packages("jrGgplot2")

library(jrGgplot2)
data(Beauty)
head(Beauty)
colnames(Beauty)
dim(Beauty)
ggplot(data=Beauty) + geom_point(aes(x=age, y=beauty))
g = ggplot(data=Beauty)
g1 = g + geom_point(aes(x=age, y=beauty))
g + geom_point(aes(x=age, y=beauty, colour=gender))
#g + geom_point(aes(x=age, y=beauty, alpha=evaluation, colour=gender))
# **************************************

library(jrGgplot2)
install.packages("ggrepel")
library("ggrepel")
data(bond)
data(bond, package = "jrGgplot2")
library("ggplot2")
g = ggplot(data = bond,  mapping = aes(x = Kills, y = Alcohol_Units))
just_connery =  bond[bond == "Connery"]
ggplot(bond, aes(Kills, Alcohol_Units)) + geom_point() + geom_r(data = just_connery, mapping = aes(x=kills, 
                                                                                                            y = Alcohol_Units, label = Actor), colour = "blue")
??geom_
g = ggplot(bond, aes(Kills, Alcohol_Units))
g


d = data.frame(x= 1:50, y = 1:50, z = 0:9)
g =ggplot(d, aes(x,y))
g + geom_point()
g + geom_point(aes(colour = z))

g + geom_point(aes(colour = as.character(z)))

g + geom_point(aes(colour="fhsjhdk")) # on peut ecrire n'importe quoi et on aura le mm resultat 
g + geom_point(alpha = 0.4)

help("stat_smooth")

ggplot(bond, aes(x = Alcohol_Units, y= Kills)) + stat_smooth(aes(colour = Nationality), se = FALSE , method = "lm")
ggplot(bond, aes(x = Alcohol_Units, y= Kills)) + stat_smooth(aes(colour = Nationality), se = FALSE )
#
ggplot(bond, aes(x =Actor, Alcohol_Units
                 )) + stat_summary(geom = "point", fun.y= mean)

ggplot(bond, aes(x =Actor, y = Alcohol_Units )) + ylim(c(0,20)) +
  stat_summary(fun.ymin = function(i) mean(i) - std_err(i),
               fun.ymax = function(i) mean(i) + std_err(i), 
              colour = "steelblue", geom = "errorbar",
              with = 0.2, lwd = 1)+  ylim(c(0, 20))


std_err = function(i)
  dt(0.975, length(i) - 1) * sd(i) / sqrt(length(i))

ggplot(bond, aes(x = Actor, y = Alcohol_Units)) +
  stat_summary(fun.ymin = function(i) mean(i) - std_err(i),
               fun.ymax = function(i) mean(i) + std_err(i),
               colour = "steelblue", geom = "errorbar",
               width = 0.2, lwd = 2) +
  
  ylim(c(0, 20))


ggplot(bond, aes(x = Actor, y = Alcohol_Units)) +
  stat_summary(fun.ymin = function(i) sd(i) - std_err(i),
               fun.ymax = function(i) sd(i) + std_err(i),
               colour = "steelblue", geom = "errorbar",
               width = 0.2, lwd = 2) +
  
  ylim(c(0, 20))

data(movies, package = "ggplot2movies")
g = ggplot(movies, aes(x=length)) + xlim(0, 200) +
  geom_histogram(aes(y=..density..), binwidth=3)

g + facet_grid(Comedy ~ .)





#install.packages("ggridges")
library("ggridges")
ggplot(movies,
       aes(x = length, y = year,
           group = year, height = ..density..)) +
  geom_density_ridges(scale = 10, alpha = 0.7) +
  theme_ridges(grid=FALSE) +
  scale_x_log10(limits = c(1, 500),
                breaks = c(1, 10, 100, 1000),
                expand = c(0.01, 0)) +
  scale_y_reverse(breaks = seq(2000, 1900, by = -20),
                  expand = c(0.01, 0))

#load data 
data(movies, package = "ggplot2movies")

#Canvas & limits 
g = ggplot(subset(movies, budget>0),aes(y = length))+ylim(0,500)
g+geom_point(aes(x=budget))


data(movies, package = "ggplot2movies")
known_budget = movies[!is.na(movies), ]
h = ggplot(known_budget, aes(y = length)) + ylim(0, 500)
h1 = h + geom_point(aes(budget), alpha = 0.2)


h3 = h1 + scale_x_log10()
## Or equivalently
h1 + scale_x_continuous(trans = "log10")



data(bond, package = "jrGgplot2")
g = ggplot(bond, aes(x = Alcohol_Units, y = Kills)) +
  geom_point(aes(colour = Actor))
g + scale_colour_hue(l = 70, c = 60)
g + scale_colour_brewer(palette = "PuOr", type = "div")

g + scale_colour_manual(values = c(
  "Brosnan" = rgb(192,192,192, maxColorValue = 255), #silver
  "Connery" = "Gold",
  "Craig" = rgb(205, 127, 50, maxColorValue = 255), #Bronze
  "Dalton" = "tomato1",
  "Lazenby" = "tomato2",
  "Moore" = "tomato3"))

g + scale_colour_grey()

par(mfrow=c(2, 2))




g = ggplot(bond, aes(x = Actor, y = Alcohol_Units))
(g1 = g + geom_point())
g2 = g + geom_point(aes(fill = Actor))
vplayout = function(x, y)+
  viewport(layout.pos.row = 1, layout.pos.col = 1:2)

print(g1, vp = vplayout(1, 1:2))
print(g1)

library("grid")
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))

library("gridExtra")
grid.arrange(g1, g2, nrow=2)


print(g1, vp = vplayout(1, 1:2))
data(bond)