#les librairies utilisees pour la manipulation de donnees
library(tidyverse) 
library(ggplot2)
library(rafalib)

#data


### lancer cette commande dans le terminal pour telecharger le fichier readCounts.csv
###curl -LO http://raw.githubusercontent.com/fpsom/CODATA-RDA-Advanced-Bioinformatics-2018/master/files/readCounts.csv

# pour importer les donnees : file -> import dataset -> readCounts.csv

readCounts <- read.csv("readCounts.csv")

View(readCounts)
summary(readCounts)
#ggplot; plot basic qui nous permet de visualiser nos donnees sous forme de graphe  
#on ajoute geom_point() 
# pour ne pas avoir de confusion entre le nom de notre fichier et une colonne de fichier on va modifier le nom de fichier
#stat_smooth pour ajouter le modele liniaire

dataSamples <-  readCounts

ggplot(data= dataSamples)+geom_point( aes(x= Sample, y = ReadCount ),size = 3) + 
  stat_smooth(aes(x= Sample, y = ReadCount ), method = "lm",formula = y ~ x , se = TRUE)
###### facet_grid pour afficher chaque chaque plot selon 'Treatemet'

## scale_color_manual pour preciser les couleurs manuelement
## stat_smooth pour ajouter la ligne qui relies les points

ggplot(data=dataSamples) +
  facet_grid( ~ Treatment) +
  geom_point(aes(x = Sample, y = ReadCount, colour = Treatment), size = 3) +
  scale_color_manual(values=c("blue", "green", "red")) +
  stat_smooth(aes(x = Sample, y = ReadCount), method = "lm", formula = y ~ x, se = TRUE) +
  labs(x = "Sample", y = "Read Count", title = "All read counts")
  
# on doit choisir le plot qui contient plus de points regroupees

####### on va utiliser deux facteur 

ggplot( data = dataSamples) +
  facet_grid(Student ~ Treatment) + 
  geom_point( aes(x = Sample, y = ReadCount, color = Treatment), size = 3 ) +
  scale_color_manual( values = c ("blue", "green", "red")  ) +
  stat_smooth( aes ( x = Sample, y = ReadCount), method = "lm", formula = y ~ x, se = TRUE   )

##### Plot 1 : Lane & treatements

ggplot( data = dataSamples) +
  facet_grid(Lane ~ Treatment) + 
  geom_point( aes(x = Sample, y = ReadCount, color = Treatment), size = 3 ) +
  scale_color_manual( values = c ("blue", "green", "red")  ) +
  stat_smooth( aes ( x = Sample, y = ReadCount), method = "lm", formula = y ~ x, se = TRUE   )

##### Plot 2 :  date & treatements


ggplot( data = dataSamples) +
  facet_grid(Library_Prep_Date ~ Treatment  ) + 
  geom_point( aes(x = Sample, y = ReadCount, color = Treatment), size = 3 ) +
  scale_color_manual( values = c ("blue", "green", "red")  ) +
  stat_smooth( aes ( x = Sample, y = ReadCount), method = "lm", formula = y ~ x, se = TRUE   )

#### on va filtrer les donnees


controlPop <- filter(dataSamples, Treatment == "Blue") %>% select(ReadCount) %>% unlist
treatmentPop <- filter(dataSamples, Treatment == "Green") %>% select(ReadCount) %>% unlist
print ( mean(controlPop))
print(mean(treatmentPop))
obsdiff <- abs( mean(treatmentPop) - mean(controlPop))
print(obsdiff)
set.seed(1)
control <- sample(controlPop, 10)
mean(control)

control <- sample(controlPop, 10)
mean(control)

control <- sample(controlPop, 10)
mean(control)

n <- 1000
null <- vector("numeric", n)
for (i in 1:n) {
  # 10 control samples
  control <- sample(controlPop, 10)
  
  # 10 "treatment" samples
  treatment <- sample(controlPop, 10)
  
  null[i] <- mean(treatment) - mean(control)
  
}

mean(null >= obsdiff)

x <- controlPop
smallest  <- floor(min(x))

largest <- ceiling(max(x))

values <- seq(smallest, largest , len = 300)
heightcdf <- ecdf(x)
plot(values, heightcdf(values), type = "l")

bins <- seq(60, 140, 10)
hist(x, breaks = bins)

n <- 500
nullplot(-15, 15, -1, 45, xlab="Observed differences (diff counts)", ylab="Frequency")
totals <- vector("numeric",41)
for (i in 1:n) {
  control <- sample(controlPop, 10)
  treatment <- sample(controlPop, 10)
  nulldiff <- mean(treatment) - mean(control)
  j <- pmax(pmin(round(nulldiff)+20, 41), 1)
  totals[j] <- totals[j]+1
  text(j-20,totals[j],pch=15,round(nulldiff,1))
  # if(i < 15) Sys.sleep(1) ## You can add this line to see values appear slowly
}

hist(null, freq = TRUE)
abline(v = obsdiff, col = "red", lwd = 2)
1 - pnorm(obsdiff, mean(null) , ds(null))


########## T-test
set.seed(1)
controlPop <- filter(dataSamples, Treatment == "Blue") %>% select(ReadCount) %>% unlist
treatmentPop <- filter(dataSamples, Treatment == "Green") %>% select(ReadCount) %>% unlist
control <- sample(controlPop, 10)
treatment <- sample(treatmentPop, 10)
diff <- abs(mean(treatment) - mean(control))
print(diff)


sd(control) / sqrt(length(control))

se <- sqrt(
  var(treatment) / length(treatment) +
    var(control) / length(control) 
)

tstat <- diff /se
print(tstat)

# 0 veut dire qu il n y a pas de difference entre l tratement et control