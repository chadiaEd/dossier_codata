

installed.packages("kohonen")
library(kohonen)
help(som)
help(somgrid)


##### sample1 

d1 = read.table("Sample1",header = FALSE)
d1 = as.matrix(d1[,-1])
k=som(d1,somgrid(20,20))

plot(k,type = 'counts')

plot(k)
plot(k,type = 'mapping')

plot(k,type = 'codes')



##### sample2 

d2 = read.table("Sample2",header = FALSE)
d2 = as.matrix(d2[,-1])
k=som(d2,somgrid(20,20))

plot(k,type = 'counts')



##### sample3 

d3 = read.table("Sample3",header = FALSE)
d3 = as.matrix(d3[,-1])
k=som(d3,somgrid(20,20))

plot(k,type = 'counts')

##          8lDAUE28Rfr1
