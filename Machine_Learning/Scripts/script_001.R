

#Packages
install.packages("data.table")
install.packages("ggplot2")
install.packages("recommenderlab")
#load movies.csv file
movies = read.csv("data/movies.csv")

#list the structure of movies
str(movies)

# show first 10 rows of movie dataset

head(movies, n=10) 

# load ratings.csv file

ratings = read.csv("data/ratings.csv") 

# list the structure of movies
str(ratings)

# show first 10 rows of ratings dataset 

head(ratings, n=10) 

# creating a histogram
library(ggplot2)
ggplot(data = ratings, aes(x=timestamp) )+  geom_histogram() 


#exercice 3
library(data.table)
movgen <- as.data.frame(movies$genres, stringsAsFactors=FALSE) 

movgen2 <- as.data.frame(tstrsplit(movgen[,1], '[|]',type.convert = TRUE), stringsAsFactors = FALSE)
  movgen2
colnames(movgen2) <- c(1:7)
 head(movgen2, n=4)
 
 #exercice 4
 
 #Create a matrix with columns representing every unique genre, and indicate 
 # whether a genre was present or not in each movie.
 
 
 
 movgen_list <- c ("Action", "Adventure", "Animation",
                   "Children", "Comedy", "Crime", "Documentary", "Drama",
                   "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery",
                   "Romance", "Sci-Fi", "Thriller", "War", "Western")
  
 
 matrix <- matrix(0,nrow = nrow(movgen2)+1,ncol = length(movgen_list))

matrix=1 (movgen2[movgen2==movgen_list])


movgen2[movgen2==movgen_list]

if(movgen2[movgen2==movgen_list]!="NA"){ matrix=1} else{ matrix = 0}
movgen2[movgen2==movgen_list]!="NA"

matrix[movgen2[movgen2==movgen_list]]
#######

movgen_list <- c ("Action", "Adventure", "Animation",
                  "Children", "Comedy", "Crime", "Documentary", "Drama",
                  "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery",
                  "Romance", "Sci-Fi", "Thriller", "War", "Western")

  movgen_matrix<-matrix(0,9126,18)#empty matrix
movgen_matrix[1,]<-movgen_list #set first row to genre list
colnames(movgen_matrix)#set column names to genre list
#iterate through matrix
for(i in 1:nrow(movgen2)){
  for(c in 1:nrow(movgen2)){
    genmat_col=which(movgen_matrix[1,]==movgen2[i,c])
    movgen_matrix[i+1,genmat_col]<-1}
  
}

#convert dataframe
movgen_matrix2<-as.data.frame(movgen_matrix[-1,],stringsAsFactors = FALSE)#removefirst row,which was the genre list
for (c in 1:ncol(movgen_matrix2)) {
  movgen_matrix2[,c]<-as.integer(movgen_matrix2[,c])
  
}

#exercice 5

#Exercice 5

binary_ratings <- ratings
for (i in 1:nrow(binary_ratings)) {
  if(binary_ratings[i,3] > 3){
    binary_ratings[i,3] <- 1
  }
  else
    binary_ratings[i,3] <- -1
}
head(binary_ratings, n=7)

#installed.packages("reshape2")

library("reshape2")

ratings <- read.csv("data/ratings.csv")

ratings$rating <- as.integer(ratings$rating)

index <- ratings$rating <= 3
ratings$rating[index] <- -1

index <- ratings$rating %in% c(4,5)
ratings$rating[index] <- 1

binary_ratings <- ratings

binary_ratings2 <- dcast(binary_ratings, movieId~userId, value.var = "rating", na.rm=FALSE)

binary_ratings2[is.na(binary_ratings2)] <- 0

binary_ratings2 = binary_ratings2[,-1] #remove movieIds col. Rows are movieId s, cols are userIds

head(binary_ratings2, n= 4)

#########
#Remove rows that are not rated from movies dataset
unique_movieIds <- length(unique(movies$movieId)) #9125
unique_ratings <- length(unique(ratings$movieId)) #9066
movies2 <- movies[-which((unique_movieIds %in% unique_ratings) == FALSE),]
rownames(movies2) <- NULL
#Remove rows that are not rated from movgen_matrix2
movgen_matrix3 <- movgen_matrix2[-which((unique_movieIds %in% unique_ratings)
                                        == FALSE),]
rownames(movgen_matrix3) <- NULL



##

result = matrix(0,18,671)
for( c in 1:ncol(binary_ratings2)){
  for(i in ncol(movgen_matrix3)){
    result[i,c] <- sum((movgen_matrix3[,i]) * (binary_ratings2[,c]))
  }
}

#convert to binary scale

for (i in 1:nrow(result)) {
  for (j in 1:ncol(result)) {
    if(result[i]<0){
      result[i,j] <- 0
      
    }
    else{
      result[i,j] <- 1
    }

  }
}

dim(result)
