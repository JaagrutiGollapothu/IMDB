setwd("~/Documents/geetha")
imdb <- read.csv("movie_metadata.csv",header = TRUE, sep = "," )
imdb
library(ggplot2) # ggplot2 package used
#1.Histogram for Variation Of Frequency For Movies Released
hist(imdb$title_year,xlab = "Year of Release",ylab = "Number of Movies released",border = "Red",col = "pink",main = "Variation in Frequency of Movies Released")


#2. most use of language english over all countries and countries using many languages
qplot(country,language,data = imdb,color = language,,main="Linguistic Relation",aes(x=country))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


#3degrading performance of movies though number of movies increasing in ayear
qplot(imdb$title_year,imdb$imdb_score,color = imdb$content_rating,xlab = "Year Of Release",ylab  = "IMDB Score",main = "Degrading performance of Movies") 


#4.Success Rate Based On Actors
par(mfrow = c(2,2))
plot(imdb$movie_facebook_likes,imdb$actor_1_facebook_likes,col = "blue",main = "Plot-Actor1 Fb likes/Movie Fb likes",abline(lm(imdb$actor_1_facebook_likes~imdb$movie_facebook_likes)),cex = 1.3,pch = 16,xlab = "Actor-1 Fb likes",ylab = "Movie Fb likes" );plot(imdb$movie_facebook_likes,imdb$actor_2_facebook_likes,col = "blue",main = "Plot-Actor2 Fb likes/Movie Fb likes",abline(lm(imdb$actor_2_facebook_likes~imdb$movie_facebook_likes)),cex = 1.3,pch = 16,xlab = "Actor-2 Fb likes",ylab = "Movie Fb likes" )
plot(imdb$movie_facebook_likes,imdb$actor_3_facebook_likes,col = "blue",main = "Plot-Actor3 Fb likes/Movie Fb likes",abline(lm(imdb$actor_3_facebook_likes~imdb$movie_facebook_likes)),cex = 1.3,pch = 16,xlab = "Actor-2 Fb likes",ylab = "Movie Fb likes" );plot(imdb$movie_facebook_likes,imdb$cast_total_facebook_likes,col = "blue",main = "Plot-TotalCast Fb likes/Movie Fb likes",abline(lm(imdb$cast_total_facebook_likes~imdb$movie_facebook_likes)),cex = 1.3,pch = 16,xlab = "Total Cast Fb likes",ylab = "Movie Fb likes" )
par(mfrow = c(1,1))


#5.Distribution of IMDb Scorefor Various Genres

#splitting of genere column to atomic values
gen_dup <- imdb$genres
gen_dup
sp2 <- NULL
i <- 1
for(sp in gen_dup){
  sp1 <- strsplit(sp,"|",fixed = TRUE)
  sp2 <- c(sp2,sp1)
}
n = 1
sp4 <- NULL
counter <- c(0)
for(sp in sp2)
{
  for(s in sp){
    
    sp3 <- strsplit(s," ",fixed = TRUE)
    sp4 <- c(sp4,sp3)
    counter[n] = counter[n] + 1
    counter[ n + 1] = c(0)
  }
  
  n = n + 1 
}
gen <- unlist(sp4)
gen
genre_rate = data.frame(x = gen)
genre_rate$gen_rating = 0
z = 1
l = 1
z = 1
l = 1
m = 1
k = 1
for(j in 1:length(counter))
{
  for(k in 1:j)
  {
    genre_rate$gen_rating[m] = imdb$imdb_score[l]
    m = m + 1
  }
  l = l + 1
}
imdb$imdb_score[3]
genre_rate$gen_rating    
write.csv(genre_rate,"genre_rating.csv")
boxplot(genre_rate$gen_rating ~ genre_rate$x,col = c("cadetblue","antiquewhite3","coral3","cornflowerblue","cyan3","blueviolet","dimgrey","darkorange3","darkolivegreen3","deeppink1"),xlab = "Genre",ylab = "IMDb Score",main = "Distribution of IMDb score for various Genres")

