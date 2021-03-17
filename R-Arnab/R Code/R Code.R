#title: "Final Code"
#author: "Arnab Suklabaidya"
#date: "20/02/2021"


#install packages

install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")

#loading required libraires
library(tidyverse)
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)


# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

# Excel XLSX
rio_xlsx <- import("C:/Users/ARNAB/Desktop/R-Arnab/Data Set/GDP Data.xlsx")
head(rio_xlsx)

#Loading data onto my data object
mydata = rio_xlsx
str(mydata)

#.......Data Summarization.........#

firstyear = mydata$X2027
secondyear = mydata$X2037
thirdyear = mydata$X2047
forthyear = mydata$X2067
LR = mydata$X2087

#calculate mean
mean(firstyear)
mean(secondyear)
mean(thirdyear)
mean(forthyear)
mean(LR)

#calculate Median
median(firstyear)
median(secondyear)
median(thirdyear)
median(forthyear)
median(LR)

#mode Function
Mode= function(x){
  ta = table(x)
  tam = max(ta)
  if(all(ta==tam))
    mod= NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta==tam])
  else
    mod=names(ta)[ta==tam]
  return(mod)
  }

#calculate Mode
Mode(firstyear)
Mode(secondyear)
Mode(thirdyear)
Mode(forthyear)
Mode(LR)

#measure of variability

#Calculating standard deviation
sd(firstyear)
sd(secondyear)
sd(thirdyear)
sd(forthyear)
sd(LR)

#Calculating Varience
var(firstyear)
var(secondyear)
var(thirdyear)
var(forthyear)
var(LR)

#Calculating IQR
quantile(firstyear)
quantile(secondyear)
quantile(thirdyear)
quantile(forthyear)
quantile(LR)


#......Data Visualization......#


#data Plot of 2027 wrt the countries
ggplot(data = mydata) +
  geom_point(mapping = aes(x= X2027, y=COUNTRY, 
                           col = "white"	))

#data Plot of 2037 wrt the countries
ggplot(data = mydata) +
  geom_point(mapping = aes(x= X2037, y=COUNTRY,
                         
                           col = "red"))

#data Plot of 2047 wrt the countries
ggplot(data = mydata) +
  geom_point(mapping = aes(x= X2047, y=COUNTRY,  
                           col = "white"	))

#data Plot of 2067 wrt the countries
ggplot(data = mydata) +
  geom_point(mapping = aes(x= X2067, y=COUNTRY,  
                           col = "white"	))


#data Plot of 2087 wrt the countries
ggplot(data = mydata) +
  geom_point(mapping = aes(x= X2087, y=COUNTRY,	 
                           col = "white"))


str(mydata)
Con = mydata$COUNTRY

# Numeric Data _ Histogram

hist(firstyear ,
     breaks = 5,
     main = "2027",
     
     col = "light blue")

hist(secondyear ,
     breaks = 5,
     main = "2037",
     xlab = "",
     col = "light blue"
    )

  hist(thirdyear ,
     breaks = 5,
     main = "2047",
     xlab = "",
     col = "light blue")
hist(forthyear ,
     breaks = 5,
     main = "2067",
    
     col = "light blue")

hist(LR ,
     breaks = 5,
     main = "2087",
     xlab = "",
     col = "light blue")

#catagorical data - BarPlot
table(Con)
barplot(table(Con))


#.....Data Normalization....#
str(mydata)

#select all numeric varaibles in the dataset
mydata_numeric = select(mydata,c(2,3,4,5,6))
View(mydata_numeric)

#ploting the numeric data
plot(mydata_numeric,
     col = "#cc0000",  # Hex code for datalab.cc red
     pch = 19,         # Use solid circles for points
   
     )





#....K means clustering.....#

#WSS Plot function
wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data, 2, var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
    plot(1:nc, wss, type = "b", xlab = "NUmber of Clusters", 
         ylab = "Within groups sum of squares")
  
  }


# WSS plot to choose maximum number of clusters
wssplot(mydata_numeric)

#spoting the kink in the curve

#kmeans clustering
KM = kmeans(mydata_numeric, 2)

#Evaluating cluster analysis

#cluster Plot
autoplot(KM, mydata_numeric, frame= TRUE)

#cluster Centers
KM$centers




# creating the PCA obj using  mydata set


mydata_numeric.pca <- mydata_numeric[c( 2, 3, 4)] 
pca.obj <- prcomp(mydata_numeric.pca)

# ggfortify way - w coloring
autoplot(pca.obj) + theme_minimal()  


# ggplot2 way - w coloring
library(ggplot2)
dtp <- data.frame('GDP' = mydata$X2027, pca.obj$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
  geom_point(aes(x = PC1, y = PC2, col = GDP)) + 
  theme_minimal() 




# Ward Hierarchical Clustering


d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")




  # CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
