AUTHOR: NAVEEN BALARAJU


#########################################################################################
################################# LOADING DATASET #######################################
#########################################################################################

## Primate.Scapulae is a Dataset that contains the pysiological measurment
## of the scapula bone from 5 different genera of primates i.e 
## 1.Gibbons(Hylobates)
## 2.Orangutans(Pongo)
## 3.Chimpanzees(Pan)
## 4.Gorillas(gorilla)
## 5.Man(Homo)
## The data set consists of 11 variables and 105 observations.The variables "AD.BD","AD.CD","EA.CD" 
## "Dx.CD","SH.ACR" are the 5 different indices of the scapulae bones.The variable
## "EAD","beta","gamma" are the angles related to the bone.Since, gamma angle is
## is missing for Homo,the gamma variable is deleted!


load(file = "/Users/naveen/Desktop/SDM\ 2/assignment\ 2/primate.scapulae.rda")
summary(primate.scapulae)  
primate.scapulae[["gamma"]]=NULL
dummy_data=primate.scapulae
###########################################################################################
###############################  EXPLORATORY DATA ANALYSIS ################################
###########################################################################################

## Analysis of "genus" variable
quartz()
hist(primate.scapulae$genus,col = rainbow(14),main = "HISTOGRAM OF GENUS",
     xlab = "Genus")
## Genus is a numeric vector indicating different orders of the primates.
## 59-Homo;58-Gorilla;57-Pan;56-pongo;54-Hylobates. From, the histogram
## I can infer that 38% of genus is Homo,13.3% is gorilla,19% pan,14.2 % pongo
## and 15.5 % hlyobates

## Analysis of "AD.BD" Index
quartz()
hist(primate.scapulae$AD.BD,col = rainbow(14),ylim = c(0,30),xlab = "AD.BD INDEX",
     main = "HISTOGRAM OF AD.BD INDEX")
## From the histogram, I can infer that 4% of the observations have AB.BD index
## in the range of 20-30, 26% between 30-40, 23% between 40-50, 10% between 50-60,
## 14 % between 60-70,22% between 70-80 and only 1% above 80.

## Analysis of "AD.CD" Index
quartz()
hist(primate.scapulae$AD.CD,col = rainbow(14),xlim = c(0,250),xlab = "AD.BD INDEX",
     main = "HISTOGRAM OF AD.CD INDEX")
## From the histogram, I can infer that about 52% of observations have
## AD.CD index values between 23-50, 28% between 50-100 and the rest
## 20% have more than 100

## Analysis of "EA.CD" Index
quartz()
hist(primate.scapulae$EA.CD,xlim = c(40,100),col = rainbow(14),xlab = "EA.CD INDEX",
     main="HISTOGRAM OF EA.CD INDEX")
## From the histogram, I can infer that about 25% of the observations have EA.CD index value
## between 45-60, 35% between 60-75 and 40% above 75

## Analysis of "Dx.CD" Index
quartz()
hist(primate.scapulae$Dx.CD,col = rainbow(14),xlab = "Dx.CD INDEX",
     main = "HISTOGRAM OF Dx.CD INDEX",ylim = c(0,35))
## From the histogram, I can infer that about 24% of the observations have Dx.CD index
## between 11-12.5, 57% between 12.5-13.5 and 19% above 13.5

## Analysis of "SH.ACR" Index
quartz()
hist(primate.scapulae$SH.ACR,col = rainbow(14),xlab = "SH.ACR INDEX",ylim = c(0,25),
     main = "HISTOGRAM OF SH.ACR INDEX")
## From the histogram,I can infer that about 8% of the observations have SH.ACR INDEX
## value between 40-50, 34% between 50-60, 40% between 60-70 and 18% above 70

## Analysis of EAD Angle
quartz()
hist(primate.scapulae$EAD,col = rainbow(14),xlab = "EAD ANGLE",ylim = c(0,25),
     main = "HISTOGRAM OF EAD ANGLE")
## From the histogram,I can infer that about 21% of the observation have EAD angle
## between 90-100, 37% between 100-110,28% between 110-120 and 14% greater than 120

## Analysis of beta Angle
quartz()
hist(primate.scapulae$beta,col = rainbow(14),main = "HISTOGRAM OF BETA ANGLE",
     ylim = c(0,25),xlim = c(10,60),xlab = "BETA ANGLE")
## From the histogram, I can infer that about 17% of the observations have 
## Beta angle between 10-20, 29% between 20-30,18% between 30-40 and 36%
## above 40


#########################################################################################
################################# HIERARCHIAL CLUSTERING ################################
#########################################################################################

## Since genus,class and class digits have specific values of each genera
## of primates,choosing these variables in calculating dissimilarity measure
## is not a good choice
## genus   class         classdigits
## 54     hylobates          1
## 56     pongo              2
## 57     pan                3
## 58     gorilla            4
## 59     Homo               5

## For the futher analysis, I have not considered class,classdigits
primate.scapulae[["class"]]=NULL
primate.scapulae[["classdigit"]]=NULL


## Hierarchical clustering with single linkage using Euclidean distance as the
## dissimilarity measure

## scaling the Data before finding the dissimilarity measure
library(ggplot2)
scaled_data=scale(primate.scapulae)
single_model=hclust(dist(scaled_data),method = "single")
quartz()
plot(single_model,labels=dummy_data$class, main = "DENDROGRAM WITH SINGLE LINKAGE",cex=0.7)
cut_single=cutree(single_model,k=4)
rect.hclust(single_model,k=4,border = "blue")
s2= silhouette(cut_single,dist = dist(scaled_data))
quartz()
plot(s2,main="silhouette plot for k=4 Single linkage",cex=0.7)

## The dendrogram for single linkage shows  that Homo,Hylobates and pongo forms seprate
## clusters, while pan and gorilla are combined into one cluster showing that
## these two variables are correlated and hence I have choosen k= 4 and for
## the further analysis,I have assumed pan and gorilla as one cluster. The dendrogram
## generated by single linkage show many singleton clusters that are less compact
## so choosing Sigle linkage is not a good choice. This method peforms the worst
## with more intracluster and intercluster distances and also low silhoutte values 0.32





## Hierarchical clustering with Average linkage using Euclidean distance as the
## dissimilarity measure

Avg_model=hclust(dist(scaled_data),method = "average")
quartz()
plot(Avg_model,labels=dummy_data$class,main = "DENDROGRAM WITH AVERAGE LINKAGE",cex=0.7)
cut_Avg=cutree(Avg_model,k=4)
rect.hclust(Avg_model,k=4,border = "red")
s1= silhouette(cut_Avg,dist = dist(scaled_data))
quartz()
plot(s1,main="silhouette plot for k=4 Average linkage")

## The dendrogram for Average linkage shows that Homo,Hylobates and pongo forms seprate
## clusters, while pan and gorilla are combined into one cluster showing that
## these two variables are correlated and hence I have choosen k= 4.The dendrogram
## produced by average linkage shows 4  compact clusters, with less intracluster and
## intercluster distances.This method performs the best as it  mis classifies one 
## Hylobates point and have a silhouette value 0.43





## Hierarchical clustering with complete linkage using Euclidean distance as the
## dissimilarity measure
comp_model=hclust(dist(scaled_data),method = "complete")
quartz()
plot(comp_model,labels=dummy_data$class,main = "DENDROGRAM WITH COMPLETE LINKAGE",cex=0.6)
cut_comp=cutree(comp_model,k=4)
rect.hclust(comp_model,k=4,border = "red")
s= silhouette(cut_comp,dist = dist(scaled_data))
quartz()
plot(s,main="silhouette plot for k=4 complete linkage")

## The dendrogram for complete linkage shows that Homo,Hylobates and pongo forms seprate
## clusters, while pan and gorilla are combined into one cluster showing that
## these two variables are correlated and hence I have choosen k= 4.The dendrogram
## produced by average linkage shows 4 compact clusters, with less intracluster and
## intercluster distances.This method also have a silhouette value 0.43, which is similar
## to othe average linakge but mis classifies two hylobates points as pongo and hence 
## I infer hierarchical clustering with avearage linkage is the best suited for this
## data set. 
## The clustering results did not match my expecataions as there were 5 different group
## of primates, it gave me an  impression that there are 5 clusters in the data set,
## but after the analysis it was found that there are 4 clusters with pan and gorilla
## combined in a single cluster




##########################################################################################
################################### K-MEDOIDS ############################################
##########################################################################################
library("cluster")
library("fpc")
medo_k=pamk(scaled_data)
medo_k$nc  # number of clusters

## From the K-medoids technique, it is found that there are  4 clusters in the 
## in the dataset,which matches the number of clusters produced due to hierarchical
## clustering



names(medo_k)
table(medo_k$pamobject$clustering, dummy_data$class)

layout(matrix(c(1,2), 1, 2))
plot(medo_k$pamobject)

## To check for for k=5
k_med5=pamk(scaled_data,5)
table(k_med5$pamobject$clustering, dummy_data$class)
layout(matrix(c(1,2), 1, 2))
plot(k_med5$pamobject)

## By analyzing  the plots generated by the k-medoids, it shows that there are 4 cluters
## and also the correspondig silhouette values is 0.44.
## On comparing the clusters generated by the Hierarchical clustering and by the analytical methods
## I conclude that hierarchical cluterings methods provide better interpretable results
## via dendrograms




