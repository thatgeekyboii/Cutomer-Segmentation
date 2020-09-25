#################################  INSTALLING REQUIRED PACKAGES     ##########################
#install.packages("rfm")
#install.packages("descriptr")

#install.packages("magrittr")
#install.packages("lubridate")
#install.packages("kableExtra")
library(arules)
library(lubridate)
library(magrittr)
library(rfm)
library(kableExtra)
#install.packages("plotrix")
library(ggplot2)
#remove.packages("lubridate")


##################################  LOADING THE DATASET    ##############################
customer_data=read.csv("/Users/vaibhav/Downloads/customer-segmentation-dataset/Mall_Customers.csv")

############################ SCANNING THROUGH THE DATA    ################################
str(customer_data)

names(customer_data)

head(customer_data)

summary(customer_data$Age)




#table(customer_data)


sd(customer_data$Age)     #####SD stands for Standard Deviation   [1] 13.96901

summary(customer_data$Annual.Income..k..)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#15.00   41.50   61.50   60.56   78.00  137.00 


sd(customer_data$Annual.Income..k..)  ####  [1] 26.26472

summary(customer_data$Age)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#18.00   28.75   36.00   38.85   49.00   70.00 

sd(customer_data$Spending.Score..1.100.) #[1] 25.82352



############################### BAR PLOT FOR GENDER COMPARISION   ###########################3
a=table(customer_data$Gender)
barplot(a,main="Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))


pct=round(a/sum(a)*100)   #####Calculating the percentage
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)



###################### PIE CHART FOR  RATIO    ####################
pie3D(a,labels=lbs,
      main="PRatio of Female and Male")

########################   PLOTTING AGE COUNT   ####################
hist(customer_data$Age,col="blue",main="Histogram to Show Count of Age Class",
     xlab="Age Range",
     ylab="Frequency",
     labels=TRUE)

########################   PRINTING THE INCOME DETAILS   ####################

summary(customer_data$Annual.Income..k..)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#15.00   41.50   61.50   60.56   78.00  137.00 

##########################  ANNUAL INCOME   ##############################


hist(customer_data$Annual.Income..k..,col="orange", main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)
########################  DESCRIPTIVE ANALYSIS OF SPENDING SCORE   ##################


boxplot(customer_data$Spending.Score,
        horizontal=TRUE,
        col="Pink",
        main="BoxPlot for Descriptive Analysis of Spending Score")


######################  K MEANS ALGORITHM FOR ANALYSIS   ###########################



#######################  ELBOW METHOD     #############################
library(purrr)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}
k.values <- 1:10
iss_values <- map_dbl(k.values, iss)
plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")




pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)
pcclust$rotation[,1:2]

#################   CONSIDERING 6 CLUSTERS     ###########################

k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6

ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")
%>% 



##################     PLOTTING    ###################################

ggplot(customer_data, aes(x =Spending.Score..1.100., y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")


