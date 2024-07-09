library(ggplot2) #importing necessary libraries
library(purrr)

# Loading the dataset
df <- read.csv('C:/Users/Prashant Verma/Downloads/Mall_Customers.csv') 
head(df)

#pre-precessing the data
summary(df)

sum(is.na(df)) 
sum(duplicated(df))

gender <- table(df$Genre) 
print(gender)

#visualisation of data
barplot(gender,main='Bar plot of Gender',xlab='Gender',ylab='Count', 
        col=rainbow(2),legend=rownames(gender))

#in pie chart
percent <- gender/sum(gender) * 100 
print(percent) 
labels <- paste(c('Female','Male'),percent,'%') 
print(labels) 
pie(percent,col=rainbow(2),labels=labels)

#Visualisation of age distribution
par(mar=c(4, 4, 4, 4) + 0.1, mfrow=c(1, 1), cex=0.7)

hist(df$Age,breaks=5,col='blue',labels=T, main='Histgram of Age',xlab='Age')

#for annual income
hist(df$Annual.Income..k..,col='red',labels=T,main='Distribution of Annual Income',xlab='Annual Income')

#for spending score
hist(df$Spending.Score..1.100.,col='orange',labels=T,main='Distribution of Spending Score',xlab='Spending Score')

#visualising the relationship between age and annual income
plot(df$Age,df$Annual.Income..k..,col='black',xlab='Age',ylab='Annual Income')

#Using k-means for segmenting customers and determining optimal value using Elbow method
fun <- function(k){ 
  kmeans(df[,3:5],k,iter.max=100,nstart = 100,algorithm='Lloyd')$tot.withinss 
} 

k.values <- 1:10 
fun_value <- map_dbl(k.values,fun) 
plot(k.values,fun_value,type='b',xlab='Number of clusters',ylab='Total sum of squares')

#From the graph, k=5 is the optimal value 
k5<- kmeans(df[,3:5],5,iter.max = 100,nstart = 50,algorithm = 'Lloyd') 
print(k5)

#Visualising cluster results
ggplot(df, aes(x = Annual.Income..k..,y = Spending.Score..1.100.)) + 
  geom_point(stat = 'identity',aes(col = as.factor(k5$cluster))) + 
  scale_color_discrete(name='Clusters',breaks = c('1','2','3','4','5'), 
                       labels = c('C1','C2','C3','C4','C5')) + 
  ggtitle('Customer Segmentation using Kmeans') + labs(x='Annual Income', y='Spending Score')