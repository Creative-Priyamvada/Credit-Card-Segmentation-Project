# Remove all objects from R
rm(list = ls())

# Set working directory
setwd("C:/Users/Puja/Documents/credit_card_segmentation")
getwd()

#Read the file
credit <- read.csv("credit-card-data.csv")

#Explanotary data analysis
str(credit)
summary(credit)
dim(credit)

#We take out the CUST_ID since it was a unique variable and we can't get further information from it.
credit$CUST_ID = NULL
dim(credit)

#Missing Value Analysis

sum(is.na(credit))
missing_val = data.frame(apply(credit,2,function(x){sum(is.na(x))}))
#Convert row names into columns
missing_val$Columns = row.names(missing_val)
row.names(missing_val) = NULL
#Rename first variable name
names(missing_val)[1] =  "Missing_percentage"

#Calculate percentage
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(credit)) * 100
#Rearranging the columns
missing_val = missing_val[,c(2,1)]
View(missing_val)

#Write output result back into the disc
write.csv(missing_val, "Miising_perc.csv", row.names = F)

#We see that in all the variable missing value is less than 30%, we will impute the missing value.

#Plot bar graph for missing value
ggplot(data = missing_val[14:16,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage") + theme_bw()

#Determing the correct method to imputate (Mean, Median, or KNN imputation)

#Actual value of credit[13,15]
#Actual value = 490.207
#Mean         = 864.2498
#Median       = 335.599
#knn          = 7366.6282

#so, we chose knn imputation method to fill the missing values

#Mean Method
#credit$MINIMUM_PAYMENTS[is.na(credit$MINIMUM_PAYMENTS)] = mean(credit$MINIMUM_PAYMENTS, na.rm = T)
#credit$CREDIT_LIMIT[is.na(credit$CREDIT_LIMIT)] = mean(credit$CREDIT_LIMIT, na.rm = T)

#Median Method
#credit$MINIMUM_PAYMENTS[is.na(credit$MINIMUM_PAYMENTS)] = median(credit$MINIMUM_PAYMENTS, na.rm = T)


# kNN Imputation
library(DMwR)
credit = knnImputation(credit, k = 3)
sum(is.na(credit$MINIMUM_PAYMENTS))
sum(is.na(credit$CREDIT_LIMIT))
write.csv(credit, "credit-card-data1.csv", row.names = F)

#Data Distribution

#Customer Balance
#install.packages("plotly")
library(plotly)
#install.packages("ggthemes")
library(ggthemes)

plot1 <-  ggplot(credit, aes(x=BALANCE)) +
  geom_histogram(col = "cyan", fill = "dodgerblue", bins = 30) +
  labs(x = "Balance", y = "Frequency", title = "Histogram of Customer Balance") +
  theme_igray()


ggplotly(plot1)

#customer Purchase
plot2 <-  ggplot(credit, aes(x=PURCHASES)) +
  geom_histogram(col = "lawngreen", fill = "springgreen4", bins = 40) +
  labs(x = "Purchase", y = "Frequency", title = "Histogram of Customer Purchase") +
  theme_igray()


ggplotly(plot2)

#Credit Limit
plot3 <-  ggplot(credit, aes(x=CREDIT_LIMIT)) +
  geom_histogram(col = "yellow2", fill = "orangered2", bins = 30) +
  labs(x = "Credit Limit", y = "Frequency", title = "Histogram of Credit Limit") +
  theme_igray()


ggplotly(plot3)

#Tenure
plot4 <-  ggplot(credit, aes(x=TENURE)) +
  geom_bar(col = "magenta", fill = "maroon3") +
  labs(x = "Tenure", y = "Frequency", title = "Bar Chart of Tenure") +
  theme_igray()


ggplotly(plot4)

#Deriving new KPI's

credit$Monthly_Avg_PURCHASES <- credit$PURCHASES/(credit$TENURE)
credit$Monthly_CASH_ADVANCE <- credit$CASH_ADVANCE/(credit$TENURE)
credit$LIMIT_USAGE <- credit$BALANCE/credit$CREDIT_LIMIT
credit$MIN_PAYMENTS_RATIO <- credit$PAYMENTS/credit$MINIMUM_PAYMENTS

write.csv(credit,"New_variables_creation.csv")

# Variable Reduction (Factor Analysis)

#(i)Correlation matrix
Num_Vars <- c(
  "BALANCE",
  "BALANCE_FREQUENCY",
  "PURCHASES",
  "Monthly_Avg_PURCHASES",
  "ONEOFF_PURCHASES",
  "INSTALLMENTS_PURCHASES",
  "CASH_ADVANCE",
  "Monthly_CASH_ADVANCE",
  "PURCHASES_FREQUENCY",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "CASH_ADVANCE_FREQUENCY",
  "CASH_ADVANCE_TRX",
  "PURCHASES_TRX",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PAYMENTS",
  "MINIMUM_PAYMENTS",
  "MIN_PAYMENTS_RATIO",
  "PRC_FULL_PAYMENT",
  "TENURE")

Step_nums <- credit[Num_Vars]
corrm<- cor(Step_nums)    
View(corrm)
write.csv(corrm, "Correlation_matrix.csv")

#(ii)Correlation matrix plot
library(GGally)
ggcorr(credit, 
       label = T, 
       label_size = 3,
       label_round = 2,
       hjust = 1,
       size = 3, 
       color = "royalblue",
       layout.exp = 5,
       low = "dodgerblue", 
       mid = "gray95", 
       high = "red2",
       name = "Correlation")

#PCA analysis : dimensionality reduction

scaled.credit <- scale(credit)
credit_pca <- prcomp(scaled.credit)
credit_pca
#variance interpretation
summary(credit_pca)

#Dimensionality Reduction
credit_new <- credit_pca$x[,1:10]  #Based on interepertations from above, we will decide to take only 10 dimensions and put it to new dataset called credit_new

#K-Means

#Elbow Method

fviz_nbclust(credit_new, 
             kmeans, 
             method = "wss",
             linecolor = "green4") +
  geom_vline(xintercept = c(4,6), linetype = 2, col = "red") +
  theme_igray()

#Based on the Elbow Method, we can say that the potential number of cluster (k) that may represents the customer segmentation is lies between 4 and 7

#k-means
for(i in 4:7){
  set.seed(289)
  model <- kmeans(credit_new, i)
  print(paste("WSS of K",i, "=",model$tot.withinss))
  print(paste("BSS Proportion of K",i, "=", model$betweenss/model$totss))
  print(paste("Cluster Size of K",i, "="))
  print(paste(model$size))
  print(fviz_cluster(model, credit, palette = "Set1") +
          theme_igray())
}

#As we can see, the smallest WSS and biggest R2 value is given by K = 7.
#But when we see the Cluster Plot, there is no significance different between K = 7 and the K = 6,
#therefore we will use K = 6 as the number of Clusters, since we don't want too many Clusters and
#focusing our treatment to the Customers.

credit_km <- kmeans(credit_new, 6)
credit$CLUSTER <- credit_km$cluster
head(credit, 10)

