data("udist_play_98105")
udist <- data.table(udist_play_98105) 
head(udist)
library(kknn)
library(stats) 
library(ggplot2)
library(data.table)
##k-means

for_kmeans <- udist[, list(alternative, rock)]
output <- kmeans(for_kmeans, centers=3)
udist[, kmean_species:=output$cluster]

ggplot(udist, aes(x=alternative, y=rock, color=factor(kmean_species))) +
  geom_point() +
  labs(title="Predicted Clusters, K=3")

##k-nn

set.seed(250)

new_order <- sample(nrow(udist))
udist <- udist[new_order]

## split this shuffled dataset in two: a training set with about 90% of the data, 
## and a test set with about 10%
train <- udist[1:18, list(alternative, rock, companies)] #training set
test <- udist[19:20, list(alternative, rock, companies)] #testing set

## Run the k-NN algorithm, with k=3 to start with.
knn_output1 <- kknn(companies~., train, test, k=3)
test[, predicted_species:=knn_output1$fitted.values] #add the output to the testing test
#plotA predicted vs dark species for knn, k=3
ggplot(test, aes(x=alternative, y=rock)) +
  geom_point(aes(color=companies), size=5, alpha=0.3) +
  geom_point(aes(color=predicted_species)) +
  geom_point(data=train, aes(color=companies), alpha=0.4, shape=2) +
  labs(title="Predicted (dark) vs Real (opaque) Companies, k=3")

## test a few other values of k, using for loop.

k_values <- c(5, 7, 9) #list of k values

for (this_k in k_values){
  
  ## run k-nn
  knn_output2 <- kknn(companies~., train, test, k=this_k)
  
  ## predict a new rank 
  test[, predicted_species:=knn_output2$fitted.values]
  
  ## predict results
  plot <- ggplot(test, aes(x=alternative, y=rock)) +
    geom_point(aes(color=companies), size=5, alpha=0.3) +
    geom_point(aes(color=predicted_species)) +
    geom_point(data=train, aes(color=companies), alpha=0.4, shape=2) +
    labs(title=paste0("Predicted (dark) vs Real (opaque) companies, k=", this_k))
  
  print(plot)
  
  ## Print out how many times it mispredicts
  number_wrong = nrow(test[companies!=predicted_species])
  pct_wrong = number_wrong/nrow(test)
  print(paste0("With k=", this_k, " k-NN makes ", number_wrong, " incorrect predictions, ", pct_wrong, " percent."))
  
}
