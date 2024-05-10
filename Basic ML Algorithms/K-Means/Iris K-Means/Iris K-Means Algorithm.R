library(ggplot2)
library(cluster)

# Load the data set.
iris <- read.csv("iris.csv", header = FALSE)
colnames(iris) <- c('sepal_length', 'sepal_width', 'petal_length', 'petal_width', 'class')

# View structure and summary of the iris dataset
str(iris)
summary(iris)

ggplot(iris, aes(x = sepal_length, y = sepal_width, color = petal_length, size = petal_width, shape = class)) +
  geom_point() + scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Iris Dataset: Four-Dimensional Visualization", x = "Sepal Length", y = "Sepal Width", 
  color = "Petal Length", size = "Petal Width") + theme_minimal()

set.seed(123)
iris_cluster <- kmeans(iris[,1:4], centers = 3) # meaning k=10
iris$Cluster <- as.factor(iris_cluster$cluster)
iris_cluster

ggplot(iris, aes(petal_length, petal_width, color = Cluster)) + geom_point()

table(iris_cluster$cluster, iris$class)

# clusplot(iris, irisCluster$cluster, color=T, shade=T, labels=0, lines=0)

