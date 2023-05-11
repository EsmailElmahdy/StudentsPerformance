###################### STEP 1 -> Cleaning Data & Understanding #################
# import Data
library(readr)

StudentsPerformance <- read_csv("StudentsPerformance.csv")
MyData <- StudentsPerformance

# Drop the ID Column
MyData <- MyData[, -1]

# Make all columns names small and be sure that every columns has a uniqe name
library(janitor)

MyData <- clean_names(MyData)

# Remove Empty Rows Or Columns
library(tidyr)

MyData <- remove_empty(MyData, which = c("rows", "cols"), quiet = FALSE)

#Remove NULL Values
sapply(MyData, function(x)(sum(is.na(x)))) # Counting NULL
MyData <- drop_na(MyData) # there is no nulls value but to be sure

###################### STEP 2 -> Statistics ####################################

# Print max, min, 1stQ, 3rdQ, Median and Mod for all columns
summary(MyData)

                      ###### Correlation ###### 

cor(MyData$g1, MyData$g2)
cor(MyData$g1, MyData$g3)
cor(MyData$g2, MyData$g3)

cor(MyData$failures, MyData$g1)
cor(MyData$failures, MyData$g2)
cor(MyData$failures, MyData$g3)

cor(MyData$absences, MyData$g1)
cor(MyData$absences, MyData$g2)
cor(MyData$absences, MyData$g3)

cor(MyData$studytime, MyData$g1)
cor(MyData$studytime, MyData$g2)
cor(MyData$studytime, MyData$g3)

                      ###### Variance ###### 

nam <- c("Age", "GoOut", "StudyTime", "Failures", "Health", "Absences", "G1", "G2", "G3")
Variance1 <- list(var(MyData$age), var(MyData$goout), var(MyData$studytime), var(MyData$failures),
                  var(MyData$health), var(MyData$absences), var(MyData$g1), var(MyData$g2), var(MyData$g3))
names(Variance1) <- nam
Variance1

                      ###### Standard Deviation ###### 

StandardDeviation <- list(sd(MyData$age), sd(MyData$goout), sd(MyData$studytime), sd(MyData$failures),
                         sd(MyData$health), sd(MyData$absences), sd(MyData$g1), sd(MyData$g2), sd(MyData$g3))
names(StandardDeviation) <- nam
StandardDeviation

###################### STEP 3 -> visualization #################################
                      ###### Boxplot ###### 
library(reshape2)

data_long <- melt(MyData) # to Draw only the Numerical Data

library(ggplot2)

ggplot(data_long) +
  geom_boxplot(mapping = aes(x = variable, y = value), fill = "#69b3a2", color = "#404080", 
               outlier.colour = "#211878", outlier.size = 1)

                      ###### Plot ###### 

# Grades With each other
ggplot(MyData) +
  geom_point(mapping = aes(x = g1, y = g2), color = "#404080")

ggplot(MyData) +
  geom_point(mapping = aes(x = g1, y = g3), color = "#404080")

ggplot(MyData) +
  geom_point(mapping = aes(x = g2, y = g3), color = "#404080")

# Absences with Grades
ggplot(MyData) +
  geom_point(mapping = aes(x = absences, y = g1), color = "#404080")

ggplot(MyData) +
  geom_point(mapping = aes(x = absences, y = g2), color = "#404080")

ggplot(MyData) +
  geom_point(mapping = aes(x = absences, y = g3), color = "#404080")

# Study time with Grades 
ggplot(MyData) +
  geom_point(mapping = aes(x = studytime, y = g1), colour = "#404080")

ggplot(MyData) +
  geom_point(mapping = aes(x = studytime, y = g2), color = "#404080")

ggplot(MyData) +
  geom_point(mapping = aes(x = studytime, y = g3), color = "#404080")

                      ###### Histogram ###### 

# Grades with each other
ggplot(MyData, aes(Grade1+Grade2)) +
  geom_histogram(mapping = aes(x = g1, fill = "Grade 1"), alpha = 0.6, color = "#e9ecef") + 
  geom_histogram(mapping = aes(x = g2, fill = "Grade 2"), alpha = 0.6, color = "#e9ecef") + 
  scale_fill_manual(values = c("#69b3a2", "#404080"))

ggplot(MyData, aes(Grade1+Grade3)) +
  geom_histogram(mapping = aes(x = g1, fill = "Grade 1"), alpha = 0.6, color = "#e9ecef") + 
  geom_histogram(mapping = aes(x = g3, fill = "Grade 3"), alpha = 0.6, color = "#e9ecef") + 
scale_fill_manual(values = c("#69b3a2", "#404080"))

ggplot(MyData, aes(Grade2+Grade3)) +
  geom_histogram(mapping = aes(x = g2, fill = "Grade 2"), alpha = 0.6, color = "#e9ecef") + 
  geom_histogram(mapping = aes(x = g3, fill = "Grade 3"), alpha = 0.6, color = "#e9ecef") + 
  scale_fill_manual(values = c("#69b3a2", "#404080"))


# Absences with Grades
ggplot(MyData, aes(Grade1+Asbences)) +
  geom_histogram(mapping = aes(x = g1, fill = "Grade 1"), alpha = 0.6, color = "#e9ecef") + 
  geom_histogram(mapping = aes(x = absences, fill = "Absences"), alpha = 0.6, color = "#e9ecef") + 
  scale_fill_manual(values = c("#69b3a2", "#404080"))

ggplot(MyData, aes(Grade2+Asbences)) +
  geom_histogram(mapping = aes(x = g2, fill = "Grade 2"), alpha = 0.6, color = "#e9ecef") + 
  geom_histogram(mapping = aes(x = absences, fill = "Absences"), alpha = 0.6, color = "#e9ecef") + 
  scale_fill_manual(values = c("#69b3a2", "#404080"))

ggplot(MyData, aes(Grade3+Asbences)) +
  geom_histogram(mapping = aes(x = g3, fill = "Grade 3"), alpha = 0.6, color = "#e9ecef") + 
  geom_histogram(mapping = aes(x = absences, fill = "Absences"), alpha = 0.6, color = "#e9ecef") + 
  scale_fill_manual(values = c("#69b3a2", "#404080"))


# Study time with Grades
ggplot(MyData, aes(Grade1+StudyTime)) +
  geom_histogram(mapping = aes(x = g1, fill = "Grade 1"), alpha = 0.6, color = "#e9ecef") + 
  geom_histogram(mapping = aes(x = studytime, fill = "Study Time"), alpha = 0.6, color = "#e9ecef") + 
  scale_fill_manual(values = c("#69b3a2", "#404080"))

ggplot(MyData, aes(Grade2+StudyTime)) +
  geom_histogram(mapping = aes(x = g2, fill = "Grade 2"), alpha = 0.6, color = "#e9ecef") + 
  geom_histogram(mapping = aes(x = studytime, fill = "Study Time"), alpha = 0.6, color = "#e9ecef") + 
  scale_fill_manual(values = c("#69b3a2", "#404080"))

ggplot(MyData, aes(Grade3+StudyTime)) +
  geom_histogram(mapping = aes(x = g3, fill = "Grade 3"), alpha = 0.6, color = "#e9ecef") + 
  geom_histogram(mapping = aes(x = studytime, fill = "Study Time"), alpha = 0.6, color = "#e9ecef") + 
  scale_fill_manual(values = c("#69b3a2", "#404080"))


# Failures With Absences
ggplot(MyData, aes(Failures+Absences)) +
  geom_histogram(mapping = aes(x = failures, fill = "Failures"), alpha = 0.6, color = "#e9ecef") + 
  geom_histogram(mapping = aes(x = absences, fill = "Absences"), alpha = 0.6, color = "#e9ecef") + 
  scale_fill_manual(values = c("#69b3a2", "#404080"))


# Health With Absences
ggplot(MyData, aes(Health+Asbences)) +
  geom_histogram(mapping = aes(x = health, fill = "Health"), alpha = 0.6, color = "#e9ecef") + 
  geom_histogram(mapping = aes(x = absences, fill = "Absences"), alpha = 0.6, color = "#e9ecef") + 
  scale_fill_manual(values = c("#69b3a2", "#404080"))


###################### STEP 4 -> Models ########################################

# Change all data to Numeric 
NumericData <- transform(MyData, school = as.numeric(as.factor(school)), 
                         sex = as.numeric(as.factor(sex)),
                         fjob = as.numeric(as.factor(fjob)),
                         mjob = as.numeric(as.factor(mjob)),
                         internet = as.numeric(as.factor(internet)),
                         romantic = as.numeric(as.factor(romantic)))

# Scale data to make models
NumericData2 <- NumericData
NumericData2 <- scale(NumericData2) 

                      ###### 1 -> Kmeans ###### 

# Select the optimal number of clusters with more than one method
library(factoextra)

fviz_nbclust(NumericData2, kmeans, method = "wss")
fviz_nbclust(NumericData2, kmeans, method = "silhouette")
fviz_nbclust(NumericData2, kmeans, method = "gap_stat")

# Make the Kmeans model
model <- kmeans(NumericData2, 2)

# Make a table to know how many schools and (M | F) in each cluster
com <- table(MyData$school, model$cluster)
com
com2 <- table(MyData$sex, model$cluster)
com2
com3 <- table(MyData$g1, model$cluster)
com3

# Make a new column with the number of cluster to all records
KmeansData <- cbind(MyData, ClusterNum = model$cluster)
View(KmeansData)

# Print the Centers of every columns in each cluster
model$centers

# Visualize the Kmeans model with more than one plot
library(ggfortify)

fviz_cluster(model, data = NumericData2) # Clear Visualize
autoplot(model, NumericData2)
autoplot(model, NumericData2, frame = TRUE) # Clear Visualize

                      ###### 2 -> Hierarchy clustering ######

# Measure the distance between nodes with euclidean methode
Distance <- dist(NumericData2, method = "euclidean")

# Make the Hierarchy clustering model
hc1 <- hclust(Distance, method = "complete")
hc2 <- hclust(Distance, method = "complete")

# Visualize the tree and cut it to more than one cluster
plot(hc1)
rect.hclust(hc1, k = 6, border = "red")

# Second Model Visualize
plot(hc2)
rect.hclust(hc1, k = 8, border = "red")

# Make a new column with the number of cluster to all records
groups <- cutree(hc1, k = 6)
HierarchyClusteringData <- cbind(MyData, ClusterNum = groups)
View(HierarchyClusteringData)

                      ###### 3 -> K-medoids clustering ###### 

# Select the optimal number of clusters with more than one method
library(cluster)

fviz_nbclust(NumericData2, pam, method = "wss")
fviz_nbclust(NumericData2, pam, method = "silhouette")
fviz_nbclust(NumericData2, pam, method = "gap_stat") 

# Make the K-medoids model
PamRes2 <- pam(NumericData2, 2, metric = "euclidean") # Silhouette
PamRes4 <- pam(NumericData2, 4, metric = "euclidean") # wss
PamRes8 <- pam(NumericData2, 8, metric = "euclidean") # gap_stat

# Print medoids for all models
PamRes2$medoids
PamRes4$medoids
PamRes8$medoids

# Make a new column with the number of cluster to all records
final_data2 <- cbind(MyData, cluster = PamRes2$cluster)
final_data4 <- cbind(MyData, cluster = PamRes4$cluster)
final_data8 <- cbind(MyData, cluster = PamRes8$cluster)

# Visualize the K-medoids model with more than one plot
autoplot(PamRes2, NumericData2, frame = TRUE)
autoplot(PamRes4, NumericData2, frame = TRUE)
autoplot(PamRes8, NumericData2, frame = TRUE)

fviz_cluster(PamRes2, data = NumericData2)
fviz_cluster(PamRes4, data = NumericData2)
fviz_cluster(PamRes8, data = NumericData2)