library(tidyverse)
library(data.table)
library(lubridate)

data <- fread(paste0("Wharton People Analytics Data.csv"), header = T, stringsAsFactors = F, data.table = T)
colnames(data)
view(data)
# [1] "staff_id"           "assignment_number"  "departure_date"     "return_date"        "assignment_country"
# [6] "pool"               "job_title"          "age_bracket"        "sex"                "first_departure"   
# [11] "duration_days" 
assign_start    <- as_date(data$departure_date)
assign_end      <- as_date(data$return_date)
assign_interval   <- interval(assign_start, assign_end)

# assign_duration <- as.duration(assign_interval, hours)
assign_duration <- (as.numeric(as.period(assign_interval, unit = 'day')) / 86400)
assign_duration_days <-  #86400 seconds per day
  data$duration_days <- assign_duration

data_new <- data[,c(2,6,8,11)]
str(data_new)
data_test <- data_new[,c(1,3,4)]
str(data_test)
data_test <- data_test  %>% na.omit() %>%  scale()
view(data_test)

# Class Analysis
library("cluster")
library("factoextra")
library("magrittr")

set.seed(123)
nrow(data_test)
data_test <- data_test[1:1000,]

# K-Means Clustering
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(data_test, k, nstart=50,iter.max = 15 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
km.res <- kmeans(data_test, 4, nstart = 25)
# Visualize
fviz_cluster(km.res, data = data_test,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# Hierarchical clustering
res.hc <- data_test %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2") 
# Visualize 
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE)

# # Density Based Clustering
# set.seed(123)
# km.dense <- kmeans(data_test, 5, nstart = 25)
# fviz_cluster(km.dense, data_test,  geom = "point", 
#              ellipse= FALSE, show.clust.cent = FALSE,
#              palette = "jco", ggtheme = theme_classic())
# library(fpc)
# library(dbscan)
# set.seed(123)
# as.matrix(data_test)
# db <- dbscan(data_test, eps = 0.15, MinPts = 5)
# fviz_cluster(db, data = data_test, stand = FALSE,
#              ellipse = FALSE, show.clust.cent = FALSE,
#              geom = "point",palette = "jco", ggtheme = theme_classic())

unique(data_new) #59 
# 59 assignments, 11 job pool, 5 job brackets

res.pca <- prcomp(data_test, scale = FALSE)
fviz_pca_ind(data_test, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Wharton")

mean(data$duration_days)
str(data$duration_days)

ggplot(data, aes(x = job_title, y = assignment_number)) +
  geom_boxplot()
