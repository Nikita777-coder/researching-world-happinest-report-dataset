# Loading data
df <- read.csv("/Users/davydoff/Desktop/data.csv")

# Remove unnecessary coloums and set rownames
rownames(df) <- df$Country.Name
df$Country.Name <- NULL
df$Year <- NULL
?dist
#Install package ape
install.packages("ape")
library("ape")

#-----Distance methods-----

# Method = "euclidean"
dd <- dist(scale(df), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc, hang = -1, cex = 0.6)

# Method = "maximum"
dd <- dist(scale(df), method = "maximum")
hc <- hclust(dd, method = "ward.D2")
plot(hc, hang = -1, cex = 0.6)

# Method = "manhattan"
dd <- dist(scale(df), method = "manhattan")
hc <- hclust(dd, method = "ward.D2")
plot(hc, hang = -1, cex = 0.6)

# Method = "minkowski"
dd <- dist(scale(df), method = "minkowski")
hc <- hclust(dd, method = "ward.D2")
plot(hc, hang = -1, cex = 0.6)

# Method = "canberra"
dd <- dist(scale(df), method = "canberra")
hc <- hclust(dd, method = "ward.D2")
plot(hc, hang = -1, cex = 0.6)


#----Cluster methods----

#Method = single(Ближний сосед)
dd <- dist(scale(df), method = "canberra")
hc <- hclust(dd, method = "single")
plot(hc, hang = -1, cex = 0.6)

#Method = complete(Дальний сосед)
dd <- dist(scale(df), method = "canberra")
hc <- hclust(dd, method = "complete")
plot(hc, hang = -1, cex = 0.6)

#Method = median(Медиана)
dd <- dist(scale(df), method = "canberra")
hc <- hclust(dd, method = "median")
plot(hc, hang = -1, cex = 0.6)

#Method = centroid(Центр масс)
dd <- dist(scale(df), method = "canberra")
hc <- hclust(dd, method = "centroid")
plot(hc, hang = -1, cex = 0.6)

#Method = ward.D2(Метод Варда)
dd <- dist(scale(df), method = "canberra")
hc <- hclust(dd, method = "ward.D2")
plot(hc, hang = -1, cex = 0.6)

#----Colored plots----

# Classificate for 4 clasters
colors = c("red", "blue", "green", "black")
clus4 = cutree(hc, 4)
plot(as.phylo(hc), tip.color = colors[clus4],
     label.offset = 1, cex = 0.7)

# Classificate for 3 clasters
colors = c("red", "blue", "green")
clus4 = cutree(hc, 3)
plot(as.phylo(hc), tip.color = colors[clus4],
     label.offset = 1, cex = 0.7)


#--------KMEANS--------#

#Install packages factoextra and cluster
install.packages("factoextra")
install.packages("cluster")
library("factoextra")
library("cluster")

set.seed(1)

# Graph of optimal clasters
fviz_nbclust(df, kmeans, method = "wss")

# Classificate for 3 clasters
kmeans.re <- kmeans(df, centers = 3, nstart = 20)

# Cluster identification for each observation
kmeans.re
y_kmeans <- kmeans.re$cluster
#Cluster plot
clusplot(df,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster plot"))
# Another cluster plot
fviz_cluster(kmeans.re, data = df)


# Classificate for 4 clasters
kmeans.re <- kmeans(df, centers = 4, nstart = 20)
# Cluster identification for each observation
kmeans.re
y_kmeans <- kmeans.re$cluster
#Cluster plot
clusplot(df,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster plot"))
#Another cluster plot
fviz_cluster(kmeans.re, data = df)
