library(readxl)
library(factoextra)
library(ggplot2)

data <- read.csv("C:/Users/USER/Downloads/Data IPM Jatim 2022.xlsx - Sheet1.csv")
View(data)

data$Rata.Rata.Lama.Sekolah <- as.numeric(gsub(",", ".", data$Rata.Rata.Lama.Sekolah))
data$Angka.Harapan.Hidup <- as.numeric(gsub(",", ".", data$Angka.Harapan.Hidup))
data$Harapan.Lama.Sekolah <- as.numeric(gsub(",", ".", data$Harapan.Lama.Sekolah))
View(data)

# Cek Outlier dengan Boxplot
par(mfrow = c(2, 2))

for (i in 3:6) {
  boxplot(data[, i], main = colnames(data)[i])
}

par(mfrow = c(1,1))

data <- data[,3:6]
data.stds<-scale(data) # Scaling

fviz_nbclust(data, kmeans, method = 'wss')
fviz_nbclust(data, kmeans, method = "silhouette")

Clustering <- kmeans(data.stds,centers=7,nstart=25)

fviz_cluster(Clustering, geom = 'point', data = data.stds)+ggtitle('k=7')
