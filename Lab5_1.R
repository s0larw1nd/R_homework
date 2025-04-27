data <- read.csv("D:/Downloads/DataSets_Lab_5/DataSets_Lab_5/05_страны_уров_жизни/zzz.csv", sep = ";")
data <- transform(data,  
                  Birth_Rate = as.numeric(gsub(",", ".", gsub(" ", "", Birth_Rate, fixed = TRUE), fixed = TRUE)),
                  Death_Rate = as.numeric(gsub(",", ".", gsub(" ", "", Death_Rate, fixed = TRUE), fixed = TRUE)),
                  Child_Mortality = as.numeric(gsub(",", ".", gsub(" ", "", Child_Mortality, fixed = TRUE), fixed = TRUE)),
                  Life_male = as.numeric(gsub(",", ".", gsub(" ", "", Life_male, fixed = TRUE), fixed = TRUE)),
                  Life_female = as.numeric(gsub(",", ".", gsub(" ", "", Life_female, fixed = TRUE), fixed = TRUE)),
                  Salary = as.numeric(gsub(" ", "", Salary, fixed = TRUE)),
                  Region = as.numeric(gsub(" ", "", Region, fixed = TRUE)))
data <- data[data$Salary!=-9999,]

#Дескриптивный анализ данных
#Меры центральной тенденции
Mean = sapply(data[-1], mean)
Median = sapply(data[-1], median)
mode_func <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
Mode = sapply(data[-1], mode_func)

#Меры изменчивости
range_func <- function(x) {
  diff(range(x))
}
Range = sapply(data[-1], range_func)
Dispersion = sapply(data[-1],var)
Standard_deviation = sapply(data[-1],sd)
Interquartile_range = sapply(data[-1],IQR)

data_orig <- data
data <- data[-8]
data_labels <- data[,1]
data <- data[,-1]
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)
data <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

#Метод локтя
install.packages("factoextra")
library (factoextra)
library (cluster)
fviz_nbclust(data, kmeans, method = "wss")

#Метод среднего силуэта
fviz_nbclust(data, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")

#Статистика разрыва
gap_stat <- clusGap(data, FUN = kmeans, nstart = 5,K.max =5, B = 5)
fviz_gap_stat(gap_stat)

#Алгоритм консенсуса
install.packages('parameters')
library(parameters)
n_clust <- n_clusters(data, package = c("easystats", "NbClust", "mclust"), standardize = FALSE)
n_clust
plot(n_clust)

#Дендрограмма
dist.data <- dist(data)
clust.data <- hclust(dist.data, "ward.D")
plot(clust.data, data_labels, cex=0.5)
rect.hclust(clust.data, k=2, border="red")
rect.hclust(clust.data, k=4, border="green")

#Графики
groups <- cutree(clust.data, k=4)
g1<-colMeans(data[groups==1, 1:6])
g2<-colMeans(data[groups==2, 1:6])
g3<-colMeans(data[groups==3, 1:6])
g4<-colMeans(data[groups==4, 1:6])
df<-data.frame(g1,g2,g3,g4)
df1<-t(df)
df<-t(df1)
#Столбчатые диаграмм
barplot(df,
        main = "Groups of LQI", axes = FALSE,
        col=c("red","green","blue","yellow","grey","purple"),
        beside=TRUE)
axis(2, at = 0:5, labels = 0:5)
legend("top", legend = rownames(df), col=c("red","green","blue","yellow","grey","purple"), lwd=
         10, bty = "n")
#Боксплоты
boxplot(Birth_Rate ~ Region, data = data_orig, ylab = "Birth_Rate", frame = FALSE, col = "lightgray")
boxplot(Death_Rate ~ Region, data = data_orig, ylab = "Death_Rate", frame = FALSE, col = "lightgray")
boxplot(Child_Mortality ~ Region, data = data_orig, ylab = "Child_Mortality", frame = FALSE, col = "lightgray")
boxplot(Life_male ~ Region, data = data_orig, ylab = "Life_male", frame = FALSE, col = "lightgray")
boxplot(Life_female ~ Region, data = data_orig, ylab = "Life_female", frame = FALSE, col = "lightgray")
boxplot(Salary ~ Region, data = data_orig, ylab = "Salary", frame = FALSE, col = "lightgray")

#Кластеризация
km.res <- kmeans(data, 4, nstart = 10)
fviz_cluster(km.res, data, ellipse.type = "norm")
fviz_cluster(km.res, data, palette = "Set2", ggtheme = theme_minimal())

#Скаттерплот
pairs(data,main= "Индекс качества жизни", col = c("red","green","blue","yellow","grey","purple"))
pairs(data,main= "Индекс качества жизни", col = c("red","green","blue","yellow","grey","purple"),lower.panel=NULL)

#3D скаттерплот
install.packages("scatterplot3d")
library("scatterplot3d")
colors <- c("red","green","blue","yellow","grey","purple")
colors <- colors[as.numeric(data_orig$Region)]
s3d <- scatterplot3d(data_orig[,5:7], main= "Регионы", pch = 16, color = colors)
legend("right", legend = 1:6, col = c("red","green","blue","yellow","grey","purple"), pch = 16)