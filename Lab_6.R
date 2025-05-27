install.packages('car')
library(car)

data <- read.csv("D:/Downloads/Athlet_Events/Athlet_Events/athlete_events.csv", sep = ",")

data_athletics <- data[data$Sport=="Athletics",]

#Дескриптивный анализ данных
data_num <- data_athletics[c("Age","Height","Weight","Year")]
#Меры центральной тенденции
Mean = sapply(data_num, mean, na.rm = TRUE)
Median = sapply(data_num, median, na.rm = TRUE)
mode_func <- function(x) {
  x <- na.omit(x)
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
Mode = sapply(data_num, mode_func)

#Меры изменчивости
range_func <- function(x) {
  x <- na.omit(x)
  diff(range(x))
}
Range = sapply(data_num, range_func)
Dispersion = sapply(data_num,var, na.rm = TRUE)
Standard_deviation = sapply(data_num,sd, na.rm = TRUE)
Interquartile_range = sapply(data_num,IQR, na.rm = TRUE)

# Гипотеза о среднем весе = 80
data_rasp <- head(na.omit(data_athletics[c("Age","Height","Weight","Year")]),5000)

shapiro.test(data_rasp$Weight)
shapiro.test(data_rasp$Weight)

qqnorm(data_rasp$Weight)
qqline(data_rasp$Weight,col=2)
qqPlot(data_rasp$Weight)

wilcox.test(data_rasp$Weight,mu=80,conf.int=TRUE)

# Гипотеза о равенстве среднего мужчин в Плавании и Гимнастике
data_swimming_m <- sapply(as.vector(head(na.omit(data[data$Sport=="Swimming",]["Weight"]),5000)), as.numeric)
data_gymnastics_m <- sapply(as.vector(head(na.omit(data[data$Sport=="Gymnastics",]["Weight"]),5000)), as.numeric)

shapiro.test(data_swimming_m)
shapiro.test(data_gymnastics_m)

wilcox.test(data_swimming_m, data_gymnastics_m)