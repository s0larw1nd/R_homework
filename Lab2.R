df <- read.csv('Responses.csv')[-1]
df[-1] <- df[-1] / 10

apply(na.omit(df[-1]),2, max)
apply(na.omit(df[-1]),2, min)
apply(na.omit(df[-1]),2, mean)

apply(na.omit(df[-1]),2, function(x) sum(0.7 < x))
apply(na.omit(df[-1]),2, function(x) sum(0.3 > x))
sum(df$Фэнтези > 0.7)
sum(df$Фэнтези < 0.3)

top <- sort(apply(na.omit(df[-1]),2, mean), decreasing = TRUE)
top
unname(top)

df[(df$Фэнтези > 0.5) & (df$Детективы == 1.0),]

barplot(top,
        main = "Рейтинг жанров по предпочтениям", 
        col = length(top), 
        las = 2, 
        ylab = "Среднее предпочтений")

barplot(top, 
        main = "Рейтинг жанров по предпочтениям", 
        col = length(top), 
        las = 1,
        horiz = TRUE, 
        xlab = "Среднее предпочтений")

mydata_csv <- read.csv('test.csv')

install.packages("readxl")
library("readxl")
mydata_xlsx <- read_excel("test.xlsx")

mode_func <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

#Меры центральной тенденции
Mean = sapply(df[-1], mean)
Median = sapply(df[-1], median)
Mode = sapply(df[-1], mode_func)

#Меры изменчивости
range_func <- function(x) {
  diff(range(x))
}
Range = sapply(df[-1], range_func)
Dispersion = sapply(df[-1],var)
Standard_deviation = sapply(df[-1],sd)
Interquartile_range = sapply(df[-1],IQR)

boxplot(x = as.list(df[-1]), las=2)

df[order(df$Фэнтези,decreasing=TRUE),]

subdataset_fantasy <- subset(df, df$Фэнтези > 0.7)
dim(subdataset_fantasy)

Mean_fantasy = sapply(subdataset_fantasy[-1], mean)
Median_fantasy = sapply(subdataset_fantasy[-1], median)
Mode_fantasy = sapply(subdataset_fantasy[-1], mode_func)

barplot(sort(apply(subdataset_fantasy[-1],2, mean), decreasing = TRUE), 
        main = "Рейтинг жанров по предпочтениям", 
        las = 2, 
        ylab = "Среднее предпочтений")

boxplot(x = as.list(subdataset_fantasy[-1]), las=2)

df <- rbind(df, data.frame(Фамилия="Пикулев",
                           Фэнтези=0.8,
                           Детективы=0.9,
                           Фантастика=1.0,
                           Романы=0.5,
                           Триллеры=0.8,
                           Классическая.литература=0.2,
                           Научно.популярная.литература=0.3,
                           Психология=0.3,
                           Биографии=0.2,
                           Техническая.литература=0.6))

df1 <- df[c(1,2,3,4,5,6)]
#df1 <- df[c(-7,-8,-9,-10,-11)]
#df1 <- df[names(df) %in% c("Фамилия","Фэнтези","Детективы","Фантастика","Романы","Триллеры")]
#df1 <- df
#df1$Классическая.литература <- df1$Научно.популярная.литература <- df1$Психология <- df1$Биографии <- df1$Техническая.литература <- NULL
df2 <- df[c(1,7,8,9,10,11)]
dfm <- merge(df1,df2,by="Фамилия")
