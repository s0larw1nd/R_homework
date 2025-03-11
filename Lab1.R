# Задание 1
p <- 7:4
q <- 0:3

p + q
p - q
p * q
p / q
p ** q
q ** p

# Задание 2
c(rbind(0, seq(2, 20, 2)))
2**(1:20)
c(1,10,100,1000,10000)
10**(0:5)

# Задание 3
sum(1/(seq(1,50,by=1)*seq(2,51,by=1)))
sum(1/2*seq(0,20,by=1))
sum(seq(1,28,by=3)/3**seq(0,9,by=1))
v <- seq(1,28,by=3)/3**seq(0,9,by=1)
length(v[v>0.5])

# Задание 4
vec3 <- seq(3,27,by=3)
vec3[c(2,5,7)]
vec3[length(vec3)-1]
vec3[-(length(vec3)-1)]
vec3[-6]
vec3[100]
vec3[c(-1,-length(vec3))]
vec3[4<vec3 & vec3<10]
vec3[vec3<4 | vec3>10]

# Задание 13
num = 2:99
div = 2:9
pd <- sapply(div, function(d) num %% d == 0)
sum(rowSums(pd) > 0)

# Задание 28
df <- data.frame(rank=c(1:8,28,NA),
                 country=c('Германия','Австралия','Новая Зеландия','Дания','Норвегия','Исландия','США','Канада','Япония','Япония'),
                 index=c(0.946,0.923,0.923,0.92,0.919,0.918,0.899,0.891,0.85, NA),
                 spendings=c(4.6,5.1,7.2,8.7,7.3,7.8,5.4,4.8,3.8,NA))
df['continent'] <- c('Европа','Австралия','Океания','Европа','Европа','Европа','Америка','Америка','Азия','Азия')
df$rank[is.na(df$rank)]<-mean(df$rank,na.rm=TRUE)
df$index[is.na(df$index)]<-mean(df$index,na.rm=TRUE)
df$spendings[is.na(df$spendings)]<-mean(df$spendings,na.rm=TRUE)
df <- df[order(df$spendings),]
dfEurope <- df[df$continent=='Европа',]
dfAmerica <- df[df$continent=='Америка',]
