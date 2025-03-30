install.packages("readxl")
library(readxl)

norway_freestyle <- read_excel("D:/Programming/R_homework/freestyle.xlsx")

places_m <- norway_freestyle$Мужские
places_f <- norway_freestyle$Женские
olympiads <- norway_freestyle$Год

barplot(height=rowSums(df[, c("М1", "М2", "М3","М4", "М5", "М6", "М7", "М8")], na.rm = TRUE),
        names.arg = olympiads,
        xlab="Олимпиады",
        ylab="Медали",
        main="Медали Норвегии по фристайлу (Мужские)")

barplot(height=rowSums(df[, c("Ж1", "Ж2", "Ж3","Ж4", "Ж5", "Ж6", "Ж7", "Ж8")], na.rm = TRUE),
        names.arg = olympiads,
        xlab="Олимпиады",
        ylab="Медали",
        main="Медали Норвегии по фристайлу (Женские)")

df_male <- df[, c("Год", "М1", "М2", "М3", "М4", "М5", "М6", "М7", "М8")]
medals_male <- aggregate(. ~ Год, data = df_male, sum, na.rm = TRUE)
medals_matrix <- as.matrix(medals_male[, -1])
rownames(medals_matrix) <- medals_male$Год
barplot(t(medals_matrix), beside = TRUE, legend.text = colnames(medals_matrix), 
        col = rainbow(ncol(medals_matrix)), main = "Медали Норвегии по фристайлу (Мужские)", xlab = "Олимпиады", ylab = "Медали")

df_female <- df[, c("Год", "Ж1", "Ж2", "Ж3", "Ж4", "Ж5", "Ж6", "Ж7", "Ж8")]
medals_female <- aggregate(. ~ Год, data = df_female, sum, na.rm = TRUE)
medals_matrix_f <- as.matrix(medals_female[, -1])
rownames(medals_matrix_f) <- medals_female$Год
barplot(t(medals_matrix_f), beside = TRUE, legend.text = colnames(medals_matrix_f), 
        col = rainbow(ncol(medals_matrix_f)), main = "Медали Норвегии по фристайлу (Женские)", xlab = "Олимпиады", ylab = "Медали")

f_places_m <- rowSums(df[, "М1"], na.rm = TRUE)
f_places_f <- rowSums(df[, "Ж1"], na.rm = TRUE)

valid_indices_m <- !is.na(f_places_m) & f_places_m > 0
valid_indices_f <- !is.na(f_places_f) & f_places_f > 0

colors_m <- rainbow(sum(valid_indices_m))
colors_f <- rainbow(sum(valid_indices_f))

pie(f_places_m[valid_indices_m], labels = olympiads[valid_indices_m], col = colors_m, 
    main = "Медали за первое место (мужские)")

legend("topright", legend = f_places_m[valid_indices_m], 
       fill = colors_m)

pie(f_places_f[valid_indices_f], labels = olympiads[valid_indices_f], col = colors_f, 
    main = "Медали за первое место (женские)")

legend("topright", legend = f_places_f[valid_indices_f], 
       fill = colors_f)

prized_m <- rowSums(df[, c("М1", "М2", "М3")], na.rm = TRUE)
prized_f <- rowSums(df[, c("Ж1", "Ж2", "Ж3")], na.rm = TRUE)
plot(olympiads, prized_m, type = "o", col = "blue", xlab = "Олимпиады", ylab = "Медали", main = "Тенденция изменения количества призовых мест (мужские)", xaxt="n")
axis(1, at=olympiads, labels=olympiads)
plot(olympiads, prized_f, type = "o", col = "blue", xlab = "Олимпиады", ylab = "Медали", main = "Тенденция изменения количества призовых мест (женские)", xaxt="n")
axis(1, at=olympiads, labels=olympiads)

#Летние (золото)
summer_gold <- read_excel("D:/Programming/R_homework/summer.xlsx")
USA <- summer_gold$США
China <- summer_gold$Китай
Japan <- summer_gold$Япония
Australia <- summer_gold$Австралия
France <- summer_gold$Франция
Netherlands <- summer_gold$Нидерланды
Britain <- summer_gold$Великобритания
Russia <- summer_gold$Россия
Germany <- summer_gold$Германия
South.Korea <- summer_gold$`Южная Корея`
olympiads <- summer_gold$Год

plot(olympiads, USA, type="o", col="red", pch=16, 
     ylim=c(min(USA, China, Japan, Australia, France, Netherlands, Britain, Russia, Germany, South.Korea, na.rm=TRUE), 
            max(USA, China, Japan, Australia, France, Netherlands, Britain, Russia, Germany, South.Korea, na.rm=TRUE)),
     xlab="Год Олимпиады", ylab="Медали", main="Динамика количества золотых медалей у первых 7 стран-призёров за последние 6 летних ОИ", xaxt="n")
axis(1, at=olympiads, labels=olympiads)

lines(olympiads, China, type="o", col="blue", pch=16)
lines(olympiads, Japan, type="o", col="green", pch=16)
lines(olympiads, Australia, type="o", col="purple", pch=16)
lines(olympiads, France, type="o", col="orange", pch=16)
lines(olympiads, Netherlands, type="o", col="brown", pch=16)
lines(olympiads, Britain, type="o", col="pink", pch=16)
lines(olympiads, Russia, type="o", col="cyan", pch=16)
lines(olympiads, Germany, type="o", col="darkgreen", pch=16)
lines(olympiads, South.Korea, type="o", col="darkblue", pch=16)

legend("topright", legend=c("США", "Китай", "Япония", "Австралия", "Франция", "Нидерланды", "Британия", "Россия", "Германия", "Южная Корея"),
       col=c("red", "blue", "green", "purple", "orange", "brown", "pink", "cyan", "darkgreen", "darkblue"),
       pch=16, cex=0.8)

#Зимние (золото)
winter_gold <- read_excel("D:/Programming/R_homework/winter.xlsx")
Norway <- winter_gold$Норвегия
Germany <- winter_gold$Германия
USA <- winter_gold$США
Canada <- winter_gold$Канада
Russia <- winter_gold$Россия
France <- winter_gold$Франция
Italy <- winter_gold$Италия
Austria <- winter_gold$Австрия
Sweden <- winter_gold$Швеция
South.Korea <- winter_gold$`Южная Корея`
Switzerland <- winter_gold$Швейцария
China <- winter_gold$Китай
Netherlands <- winter_gold$Нидерланды
olympiads <- winter_gold$Год

plot(olympiads, Norway, type="o", col="blue", pch=16, 
     ylim=c(min(Norway, Germany, USA, Canada, Russia, France, Italy, Austria, Sweden, South.Korea, Switzerland, China, Netherlands, na.rm=TRUE),
            max(Norway, Germany, USA, Canada, Russia, France, Italy, Austria, Sweden, South.Korea, Switzerland, China, Netherlands, na.rm=TRUE)),
     xlab="Год Олимпиады", ylab="Медали", main="Динамика количества золотых медалей у первых 7 мест за последние 6 зимних ОИ", xaxt="n")
axis(1, at=olympiads, labels=olympiads)

lines(olympiads, Norway, type="o", col="blue", pch=16)
lines(olympiads, Germany, type="o", col="green", pch=16)
lines(olympiads, USA, type="o", col="purple", pch=16)
lines(olympiads, Canada, type="o", col="orange", pch=16)
lines(olympiads, Russia, type="o", col="brown", pch=16)
lines(olympiads, France, type="o", col="pink", pch=16)
lines(olympiads, Italy, type="o", col="cyan", pch=16)
lines(olympiads, Austria, type="o", col="darkgreen", pch=16)
lines(olympiads, Sweden, type="o", col="darkblue", pch=16)
lines(olympiads, South.Korea, type="o", col="darkred", pch=16)
lines(olympiads, Switzerland, type="o", col="coral", pch=16)
lines(olympiads, China, type="o", col="gold", pch=16)
lines(olympiads, Netherlands, type="o", col="aquamarine", pch=16)

legend("topleft", 
       legend=c("Норвегия", "Германия", "США", "Канада", "Россия", "Франция", "Италия", "Австрия", "Швеция", "Южная Корея", "Швейцария", "Китай", "Нидерланды"),
       col=c("blue", "green", "purple", "orange", "brown", "pink", "cyan", "darkgreen", "darkblue", "darkred","coral","gold","aquamarine"),
       pch=16, cex=0.8)

#Летние (3 призовых)
summer_prized <- read_excel("D:/Programming/R_homework/summer(3 best).xlsx")
USA <- summer_prized$США
China <- summer_prized$Китай
Japan <- summer_prized$Япония
Australia <- summer_prized$Австралия
France <- summer_prized$Франция
Netherlands <- summer_prized$Нидерланды
Britain <- summer_prized$Великобритания
Russia <- summer_prized$Россия
Germany <- summer_prized$Германия
South.Korea <- summer_prized$`Южная Корея`
olympiads <- summer_prized$Год

plot(olympiads, USA, type="o", col="red", pch=16, 
     ylim=c(min(USA, China, Japan, Australia, France, Netherlands, Britain, Russia, Germany, South.Korea, na.rm=TRUE), 
            max(USA, China, Japan, Australia, France, Netherlands, Britain, Russia, Germany, South.Korea, na.rm=TRUE)),
     xlab="Год Олимпиады", ylab="Медали", main="Динамика количества медалей у первых 7 мест за последние 6 летних ОИ", xaxt="n")
axis(1, at=olympiads, labels=olympiads)

lines(olympiads, China, type="o", col="blue", pch=16)
lines(olympiads, Japan, type="o", col="green", pch=16)
lines(olympiads, Australia, type="o", col="purple", pch=16)
lines(olympiads, France, type="o", col="orange", pch=16)
lines(olympiads, Netherlands, type="o", col="brown", pch=16)
lines(olympiads, Britain, type="o", col="pink", pch=16)
lines(olympiads, Russia, type="o", col="cyan", pch=16)
lines(olympiads, Germany, type="o", col="darkgreen", pch=16)
lines(olympiads, South.Korea, type="o", col="darkblue", pch=16)

legend("topright", legend=c("США", "Китай", "Япония", "Австралия", "Франция", "Нидерланды", "Британия", "Россия", "Германия", "Южная Корея"),
       col=c("red", "blue", "green", "purple", "orange", "brown", "pink", "cyan", "darkgreen", "darkblue"),
       pch=16, cex=0.8)

#Зимние (3 призовых)
winter_prized <- read_excel("D:/Programming/R_homework/winter(3 best).xlsx")
Norway <- winter_prized$Норвегия
Germany <- winter_prized$Германия
USA <- winter_prized$США
Canada <- winter_prized$Канада
Russia <- winter_prized$Россия
France <- winter_prized$Франция
Italy <- winter_prized$Италия
Austria <- winter_prized$Австрия
Sweden <- winter_prized$Швеция
South.Korea <- winter_prized$`Южная Корея`
Switzerland <- winter_prized$Швейцария
China <- winter_prized$Китай
Netherlands <- winter_prized$Нидерланды
olympiads <- winter_prized$Год

plot(olympiads, Norway, type="o", col="blue", pch=16, 
     ylim=c(min(Norway, Germany, USA, Canada, Russia, France, Italy, Austria, Sweden, South.Korea, Switzerland, China, Netherlands, na.rm=TRUE),
            max(Norway, Germany, USA, Canada, Russia, France, Italy, Austria, Sweden, South.Korea, Switzerland, China, Netherlands, na.rm=TRUE)),
     xlab="Год Олимпиады", ylab="Медали", main="Динамика количества медалей у первых 7 мест за последние 6 зимних ОИ", xaxt="n")
axis(1, at=olympiads, labels=olympiads)

lines(olympiads, Norway, type="o", col="blue", pch=16)
lines(olympiads, Germany, type="o", col="green", pch=16)
lines(olympiads, USA, type="o", col="purple", pch=16)
lines(olympiads, Canada, type="o", col="orange", pch=16)
lines(olympiads, Russia, type="o", col="brown", pch=16)
lines(olympiads, France, type="o", col="pink", pch=16)
lines(olympiads, Italy, type="o", col="cyan", pch=16)
lines(olympiads, Austria, type="o", col="darkgreen", pch=16)
lines(olympiads, Sweden, type="o", col="darkblue", pch=16)
lines(olympiads, South.Korea, type="o", col="darkred", pch=16)
lines(olympiads, Switzerland, type="o", col="coral", pch=16)
lines(olympiads, China, type="o", col="gold", pch=16)
lines(olympiads, Netherlands, type="o", col="aquamarine", pch=16)

legend("topleft", 
       legend=c("Норвегия", "Германия", "США", "Канада", "Россия", "Франция", "Италия", "Австрия", "Швеция", "Южная Корея", "Швейцария", "Китай", "Нидерланды"),
       col=c("blue", "green", "purple", "orange", "brown", "pink", "cyan", "darkgreen", "darkblue", "darkred","coral","gold","aquamarine"),
       pch=16, cex=0.8)

#Фристайл
men <- rowSums(df[, c("М1", "М2", "М3","М4", "М5", "М6", "М7", "М8")], na.rm = TRUE)[-1]
women <- rowSums(df[, c("Ж1", "Ж2", "Ж3","Ж4", "Ж5", "Ж6", "Ж7", "Ж8")], na.rm = TRUE)[-1]
olympiads <- norway_freestyle$Год[-1]

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(olympiads, men, type="o", col="blue", pch=16, 
     ylim=c(min(men, women, na.rm=TRUE),
            max(men, women, na.rm=TRUE)),
     xlab="Год Олимпиады", ylab="Медали", main="Динамика призовых мест у Норвегии за последние 6 зимних ОИ по фристайлу", xaxt="n")
axis(1, at=olympiads, labels=olympiads)
lines(olympiads, women, type="o", col="red", pch=16)
legend("bottomright", 
       legend=c("Мужчины","Женщины"),
       col=c("blue", "red"),
       pch=16, cex=0.8)

pie(c(sum(na.omit(men)),sum(na.omit(women))),labels=c("Мужчины","Женщины"), col = c("blue","red"), main = "Всего медалей")
legend("topright", legend = c(paste("Мужчины:", sum(na.omit(men))), paste("Женщины:", sum(na.omit(women)))), fill = c("blue","red"))

men[is.na(men)] <- 0
women[is.na(women)] <- 0
data_matrix <- rbind(men, women)
barplot(data_matrix, beside = TRUE, col = c("blue", "red"), 
        names.arg = olympiads, 
        ylim = c(0, max(data_matrix)), density = c(50, 50),main="Медали мужчин и женщин в Олимпиадах")
legend("topleft", legend = c("Мужчины","Женщины"), fill = c("blue", "red"))
