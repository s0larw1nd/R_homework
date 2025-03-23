places_m <- c(NA,NA,NA,2,2,4,3)
places_f <- c(3,2,2,3,NA,1,2)
olympiads <- c(1998,2002,2006,2010,2014,2018,2022)

barplot(height=places_m,
        names.arg = olympiads,
        xlab="Олимпиады",
        ylab="Медали",
        sub="Мужские медали по олимпиадам")

barplot(height=places_f,
        names.arg = olympiads,
        xlab="Олимпиады",
        ylab="Медали",
        sub="Женские медали по олимпиадам")

f_places_m <- c(NA,NA,NA,0,0,1,1)
f_places_f <- c(0,1,0,0,NA,0,0)

valid_indices_m <- !is.na(f_places_m) & f_places_m > 0
pie(f_places_m[valid_indices_m], labels=olympiads[valid_indices_m], main="Медали за первое место (мужские)")

valid_indices_f <- !is.na(f_places_f) & f_places_f > 0
pie(f_places_f[valid_indices_f], labels=olympiads[valid_indices_f], main="Медали за первое место (женские)")

prized_m <- c(NA,NA,NA,1,0,1,1)
prized_f <- c(1,1,1,1,NA,0,0)
plot(olympiads, prized_m, type = "o", col = "blue", xlab = "Олимпиады", ylab = "Медали", main = "Тенденция изменения количества призовых мест (мужские)")
plot(olympiads, prized_f, type = "o", col = "blue", xlab = "Олимпиады", ylab = "Медали", main = "Тенденция изменения количества призовых мест (женские)")

