library(tidyr)
library(dplyr)
library(zoo)
library(car)

dt <- read.csv("D:/Downloads/P_Data_Extract_From_World_Development_Indicators/data.csv", check.names = FALSE)

data <- dt %>%
  pivot_longer(
    cols = -c("Country Name", "Country Code", "Series Name", "Series Code"),
    names_to = "Year",
    values_to = "Value"
  ) %>% mutate(Year = as.integer(substr(Year, start = 1, stop = 4))) %>% mutate(Value=as.numeric(Value))

data[data==".."]<-NA
names(data) <- gsub(" ", "_", names(data))

replace_na_with_normal <- function(df) {
  group_stats <- df %>%
    group_by(Series_Code) %>%
    summarise(
      mean_val = mean(Value, na.rm = TRUE),
      sd_val = sd(Value, na.rm = TRUE)
    )
  
  df_filled <- df %>%
    group_by(Series_Code) %>%
    mutate(
      Value = ifelse(
        is.na(Value),
        rnorm(n(), 
              mean = group_stats$mean_val[group_stats$Series_Code == first(Series_Code)], 
              sd = group_stats$sd_val[group_stats$Series_Code == first(Series_Code)]),
        Value
      )
    ) %>%
    ungroup()
  
  return(df_filled)
}

data <- replace_na_with_normal(data)
data[data=="NaN"]<-0

#Кривая прироста ВВП
gdp_rise = unname(as.vector(data[data$Series_Code=="NY.GDP.MKTP.KD.ZG", "Value"]))[[1]]
plot(1960:2024,
     gdp_rise)
lines(1960:2024, gdp_rise, col = "red")

#Взаимное влияние атрибутов
wide_df <- reshape2::dcast(data, Year ~ Series_Name, value.var = "Value")
constant_series <- names(wide_df)[apply(wide_df, 2, function(x) sd(x, na.rm = TRUE) == 0)]
clean_data <- wide_df[, !(names(wide_df) %in% c(constant_series, "Year"))]
cor_matrix <- cor(clean_data, use = "pairwise.complete.obs")
scatterplotMatrix(cor_matrix, spread=FALSE, lty.smooth=2, main="Матрица диаграмм рассеяния")

names(clean_data) <- gsub(" ", "_", names(clean_data))
#Регрессионный анализ
fit <- lm(`GDP_(current_US$)` ~ ., data=clean_data)
summary(fit)

#Предсказания
names(clean_data)[names(clean_data) == 'GDP_(current_US$)'] <- 'GDP'
names(clean_data)[names(clean_data) == 'Birth_rate,_crude_(per_1,000_people)'] <- 'Birth_Rate'
names(clean_data)[names(clean_data) == 'Life_expectancy_at_birth,_total_(years)'] <- 'Life_Expect'
names(clean_data)[names(clean_data) == 'Medium_and_high-tech_manufacturing_value_added_(%_manufacturing_value_added)'] <- 'Manufacturing'

fit_small <- lm(GDP ~ Birth_Rate + Life_Expect + Manufacturing, 
                data=clean_data)
new_data <- data.frame(
   Birth_Rate = 10,
   Life_Expect = 80,
   Manufacturing = 25
)
predictions <- predict(fit_small, newdata = new_data)
print(predictions)