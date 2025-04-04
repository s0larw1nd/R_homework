install.packages("rvest")
library(rvest)

columns <- html_nodes(read_html("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2014"), "table>thead>tr>th") %>% html_text()%>%as.array();
selector<-"table>tbody"

table <- data.frame(matrix(ncol = length(columns)+2, nrow = 0))
colnames(table) <- c(columns,"Year","Mid")

years <- 2014:2021
url_template <- "https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=%d"

for (year in years) {
  url <- sprintf(url_template, year)
  page <- read_html(url)
  
  tbl <- html_nodes(page, "table>tbody") %>% html_table() %>% as.data.frame()
  colnames(tbl) <- columns
  tbl$Rank <- 1:NROW(tbl)
  tbl$Year <- year
  tbl$Mid <- FALSE
  
  table <- rbind(table,tbl)
}

url_template <- "https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=%d-mid"
for (year in years) {
  url <- sprintf(url_template, year)
  page <- read_html(url)
  
  tbl <- html_nodes(page, "table>tbody") %>% html_table() %>% as.data.frame()
  colnames(tbl) <- columns
  tbl$Rank <- 1:NROW(tbl)
  tbl$Year <- year
  tbl$Mid <- TRUE
  
  table <- rbind(table,tbl)
}

countries <- c("Georgia", "Mexico", "Poland", "Italy", "Cyprus")

table_countries <- subset(table, Country %in% countries)
table_countries <- table_countries[order(table_countries$Year, table_countries$Mid), ]

colors <- c("red", "blue", "green", "purple", "orange")
indices <- colnames(table_countries)[-c(2,12,13)]

for (index in indices) {
  table_countries[[index]] <- as.numeric(gsub("-", NA, table_countries[[index]]))
}

years <- 2014:2021
years_labels <- rep(years, each = 2)
years_labels[seq(2, length(years_labels), by = 2)] <- paste0(years_labels[seq(2, length(years_labels), by = 2)], "-mid")

years_positions <- rep(years, each = 2) + rep(c(0, 0.5), length.out = length(years_labels))

par(mfrow = c(2, 5 + 1), mar = c(4, 4, 2, 1))

for (index in indices) {
  plot(NA, xlim = range(years_positions), ylim = range(table_countries[[index]], na.rm = TRUE),
       xaxt = "n", xlab = "Год", ylab = "Ранг", main = paste("График для", index))
  
  for (i in seq_along(countries)) {
    sub_df <- subset(table_countries, Country == countries[i])
    x_positions <- sub_df$Year + ifelse(sub_df$Mid, 0.5, 0) 
    lines(x_positions, sub_df[[index]], col = colors[i], lwd = 2)
  }
  
  axis(1, at = years_positions, labels = years_labels, las = 2, cex.axis = 0.8)
}

plot.new()
legend("center", legend = countries, col = colors, lwd = 2, pch = 16, cex = 1.2, title = "Страны")

#Задание 4
museum_names <- html_nodes(read_html("https://ru.wikipedia.org/wiki/%D0%9A%D0%B0%D0%BB%D0%B8%D0%BD%D0%B8%D0%BD%D0%B3%D1%80%D0%B0%D0%B4#%D0%9C%D1%83%D0%B7%D0%B5%D0%B8"),
                           xpath = "//div/ul/li[a and (./text()[contains(., 'музе')]) or (a[contains(normalize-space(.), 'Музей')])]/a[1]") %>% html_text()%>%as.array()%>%unique();

museum_links <- html_nodes(read_html("https://ru.wikipedia.org/wiki/%D0%9A%D0%B0%D0%BB%D0%B8%D0%BD%D0%B8%D0%BD%D0%B3%D1%80%D0%B0%D0%B4#%D0%9C%D1%83%D0%B7%D0%B5%D0%B8"),
                                  xpath = "//div/ul/li[a and (./text()[contains(., 'музе')]) or (a[contains(normalize-space(.), 'Музей')])]/a[1]") %>% html_attr("href") %>% as.array() %>% unique();

museums_wikilinks <- paste0("https://ru.wikipedia.org", museum_links)

museums_addr <- c()
museums_info <- c()
for (link in museums_wikilinks) {
  temp = html_node(read_html(link),
                   xpath = "//th[contains(text(), 'Адрес')]/following-sibling::td[1]/span") %>% html_text() %>% as.vector();
  museums_addr <- c(museums_addr, temp)
  
  temp = html_node(read_html(link),
                   xpath = "//table[@data-name='Музей']/following-sibling::p[1]") %>% html_text() %>% as.vector();
  museums_info <- c(museums_info, gsub("\n", "", temp))
}

museums_df <- data.frame(
  museum_names,
  unique(museums_addr),
  unique(museums_info),
  unique(museums_wikilinks)
)
colnames(museums_df) <- c("Название", "Адрес", "Описание", "Ссылка");

