library("rvest")
library("data.table")
library("magrittr")
library("dplyr")
library("ggplot2")


x <- c('baker','age','occupation','hometown','season')

#___________________________________season 2__________________________________________

# get season 2 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_2)"
webpage <- read_html(url)
table2 <- html_nodes(webpage,'table.wikitable')
table2 <- html_table(table2, header = TRUE)

season2 <- data.frame(table2[1])
season2['season'] = '2'
colnames(season2) <- x

#___________________________________season 3__________________________________________

# get season 3 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_3)"
webpage <- read_html(url)
table3nodes <- html_nodes(webpage,'table.wikitable')
table3 <- html_table(table3nodes, header = TRUE)

season3 <- data.frame(table3[1])
season3['season'] = '3'

season3 <- season3 %>%
  subset(select = -c(Links))

colnames(season3) <- x


#___________________________________season 4__________________________________________

# get season 4 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_4)"
webpage <- read_html(url)
table4nodes <- html_nodes(webpage,'table.wikitable')
table4 <- html_table(table4nodes, header = TRUE)

season4 <- data.frame(table4[1])
season4['season'] = '4'

season4 <- season4 %>%
  subset(select = -c(Links))

colnames(season4) <- x


#___________________________________season 5__________________________________________

# get season 5 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_5)"
webpage <- read_html(url)
table5nodes <- html_nodes(webpage,'table.wikitable')
table5 <- html_table(table5nodes, header = TRUE)

season5 <- data.frame(table5[1])
season5['season'] = '5'

season5 <- season5 %>%
  subset(select = -c(links))

colnames(season5) <- x

#___________________________________season 6__________________________________________

# get season 6 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_6)"
webpage <- read_html(url)
table6nodes <- html_nodes(webpage,'table.wikitable')
table6 <- html_table(table6nodes, header = TRUE)

season6 <- data.frame(table6[1])
season6['season'] = '6'

season6<- season6 %>%
  subset(select = -c(Links))

colnames(season6) <- x

#___________________________________season 7__________________________________________

# get season 7 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_7)"
webpage <- read_html(url)
table7nodes <- html_nodes(webpage,'table.wikitable')
table7 <- html_table(table7nodes, header = TRUE)

season7 <- data.frame(table7[1])
season7['season'] = '7'

season7 <- season7 %>%
  subset(select = -c(Links))

colnames(season7) <- x

#___________________________________season 8__________________________________________

# get season 8 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_8)"
webpage <- read_html(url)
table8nodes <- html_nodes(webpage,'table.wikitable')
table8 <- html_table(table8nodes, header = TRUE)

season8 <- data.frame(table8[1])
season8['season'] = '8'

season8 <- season8 %>%
  subset(select = -c(Links))

colnames(season8) <- x

#___________________________________season 9__________________________________________

# get season 9 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_9)"
webpage <- read_html(url)
table9nodes <- html_nodes(webpage,'table.wikitable')
table9 <- html_table(table9nodes, header = TRUE)

season9 <- data.frame(table9[1])
season9['season'] = '9'

season9 <- season9 %>%
  subset(select = -c(Links))

colnames(season9) <- x

#___________________________________season 10__________________________________________

# get season 10 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_10)"
webpage <- read_html(url)
table10nodes <- html_nodes(webpage,'table.wikitable')
table10 <- html_table(table10nodes, header = TRUE)

season10 <- data.frame(table10[1])
season10['season'] = '10'

season10 <- season10 %>%
  subset(select = -c(Links))

colnames(season10) <- x

#___________________________________season 11__________________________________________

# get season 11 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_11)"
webpage <- read_html(url)
table11nodes <- html_nodes(webpage,'table.wikitable')
table11 <- html_table(table11nodes, header = TRUE)

season11 <- data.frame(table11[1])
season11['season'] = '11'

season11 <- season11 %>%
  subset(select = -c(Links))

colnames(season11) <- x

#___________________________________season 12__________________________________________

# get season 12 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_12)"
webpage <- read_html(url)
table12nodes <- html_nodes(webpage,'table.wikitable')
table12 <- html_table(table12nodes, header = TRUE)

season12 <- data.frame(table12[1])
season12['season'] = '12'

season12 <- season12 %>%
  subset(select = -c(Links))

colnames(season12) <- x

#___________________________________combine all seasons__________________________________________


contestants <- rbind(season2, season3, season4, season5, season6, season7, season8, season9, season10, season11, season12)

write.csv(contestants,"/Users/starshine1000/Desktop/GBBO_contestants_042622.csv")

