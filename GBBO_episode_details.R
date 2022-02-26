library("rvest")
library("data.table")
library("magrittr")
library("dplyr")
library("ggplot2")


x <- c('episode','airdate','viewers_millions','7day_viewers', '28day_viewers', 'BBC_iplayer_requests', 'season')

#___________________________________season 2__________________________________________

# get season 2 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_2)"
webpage <- read_html(url)
table2 <- html_nodes(webpage,'table.wikitable')
table2 <- html_table(table2, header = TRUE)
colnames(season2) <- x
season2 <- data.frame(table2[11])
season2 <- select(season2,-BBC.Twoweekly.ranking)
season2['7day_viewers'] = NA
season2['28day_viewers'] = NA
season2['BBC_iplayer_requests'] =NA
season2['season'] = '2'
colnames(season2) <- x

#___________________________________season 3__________________________________________

# get season 2 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_3)"
webpage <- read_html(url)
table3 <- html_nodes(webpage,'table.wikitable')
table3 <- html_table(table3, header = TRUE)
season3 <- data.frame(table3[13])
season3 <- select(season3,-BBC.Twoweekly.ranking)
season3['7day_viewers'] = NA
season3['28day_viewers'] = NA
season3['BBC_iplayer_requests'] =NA
season3['season'] = '3'
colnames(season3) <- x


#___________________________________season 4__________________________________________

# get season 4 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_4)"
webpage <- read_html(url)
table4 <- html_nodes(webpage,'table.wikitable')
table4 <- html_table(table4, header = TRUE)
season4 <- data.frame(table4[18])
season4 <- select(season4,-c(BBC.Twoweekly.ranking,Weekly.rankingall.channels.42.))
season4['7day_viewers'] = NA
season4['28day_viewers'] = NA
season4['BBC_iplayer_requests'] =NA
season4['season'] = '4'
colnames(season4) <- x

#___________________________________season 5__________________________________________

# get season 5 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_5)"
webpage <- read_html(url)
table5 <- html_nodes(webpage,'table.wikitable')
table5 <- html_table(table5, header = TRUE)
season5 <- data.frame(table5[18])
season5 <- select(season5,-c(BBC.Oneweekly.ranking,Weekly.rankingall.channels.62.))
season5['7day_viewers'] = NA
season5['28day_viewers'] = NA
season5['BBC_iplayer_requests'] =NA
season5['season'] = '5'
colnames(season5) <- x

#___________________________________season 6__________________________________________

# get season 6 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_6)"
webpage <- read_html(url)
table6 <- html_nodes(webpage,'table.wikitable')
table6 <- html_table(table6, header = TRUE)
season6 <- data.frame(table6[18])
season6 <- select(season6,-c(BBC.Oneweekly.ranking,Weekly.rankingall.channels.52.))
season6['viewers_millions'] = NA
season6['season'] = '6'
season6 <- season6 %>% relocate(viewers_millions, .before = X7.day.viewers.millions.)
colnames(season6) <- x

#___________________________________season 7__________________________________________

# get season 7 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_7)"
webpage <- read_html(url)
table7 <- html_nodes(webpage,'table.wikitable')
table7 <- html_table(table7, header = TRUE)
season7 <- data.frame(table7[15])
season7 <- select(season7,-c(BBC.Oneweekly.ranking,Weekly.rankingall.channels.52.))
season7['viewers_millions'] = NA
season7['season'] = '7'
season7 <- season7 %>% relocate(viewers_millions, .before = X7.day.viewers.millions.)
colnames(season7) <- x

#___________________________________season 8__________________________________________

# get season 8 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_8)"
webpage <- read_html(url)
table8 <- html_nodes(webpage,'table.wikitable')
table8 <- html_table(table8, header = TRUE)
season8 <- data.frame(table8[15])
season8 <- select(season8,-c(Channel.4weekly.ranking,Weekly.rankingall.channels.38.))
season8['viewers_millions'] = NA
season8['BBC_iplayer_requests'] = NA
season8['season'] = '8'
season8 <- season8 %>% relocate(viewers_millions, .before = X7.day.viewers.millions.)
colnames(season8) <- x


#___________________________________season 9__________________________________________

# get season 9 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_9)"
webpage <- read_html(url)
table9 <- html_nodes(webpage,'table.wikitable')
table9 <- html_table(table9, header = TRUE)
season9 <- data.frame(table9[15])
season9 <- select(season9,-c(Channel.4weekly.ranking,Weekly.rankingall.channels.38.))
season9['viewers_millions'] = NA
season9['BBC_iplayer_requests'] = NA
season9['season'] = '9'
season9 <- season9 %>% relocate(viewers_millions, .before = X7.day.viewers.millions.)
colnames(season9) <- x

#___________________________________season 10__________________________________________

# get season 10 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_10)"
webpage <- read_html(url)
table10 <- html_nodes(webpage,'table.wikitable')
table10 <- html_table(table10, header = TRUE)
season10 <- data.frame(table10[15])
season10 <- select(season10,-c(Channel.4weekly.ranking,Weekly.rankingall.channels.30.))
season10['viewers_millions'] = NA
season10['BBC_iplayer_requests'] = NA
season10['season'] = '10'
season10 <- season10 %>% relocate(viewers_millions, .before = X7.day.viewers.millions.)
colnames(season10) <- x

#___________________________________season 11__________________________________________

# get season 11 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_11)"
webpage <- read_html(url)
table11 <- html_nodes(webpage,'table.wikitable')
table11 <- html_table(table11, header = TRUE)
season11 <- data.frame(table11[15])
season11 <- select(season11,-c(Channel.4weekly.ranking,Weekly.rankingall.channels.30.))
season11['viewers_millions'] = NA
season11['BBC_iplayer_requests'] = NA
season11['season'] = '11'
season11 <- season11 %>% relocate(viewers_millions, .before = X7.day.viewers.millions.)
colnames(season11) <- x

#___________________________________season 12__________________________________________

# get season 12 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_12)"
webpage <- read_html(url)
table12 <- html_nodes(webpage,'table.wikitable')
table12 <- html_table(table12, header = TRUE)
season12 <- data.frame(table12[13])
season12 <- select(season12,-c(Channel.4weekly.ranking,Weekly.rankingall.channels.22.))
season12['viewers_millions'] = NA
season12['BBC_iplayer_requests'] = NA
season12['season'] = '12'
season12 <- season12 %>% relocate(viewers_millions, .before = X7.day.viewers.millions.)
colnames(season12) <- x


#___________________________________combine all seasons__________________________________________


episodes <- rbind(season2, season3, season4, season5, season6, season7, season8, season9, season10, season11, season12)

write.csv(contestants,"/Users/starshine1000/Desktop/GBBO_episode_details.csv")


