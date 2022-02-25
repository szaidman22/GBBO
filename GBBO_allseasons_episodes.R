library("rvest")
library("data.table")
library("magrittr")
library("dplyr")
library("ggplot2")

#___________________________________season 2__________________________________________

# get season 2 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_2)"
webpage <- read_html(url)
table2 <- html_nodes(webpage,'table.wikitable')
table2 <- html_table(table2, header = TRUE)

#construct a for loop to build season 2 comprehensive data frame
count <- 0
season2 <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c('baker','signature','technical.rank','showstopper','episode', 'signatuare.name','technical.name','showstopper.name','season')
colnames(season2) <- x

for (episode in table2[3:10]) {
  ep <- data.frame(episode)
  count = count +1
  ep['episode'] = count
  
  library(stringr)
  signature_name <- str_replace_all(colnames(ep[2]), "[[:punct:]]", " ")
  ep['signature.challenge'] = str_remove(signature_name, 'Signature.')
  
  technical_name <- str_replace_all(colnames(ep[3]), "[[:punct:]]", " ")
  ep['technical.challenge'] = str_remove(technical_name, 'Technical.')
  
  showstopper_name <- str_replace_all(colnames(ep[4]), "[[:punct:]]", " ")
  ep['showstopper.challenge'] = str_remove(showstopper_name, 'Showstopper.')
  
  ep['season'] = '2'
  
  colnames(ep) <- x
  
  season2 <- rbind(season2, ep)
}

#___________________________________season 3__________________________________________

# get season 3 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_3)"
webpage <- read_html(url)
table3nodes <- html_nodes(webpage,'table.wikitable')
table3 <- html_table(table3nodes, header = TRUE)

#construct a for loop to build season 2 comprehensive data frame
count <- 0
season3 <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c('baker','signature','technical.rank','showstopper','episode', 'signatuare.name','technical.name','showstopper.name','season')
colnames(season3) <- x

for (episode in table3[3:12]) {
  ep <- data.frame(episode)
  count = count +1
  ep['episode'] = count
  
  library(stringr)
  signature_name <- str_replace_all(colnames(ep[2]), "[[:punct:]]", " ")
  ep['signature.challenge'] = str_remove(signature_name, 'Signature.')
  
  technical_name <- str_replace_all(colnames(ep[3]), "[[:punct:]]", " ")
  ep['technical.challenge'] = str_remove(technical_name, 'Technical.')
  
  showstopper_name <- str_replace_all(colnames(ep[4]), "[[:punct:]]", " ")
  ep['showstopper.challenge'] = str_remove(showstopper_name, 'Showstopper.')
  
  ep['season'] = '3'
  
  colnames(ep) <- x
  
  season3 <- rbind(season3, ep)
}

#___________________________________season 4__________________________________________

# get season 4 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_4)"
webpage <- read_html(url)
table4nodes <- html_nodes(webpage,'table.wikitable')
table4 <- html_table(table4nodes, header = TRUE)

#construct a for loop to build season 2 comprehensive data frame
count <- 0
season4 <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c('baker','signature','technical.rank','showstopper','episode', 'signatuare.name','technical.name','showstopper.name','season')
colnames(season4) <- x

for (episode in table4[3:12]) {
  ep <- data.frame(episode)
  count = count +1
  ep['episode'] = count
  
  library(stringr)
  signature_name <- str_replace_all(colnames(ep[2]), "[[:punct:]]", " ")
  ep['signature.challenge'] = str_remove(signature_name, 'Signature.')
  
  technical_name <- str_replace_all(colnames(ep[3]), "[[:punct:]]", " ")
  ep['technical.challenge'] = str_remove(technical_name, 'Technical.')
  
  showstopper_name <- str_replace_all(colnames(ep[4]), "[[:punct:]]", " ")
  ep['showstopper.challenge'] = str_remove(showstopper_name, 'Showstopper.')
  
  ep['season'] = '4'
  
  colnames(ep) <- x
  
  season4 <- rbind(season4, ep)
}

#___________________________________season 5__________________________________________

# get season 5 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_5)"
webpage <- read_html(url)
table5nodes <- html_nodes(webpage,'table.wikitable')
table5 <- html_table(table5nodes, header = TRUE)

#construct a for loop to build season 2 comprehensive data frame
count <- 0
season5 <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c('baker','signature','technical.rank','showstopper','episode', 'signatuare.name','technical.name','showstopper.name','season')
colnames(season5) <- x

for (episode in table5[3:12]) {
  ep <- data.frame(episode)
  count = count +1
  ep['episode'] = count
  
  library(stringr)
  signature_name <- str_replace_all(colnames(ep[2]), "[[:punct:]]", " ")
  ep['signature.challenge'] = str_remove(signature_name, 'Signature.')
  
  technical_name <- str_replace_all(colnames(ep[3]), "[[:punct:]]", " ")
  ep['technical.challenge'] = str_remove(technical_name, 'Technical.')
  
  showstopper_name <- str_replace_all(colnames(ep[4]), "[[:punct:]]", " ")
  ep['showstopper.challenge'] = str_remove(showstopper_name, 'Showstopper.')
  
  ep['season'] = '5'
  
  colnames(ep) <- x
  
  season5 <- rbind(season5, ep)
}

#___________________________________season 6__________________________________________

# get season 6 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_6)"
webpage <- read_html(url)
table6nodes <- html_nodes(webpage,'table.wikitable')
table6 <- html_table(table6nodes, header = TRUE)

#construct a for loop to build season 2 comprehensive data frame
count <- 0
season6 <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c('baker','signature','technical.rank','showstopper','episode', 'signatuare.name','technical.name','showstopper.name','season')
colnames(season6) <- x

for (episode in table6[3:12]) {
  ep <- data.frame(episode)
  count = count +1
  ep['episode'] = count
  
  library(stringr)
  signature_name <- str_replace_all(colnames(ep[2]), "[[:punct:]]", " ")
  ep['signature.challenge'] = str_remove(signature_name, 'Signature.')
  
  technical_name <- str_replace_all(colnames(ep[3]), "[[:punct:]]", " ")
  ep['technical.challenge'] = str_remove(technical_name, 'Technical.')
  
  showstopper_name <- str_replace_all(colnames(ep[4]), "[[:punct:]]", " ")
  ep['showstopper.challenge'] = str_remove(showstopper_name, 'Showstopper.')
  
  ep['season'] = '6'
  
  colnames(ep) <- x
  
  season6 <- rbind(season6, ep)
}

#___________________________________season 7__________________________________________

# get season 7 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_7)"
webpage <- read_html(url)
table7nodes <- html_nodes(webpage,'table.wikitable')
table7 <- html_table(table7nodes, header = TRUE)

#construct a for loop to build season 2 comprehensive data frame
count <- 0
season7 <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c('baker','signature','technical.rank','showstopper','episode', 'signatuare.name','technical.name','showstopper.name','season')
colnames(season7) <- x

for (episode in table7[3:12]) {
  ep <- data.frame(episode)
  count = count +1
  ep['episode'] = count
  
  library(stringr)
  signature_name <- str_replace_all(colnames(ep[2]), "[[:punct:]]", " ")
  ep['signature.challenge'] = str_remove(signature_name, 'Signature.')
  
  technical_name <- str_replace_all(colnames(ep[3]), "[[:punct:]]", " ")
  ep['technical.challenge'] = str_remove(technical_name, 'Technical.')
  
  showstopper_name <- str_replace_all(colnames(ep[4]), "[[:punct:]]", " ")
  ep['showstopper.challenge'] = str_remove(showstopper_name, 'Showstopper.')
  
  ep['season'] = '7'
  
  colnames(ep) <- x
  
  season7 <- rbind(season7, ep)
}

#___________________________________season 8__________________________________________

# get season 8 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_8)"
webpage <- read_html(url)
table8nodes <- html_nodes(webpage,'table.wikitable')
table8 <- html_table(table8nodes, header = TRUE)

#construct a for loop to build season 2 comprehensive data frame
count <- 0
season8 <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c('baker','signature','technical.rank','showstopper','episode', 'signatuare.name','technical.name','showstopper.name','season')
colnames(season8) <- x

for (episode in table8[3:12]) {
  ep <- data.frame(episode)
  count = count +1
  ep['episode'] = count
  
  library(stringr)
  signature_name <- str_replace_all(colnames(ep[2]), "[[:punct:]]", " ")
  ep['signature.challenge'] = str_remove(signature_name, 'Signature.')
  
  technical_name <- str_replace_all(colnames(ep[3]), "[[:punct:]]", " ")
  ep['technical.challenge'] = str_remove(technical_name, 'Technical.')
  
  showstopper_name <- str_replace_all(colnames(ep[4]), "[[:punct:]]", " ")
  ep['showstopper.challenge'] = str_remove(showstopper_name, 'Showstopper.')
  
  ep['season'] = '8'
  
  colnames(ep) <- x
  
  season8 <- rbind(season8, ep)
}

#___________________________________season 9__________________________________________

# get season 9 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_9)"
webpage <- read_html(url)
table9nodes <- html_nodes(webpage,'table.wikitable')
table9 <- html_table(table9nodes, header = TRUE)

#construct a for loop to build season 2 comprehensive data frame
count <- 0
season9 <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c('baker','signature','technical.rank','showstopper','episode', 'signatuare.name','technical.name','showstopper.name','season')
colnames(season9) <- x

for (episode in table9[3:12]) {
  ep <- data.frame(episode)
  count = count +1
  ep['episode'] = count
  
  library(stringr)
  signature_name <- str_replace_all(colnames(ep[2]), "[[:punct:]]", " ")
  ep['signature.challenge'] = str_remove(signature_name, 'Signature.')
  
  technical_name <- str_replace_all(colnames(ep[3]), "[[:punct:]]", " ")
  ep['technical.challenge'] = str_remove(technical_name, 'Technical.')
  
  showstopper_name <- str_replace_all(colnames(ep[4]), "[[:punct:]]", " ")
  ep['showstopper.challenge'] = str_remove(showstopper_name, 'Showstopper.')
  
  ep['season'] = '9'
  
  colnames(ep) <- x
  
  season9 <- rbind(season9, ep)
}

#___________________________________season 10__________________________________________

# get season 10 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_10)"
webpage <- read_html(url)
table10nodes <- html_nodes(webpage,'table.wikitable')
table10 <- html_table(table10nodes, header = TRUE)

#construct a for loop to build season 2 comprehensive data frame
count <- 0
season10 <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c('baker','signature','technical.rank','showstopper','episode', 'signatuare.name','technical.name','showstopper.name','season')
colnames(season10) <- x

for (episode in table10[3:12]) {
  ep <- data.frame(episode)
  count = count +1
  ep['episode'] = count
  
  library(stringr)
  signature_name <- str_replace_all(colnames(ep[2]), "[[:punct:]]", " ")
  ep['signature.challenge'] = str_remove(signature_name, 'Signature.')
  
  technical_name <- str_replace_all(colnames(ep[3]), "[[:punct:]]", " ")
  ep['technical.challenge'] = str_remove(technical_name, 'Technical.')
  
  showstopper_name <- str_replace_all(colnames(ep[4]), "[[:punct:]]", " ")
  ep['showstopper.challenge'] = str_remove(showstopper_name, 'Showstopper.')
  
  ep['season'] = '10'
  
  colnames(ep) <- x
  
  season10 <- rbind(season10, ep)
}

#___________________________________season 11__________________________________________

# get season 11 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_11)"
webpage <- read_html(url)
table11nodes <- html_nodes(webpage,'table.wikitable')
table11 <- html_table(table11nodes, header = TRUE)

#construct a for loop to build season 2 comprehensive data frame
count <- 0
season11 <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c('baker','signature','technical.rank','showstopper','episode', 'signatuare.name','technical.name','showstopper.name','season')
colnames(season11) <- x

for (episode in table11[3:12]) {
  ep <- data.frame(episode)
  count = count +1
  ep['episode'] = count
  
  library(stringr)
  signature_name <- str_replace_all(colnames(ep[2]), "[[:punct:]]", " ")
  ep['signature.challenge'] = str_remove(signature_name, 'Signature.')
  
  technical_name <- str_replace_all(colnames(ep[3]), "[[:punct:]]", " ")
  ep['technical.challenge'] = str_remove(technical_name, 'Technical.')
  
  showstopper_name <- str_replace_all(colnames(ep[4]), "[[:punct:]]", " ")
  ep['showstopper.challenge'] = str_remove(showstopper_name, 'Showstopper.')
  
  ep['season'] = '11'
  
  colnames(ep) <- x
  
  season11 <- rbind(season11, ep)
}


#___________________________________season 12__________________________________________

# get season 11 tables from wikipedia
url <- "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_12)"
webpage <- read_html(url)
table12nodes <- html_nodes(webpage,'table.wikitable')
table12 <- html_table(table11nodes, header = TRUE)

#construct a for loop to build season 2 comprehensive data frame
count <- 0
season12 <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c('baker','signature','technical.rank','showstopper','episode', 'signatuare.name','technical.name','showstopper.name','season')
colnames(season12) <- x

for (episode in table12[3:12]) {
  ep <- data.frame(episode)
  count = count +1
  ep['episode'] = count
  
  library(stringr)
  signature_name <- str_replace_all(colnames(ep[2]), "[[:punct:]]", " ")
  ep['signature.challenge'] = str_remove(signature_name, 'Signature.')
  
  technical_name <- str_replace_all(colnames(ep[3]), "[[:punct:]]", " ")
  ep['technical.challenge'] = str_remove(technical_name, 'Technical.')
  
  showstopper_name <- str_replace_all(colnames(ep[4]), "[[:punct:]]", " ")
  ep['showstopper.challenge'] = str_remove(showstopper_name, 'Showstopper.')
  
  ep['season'] = '12'
  
  colnames(ep) <- x
  
  season12 <- rbind(season12, ep)
}




#___________________________________combine all seasons__________________________________________

episodes.data <- rbind(season2, season3, season4, season5, season6, season7, season8, season9, season10, season11, season12)

write.csv(episodes.data,"/Users/starshine1000/Desktop/GBBO_allseasons_episodes_022422.csv")


