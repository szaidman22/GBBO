#install.packages("rvest")
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("tidyr")
library("rvest")
library("data.table")
library("magrittr")
library("dplyr")
library("ggplot2")
library("tidyr")
library("stringr")
library("xml2")



#get episodes data to be joined with status data later
episodes.data <- read.csv("GBBO_allseasons_episodes_022422.csv")

#make id variable for episodes.data
episodes.data["baker.season.episode"] = str_c(episodes.data$baker,episodes.data$season,episodes.data$episode,sep = '_')

#create empty data frame for cleaned status data
results <- data.frame()

#loop to pull status data for seasons 3-12 
for (x in 3:11) {
  url <- gsub("yyy", x, "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_yyy)")
  webpage <- read_html(url)
  tables <- html_nodes(webpage,'table.wikitable')
  tables <- html_table(tables, header = TRUE, fill = TRUE)
  
  #the elimination chart with status information is the 2nd table 
  result <- data.frame(tables[2])
  
  #keep only relevant columns (some seasons have blank columns beyond episode 10)
  result <- subset(result, select = c("Elimination.chart","Elimination.chart.1","Elimination.chart.2","Elimination.chart.3","Elimination.chart.4","Elimination.chart.5","Elimination.chart.6","Elimination.chart.7","Elimination.chart.8","Elimination.chart.9","Elimination.chart.10"))
  
  #pivot data from wide to long
  result.pivot <- tidyr::pivot_longer(subset(result, Elimination.chart != 'Baker'),
                                      cols = starts_with("Elimination.chart."), 
                                      names_to = "episode", 
                                      values_to = "result", 
                                      names_prefix = "Elimination.chart.")
  
  #create column for season
  result.pivot['season'] = x
  
  #label first column of pivoted table baker
  names(result.pivot)[1] <- "baker"
  
  #remove episodes after elimination
  result.pivot <- result.pivot %>% group_by(baker) %>%
    mutate(out = min(which(result %in% c('OUT','WD') | row_number() == n()))) %>%
    filter(row_number() <= out) %>%
    select(-out)
  
  #extract background color information using xml method found online
  colors <- webpage %>% html_nodes(xpath='//*[@id="mw-content-text"]/div/table[3]') %>% html_nodes('td')
  colors <- bind_rows(lapply(xml_attrs(colors), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  
  #need to duplicate certain rows to unmerge merged cells and have one row per episode per baker in final table
  colors$colspan <- replace(colors$colspan, is.na(colors$colspan), 1)
  dups <- colors %>% transform(colspan = as.numeric(colspan)) 
  colorsduped <- dups[rep(seq_len(nrow(dups)), as.numeric(unlist(c(dups$colspan)))), ]
  colorsduped <- colorsduped %>% filter(tolower(colorsduped$style) %like% "background") 
  colorsduped <- colorsduped %>% filter(tolower(colorsduped$style) %like% "background") %>% filter (!style %like% "darkgrey") %>% filter (!style %like% "Silver" )
  
  #make new column with relevant background color meanings
  colorsduped <- colorsduped %>% mutate(status =
                                          case_when(tolower(style) %like% "cornflower" ~ "Favorite", 
                                                    tolower(style) %like% "plum" ~ "Least Favorite",
                                                    TRUE ~ "")
  )
  
  #reset index that got messed with by the duplications
  rownames(colorsduped) <- 1:nrow(colorsduped)
  
  #merge result.pivot and colorsduped to get all information in one place
  final <- merge(result.pivot, colorsduped[,c("style","status")], by = 0)
  
  #add data for each sesason to final dataframe
  results <- rbind(results, final)
  
}

#make id variable
results["baker.season.episode"] = str_c(results$baker,results$season,results$episode,sep = '_')

#-------------- season 2 --------------
# season 2 is separate because it only has 8 episodes

url = "https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_2)"
webpage <- read_html(url)
tables <- html_nodes(webpage,'table.wikitable')
tables <- html_table(tables, header = TRUE)

result <- data.frame(tables[2])

result <- subset(result, select = c("Elimination.chart","Elimination.chart.1","Elimination.chart.2","Elimination.chart.3","Elimination.chart.4","Elimination.chart.5","Elimination.chart.6","Elimination.chart.7","Elimination.chart.8"))

result.pivot <- tidyr::pivot_longer(subset(result, Elimination.chart != 'Baker'),
                                    cols = starts_with("Elimination.chart."), 
                                    names_to = "episode", 
                                    values_to = "result", 
                                    names_prefix = "Elimination.chart.")

result.pivot['season'] = 2
names(result.pivot)[1] <- "baker"

colors <- webpage %>% html_nodes(xpath='//*[@id="mw-content-text"]/div/table[3]') %>% html_nodes('td')
colors <- bind_rows(lapply(xml_attrs(colors), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
colors$colspan <- replace(colors$colspan, is.na(colors$colspan), 1)
dups <- colors %>% transform(colspan = as.numeric(colspan)) 
colorsduped <- dups[rep(seq_len(nrow(dups)), as.numeric(unlist(c(dups$colspan)))), ]
colorsduped <- colorsduped %>% filter(tolower(colorsduped$style) %like% "background")

colorsduped <- colorsduped %>% mutate(status =
                                        case_when(tolower(style) %like% "cornflower" ~ "Favorite", 
                                                  tolower(style) %like% "plum" ~ "Least Favorite",
                                                  TRUE ~ "")
)

rownames(colorsduped) <- 1:nrow(colorsduped)
final2 <- merge(result.pivot, colorsduped[,c("style","status")], by = 0)

final2["baker.season.episode"] = str_c(final2$baker,final2$season,final2$episode,sep = '_')


#---------------season 12---------------


url <- ("https://en.wikipedia.org/wiki/The_Great_British_Bake_Off_(series_12)")
webpage <- read_html(url)
tables <- html_nodes(webpage,'table.wikitable')
tables <- html_table(tables, header = TRUE, fill = TRUE)

#the elimination chart with status information is the 2nd table 
result <- data.frame(tables[2])

#keep only relevant columns (some seasons have blank columns beyond episode 10)
result <- subset(result, select = c("Elimination.chart","Elimination.chart.1","Elimination.chart.2","Elimination.chart.3","Elimination.chart.4","Elimination.chart.5","Elimination.chart.6","Elimination.chart.7","Elimination.chart.8","Elimination.chart.9","Elimination.chart.10"))

#pivot data from wide to long
result.pivot <- tidyr::pivot_longer(subset(result, Elimination.chart != 'Baker'),
                                    cols = starts_with("Elimination.chart."), 
                                    names_to = "episode", 
                                    values_to = "result", 
                                    names_prefix = "Elimination.chart.")

#create column for season
result.pivot['season'] = 12

#label first column of pivoted table baker
names(result.pivot)[1] <- "baker"

#remove episodes after elimination
result.pivot <- result.pivot %>% group_by(baker) %>%
  mutate(out = min(which(result %in% c('OUT','WD') | row_number() == n()))) %>%
  filter(row_number() <= out) %>%
  select(-out)

#extract background color information using xml method found online
colors <- webpage %>% html_nodes(xpath='//*[@id="mw-content-text"]/div/table[3]') %>% html_nodes('td')
colors <- bind_rows(lapply(xml_attrs(colors), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))

#need to duplicate certain rows to unmerge merged cells and have one row per episode per baker in final table
colors$colspan <- replace(colors$colspan, is.na(colors$colspan), 1)
dups <- colors %>% transform(colspan = as.numeric(colspan)) 
colorsduped <- dups[rep(seq_len(nrow(dups)), as.numeric(unlist(c(dups$colspan)))), ]
colorsduped <- colorsduped %>% filter(tolower(colorsduped$style) %like% "background") 
colorsduped <- colorsduped %>% filter(tolower(colorsduped$style) %like% "background") %>% filter (!style %like% "darkgrey") %>% filter (!style %like% "Silver" )

#make new column with relevant background color meanings
colorsduped <- colorsduped %>% mutate(status =
                                        case_when(tolower(style) %like% "cornflower" ~ "Favorite", 
                                                  tolower(style) %like% "plum" ~ "Least Favorite",
                                                  TRUE ~ "")
)

#reset index that got messed with by the duplications
rownames(colorsduped) <- 1:nrow(colorsduped)

#merge result.pivot and colorsduped to get all information in one place
final12 <- merge(result.pivot, colorsduped[,c("style","status")], by = 0)

final12 <- merge(result.pivot, colorsduped[,c("style","status")], by = 0)

final12["baker.season.episode"] = str_c(final12$baker,final12$season,final12$episode,sep = '_')


results <- rbind(results, final2, final12)


#left join results to episodes.data using id variable
joined <- merge(x=episodes.data,y=results[ , c("baker.season.episode","result","status")],by="baker.season.episode",all.x=TRUE)

write.csv(joined,"GBBO_allseasons_episodes_and_status_022422.csv")

