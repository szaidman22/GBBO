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

#get episodes data to be joined with status data later
setwd("") #whichever
episodes.data <- read.csv("bakeoff_episodes_2to11.csv")

#make id variable for episodes.data
episodes.data["baker.season.episode"] = str_c(episodes.data$baker,episodes.data$season,episodes.data$episode,sep = '_')

#create empty data frame for cleaned status data
results <- data.frame()

#loop to pull status data for seasons 3-11 
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

  #extract background color information using xml method found online
  colors <- webpage %>% html_nodes(xpath='//*[@id="mw-content-text"]/div/table[3]') %>% html_nodes('td')
  colors <- bind_rows(lapply(xml_attrs(colors), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  
  #need to duplicate certain rows to unmerge merged cells and have one row per episode per baker in final table
  colors$colspan <- replace(colors$colspan, is.na(colors$colspan), 1)
  dups <- colors %>% transform(colspan = as.numeric(colspan)) 
  colorsduped <- dups[rep(seq_len(nrow(dups)), as.numeric(unlist(c(dups$colspan)))), ]
  colorsduped <- colorsduped %>% filter(tolower(colorsduped$style) %like% "background")

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

result <- subset(result, select = c("Elimination.chart","Elimination.chart.1","Elimination.chart.2","Elimination.chart.3","Elimination.chart.4","Elimination.chart.5","Elimination.chart.6","Elimination.chart.7","Elimination.chart.8","Elimination.chart.9","Elimination.chart.10"))
                             
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
final <- merge(result.pivot, colorsduped[,c("style","status")], by = 0)

results <- rbind(results, final)
results["baker.season.episode"] = str_c(results$baker,results$season,results$episode,sep = '_')

#left join results to episodes.data using id variable
joined <- merge(x=episodes.data,y=results[ , c("baker.season.episode","result","status")],by="baker.season.episode",all.x=TRUE)
