#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(emojifont)
library(plotly)
library(DiagrammeR)
library(RColorBrewer)
library(shinyjs)
library(rgdal) 
library(leaflet)
library(leaflet.extras)
library(ggmap)
library(sp)
library(fontawesome) 
library(tidytext)
library(ggthemes)
library(stringr)
library(svglite)
library(htmltools) 
library(htmlwidgets) 
library(extrafont)
library(bslib)
library(tokenizers)
library(RWeka)
library(tm)
library(tidytext)
library(rvest)
library(shinythemes)
library(wordcloud)
library(quanteda)
library(magrittr)
library(ggpubr)
library(ggalt)   
library(ggrepel)
library(rvest)
library(stringr)
library(plotly)
library(bslib)
library(maditr)







# Define UI for application that draws a histogram
ui <- fluidPage(
  setBackgroundColor("white"),
  
  tags$head(tags$style(
    HTML('
         #sidebar1, #sidebar2, #tab1, #sidebar3, #sidebar4, #tab1_1, #tab1_2 {
            background-color: #ffdbfa;
            border-color: #edd99f;
            font-family: "Arial"
         }
         #col, #hometowns, #starbaker {
            font-family: "Arial"
         }
         
         #contentbar {
         border-bottom-left-radius: 10px;
         border-bottom-right-radius: 10px;


         }
         
         .leaflet-control {
            text-align: left;
         }
         
        
    ')
  )),
  column(2,
  div(id="contentbar",
    
    column(12,div(style = "height:80px;background-color: #de4e74;")),
    div(img(src="contents.png", width = "160px", height="50px"),align="center"),
    div(img(src="goodies.png", width = "85%", height="100px"),align="center"),
    column(12,div(style = "height:20px;background-color: #de4e74;;")),
    column(12, id="contentbar", align="center", 
           actionButton("btn1", "The Basics",
                        onclick = "location.href='#basics';",
                        style = "background-color: white;"),
           br(),
           column(12,div(style = "height:10px;background-color: #de4e74;;")),
           actionButton("btn1", "Hometowns",
                        onclick = "location.href='#hometowns';",
                        style = "background-color: white;"),
           br(),
           column(12,div(style = "height:10px;background-color: #de4e74;;")),
           actionButton("btn1", "Occupation", 
                        onclick = "location.href='#oc';",
                        style = "background-color: white;"),
           br(),
           column(12,div(style = "height:10px;background-color: #de4e74;;")),
           actionButton("btn1", "Themes", 
                        onclick = "location.href='#weektheme';",
                        style = "background-color: white;"),
           br(),
           column(12,div(style = "height:10px;background-color: #de4e74;;")),
           actionButton("btn1", "Compare Seasons", 
                        onclick = "location.href='#starbaker';",
                        style = "background-color: white;"),
           br(),
           column(12,div(style = "height:10px;background-color: #de4e74;;")),
           actionButton("btn1", "Scores by Episode", 
                        onclick = "location.href='#bakerperf';",
                        style = "background-color: white;"),
           br(),
           column(12,div(style = "height:10px;background-color: #de4e74;;")),
           actionButton("btn1", "Ingredients", 
                        onclick = "location.href='#ingr';",
                        style = "background-color: white;"
                        ),
           br(),
           column(12,div(style = "height:50px;background-color: #de4e74;")),
           br(),
           style = "background-color:  #de4e74;"
    ), style = "position:fixed; width:inherit;background-color: #de4e74;")),
  
  column(10,
  div(fluidPage(

      
  column(12, id = "col",align="center",
  column(12,div(style = "height:70px;background-color: white;")),
  img(src="title.png", width = "700px", height="100%", align="center"),
  br(),
  #img(src="paul.png", width = "250px", height="100%"),
  img(src="cutie.png", width = "150px", height="100%"),
  #img(src="prue.png", width = "200px", height="100%"),
  
  br(),
  h3("Everything you need to know about the Great British Bake Off to become a champion!", align = "center")),
  column(12,div(style = "height:150px;background-color: white;")),
  column(12, align="center",
  img(src="strawarrow.png", width = "100px", height="100%")),
  column(12,div(style = "height:150px;background-color: white;")),
  column(12, align="center",id="col",
         img(src="bouche.png", width = "150px", height="100%"),
         column(12,div(style = "height:30px;background-color: white;")),
          h4("Keep scrolling to learn what it takes to make it in the world of competitive 
             British amateur baking!")),
  column(12,div(style = "height:150px;background-color: white;")),
  column(12, align="center",
         img(src="strawarrow.png", width = "100px", height="100%")),
  column(12,div(style = "height:150px;background-color: white;")),
  column(12, align="center", id ="col",
         br(),
         h2(id="basics",img(src="basics.png", width = "200px", height="100%")),
         br(),
         img(src="cast.png", width = "600px", height="100%"),
         br(),
         p("credit: Channel 4"),
         br(),
         column(12, id="col",
         column(1),
         column(10, align= "left", id="col",
                h3('Seasons'),
                p("The Great British Bake Off has had 12 seasons (also referred to as series),
                  with the first season airing in August 2010. Season 1 had a considerably different
                  structure from seasons 2-12, so we have omitted season 1 in our Journey to Star Baker.
                  All seasons with the exception of season 1 and 2 have 10 episodes.",style = "font-size:16px;"),
         column(1))),
         column(12, id="col",
         column(1),
         column(10, align="left",
         h3('Episode Structure')),
         column(1)),
         
         column(12, id="col",
         column(1),
         column(10, align= "center",
         grVizOutput("episodeflow",width = "70%", height = "250px")),
         column(1)),
         
  
        column(1),
       column(10, align= "left", id="col",
         
         p("Each episode consists of three challenges, always in the same order: the Signature, 
           the Technical, and the Showstopper. For the Signature and Showstopper, bakers choose 
           their own recipes that fit within the challenge brief, and can prepare ahead of time. 
           For the Technical, bakers must follow a recipe they are given on the spot, without practice. 
           The Technical is judged blind and bakers are ranked based on their final product. 
           Each challenge has a strict time limit.",style = "font-size:16px;"),
         br(),
         p("Successful bakers must be skilled not only in the art of baking, but in time-management
           and organization. At the end of each episode, the judges assess baker performance in all three
           challenges holistically, taking into account taste, creativity, and presentation. Judges 
           discuss favorite and least-favorite bakers, crown a Star Baker for the episode, 
           and eliminate one (occasionally 2 or 0) baker(s).",style = "font-size:16px;")),
       column(1),
       column(12,div(style = "height:20px;background-color: white;")),
       column(12, id="col", align="center",
         h3('Score calculation:'),
         tags$h4(tags$span(style="color:#86dba5", "1 point for judge favorite")),
         tags$h4(tags$span(style="color:goldenrod", "2 points for star baker",icon('star'))),
         tags$h4(tags$span(style="color:#e68a95", "-1 point for judge least favorite")),
       br(),
       br(),
       column(1),
       column(10, align= "left",id="col",
       p("Bakers have been assigned an unofficial numeric score based on the results of judgings. 
         This score will be used throughout the Journey to Starbaker to assess baker performance."
         ,style = "font-size:16px;")),
       column(1)
       ),
         column(12,div(style = "height:150px;background-color: white;")),
         column(12,align="center",
         img(src="strawarrow.png", width = "100px", height="100%"))),
         column(12,div(style = "height:150px;background-color: white;"),
                
        column(1),
        column(10, id= "col", align="center",
        h2(id="hometowns",img(src="hometowns.png", width = "550px", height="100%")),
        
        br(),
        p("The Great British Bake off is a British television baking competition and hence has contestants from 
        all over the United Kingdom. View this United Kingdom map to learn more about the contestants' 
        and winners' backgrounds, including biographical information, occupation, and season performance. Where are your favorite bakers from?",
          align="left", style = "font-size:16px;"),
        p('This map is interactive! Hover over the map to learn more about a local administrative county and hover 
          or click the markers to learn more about a specific individual.',
          align="left", style = "font-size:16px;color:#db48a5;"),
        leafletOutput('uk_map', height="400px"),
        br(),
        br(),
        p("It is notable that none of the winners originate from the same hometown or local administrative county. Furthermore, 
          there are no winners (yet) from Ireland or Wales and most contestants are from Southern England!",
          align="left", style = "font-size:16px;")
        ),
        column(1),
    
        column(12,div(style = "height:150px;background-color: white;")),
        column(12,align="center",
               img(src="strawarrow.png", width = "100px", height="100%"))),
        column(12,div(style = "height:150px;background-color: white;"),
                
        column(1),
        column(10, id = "col", align="center",
        h2(id="oc",img(src="occupations.png", width = "550px", height="100%")),
        p("All amateur bakers are invited to the Great British Bake Off. Check out the occupations of 
          the contestants across all seasons." , align="left", style = "font-size:16px;"),
        p('This chart is interactive! Hover over to see the specific occupations that 
          fell within each category', align="left", style = "font-size:16px;color:#db48a5;"),
        plotlyOutput('occupation'),
        br(),
        p('"Business" Category had the most contestants (18), but did not produce any winners. 
          Science/Technology and Student category produced 2 winners each, despite their unassuming 
          size in the total population', align="left", style = "font-size:16px;")
        ),
        column(1),
 
        column(12,div(style = "height:150px;background-color: white;")),
        column(12,align="center",
               img(src="strawarrow.png", width = "100px", height="100%"))),
        column(12,div(style = "height:150px;background-color: white;"),      
               
               
         column(1),
         column(10, align="center",id="col",
                h2(id="weektheme",img(src="themes.png", width = "450px", height="100%")),
                br(),
                p('Each week, the Signature, Technical and Showstopper challenges must adhere to a theme. 
                   Some themes are repeated season after season, while others are unique. 
                   The chart below shows which themes occurred in which seasons, and at what 
                  episode in the season they occurred.', align="left", style = "font-size:16px;"),
                plotOutput("wkthm2", width = "800px", height = "600px"),
                br(),
                p('Cake week almost always occurs at the beginning of the season,
                  with Biscuit week and Bread week following in either the second or 
                  third episodes. Almost every season has a Dessert week, and every season
                  has at least one Pastry themed episode. Three to four episodes every season
                  fall into a series of less-common themes, based on things like ingredients,
                  techniques, countries or time periods. Almost every season has Pâtisserie as the
                  semi-final theme.', align="left", style = "font-size:16px;")
         ),
         column(1)
  ),
  column(12,div(style = "height:150px;background-color: white;")),
  column(12,align="center",
         img(src="strawarrow.png", width = "100px", height="100%")),
  column(12,div(style = "height:150px;background-color: white;")),
  column(1),
  column(10, align = "center", id ="col",
  h2(id="starbaker",img(src="season.png", width = "500px", height="100%")),
  br(),
  p("Use this chart to explore contestant performance in each season. 
    Did the bakers with the most praise (and highest scores) always make it to the Final?
    Which star bakers were underdogs and which were shoo-ins? Use the radio
    button to view the biggest upsets in GBBO history.", 
    align="left",style = "font-size:16px;" ),
  br(),
  p('This chart is interactive! Click within the grid and hover over points to view baker details.', 
    align="center",style = "font-size:16px;color:#db48a5;"),
  radioButtons(inputId = "upsets", 
               label = 'Show Upsets?', 
               selected = "No" , 
               inline = TRUE, 
               choices = c("Yes","No"))

  
  ),
  
  column(12,div(style = "height:10px;background-color: white;")),
  
  column(12, align="center",
         plotlyOutput("all_seasons", width = "80%") 
  ),
  
  column(1),
  column(10, id="col",align="left",
  br(),
  br(),
  p("For the most part, bakers who make it to the Final scored higher than those who were 
    eliminated. Season winners do not seem to have a clear advantage over runners up 
    based on final score - once you make it to the Final, it's anyone's game.",
    style = "font-size:16px;"),
  br(),
  p("Some notable upsets that stand out in this chart:",
    style = "font-size:16px;"),
  tags$div(
    tags$ul(
      tags$li("Steph, Season 10: Steph was a clear front-runner going into the Final, with the most praise of
              any baker in GBBO history. Unfortunately, Steph's performance faltered, and she did not take
    home the crown.")
    )
  ),
  tags$div(
    tags$ul(
      tags$li("Hermine and Jürgen, Seasons 11 and 12: Hermine and Jürgen both steadily impressed judges
  and ultimately had the highest scores of their respective seasons. Both saw a swift and
    unexpected decline and were eliminated before the Final.")
    )
  )
  ),
  
  
  column(12,div(style = "height:150px;background-color: white;")),
  column(12,align="center",
         img(src="strawarrow.png", width = "100px", height="100%")),
  column(12,div(style = "height:150px;background-color: white;")),
  column(1),
  column(10, id ="col",
         h2(id="bakerperf",img(src="deepdive.png", width = "450px", height="100%"), align="center"),
         br(),
         p('How did each baker perform in each episode? Were there any shocking comebacks?
           Who got lucky and made it furthest with the least amount of praise? Use this chart 
           to explore judge favorites, least favorites, and star bakers in each season. 
           Switch to the Winners tab to compare winner trajectories.',style = "font-size:16px;"),
         br(),
         tabsetPanel(               
           tabPanel("Compare by Season",
                    id = "tab1",
                    h5('Explore judge favorites, least favorites, and star 
                       bakers and see how each contestant progressed throughout their season.'),
                    
                    br(),         
                    sidebarLayout(
                      sidebarPanel(width = 3,
                                   id="sidebar1",
                                   
                                   selectInput(inputId = "season", 
                                                label = 'Season', 
                                                selected = 5 , 
                                                choices = c(2:12)),
                                   radioButtons(inputId = "sort", 
                                                label = 'Sort by:', 
                                                selected = "maxep" , 
                                                inline = FALSE, 
                                                choices = c("Max episode" = "maxep",
                                                            "Score when eliminated" = "endsum")),
                                   radioButtons(inputId = "score", 
                                                label = 'Show final score?', 
                                                selected = "No" , 
                                                inline = FALSE, 
                                                choices = c("Yes",
                                                            "No")),
                                   tags$strong('Score calculation:'),
                                   br(), 
                                   
                                   tags$strong(tags$span(style="color:#86dba5", "1 point for judge favorite",icon('circle'))),
                                   br(), 
                                   tags$strong(tags$span(style="color:goldenrod", "2 points for star baker",icon('star'))),
                                   br(), 
                                   tags$strong(tags$span(style="color:#e68a95", "-1 point for judge least favorite",icon('circle'))),
                                   br(),
                                   tags$strong(tags$span(style="color:#fff9f0", "Eliminated",icon('thumbs-down'))),
                                   br(),
                                   tags$strong(tags$span(style="color:darkgoldenrod", "Winner",icon('trophy'))),
                                   br()
                                   
                      ),
                      mainPanel(
                        column(12, align="left",
                               h4(textOutput("caption")),
                               plotOutput("season_performance_chart", width ="600px",height="500px"),
                               p('There is no clear guarantee that being a favorite early on will get you 
                                 further in the competition. Plenty of bakers have been able to stay in
                                 the competition for several weeks while consistently being chosen as judge
                                 least favorites. In many seasons, bakers with the most star baker awards
                                 do not go on to win.'
                                 ,style = "font-size:16px;"))
                      ))
           ),
           tabPanel("Compare Winners",
                    br(),
                    sidebarLayout(
                      sidebarPanel(width =3,
                                   id = "sidebar2",
                                   radioButtons(inputId = "winsort", 
                                                label = 'Sort by:', 
                                                selected = "season" , 
                                                inline = FALSE, 
                                                choices = c("Season" = "season",
                                                            "Final Score" = "endsum")),
                                   tags$strong('Score calculation:'),
                                   br(), 
                                   tags$strong(tags$span(style="color:#86dba5", "1 point for judge favorite",icon('circle'))),
                                   br(), 
                                   tags$strong(tags$span(style="color:goldenrod", "2 points for star baker",icon('star'))),
                                   br(), 
                                   tags$strong(tags$span(style="color:#e68a95", "-1 point for judge least favorite",icon('circle'))),
                                   br(),
                      ),
                      mainPanel(
                        plotOutput("winplot",height="500px",width="600px"),
                        p('All winners have at least one star baker award, with the exception of
                          David from Season 10. All winners with the exception of Sophie from Season 8 have
                          been a judge least favorite at least once. For the most part, winners perform 
                          very well in the middle of the season, especially at episode 5.',style = "font-size:16px;")
                      )
                    )
           )
         )
         ),
         ####################### Ingredients ################ kiersten
         br(),
         br(),
         column(12,div(style = "height:150px;background-color: white;")),
         column(12,align="center",
                img(src="strawarrow.png", width = "100px", height="100%")),
         column(12,div(style = "height:150px;background-color: white;")),
         column(1, id="col")
         ,column(10, id ="col",
                 h2(id="ingr",img(src="ingredients.png", width = "550px", height="100%"), align="center"),
                 br(),
                 p('Are there ingredients to avoid in a specific challenge? 
                   Which ingredients should you be familiar with? 
                   Use the charts below to explore which ingredients you should use to become Star Baker.'
                   ,style = "font-size:16px;"),
                 
                 br(), 
                 tabsetPanel(               
                   tabPanel("Winners v. Losers",
                            id = "tab1_1",
                            br(), 
                            p('This chart displays how many times each ingredient was used by Star Bakers and Favorite Contestants
            vs. Least Favorite contestants and those voted out. Adjust the graph to display the Showstopper or Signature challenges, 
            as well as the number of words shown and how the graph is sorted.',style = "font-size:14px;"),
                            br(),
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                           id="sidebar3",
                                           
                                           radioButtons(inputId = "challenge", 
                                                        label = 'Challenge:', 
                                                        selected = "Showstopper" , 
                                                        inline = FALSE, 
                                                        choices = c("Showstopper",
                                                                    "Signature")),
                                           
                                           radioButtons(inputId = "sort_kb", 
                                                        label = 'Sort By:', 
                                                        selected = "pos_sort" , 
                                                        inline = FALSE, 
                                                        choices = c("Star Baker and Favorites" = "pos_sort",
                                                                    "Eliminated and Least Favorites" = "neg_sort",
                                                                    "Difference" = "Difference")),
                                           
                                           sliderInput(inputId = "num", 
                                                       label = "Number of Words", 
                                                       value = 15, min = 1, max = 30),
                                           
                                           
                              ),
                              mainPanel(
                                column(12, align="center",
                                       h4(textOutput("title")),
                                       plotOutput("text_graph",height="500px", width="500px"))
                              )),
                            br(),
                            p('Overall, chocolate is the most popular ingredient. Those who excel in GBBO tend to use chocolate
               more in the showstopper challenge than those eliminated or least favorite, but they use chocolate less
               often in the signature challenge. Judges tend not to like strawberry as an ingredient. Those eliminated
               or least favorite used strawberries 10 times, while those who were Star Baker or Favorite only used
               them twice in the showstopper challenge. Star Bakers and Favorites tend to use white chocolate more
                 than those eliminated or least favorite in the showstopped challenge. Judges tend to like hazelnut
                 in the signature challenge.',style = "font-size:16px;"),
                            br()
                   ), ##### Soobin's Word Cloud in Tab 2
                   tabPanel("Word cloud by Season",
                            id = "tab1_2",
                            br(),
                            p("Anything is possible at the Great British Bake Off. Check out the common ingredients 
             across each season." ,align="left",style = "font-size:14px;"),
                            br(),
                            sidebarLayout(
                              sidebarPanel(width =2,
                                           id = "sidebar4",
                                           radioButtons(inputId = "season_ingredients", 
                                                        label = 'Season', 
                                                        selected = 5 , 
                                                        inline = FALSE, 
                                                        choices = c(2:12))
                                           
                              ),
                              mainPanel(
                                plotOutput("wordcloud",
                                           width = "100%")
                              )
                            ),
                            p('Chocolate is consistently the most popular ingredient across all seasons. However,
                 as the seasons progress, more ingredients appear commonly, making chocolate less
                 dominant ingredient',align="left",style = "font-size:16px;"),
                   )
                 ),
                 br(),
                 column(12,div(style = "height:150px;background-color: white;")),
                 column(12,align="center",
                        img(src="strawarrow.png", width = "100px", height="100%")),
                 column(12,div(style = "height:150px;background-color: white;")),
                 br(),
                 h3("You now have all of the knowledge needed to become STAR BAKER!", align="center"),
                 br(),
                 column(12,align="center",img(src="star.png", width = "350px", height="100%")),
                 br(),
                 h2(id="ingr",img(src="luck.png", width = "350px", height="100%"), align="center"),
                 br(),
                 br(),
                 div(tags$a(href="https://en.wikipedia.org/wiki/The_Great_British_Bake_Off", "All data comes from The Great British Bake Off Wikipedia pages."), align="center")
                 
                
                 )
  ))
  ))
  


# Define server logic required to draw a histogram
server <- function(input, output) {

  #import data
  gbbo_all <- read.csv("GBBO_allseasons_episodes_and_status_022422.csv")
  weekthemes <- read.csv("GBBO_week_themes.csv")
  gbbo_lat_long = read.csv("GBBO_contestants_lat_long_042622.csv")
  
  

  
  
  #################### Episode flowchart ################ Sofia
  
  output$episodeflow <- renderGrViz({
    grViz("
  digraph {
  graph [layout = dot, rankdir = LR]
  
  node [shape = rectangle, style = filled, fillcolor = '#edd99f', color = '#edd99f', fontname = 'arial']
  edge [arrowhead = none,color = '#522611']
  a [label = '1. Signature']
  b [label = '2. Technical']
  c [label = '3. Showstopper' ]
  node [shape=oval]
  d [label = 'Judging']
    
  a -> b -> c -> d
    
  edge [arrowhead = normal,color = '#522611']
  node [shape = rectangle, style = filled, fillcolor = 'goldenrod', color = 'goldenrod']  
  d -> 'Star Baker'
  
  node [fillcolor = '#86dba5', color = '#86dba5']  
  d -> 'Favorites'
  
  node [fillcolor = '#e68a95', color = '#e68a95']  
  d -> 'Least Favorites'
  
  node [fillcolor = 'gray40', color = 'gray40', fontcolor = 'white']  
  d -> 'Eliminated'
  }")
  })
  
  #################### week themes ################ Sofia
  
  
 # mycolors <- colorRampPalette(c('#db48a5','#e08cff','goldenrod','#42c8f5',"#522611"))(12)
  
  mycolors <- colorRampPalette(c('#fa8ca9','#ffdbfa','lightgoldenrod','#cce0ff',"#d4b7a9"))(12)
  
  
  output$wkthm2 = renderPlot ( weekthemes %>%
    group_by(category) %>%
    summarize(count = n(),
              avgweek = mean(week)) %>%
    mutate(ranking = rank(avgweek, ties.method = 'first')) %>%
    inner_join(weekthemes) %>%
    mutate(week_theme = factor(week_theme, levels= unique(week_theme[order(desc(ranking))]))) %>%
    mutate(season = as.factor(season)) %>%
    ggplot(aes(season, week_theme, fill=category)) + 
    geom_tile(color = 'gray20', size = .5) +
    scale_fill_manual(values = mycolors, name = "Category") +
    scale_x_discrete(position = "top",
                       labels=c("2" = "S2", "3" = "S3",
                                "4" = "S4", "5" = "S5",   
                                "6" = "S6", "7" = "S7", 
                                "8" = "S8", "9" = "S9", 
                                "10" = "S10", "11" = "S11", 
                                "12" = "S12")) +   
    labs(color = "Category") +
    geom_text(aes(label = week), color="black", size=rel(6), face="bold") +
    xlab("") + 
    ylab("") +
    theme(panel.grid.major.y = element_line(color = "#edd99f"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_line(),
          panel.border = element_rect(fill=NA,color="white", size=0.5, linetype="solid"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill="white"),
          plot.background = element_rect(fill="white"),
          legend.text = element_text(size=14),
          legend.title = element_text(size=18),
          axis.text = element_text(color="black", size=18) )
  
  )
  
  #################### Contestant Performance all Seasons Jitter ################ Sofia
  
  upsets = reactive ({
    switch(input$upsets,
           "Yes" = .8,
           "No" = 0)
  })
  
  ### Data frame for all seasons jitter ###
  jitter =  gbbo_all %>%
    mutate(outcome = case_when(result == '' ~ status, is.na(result) == FALSE ~ result)) %>%
    mutate(points = case_when(outcome == 'Least Favorite' ~ -1, 
                              outcome == 'Favorite' ~ 1,
                              outcome == 'SB' ~ +2)) %>% 
    mutate(points = case_when(is.na(points) == TRUE ~ 0, is.na(points) == FALSE ~ points)) %>%
    mutate(shapeflag = case_when(points == 2  ~ 'SB',
                                 points == -1 ~ 'LF',
                                 points == 1 ~ 'F',
                                 points == 0 & outcome == 'WINNER'  ~ 'W',
                                 points == 0 & outcome == 'OUT' ~ 'O',
                                 points == 0 & outcome == 'Runner-up' ~ 'R',
                                 TRUE ~ 'N')) %>%
    arrange(baker, episode) %>%
    group_by(season,baker) %>% 
    mutate(endstatus = last(outcome)) %>%
    mutate(maxep = max(episode)) %>%
    mutate(maxep = case_when(endstatus == 'WINNER' ~ 11L,
                             TRUE ~ maxep)) %>%
    mutate(winflag = case_when(endstatus == 'WINNER' ~ 'Winner',
                               endstatus == 'Runner-up' ~'Runner-up',
                               TRUE ~ 'Eliminated before final')) %>%
    mutate(lastepflag = case_when(episode == max(episode) ~ 'Y',
                                  TRUE ~ 'N')) %>%
    mutate(csum = cumsum(points)) %>%
    summarize(endsum = last(csum), winflag = unique(winflag), maxep = max(episode))
  
  
  ### ggplot chart for all seasons jitter ###
  p = reactive({
    set.seed(3)
    ggplot(jitter, aes(season, endsum), group = baker) +
    geom_jitter(width = .1,aes(color = winflag, 
                               size = winflag,
                               text = paste('Baker:', baker,
                                            '<br>Status:', winflag,
                                            '<br>Max Episode:', maxep,
                                            '<br>Final Score:', endsum)), 
                alpha =.8
    ) +
    scale_color_manual(name = "",
                       values = c('Winner' = 'goldenrod',
                                  'Runner-up' = '#86dba5',
                                  'Eliminated before final' = '#e68a95')
    ) +
    scale_size_manual(name = "",
                      values = c('Winner' = 5,
                                 'Runner-up' = 4,
                                 'Eliminated before final' = 2)
    ) +
    geom_text(aes(label = case_when(baker == "Hermine"  ~ 'Upset!',
                                    baker == "Jürgen" ~ 'Upset!',
                                    baker == "Steph" ~ 'Upset!',
                                    TRUE ~ '')),
             size = 6,alpha= upsets(), color = 'red') +
    scale_x_continuous(limits = c(1.8,12.2), breaks=seq(2,12,by=1)) +
    scale_y_continuous(limits = c(-4,13), breaks=seq(-4,13,by=2)) +
    coord_flip() +
    geom_vline(xintercept=2.5, color = "gray30", linetype = "dashed", size = .5) +
    geom_vline(xintercept = 3.5,color = "gray30", linetype = "dashed", size = .5) +
    geom_vline(xintercept = 4.5,color = "gray30", linetype = "dashed", size = .5) +
    geom_vline(xintercept=5.5,color = "gray30", linetype = "dashed", size = .5) +
    geom_vline(xintercept = 6.5,color = "gray30", linetype = "dashed", size = .5) +
    geom_vline(xintercept=7.5,color = "gray30", linetype = "dashed", size = .5) +
    geom_vline(xintercept = 8.5,color = "gray30", linetype = "dashed", size = .5) +
    geom_vline(xintercept=9.5,color = "gray30", linetype = "dashed", size = .5) +
    geom_vline(xintercept=10.5,color = "gray30", linetype = "dashed", size = .5) +
    geom_vline(xintercept = 11.5,color = "gray30", linetype = "dashed", size = .5) +
    labs(x = "Season", y = "Final Score") +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_text(family = "Arial"),
      text = element_text(size = 14, family = 'Arial')
    ) 
  })
  
  
  ### Plotly output ###
  output$all_seasons <-renderPlotly({
    ggplotly(p(),tooltip = c("text")) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "v", 
                           xanchor = "center", 
                           x = 1,
                           y=.3,
                           bordercolor = "#edd99f",
                           borderwidth = 2,
                           bgcolor = "#ffdbfa",
                           font = list(
                             family = "Arial",
                             size = 14,
                             color = "#000")))     
  })
  
  #################### Deep Dive Contestant Performance ################ Sofia

  ### All Bakers data frame ###
  season_df = reactive({
    gbbo_all %>%
      filter(season == input$season) %>%
      mutate(outcome = case_when(result == '' ~ status, is.na(result) == FALSE ~ result)) %>%
      mutate(points = case_when(outcome == 'Least Favorite' ~ -1, 
                                outcome == 'Favorite' ~ 1,
                                outcome == 'SB' ~ +2)) %>% 
      mutate(points = case_when(is.na(points) == TRUE ~ 0, is.na(points) == FALSE ~ points)) %>%
      mutate(shapeflag = case_when(points == 2  ~ 'SB',
                                   points == -1 ~ 'LF',
                                   points == 1 ~ 'F',
                                   points == 0 & outcome == 'WINNER'  ~ 'W',
                                   points == 0 & outcome == 'OUT' ~ 'O',
                                   points == 0 & outcome == 'Runner-up' ~ 'R',
                                   TRUE ~ 'N')) %>%
      arrange(baker, episode) %>%
      group_by(baker) %>% 
      mutate(endstatus = last(outcome)) %>%
      mutate(maxep = max(episode)) %>%
      mutate(maxep = case_when(endstatus == 'WINNER' ~ 11L,
                               TRUE ~ maxep)) %>%
      mutate(lastepflag = case_when(episode == max(episode) ~ 'Y',
                                    TRUE ~ 'N')) %>%
      mutate(csum = cumsum(points)) %>%
      mutate(endsum = last(csum)) %>%
      ungroup() %>%
      mutate(baker = factor(baker, levels= unique(baker[order(desc(get(input$sort)))])))
    
  }) 
  
  ### Winners data frame ###
  winchart = reactive({
    gbbo_all %>%
      filter(result == 'WINNER') %>%
      select(baker,season)%>%    
      inner_join(gbbo_all, by = c('baker','season')) %>%
      mutate(baker = paste(baker,"\n(Season ",season,")",sep="")) %>%
      mutate(outcome = case_when(result == '' ~ status, is.na(result) == FALSE ~ result)) %>%
      mutate(points = case_when(outcome == 'Least Favorite' ~ -1, 
                                outcome == 'Favorite' ~ 1,
                                outcome == 'SB' ~ +2)) %>% 
      mutate(points = case_when(is.na(points) == TRUE ~ 0, is.na(points) == FALSE ~ points)) %>%
      mutate(shapeflag = case_when(points == 2  ~ 'SB',
                                   points == -1 ~ 'LF',
                                   points == 1 ~ 'F',
                                   points == 0 & outcome != "WINNER" ~'N' ,
                                   points == 0 & outcome == "WINNER"  ~ 'W')) %>%
      arrange(baker, episode) %>%
      group_by(baker) %>% 
      mutate(csum = cumsum(points))  %>%
      mutate(endsum = last(csum)) %>%
      mutate(maxep = max(episode)) %>%
      mutate(lastepflag = case_when(episode == max(episode) ~ 'Y',
                                    TRUE ~ 'N')) %>%
      ungroup() %>%
      mutate(baker = factor(baker, levels= unique(baker[order(desc(get(input$winsort)))])))
  })
  
  ### Reactive functions for user input ###
  
  score = reactive ({
    switch(input$score,
           "Yes" = 6,
           "No" = 0)
  })
  
  
  output$caption <- renderText({
    paste("Season ",input$season,sep="")
  }) 
  
  ### All Baker Chart ###
  output$season_performance_chart <- renderPlot(
    
    ggplot(season_df(),aes(x=episode, y=csum, group = baker)) +
      geom_line(size = 2, color = '#edd99f') +
      facet_wrap(~baker, ncol = 1,
                 strip.position = "left") +
      geom_point(aes(color = shapeflag,size=shapeflag, shape = shapeflag)) +
      scale_color_manual(name = "shapeflag",
                         labels = c("star baker", "least favorite", "favorite"),
                         breaks=c("SB", "LF", "F"),
                         values = c('SB' = "goldenrod",'LF' = '#e68a95','F' = "#86dba5",
                                    'N' = '#edd99f','W' = '#edd99f','O' = 'black','R' = 'purple')) +
      scale_shape_manual(name = "shapeflag",
                         labels = c("star baker", "least favorite", "favorite"),
                         breaks=c("SB", "LF", "F"),
                         values = c('SB' = 8,'LF' = 16,'F' = 16,'N' = 16,'W' = 16,'O' = 4,'R' = 16)) +
      scale_size_manual(name = "shapeflag",
                        labels = c("star baker", "least favorite", "favorite"),
                        breaks=c("SB", "LF", "F"),
                        values = c('SB' = 1,'LF' = 6,'F' = 6,'N' = 6,'W' = 1,'O' = 1,'R' = 1)) +  
      geom_text(aes(label = ifelse(season_df()$shapeflag == 'SB', fontawesome('fa-star'), ''), 
                    family="fontawesome-webfont"),
                size = 10,color = 'goldenrod', vjust = .3) +
      geom_text(aes(label = ifelse(season_df()$outcome == 'WINNER', fontawesome('fa-trophy'), ''), 
                    family="fontawesome-webfont"),
                size = 12,color = 'darkgoldenrod',vjust = .4) +
      geom_text(aes(label = ifelse(season_df()$outcome == 'Runner-up', fontawesome('fa-thumbs-down'), ''), 
                    family="fontawesome-webfont"),
                size = 10,color = '#edd99f',vjust = .5) +
      geom_text(aes(label = ifelse(season_df()$outcome == 'OUT', fontawesome('fa-thumbs-down'), ''), 
                    family="fontawesome-webfont"),
                size = 10,color = '#edd99f',vjust = .5) +
      geom_text(aes(label = ifelse(season_df()$lastepflag == 'Y', csum, '')),
                size = score(),color = 'black',hjust = -2) +
      theme_minimal() +
      labs(x = "Episode") +
      theme(aspect.ratio=1/18,
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
            axis.text.x = element_text(size=20),
            strip.text.y.left = element_text(angle=0,size = 20),
            text = element_text(size = 20),
            plot.margin = margin(t = 0,r = 0,b = 0,l = 0)
            ) +
      scale_y_continuous(limits = c(-20,25), breaks=seq(0,10,by=3)) +
      scale_x_continuous(limits = c(1,12), breaks=seq(0,10,by=1)) 
  )
  
  ### Winner Chart ###
  output$winplot <- renderPlot(
    ggplot(winchart(),aes(x=episode, y=csum, color = baker, shape = shapeflag, group = baker)) +
      geom_line(size = 2, color = '#edd99f') +
      facet_wrap(~baker,ncol = 1,
                 strip.position = "left") +
      geom_point(aes(color = shapeflag,size=shapeflag)) +
      scale_color_manual(name = "shapeflag",
                         values = c('SB' = "goldenrod",'LF' = '#e68a95','F' = "#86dba5",
                                    'N' = '#edd99f','W' = '#edd99f')) +
      scale_shape_manual(name = "shapeflag",
                         values = c('SB' = 8,'LF' = 16,'F' = 16,'N' = 16,'W' = 16)) +
      scale_size_manual(name = "shapeflag",
                        values = c('SB' = 1,'LF' = 6,'F' = 6,'N' = 6,'W' = 1)) +
      geom_text(aes(label = ifelse(winchart()$shapeflag == 'SB', fontawesome('fa-star'), ''), 
                    family="fontawesome-webfont"),
                size = 10,color = 'goldenrod',vjust = .3) +
      geom_text(aes(label = ifelse(winchart()$lastepflag == 'Y', 
                                   fontawesome('fa-trophy'), ''), 
                    family="fontawesome-webfont"),
                size = 12,color = 'darkgoldenrod',vjust = .4) +
      geom_text(aes(label = ifelse(winchart()$lastepflag == 'Y', csum, '')),
                size = 6,color = 'black',hjust = -2.5) +
      labs(x = "Episode") +
      theme_minimal() +
      theme(aspect.ratio=1/14,
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
            axis.text.x = element_text(size=20),
            strip.text.y.left = element_text(angle=0,size = 20),
            text = element_text(size = 20),
            plot.margin = margin(t = 0,r = 0,b = 0,l = 0)) +
      scale_y_continuous(limits = c(-20,25), breaks=seq(0,10,by=3)) +
      scale_x_continuous(limits = c(1,12), breaks=seq(0,10,by=1))
  )
  
  
  
  #################### Hometown Map ################ Billy
  
  
  
  # Get only the winners
  gbbo_season_result_baker <- gbbo_all %>%
    filter(result == "WINNER") %>%
    select(season, result, baker)
  
  # Read in UK local admin areas from https://exploratory.io/map which uses Boundary data 
  # from UK Data Service (https://borders.ukdataservice.ac.uk/bds.html)
  local_uk_areas <- readOGR("./uk_la.geojson")
  
  
  # Add jitter for overlapping points
  gbbo_lat_long$lat <- jitter(gbbo_lat_long$lat, factor = 15)
  gbbo_lat_long$lon <- jitter(gbbo_lat_long$lon, factor = 15)
  
  # All SP
  hometown_sp <- SpatialPointsDataFrame(coords = gbbo_lat_long[,c("lon", "lat")], 
                                        data = gbbo_lat_long, 
                                        proj4string = CRS("+proj=longlat +datum=WGS84"))
  hometown_local_uk <- over(x = hometown_sp, y = local_uk_areas)
  
  hometowns_local_uk_with_count <- hometown_local_uk %>% 
    group_by(name) %>% 
    dplyr::summarize(number_of_contestants = n()) %>%
    arrange(desc(number_of_contestants))
  
  # Get number of winners to merge
  gbbo_lat_long_winners <- gbbo_lat_long %>% 
    filter(result == "WINNER")
  
  # Get contestants that were not winners
  gbbo_lat_long_contestants <- gbbo_lat_long %>%
    filter(result != "WINNER" | is.na(result))
  
  hometown_sp_winners <- SpatialPointsDataFrame(coords = gbbo_lat_long_winners[,c("lon", "lat")], 
                                                data = gbbo_lat_long_winners, 
                                                proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  hometown_local_uk_with_winner <- over(x = hometown_sp_winners, y = local_uk_areas)
  
  hometowns_local_uk_with_winner_count <- hometown_local_uk_with_winner %>% 
    group_by(name) %>% 
    dplyr::summarize(number_of_winners = n()) %>%
    arrange(desc(number_of_winners)) %>% 
    select(name, number_of_winners)
  
  hometown_local_uk %>% 
    group_by(name)
  
  # MERGE
  # First merge all counts
  local_uk_areas_with_total_contestant_count <- merge(local_uk_areas, hometowns_local_uk_with_count, by="name") 
  # Now merge winner counts
  local_uk_areas_with_total_contestant_count <- merge(local_uk_areas_with_total_contestant_count,
                                                      hometowns_local_uk_with_winner_count, by="name") 
  
  ## Replace NA with 0s
  local_uk_areas_with_total_contestant_count@data <-local_uk_areas_with_total_contestant_count@data %>%
    mutate(number_of_contestants = replace_na(number_of_contestants,0), 
           number_of_winners = replace_na(number_of_winners,0))
  
  # Labels
  admin_labs <- lapply(seq(nrow(local_uk_areas_with_total_contestant_count@data)), function(i) {
    paste0( '<p><b>Local Administrative Area</b>: ',
            local_uk_areas_with_total_contestant_count@data[i, "name"], '<p></p>', 
            '<b>Number of contestants</b>: ',
            local_uk_areas_with_total_contestant_count@data[i, "number_of_contestants"],'</p><p>', 
            '<b>Number of winners</b>: ',
            local_uk_areas_with_total_contestant_count@data[i, "number_of_winners"],'</p><p></p>') 
  })
  
  contestant_labs <- lapply(seq(nrow(gbbo_lat_long_contestants)), function(i) {
    paste0( '<p><b>Baker</b>: ', gbbo_lat_long_contestants[i, "baker"], '<p></p>',
            '<b>Season</b>: ', gbbo_lat_long_contestants[i, "season"],'</p><p>', 
            '<b>Hometown</b>: ', gbbo_lat_long_contestants[i, "hometown"],'</p><p>', 
            '<b>Occupation</b>: ', gbbo_lat_long_contestants[i, "occupation"],'</p><p>', 
            '<b>Age</b>: ', gbbo_lat_long_contestants[i, "age"],'</p><p></p>') 
  })
  
  winner_labs <- lapply(seq(nrow(gbbo_lat_long_winners)), function(i) {
    paste0( '<p><b>Baker</b>: ', gbbo_lat_long_winners[i, "baker"], '<p></p>',
            '<b>Season</b>: ', gbbo_lat_long_winners[i, "season"],'</p><p>', 
            '<b>Hometown</b>: ', gbbo_lat_long_winners[i, "hometown"],'</p><p>', 
            '<b>Occupation</b>: ', gbbo_lat_long_winners[i, "occupation"],'</p><p>', 
            '<b>Age</b>: ', gbbo_lat_long_winners[i, "age"],'</p><p></p>') 
  })
  
  # Icons
  iconSet <- awesomeIconList(Winner = makeAwesomeIcon(icon= 'king', markerColor = 'pink', iconColor = 'darkgoldenrod', library = "glyphicon", extraClasses=""),
                             Contestant = makeAwesomeIcon(icon= 'user', markerColor = 'purple', iconColor = 'white', library = "glyphicon")
  )
  
  # group names: Contestants and Winners
  groups <- c("Winners" <- "<div style='position: relative; display: inline-block;' class='awesome-marker-icon-pink awesome-marker'><i style='color: darkgoldenrod'  class='glyphicon glyphicon-king icon-darkgoldenrod '></i></div>Winners",
              "Contestants" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-purple awesome-marker'><i class='glyphicon glyphicon-user icon-white'></i></div>Contestants"
  )
  
  # Create temp folder to save svgs for session.. deletes when you quit or close
  folder <- tempfile()
  dir.create(folder)
  
  # Copied but modified sofia's code to produce chart per individual on click
  season_df1 <- gbbo_all %>%
    mutate(outcome = case_when(result == '' ~ status, is.na(result) == FALSE ~ result)) %>%
    mutate(points = case_when(outcome == 'Least Favorite' ~ -1, 
                              outcome == 'Favorite' ~ 1,
                              outcome == 'SB' ~ +2)) %>% 
    mutate(points = case_when(is.na(points) == TRUE ~ 0, is.na(points) == FALSE ~ points)) %>%
    mutate(shapeflag = case_when(points == 2  ~ 'SB',
                                 points == -1 ~ 'LF',
                                 points == 1 ~ 'F',
                                 points == 0 & outcome == 'WINNER'  ~ 'W',
                                 points == 0 & outcome == 'OUT' ~ 'O',
                                 points == 0 & outcome == 'Runner-up' ~ 'R',
                                 TRUE ~ 'N')) %>%
    arrange(baker, episode) %>%
    group_by(baker) %>% 
    mutate(endstatus = last(outcome)) %>%
    mutate(maxep = max(episode)) %>%
    mutate(maxep = case_when(endstatus == 'WINNER' ~ 11L,
                             TRUE ~ maxep)) %>%
    mutate(lastepflag = case_when(episode == max(episode) ~ 'Y',
                                  TRUE ~ 'N')) %>%
    mutate(csum = cumsum(points)) %>%
    mutate(endsum = last(csum)) %>%
    ungroup() %>%
    mutate(baker = factor(baker, levels= unique(baker[order(desc(maxep))])))
  
  createContestantWinnerPlot <- function(id, show_season) {
    # Filter contestant or winner 
    individual_df <-season_df1 %>%
      filter(baker == id & season == show_season)
    
    individual_plot <- ggplot(individual_df, aes(x=episode, y=csum, group = baker)) +
      geom_line(size = 2, color = '#edd99f') +
      geom_point(aes(color = shapeflag, size=shapeflag, shape = shapeflag)) +
      scale_color_manual(name = "shapeflag",
                         labels = c("star baker", "least favorite", "favorite"),
                         breaks=c("SB", "LF", "F"),
                         values = c('SB' = "goldenrod",'LF' = '#e68a95','F' = "#86dba5",
                                    'N' = '#edd99f','W' = '#edd99f','O' = 'black','R' = 'purple'
                         )) +
      scale_shape_manual(name = "shapeflag",
                         labels = c("star baker", "least favorite", "favorite"),
                         breaks=c("SB", "LF", "F"),
                         values = c('SB' = 8,'LF' = 16,'F' = 16,'N' = 16,
                                    'W' = 16,'O' = 4,'R' = 16
                         )) +
      scale_size_manual(name = "shapeflag",
                        labels = c("star baker", "least favorite", "favorite"),
                        breaks=c("SB", "LF", "F"),
                        values = c('SB' = 1,'LF' = 6,'F' = 6,'N' = 6,
                                   'W' = 1,'O' = 1,'R' = 1
                        )) +  
      geom_text(aes(label = ifelse(shapeflag == 'SB', fontawesome('fa-star'), ''), 
                    family="fontawesome-webfont"),size = 7,color = 'goldenrod',vjust = .3)+
      geom_text(aes(label = ifelse(outcome == 'WINNER', fontawesome('fa-trophy'), ''), 
                    family="fontawesome-webfont"),size = 9,color = 'darkgoldenrod',vjust = .4)+
      geom_text(aes(label = ifelse(outcome == 'Runner-up', fontawesome('fa-thumbs-down'), ''), 
                    family="fontawesome-webfont"),size = 7,color = '#edd99f',vjust = .5)+
      geom_text(aes(label = ifelse(outcome == 'OUT', fontawesome('fa-thumbs-down'), ''), 
                    family="fontawesome-webfont"),size = 7,color = '#edd99f',vjust = .5)+
      geom_text(aes(label = ifelse(lastepflag == 'Y', csum, '')),size = 7,color = 'black',
                hjust = -2) +
      theme_minimal() +
      labs(x = "Episode",
           y = "GBBO Score",
           title = paste("Season", show_season, "-", id, sep =" "), 
           caption = "*See score calculation for GBBO scoring details") +
      theme(aspect.ratio=1/18,
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            #axis.title.y = element_blank(),
            axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
            strip.text.y.left = element_text(angle=0),
            text = element_text(size = 16),
            plot.title = element_text(face = "bold", color = "black", size = 16, vjust = 10, hjust = 0.5),
            plot.caption = element_text(hjust = 1, vjust = -6, size = 10),
            plot.margin = margin(t = 0,  # Top margin
                                 r = 0,  # Right margin
                                 b = 0,  # Bottom margin
                                 l = 0)) +
      scale_y_continuous(limits = c(-15,24), breaks=seq(-15,24,by=3)) +
      scale_x_continuous(limits = c(1,12), breaks=seq(0,10,by=1)) 
    
    return(individual_plot)
  }
  
  showPopup <- function(id, show_season, lat, lng){
    contestantWinnerPlot <- createContestantWinnerPlot(id, show_season)
    svg_file_name <- paste("plot", id, show_season, ".svg", sep="")
    svg_file_dir <- paste(folder,svg_file_name, sep = "/")
    
    if (!file.exists(svg_file_dir)) {
      svglite(filename= svg_file_dir, width = 4.5, height = 4.5)
      print(contestantWinnerPlot)
      dev.off()
      svg_map <- readLines(svg_file_dir)
    } 
    
    svg_map <- readLines(svg_file_dir)
    
    content <- paste("<style> .svglite {display: block; margin: 0 auto; height: 250px; width: 250px}</style>", svg_map, collapse = "")
    leafletProxy("uk_map") %>%
      addPopups(lng, lat, content, layerId = id) %>% 
      addCircles(         # invisible placeholder to trigger the mymap.spin(false)
        lng = 175.322,
        lat = -37.789,
        radius = 0,
        opacity = 0,
        layerId = 'spinnerMarker'   # identifier, can be found in js: e.layer.options.layerId
      )
  }
  
  # Loading/Spinner logic inspired from https://stackoverflow.com/questions/61532955/cant-get-leaflet-spin-plugin-working-in-r-shiny
  spinPlugin <- htmlDependency(
    "spin.js", 
    "2.3.2",
    src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/spin.js/2.3.2"),
    script = "spin.min.js") # there's no spin.css
  
  leafletspinPlugin <- htmlDependency(
    "Leaflet.Spin", 
    "1.1.2",
    src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/Leaflet.Spin/1.1.2"),
    script = "leaflet.spin.min.js")
  
  registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }
  
  spin_event <- "function(el, x) {
  console.log('spin event added'); 
  var mymap = this;
  mymap.on('layerremove', function(e) {
    console.log('layerremove fired');
    if (e.layer.options.layerId == 'spinnerMarker') {
      console.log(e.layer.options.layerId);
      mymap.spin(true);
    }
  });
  mymap.on('layeradd', function(e) {
    console.log('layeradd fired');
    if (e.layer.options.layerId == 'spinnerMarker') {
      console.log(e.layer.options.layerId);
      mymap.spin(false);
    }
  });
}"
  
  # Create leaflet 
  output$uk_map <-  renderLeaflet({
    leaflet() %>% 
      setView(lng=0.0360, lat=55.4781, zoom = 5) %>%
      registerPlugin(spinPlugin) %>% 
      registerPlugin(leafletspinPlugin) %>% 
      onRender(spin_event) %>%
      clearShapes() %>% # initialise spin
      addCircles(     # invisible placeholder
        lng = 175.322,
        lat = -37.789,
        radius = 0,
        opacity = 0,
        layerId = 'spinnerMarker'   # identifier, can be found in js: e.layer.options.layerId
      ) %>%
      addProviderTiles("Stamen.TonerLite", 
                       group = "Toner Lite",
                       options = providerTileOptions(minZoom = 5, maxZoom = 15)) %>% 
      addPolygons(data = local_uk_areas_with_total_contestant_count, dashArray = "3", 
                  weight = 2, color="#ffdbfa",
                  group = "Neighborhoods",
                  highlightOptions = highlightOptions(
                    weight = 5, color = "#db48a5", 
                    dashArray = "", fillOpacity = 0.7, bringToFront = FALSE, sendToBack = TRUE),
                  label = lapply(admin_labs, htmltools::HTML)) %>%
      addAwesomeMarkers(
        lng=gbbo_lat_long_contestants$lon, lat=gbbo_lat_long_contestants$lat,
        group = groups[2],
        label = lapply(contestant_labs, htmltools::HTML),
        icon = iconSet["Contestant"],
        layerId = paste(gbbo_lat_long_contestants$firstname, gbbo_lat_long_contestants$season, sep="  ")) %>% # Too hacky?? 
      addAwesomeMarkers(
        lng=gbbo_lat_long_winners$lon, lat=gbbo_lat_long_winners$lat,
        group = groups[1],
        label = lapply(winner_labs, htmltools::HTML),
        icon = iconSet["Winner"],
        layerId = paste(gbbo_lat_long_winners$firstname, gbbo_lat_long_winners$season, sep="  ")) %>% # Too hacky??
      addLayersControl(overlayGroups = groups,
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(groups[2]) %>% # Hide all contestants to not crowd map on load
      addResetMapButton() %>%
      setMaxBounds( lng1 = -23.980872, 
                    lat1 = 66.309316,
                    lng2 = 19.423975,
                    lat2 = 37.020240) 
  })
  
  outputOptions(output, "uk_map", suspendWhenHidden=FALSE)
  
  # On marker click logic
  observeEvent(input$uk_map_marker_click, { 
    leafletProxy("uk_map") %>%
      clearPopups() %>%
      removeShape(layerId = "spinnerMarker")  # this triggers mymap.spin(true)
    
    p <- input$uk_map_marker_click
    
    name <- str_trim(substr(p$id,1,nchar(p$id)-3), side = "both") # Get the first name
    season <- str_trim(substr(p$id, nchar(p$id)-2, nchar(p$id)), side = "both") # Get the season
    
    if (is.null(p))
      return()
    
    # print(p$id)
    # print(name)
    # print(season)
    
    isolate({
      showPopup(name, season, p$lat, p$lng)
    })
  })
  
  GBBO_stopwords <- c("cake",'cakes','trifle','trifles','pastry','pastries','pie'
                      ,'pies','bar','bars','bun','buns','tatin','tarte','tart'
                      ,'tarts','bread','breads','breadsticks','brownies','meringue'
                      ,'bagels','game','tarlets','mini','rolls','baked', 'traybake'
                      ,'cornish','celebration','wedding','decorative','st','corier')
  
  clean_corpus <- function(corpus){
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
    corpus <- tm_map(corpus, removeWords, GBBO_stopwords)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, stripWhitespace)
    return(corpus)
  }
  
  #pull all SB and favorites
  SB_Favorite<- gbbo_all %>% 
    filter(result =="SB" | status == "Favorite") 
  
  output$title <- renderText({
    c("Top",input$num,"Words in the", input$challenge, "Competition")})
  
  #pull all out and least favorites
  Out_Least_Favorite <- gbbo_all %>% 
    filter(result =="OUT" | status == "Least Favorite") 
  
  pos_showstopper_df <- SB_Favorite$showstopper
  pos_signature_df <- SB_Favorite$signature
  
  neg_showstopper_df <- Out_Least_Favorite$showstopper
  neg_signature_df <- Out_Least_Favorite$signature
  
  
  pos_df = reactive ({
    switch(input$challenge,
           "Showstopper" = pos_showstopper_df,
           "Signature" = pos_signature_df)
  })
  
  neg_df = reactive ({
    switch(input$challenge,
           "Showstopper" = neg_showstopper_df,
           "Signature" = neg_signature_df)
  })
  
  
  
  # collapse the words for favorites. based on the challenge input, pull showstopper or signature
  favorite = reactive({
    stripWhitespace(paste(c(pos_df()), collapse=" " ))
  })
  
  # collapse the words for least favorites. based on the challenge input, pull showstopper or signature
  least_favorite = reactive({
    stripWhitespace(paste(c(neg_df()), collapse=" " ))
  })
  
  # vector, turn into corpus and clean the combine
  all_clean<-reactive({clean_corpus(VCorpus(VectorSource(union(favorite(),least_favorite()))))})
  
  # Bigram/Unigram tokenizer alternative to NGramTokenizer credit: https://stackoverflow.com/questions/37817975/error-in-rweka-in-r-package
  BigramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  }
  
  UnigramTokenizer <-
    function(x) {
      unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
    }
  
  tokenizer <- function(x) {
    c(UnigramTokenizer(x), BigramTokenizer(x))
  }
  
  # pull out the unigrams and bigrams
  bigram_dtm <- reactive({DocumentTermMatrix(
    all_clean(), 
    control = list(tokenize = tokenizer,
                   options(mc.cores=1))
  )})
  
  # get the tf_idy
  all_tf <-  reactive({tidy(bigram_dtm()) %>%
      bind_tf_idf(term, document, count) %>%
      arrange(desc(tf))})
  
  # add category
  all_tf_1 <- reactive({
    df <- all_tf()
    df$Category <- case_when(all_tf()$document == 1 ~ "Star Baker or Favorites",
                             all_tf()$document == 2 ~ "Eliminated or Least Favorites")
    df
  })
  
  pivot <-reactive({all_tf_1()%>%
      select(term,Category,count) %>%
      spread(key = Category, value = count) %>%
      arrange(term)})
  
  pivot1 <- reactive({
    df <- pivot()
    df$Difference <- abs(round(df$`Eliminated or Least Favorites` -  df$`Star Baker or Favorites`,2))
    names(df)[names(df) == 'Star Baker or Favorites'] <- 'pos'
    names(df)[names(df) == 'Eliminated or Least Favorites'] <- 'neg'
    df
  })
  
  all_tf_2 <- reactive({all_tf_1() %>%
      left_join(pivot1(), by = 'term')})
  
  #take the top N words
  top_words = reactive({all_tf_2() %>% 
      filter(not(all_tf_2()$term %in% c('white','forest','black')) #& 
             # case_when(identical(input$sort_kb,"pos_sort") ~ all_tf_2()$Category == "Star Baker or Favorite",
             #           identical(input$sort_kb,"neg_sort") ~ all_tf_2()$Category == "Eliminated or Least Favorite",
             #           identical(input$sort_kb,"Difference") ~ not(is.na(all_tf_2()$Category))) 
      ) %>%
      group_by(term) %>%
      summarise(n_pos = sum(pos),n_neg=sum(neg), Difference = max(Difference)) %>%
      arrange(case_when(identical(input$sort_kb,"pos_sort") ~ desc(n_pos),
                        identical(input$sort_kb,"neg_sort")~ desc(n_neg),
                        identical(input$sort_kb,"Difference") ~ desc(Difference))) %>%
      head(input$num)})
  
  #filter the data frame for the top N words
  tf_final  = reactive({all_tf_2() %>% 
      filter(all_tf_2()$term %in% top_words()$term) %>%
      group_by(term, document, Category) %>%
      summarise(Frequency = sum(count), Difference = max(Difference))})
  
  
  final <- reactive({
    df <- tf_final()
    df$data_labels = case_when(df$document =="1" ~ df$Frequency,
                               df$document == "2" ~ df$Frequency*-1)
    
    
    df$pos_sort = case_when(df$document =="1" ~ df$Frequency,
                            df$document == "2" ~ 0)
    
    df$neg_sort = case_when(df$document == "2" ~ df$Frequency,
                            df$document =="1" ~ 0)
    df
  })
  
  
  # final output
  output$text_graph <- renderPlot({
    
    ggplot(final(), aes(x = reorder(term, get(input$sort_kb)) , y = Frequency, fill = Category, stat = 'identity'))+
      labs(x='Term', y='Count')+
      scale_y_continuous(limits=c(-35, 69),labels=abs)+
      geom_rect(final(), mapping = aes(ymin=48, ymax=69, xmin=-Inf, xmax=Inf),fill='#edd99f') +
      geom_text(final(), mapping = aes(label=Difference, x=term, y=57), size=6) +
      theme_minimal()+
      theme(axis.text = element_text(size = 20),
            legend.position = "bottom",
            text = element_text(size = 20),
            legend.title = element_blank())+
      geom_text(data=filter(final(), term=="chocolate"), 
                mapping = aes(y=57, x=term, label="Difference"),
                size=6, vjust=-2, fontface="bold") +
      scale_fill_manual(values=c('#e68a95','#86dba5'))+
      scale_x_discrete(expand=c(0.1,0))+
      geom_bar(data=filter(final(),document=='2'), aes(y=-Frequency), stat='identity')+
      geom_text(data=filter(final(),document=='1'),size = 6,aes(label = Frequency,hjust = -0.4))+
      geom_text(data=filter(final(),document=='2'),size = 6,aes(label = Frequency,y=-Frequency,hjust =1.1))+
      geom_bar(data=filter(final(),document=='1'), stat='identity')+
      coord_flip()
  })
  
  
  #################### Occupation and wordcloud ################ Soobin
  
  
  ## start here 
  
  gbbo_ingredients = gbbo_all 
  #removing words in the challenge name
  lst_showstopper <- strsplit(gbbo_ingredients$showstopper.name, split = " ")
  gbbo_ingredients$showstopper <- mapply(function(x,y){gsub(paste0(x,collapse = "|"), "",gbbo_ingredients$showstopper[y])},lst_showstopper,1:length(lst_showstopper))
  lst_signature <- strsplit(gbbo_ingredients$signatuare.name, split = " ")
  gbbo_ingredients$signature <- mapply(function(x,y){gsub(paste0(x,collapse = "|"), "",gbbo_ingredients$signature[y])},lst_signature,1:length(lst_signature))
  
  gbbo_contestants_category = read.csv("GBBO_contestants_050122.csv") #file version with the occupation category. fine to remove all other 
  
  baker_ingredients_df = gbbo_ingredients%>%
    mutate(season = as.factor(season))%>%
    group_by(season)%>%
    summarise(showstopper_ingredients = stripWhitespace(paste(c(showstopper), collapse=" " )),
              signature_ingredients = stripWhitespace(paste(c(signature), collapse=" " )) )%>%
    mutate(showstopper_ingredients= gsub("([a-z])([A-Z])","\\1 \\2",showstopper_ingredients),
           signature_ingredients= gsub("([a-z])([A-Z])","\\1 \\2",signature_ingredients))
  
  baker_showstopper_ingredients_df = baker_ingredients_df%>%
    mutate(showstopper_ingredients = tolower(showstopper_ingredients),
           showstopper_ingredients = removePunctuation(showstopper_ingredients),
           showstopper_ingredients = removeNumbers(showstopper_ingredients),
           showstopper_ingredients = stripWhitespace(showstopper_ingredients))%>%
    group_by(season)%>%
    unnest_tokens(word, showstopper_ingredients)%>%
    anti_join(get_stopwords(source = "smart"))%>%
    filter(!word %in% GBBO_stopwords)%>%
    mutate(challenge_type = 'Showstopper')
  
  baker_signature_ingredients_df = baker_ingredients_df%>%
    mutate(signature_ingredients = tolower(signature_ingredients),
           signature_ingredients = removePunctuation(signature_ingredients),
           signature_ingredients = removeNumbers(signature_ingredients),
           signature_ingredients = stripWhitespace(signature_ingredients))%>%
    group_by(season)%>%
    unnest_tokens(word, signature_ingredients)%>%
    anti_join(get_stopwords(source = "smart"))%>%
    filter(!word %in% GBBO_stopwords)%>%
    mutate(challenge_type = 'Signature')
  
  all_ingredient_df = rbind(baker_signature_ingredients_df,baker_showstopper_ingredients_df)
  
  wordcloud_df = reactive({
    
    all_ingredient_df%>%
      filter(season == input$season_ingredients)%>%
      group_by(word)%>%
      count(word, sort = TRUE)
    
  })
  
  output$wordcloud <- renderPlot({
    
    set.seed(2638)
    
    wordcloud(wordcloud_df()$word, wordcloud_df()$n, max.words = 20, colors = c('#e68a95','#86dba5'),
              fixed.asp = TRUE,random.order=FALSE, scale=c(3,0.75))
    
  })
  
  
  ## occupation
  
  contestant_final_result = gbbo_all[,c('baker','season','episode','result')]%>%
    mutate(season = as.numeric(season),
           episode = as.numeric(episode))%>%
    arrange(baker, season, episode)%>%
    group_by(baker,season)%>%
    filter(episode == max(episode))
  
  gbbo_contestants_firstname <- gbbo_contestants_category %>% 
    mutate(name_or_nickname = sub('[^\"]+\"([^\"]+).*', '\\1', baker),
           firstname = sub(" .*", "", name_or_nickname))
  
  contestant_occupation  = gbbo_contestants_firstname%>%
    merge(contestant_final_result, by.x = c('firstname','season'), by.y = c('baker','season'), all.x = TRUE)%>%
    group_by(firstname, season, occupation_category, result)
  
  occupations_list = contestant_occupation %>%
    group_by(occupation_category,result)%>%
    summarise(occupation_list = stripWhitespace(paste(c(occupation), collapse=", <br> " )))%>%
    dcast(occupation_category ~ result, value.var = "occupation_list", fill = NULL)%>%
    group_by(occupation_category)
  
  colnames(occupations_list)  <- c('occupation_category',
                                   'na_list','eliminated_list',
                                   'runner_up_list','winner_list')
  
  occupations_df = contestant_occupation %>%
    group_by(occupation_category, result)%>%
    count(occupation_category, sort = TRUE)%>%
    dcast(occupation_category ~ result, value.var = "n", fill = 0)%>%
    group_by(occupation_category)
  
  colnames(occupations_df)  <- c('occupation_category','na_cnt','eliminated_cnt','runner_up_cnt','winner_cnt')
  
  occupations_df = mutate(occupations_df, total = sum(na_cnt,eliminated_cnt,runner_up_cnt,winner_cnt))%>%
    merge(occupations_list, by = "occupation_category")
  
  occupations_df$occupation_category <- factor(occupations_df$occupation_category, levels = unique(occupations_df$occupation_category)[order(occupations_df$total, decreasing = TRUE)])
  
  output$occupation <- renderPlotly({
    
    plot_ly(as.data.frame(occupations_df), y = ~occupation_category, x = ~eliminated_cnt, type = 'bar', name = 'Eliminated',hovertemplate =  ~eliminated_list, marker = list(color = '#e68a95'))%>% 
      add_trace(x = ~runner_up_cnt, name = 'Runner Up',hovertemplate =  ~runner_up_list, marker = list(color = '#86dba5'))%>%
      add_trace(x = ~winner_cnt, name = 'Winner',hovertemplate =  ~winner_list, marker = list(color = 'goldenrod'))%>%
      config(displayModeBar = F) %>%
      layout(xaxis = list(title = '# of Contestants'), 
             yaxis = list(title = '', 
            occupation_category = "total ascending"), 
            barmode = 'stack',
            font = list(
              family = "Arial",
              size = 14,
              color = "#000"))
    
  })
  
  

  
  

}


# Run the application 
shinyApp(ui = ui, server = server)




