
library(dplyr)
library(ggplot2)

gbbo_all <- read.csv("GBBO_allseasons_episodes_and_status_022422.csv")

### Data frame for all seasons dotplot 

dotplot =  gbbo_all %>%
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

#DOTPLOT

ggplot(dotplot, aes(x=as.factor(season), y=endsum), group = baker) + 
  geom_dotplot(binaxis='y', stackdir='center',
               aes(fill = winflag)
               ,dotsize = .5
               ,alpha = .8
               ,stackgroups = TRUE
  ) +
  scale_fill_manual(name = "",
                    values = c('Winner' = 'goldenrod',
                               'Runner-up' = '#86dba5',
                               'Eliminated before final' = '#e68a95')
  ) +
  scale_y_continuous(limits = c(-4,13), breaks=seq(-4,13,by=2)) +
  coord_flip() +
  geom_vline(xintercept = 1.5,color = "gray30", linetype = "dashed", size = .5) +
  geom_vline(xintercept=2.5, color = "gray30", linetype = "dashed", size = .5) +
  geom_vline(xintercept = 3.5,color = "gray30", linetype = "dashed", size = .5) +
  geom_vline(xintercept = 4.5,color = "gray30", linetype = "dashed", size = .5) +
  geom_vline(xintercept=5.5,color = "gray30", linetype = "dashed", size = .5) +
  geom_vline(xintercept = 6.5,color = "gray30", linetype = "dashed", size = .5) +
  geom_vline(xintercept=7.5,color = "gray30", linetype = "dashed", size = .5) +
  geom_vline(xintercept = 8.5,color = "gray30", linetype = "dashed", size = .5) +
  geom_vline(xintercept=9.5,color = "gray30", linetype = "dashed", size = .5) +
  geom_vline(xintercept=10.5,color = "gray30", linetype = "dashed", size = .5) +
  labs(x = "Season", y = "Final Score") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(family = "Arial"),
    text = element_text(size = 14, family = 'Arial')
  ) 
