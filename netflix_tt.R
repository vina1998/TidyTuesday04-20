library(tidyverse)
library(png)
library(grid)

#barplot_type
netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

netflix <- netflix_titles %>% group_by(type) %>% count(type)

myplot <- ggplot(netflix, aes(type,n)) +geom_col(fill="violetred4") + theme(
  panel.background= element_rect(fill = "pink", color = NA), plot.background= element_rect(fill = "pink", color = NA))

ggsave("myplot.png")

img <- readPNG("myplot.png")

#barplot_ country 
netflix_country <- netflix_titles %>% group_by(country) %>% count(country) %>% filter(n>100)

myplot2 <- ggplot(netflix_country, aes(country,n)) +geom_col(fill="royalblue4") + theme(
  panel.background= element_rect(fill = "lightskyblue2", color = NA), plot.background= element_rect(fill = "lightskyblue2", color = NA))

ggsave("myplot2.png")

img2 <- readPNG("myplot2.png")

#barplot_ director
netflix_director <- netflix_titles %>% group_by(director) %>% count(director) %>% filter(n>10) %>% na.omit()

myplot3 <- ggplot(netflix_director, aes(director,n)) +geom_col(fill="khaki4") + theme(
  panel.background= element_rect(fill = "khaki2", color = NA), plot.background= element_rect(fill = "khaki2", color = NA))

ggsave("myplot3.png")

img3 <- readPNG("myplot3.png")

#barplot_ rating
netflix_rating <- netflix_titles %>% group_by(rating) %>% count(director)  %>% na.omit()

myplot4 <- ggplot(netflix_rating, aes(rating,n)) +geom_col(fill="darkorange4") + theme(
  panel.background= element_rect(fill = "darkorange", color = NA), plot.background= element_rect(fill = "darkorange", color = NA))

ggsave("myplot4.png")

img4 <- readPNG("myplot4.png")

#barplot_ release_year
netflix_year <- netflix_titles %>% group_by(release_year) %>% count(release_year)  %>% filter(release_year>100) %>% na.omit()

myplot5 <- ggplot(netflix_year, aes(release_year,n)) +geom_col(fill="mediumpurple4") + theme(
  panel.background= element_rect(fill = "plum1", color = NA), plot.background= element_rect(fill = "plum1", color = NA))

ggsave("myplot5.png")

img5 <- readPNG("myplot5.png")
#barplot_ duration
netflix_duration<- netflix_titles %>% group_by(duration) %>% count(duration) %>% filter(n>100) %>% na.omit()

myplot6<- ggplot(netflix_duration, aes(duration,n)) +geom_col(fill="palegreen4") + theme(
  panel.background= element_rect(fill = "palegreen1", color = NA), plot.background= element_rect(fill = "palegreen1", color = NA))

ggsave("myplot6.png")

img6 <- readPNG("myplot6.png")

#final output
ggplot()+ annotate("polygon", x = c(-20, -8, -8, -20), y = c(2018, 2018, 2020.3, 2020.3), fill = "black", color = "black") + annotation_custom(rasterGrob(img,  width = unit(0.70,"npc"), height = unit(0.30,"npc")),  -20,-16, 2019,2020.3) + annotation_custom(rasterGrob(img2,  width = unit(0.70,"npc"), height = unit(0.30,"npc")),  -16,-12,2019,2020.3) + 
  annotation_custom(rasterGrob(img3,  width = unit(0.70,"npc"), height = unit(0.30,"npc")),  -12,-8,2019,2020.3) + annotation_custom(rasterGrob(img4,  width = unit(0.70,"npc"), height = unit(0.30,"npc")),  -20,-16, 2018,2019.3) +  annotation_custom(rasterGrob(img5,  width = unit(0.70,"npc"), height = unit(0.30,"npc")),  -16,-12, 2018,2019.3) +  annotation_custom(rasterGrob(img6,  width = unit(0.70,"npc"), height = unit(0.30,"npc")),  -12,-8, 2018,2019.3)+  annotate("text", x = -15.5, y = 2020.3, hjust = 1, vjust = 1, label =  "Netflix trends", size = 7, lineheight = 0.95, colour="red") + annotate("text", x = -17, y = 2020, hjust = 1, vjust = 1, label = "Popular type: movies", size =3, lineheight = 0.95, colour="white") +  annotate("text", x = -13, y = 2020, hjust = 1, vjust = 1, label = "Popular country: US", size =3, lineheight = 0.95, colour="white")+ annotate("text", x = -8, y = 2020, hjust = 1, vjust = 1, label = "Popular director: Raúl Campos
", size =3, lineheight = 0.95, colour="white")  + annotate("text", x = -17, y = 2019, hjust = 1, vjust = 1, label = "Popular rating: TV-MA", size =3, lineheight = 0.95, colour="white") + annotate("text", x = -12.5, y = 2019, hjust = 1, vjust = 1, label = "Popular release year: 2018", size =3, lineheight = 0.95, colour="white") + annotate("text", x = -8.5, y = 2019, hjust = 1, vjust = 1, label = "Popular duration: 1 season", size =3, lineheight = 0.95, colour="white")
