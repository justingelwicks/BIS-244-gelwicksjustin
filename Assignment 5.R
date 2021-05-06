rm(list=ls(all=TRUE))
cat("\014")


library(ggplot2)
library(maps)
library(mapdata)
library(here)
library(dbplyr)

usa <- map_data('usa')
state <- map_data("state")
Sd <- read_csv(here("most-recent-cohorts-all-data-elements-1.csv"))
sd1 <- filter(Sd, HIGHDEG >= 4)
sd2 <-  summarise(sd1, STABBR, C150_4_POOLED)
sd5 <- filter(sd2, C150_4_POOLED >= 0)
sd2 %>% drop_na()

#option 1 
Filter(Negate(is.null), sd2[2])

#or we can do this 
sd2<-sd2[!sapply(sd2, is.null)] 
sd2


sd2 <- mutate(sd2, C150_4_POOLED = case_when(C150_4_POOLED == NULL ~ 0)


sdd <- aggregate(sd2$C150_4_POOLED, by_list(sd2$STABBR), mean)

sd2 %>% group_by(sd2$STABBR) %>% summarise(Mean_sales = mean(sd2$C150_4_POOLED))

tbl2 <- sd2 %>% filter(!is.numeric(C150_4_POOLED))



Rate1.mean <- with(sd2, ave(C150_4_POOLED, STABBR, FUN = function(x) mean(x, na.rm = TRUE)))

sd2 %>%
  group_by(USA) %>%
  summarise_at(vars(-C150_4_POOLED), funs(mean(., na.rm=TRUE)))

ggplot(data=usa, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill='lightblue') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map') + 
  coord_fixed(1.3)

