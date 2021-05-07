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
Dictionary <- read_excel("CollegeScorecardDataDictionary.xlsx", sheet=4) 
sd1 <- filter(Sd, HIGHDEG >= 4)
sd2 <-  summarise(sd1, STABBR, C150_4_POOLED)
sd5 <- filter(sd2, C150_4_POOLED >= 0)
sd2 %>% drop_na()

st.codes<-sd.frame(
  state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                    "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                    "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                    "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                    "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
  full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                   "connecticut","district of columbia","delaware","florida","georgia",
                   "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                   "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                   "missouri","mississippi","montana","north carolina","north dakota",
                   "nebraska","new hampshire","new jersey","new mexico","nevada",
                   "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                   "rhode island","south carolina","south dakota","tennessee","texas",
                   "utah","virginia","vermont","washington","wisconsin",
                   "west virginia","wyoming"))
)


state1 <-data.frame(state=x)
refac.x<-st.codes$full[match(st.x$state,st.codes$state)]


data <- sd %>% select(C150_4, STABBR) %>% 
  mutate(C150_4 = as.numeric(C150_4))  %>%  group_by(STABBR)  %>% 
  summarize(Percent_Completion = mean(C150_4, na.rm = TRUE)*100)



sd2 %>%
  group_by(USA) %>%
  summarise_at(vars(-C150_4_POOLED), funs(mean(., na.rm=TRUE)))

ggplot(data = us_states, aes(x = long, y = lat, group = group, fill = Percent_Completion)) p + geom_polygon(color = "Grey", size = 0.1) + ggtitle(" Average Completion Rate for Students at Four-Year Institutions")


