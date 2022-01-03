library(tidyverse)
library(ggplot2)

covid <- readr::read_csv("/Users/dylanwu/Downloads/covid_19_data.csv")
#Exercise 1
countries = c("Taiwan","Italy","Japan", "South Korea", "Iran","Mainland China")
covidfinal <- tibble()
for (i in c(1:6)) {
  covidtemp <- covid[as.data.frame(covid[,"Country/Region"])==countries[i],]
  covidtemp <- covidtemp %>% 
    group_by(ObservationDate) %>% 
    summarise(
      Confirmed = sum(Confirmed),
      `Country/Region` = countries[i]
    )
  covidfinal<-bind_rows(covidfinal,covidtemp)
}
covidfinal$ObservationDate <- as.Date(covidfinal$ObservationDate, format = "%m/%d/%y")
ggplot(covidfinal, aes(ObservationDate, Confirmed, group = `Country/Region`,color = `Country/Region`))+
  geom_line()+
  geom_point()+
  labs(title="Number of Covid-19 Confirmed Cases", x = "Date", y = "Confirmed Case Number")

#Exercise 2
covidsummary <- covid[as.data.frame(covid[,"ObservationDate"])=="06/14/2020",]
covidsummarized <- tibble()
covidsummarized <- covidsummary %>%
  group_by(`Country/Region`) %>%
  summarise(
    Confirmed = sum(Confirmed),
    Deaths = sum(Deaths)
  )
covidplotted<- head(covidsummarized[order(-covidsummarized$Confirmed),], 10)
ggplot(covidplotted, aes(reorder(`Country/Region`,-Confirmed), Confirmed, fill = `Country/Region`))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = -90))+
  labs(x = "Country", y = "Number of Confirmed Cases")

#Exercise 3
covidsummarized <- mutate(covidsummarized, deathrate = Deaths/Confirmed)
covidsummarized<- head(covidsummarized[order(-covidsummarized$deathrate),], 10)
ggplot(covidsummarized, aes(reorder(`Country/Region`,-deathrate), deathrate, fill = `Country/Region`))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = -90))+
  labs(x = "Country", y = "Death Rates")
