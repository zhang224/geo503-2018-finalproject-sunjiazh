library(tidyr)
library(dplyr)
library(spData)
library(sf)
library(wbstats)
library(geofacet)
library(ggplot2)
library(maps)
library(rworldmap)
library(leaflet)
library(PerformanceAnalytics)

# download data
arrivals <- wb(indicator = "ST.INT.ARVL",startdate = 2010,enddate = 2016)
receipt <- wb(indicator = "ST.INT.TVLR.CD",startdate = 2010,enddate = 2016)


# data wrangling
arrivals_t <- spread(arrivals,date,value)
receipt_t <- receipt %>% 
  slice(303:n())%>% 
  spread(date,value) %>% 
  na.omit() %>% 
  gather("date","value",6:12) 
receipt_s <- receipt_t%>% 
  group_by(iso3c,country) %>% 
  summarise(sd=sd(value)) %>% 
  filter(sd>5000000000)
s <- receipt_s$iso3c
receipt_s <- receipt_t%>% 
  filter(iso3c==s[1]|iso3c==s[2]|iso3c==s[3]|iso3c==s[4]|iso3c==s[5]|iso3c==s[6]|iso3c==s[7])
tour <- left_join(receipt_t,arrivals,by=c("iso3c"="iso3c","iso2c"="iso2c","date"="date","country"="country")) %>% 
  na.omit()
names(tour)[names(tour)=="value.x"]="expenditures"
names(tour)[names(tour)=="value.y"]="arrivals"

# Visualization of a correlation matrix between arrivals and expenditure
m_arr_rec <- select(tour,expenditures,arrivals)
chart.Correlation(m_arr_rec, histogram=TRUE, pch=19)

# Using facet_geo to visualize the trend of the "Expenditure per Capita(US$)" through 6 years
ggplot(tour,aes(date,expenditures/arrivals))+
  geom_line()+
  facet_geo(~country,grid = "world_countries_grid1",scales="free_y")+
  xlab("Year")+
  ylab("Expenditure per Capita(US$)")+
  theme(strip.text.x=element_blank())

# Interactive Visualization of the level of the "Expenditure per Capita(US$)"
mean_ex <- tour %>% 
  mutate(expenditure=expenditures/arrivals)%>% 
  group_by(country) %>% 
  summarise(mean_expenditure=mean(expenditure))

labels=c("★","★★","★★★","★★★★","★★★★★")
mean_ex1 <- cut(mean_ex$mean_expenditure,
                breaks = quantile(mean_ex$mean_expenditure,seq(0,1,0.2)),
                include.lowest = TRUE,
                labels)
mean_ex2 <- mean_ex %>% 
  mutate(level=mean_ex1)
mean_m <- joinCountryData2Map(mean_ex2,joinCode="NAME",nameJoinColumn="country")
mean_map <- leaflet(mean_m) %>% 
  addTiles() %>% 
  addPolygons(weight=1,
              color="grey",
              label = paste("cost level in ",mean_m$country,":",mean_m$level),
              highlight=highlightOptions(weight = 2,
                                         color = "steelblue4",
                                         bringToFront = TRUE))
mean_map



