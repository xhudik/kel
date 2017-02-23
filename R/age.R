# ge characteristics
# Hudik, 15.2.2017

library(tidyverse)
library(leaflet)
library(rgdal)

## 1. Settings + data loading ##

#initial settings
setwd('/home/tomas/meee/les/czu/vypocty/R')
cred <- read.csv(file="/home/tomas/meee/les/czu/postgres_awscredential", header=TRUE, sep=":", skip = 1, stringsAsFactors = FALSE)

#if we have stored data - load them
load(file="../data/tables.rda")

#otherwise take data from DB
KEL <- src_postgres(dbname = cred$db,
                    host = cred$host,
                    port = cred$port,
                    user = cred$user,
                    password = cred$password)


chrono <- KEL %>% tbl("chrono") %>% collect(n=Inf)
core <- KEL %>% tbl("core") %>% collect(n=Inf)
plot <- KEL %>% tbl("plot") %>% collect(n=Inf)
tree <- KEL %>% tbl("tree") %>% collect(n=Inf) 


save(chrono,core,plot,tree,file="../data/tables.rda")


######### Computation ###########

#80025 trees in tree: str(tree%>%distinct(treeid))
#20477 trees in chrono: str(chrono%>%distinct(treeid))

#20477 trees in our years: str(years)
years <- chrono %>% filter(!is.na(year)) %>% group_by(treeid) %>% 
  summarise(firsty = min(year), 
            lasty = max(year)) %>% ungroup()


#test: all trees (treeid) have the same missing years
# core %>% group_by(treeid) %>% summarise(firsty = min(missing_years), 
#                                         lasty = max(missing_years),
#                                         diff=firsty-lasty,
#                                         n=n()) %>%  arrange(desc(diff))

#20512 trees in core: str(core%>%distinct(treeid))
#20356 trees in our missing_years: str(missing_years)
missing_years  <- core %>% group_by(treeid) %>% filter(!is.na(missing_years)) %>% 
  summarise(missing_years = first(missing_years)) %>% ungroup()

#20311 trees in our age: str(age)
age <- inner_join(years,missing_years, by = "treeid") %>% group_by(treeid) %>% 
  summarise(age=lasty - firsty - missing_years) %>% ungroup()

#test: all trees are contained in chrono and in tree table (treeid)
#years %>% anti_join(missing_years, by = "treeid") %>%  count(treeid, sort = TRUE)


#80025 in our tree_last_obsv: str(tree_last_obsv%>%distinct(treeid))
tree_last_obsv <-  tree %>% group_by(treeid) %>% 
  summarise(#save - each treeid has 1 plotid
    plotid = first(plotid),
    #be careful treen can contain characters!!!
    #treen = max(treen, na.rm=TRUE),
    
    #latest date
    date = max(date)
  ) %>% ungroup() 

# test
# tree %>% group_by(treeid) %>% 
#        summarise(diff = length(unique(plotid)))%>% filter(diff>1) %>%arrange(desc(diff))


#20311 trees in our plot_age: inner_join(x = tree_last_obsv , y = age, by= "treeid") %>%group_by(treeid)%>%summarise(n=n())

#Q: why treen is not the same as number of rows (they differ significantly
#nrow is often very low 1-3 (that is not ok for a number of trees in a plot)
plot_age <- inner_join(x = tree_last_obsv , y = age, by= "treeid") %>% group_by(plotid) %>%
  summarise(plot_age=sum(age,na.rm=TRUE),
            #treen=max(treen,na.rm=TRUE),
            measured_trees=n(),
            #avg_page = page/max(treen)
            avg_age = plot_age/measured_trees) %>% ungroup()

#add info on country
plot_age <- plot_age %>% left_join(plot %>% select(plotid, country), by="plotid")

#oldest plots
plot_age_arr <- plot_age %>% top_n(10,avg_age)%>% arrange(desc(avg_age))


#models
summary(lm(avg_age ~ measured_trees, data = plot_age))
summary(lm(avg_age ~ country -1, data = plot_age))
plot(lm(avg_age ~ country -1, data = plot_age))


#ggplot(data=plot_age_arr, aes(x=plotid, y=avg_age,fill(measured_trees))) + geom_bar()
ggplot(data=plot_age_arr, aes(reorder(plotid, -avg_age),y=avg_age,fill=-measured_trees))+
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45))

#maps are not suitable for our plots since we have many plots per country
countries <- readOGR("../data/countries.geojson", "OGRGeoJSON")
countries1 <- readOGR("../data/countries1.geojson", "OGRGeoJSON")

map <- leaflet(countries)

pal <- colorNumeric(
  palette = "Blues",
  domain = countries$gdp_md_est
)





########tests

a <- tree %>% group_by(plotid)%>%summarise(treen = max(treen,na.rm=TRUE),
                                      nrow=n())


plot_age2 <- inner_join(x = tree %>% group_by(treeid)%>%summarise(plotid=first(plotid)), y = age, by= "treeid") %>%group_by(plotid) %>%
  summarise(plot_age=sum(age)) %>% ungroup()

plot_age %>% arrange(desc(plot_age))
plot_age2 %>% arrange(desc(plot_age))


#be careful plot has more lines than ID (some plot id are duplicated there)
plot %>% summarise(n=n())
plot %>% distinct(plotid)%>%summarise(n=n())
duplicates <- plot%>% group_by(plotid)%>%filter(n()>1)%>% arrange(plotid)

  