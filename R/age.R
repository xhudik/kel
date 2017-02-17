# ge characteristics
# Hudik, 15.2.2017

library(tidyverse)

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



years <- chrono %>% group_by(treeid) %>% summarise(firsty = min(year), 
                                                   lasty = max(year)) %>% ungroup()

#test: all trees (treeid) have the same missing years
core %>% group_by(treeid) %>% summarise(firsty = min(missing_years), 
                                        lasty = max(missing_years),n=n(), diff=firsty-lasty) %>% 
  arrange(desc(diff))

missing_years <- core %>% group_by(treeid) %>% summarise(missing_years = first(missing_years)) %>% ungroup()

age <- inner_join(years,missing_years) %>% group_by(treeid) %>% 
  summarise(age=lasty - firsty - missing_years) %>% ungroup()

plot_age <- inner_join(x = tree, y = age, by= "treeid") %>%group_by(plotid) %>%
  summarise(plot_age=sum(age)) %>% ungroup()

plot_age %>% arrange(desc(plot_age))


#be careful plot has more lines than ID (some plot id are duplicated there)
plot %>% summarise(n=n())
plot %>% distinct(plotid)%>%summarise(n=n())
duplicates <- plot%>% group_by(plotid)%>%filter(n()>1)%>% arrange(plotid)

  