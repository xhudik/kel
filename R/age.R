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
tree$treen <-  as.numeric(tree$treen)


save(chrono,core,plot,tree,file="../data/tables.rda")


######### Computation ###########



years <- chrono %>% filter(!is.na(year)) %>% group_by(treeid) %>% summarise(firsty = min(year), 
                                                   lasty = max(year)) %>% ungroup()


#test: all trees (treeid) have the same missing years
# core %>% group_by(treeid) %>% summarise(firsty = min(missing_years), 
#                                         lasty = max(missing_years),
#                                         diff=firsty-lasty,
#                                         n=n()) %>%  arrange(desc(diff))

missing_years  <- core %>% group_by(treeid) %>% filter(!is.na(missing_years)) %>% 
  summarise(missing_years = first(missing_years)) %>% ungroup()


age <- inner_join(years,missing_years, by = "treeid") %>% group_by(treeid) %>% 
  summarise(age=lasty - firsty - missing_years) %>% ungroup()

#test: all trees are contained in chrono and in tree table (treeid)
#years %>% anti_join(missing_years, by = "treeid") %>%  count(treeid, sort = TRUE)


tree_last_obsv <-  tree %>% group_by(treeid) %>% 
  summarise(#save - each treeid has 1 plotid
    plotid = first(plotid),
    #save - each treeid has one treen
    treen = max(treen),
    #latest date
    date = max(date)
  ) %>% ungroup() %>% mutate()

# test
# tree %>% group_by(treeid) %>% 
#        summarise(diff = length(unique(plotid)))%>% filter(diff>1) %>%arrange(desc(diff))

plot_age <- inner_join(x = tree_last_obsv , y = age, by= "treeid") %>% group_by(plotid) %>%
  summarise(page=sum(age),
            avg_page = page/max(treen)) %>% ungroup()


########tests


plot_age2 <- inner_join(x = tree %>% group_by(treeid)%>%summarise(plotid=first(plotid)), y = age, by= "treeid") %>%group_by(plotid) %>%
  summarise(plot_age=sum(age)) %>% ungroup()

plot_age %>% arrange(desc(plot_age))
plot_age2 %>% arrange(desc(plot_age))


#be careful plot has more lines than ID (some plot id are duplicated there)
plot %>% summarise(n=n())
plot %>% distinct(plotid)%>%summarise(n=n())
duplicates <- plot%>% group_by(plotid)%>%filter(n()>1)%>% arrange(plotid)

  