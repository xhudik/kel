# Main structural characteristics
# Hudik, 4.2.2017


library(ineq)
library(e1071)
library(tidyverse)
library(gridExtra)

#read external function
#set setwd() properly -otherwise you want be able to source 
source("read_cred.R")
#read access to DB
cred <- read_cred()
KEL <- src_postgres(dbname = cred$db,
                    host = cred$host,
                    port = cred$port,
                    user = cred$user,
                    password = cred$password)

#list of tables
src_tbls(KEL)


#plots from Slovakia, forest type: mixed
plots <- KEL %>% tbl("plot") %>% dplyr::filter(country == "Slovakia" & foresttype == "mixed") %>% dplyr::collect()

#trees from Slovakian spots that are alive
trees <-  KEL %>% tbl("tree") %>% dplyr::filter(plotid %in% plots$plotid & onplot !=0 & status >=1 & status <= 4) %>% 
  dplyr::collect()

#basic statistics per plotid

stat <- trees %>% mutate(
  #pi*r^2 in m^2
  basal_area = pi*(dbh_mm/(2*1000))^2) %>%
  left_join(.,plots, by = "plotid") %>% group_by(plotid) %>% 
  summarise(mean_dbh = mean(dbh_mm,na.rm = TRUE), 
            sd_dbh = sd(dbh_mm,na.rm = TRUE),
            skewness_dbh = skewness(dbh_mm, na.rm = TRUE), 
            gini_dbh = ineq(dbh_mm, type = "Gini", na.rm = TRUE), 
            basal_area_plot = sum(basal_area,na.rm = TRUE),
            #basal area per hectare
            #aggregation function first() used - we are getting a vector of potsize (still 1 value) and
            #only 1 value is needed
            basal_area_hec = basal_area_plot*10000/first(plotsize),
            nTrees = n_distinct(treeid),
            density = nTrees*10000/first(plotsize) 
            )


bins <-  30
g1 <- ggplot(stat) + geom_histogram(aes(mean_dbh),fill="cornsilk3", binwidth = bins,na.rm = TRUE) 
g2 <- ggplot(stat) + geom_histogram(aes(sd_dbh),fill="cornsilk3", binwidth = bins,na.rm = TRUE) 
g3 <- ggplot(stat) + geom_histogram(aes(skewness_dbh),fill="cornsilk3", binwidth = bins/100,na.rm = TRUE) 
g4 <- ggplot(stat) + geom_histogram(aes(gini_dbh),fill="cornsilk3", binwidth = bins/300,na.rm = TRUE) 
g5 <- ggplot(stat) + geom_histogram(aes(basal_area_plot),fill="cornsilk3", binwidth = bins/100,na.rm = TRUE) 
g6 <- ggplot(stat) + geom_histogram(aes(nTrees),fill="cornsilk3", binwidth = bins,na.rm = TRUE) 
g7 <- ggplot(stat) + geom_histogram(aes(density),fill="cornsilk3", binwidth = bins,na.rm = TRUE)
g8 <- ggplot(stat) + geom_histogram(aes(basal_area_hec),fill="cornsilk3", binwidth = bins/10,na.rm = TRUE) 

grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,ncol = 2,top = "Distributions", newpage = TRUE)

