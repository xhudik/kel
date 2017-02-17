library(tidyverse)

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

src_tbls(KEL)

deadwood <- KEL %>% tbl("plot") %>% collect()
save(deadwood,file = "deadwood_tbl.Rds")

chrono <- KEL %>% tbl("chrono") %>% collect(n=Inf)
save(chrono,file = "chrono_tbl.Rds")

regeneration <- KEL %>% tbl("regeneration") %>% collect(n=Inf)
save(chrono,file = "regeneration_tbl.Rds")

plot <- KEL %>% tbl("plot") %>% collect(n=Inf)
save(chrono,file = "plot_tbl.Rds")

regeneration_subplot <- KEL %>% tbl("regeneration_subplot") %>% collect(n=Inf)
save(chrono,file = "regeneration_subplot_tbl.Rds")

tree <- KEL %>% tbl("tree") %>% collect(n=Inf)
save(chrono,file = "tree.Rds")

core <- KEL %>% tbl("core") %>% collect(n=Inf)
save(chrono,file = "core_tbl.Rds")
