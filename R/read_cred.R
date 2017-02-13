#Support function for reading file with credentials (the file has to be outside of git!!!)
#for adding a new user/file just add a line with path,e.g.
# path <- "/home/mee/secretPlace/postgresZCU.csv"

read_cred <- function(){
  #tomas
  path <- "/home/tomas/meee/les/czu/postgres_awscredential"
  cred <- read.csv(file=path, header=TRUE, sep=":", skip = 1, stringsAsFactors = FALSE)
  returnList <- list("dbname" = cred$db,
                     "host" = cred$host,
                     "port" = cred$port,
                     "user" = cred$user,
                     "password" = cred$password
                     )
  return(returnList)
}