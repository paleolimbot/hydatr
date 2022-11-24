
# this creates an abbreviated version of the HYDAT db for testing purposes

library(hydatr)
library(dplyr)

hydat_load()

if(!is_hydat(hydat_get_db())) stop("Hydat database wasn't loaded!")

db <- hydat_get_db()
db_heads <- sapply(DBI::dbListTables(db), function(x) tbl(db, x) %>% head(1000) %>% collect())

outfile <- "data-raw/Hydat_sqlite3_99999999.db"
if(file.exists(outfile)) unlink(outfile)
src_test <- DBI::dbConnect(RSQLite::SQLite(), outfile)
src_test_out <- lapply(DBI::dbListTables(db), function(x) {
  copy_to(src_test, db_heads[[x]], name = x, temporary = FALSE)
})

destfile <- "inst/Hydat_sqlite3_99999999.db"
if(file.exists(destfile)) unlink(destfile)

file.rename(outfile, destfile)
