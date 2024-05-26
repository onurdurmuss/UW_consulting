# Loading necessary libraries
library(DBI)

# Database connection function
connect_to_db <- function() {
  dbConnect(odbc::odbc(), "RetailDB")
}