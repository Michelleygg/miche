library(jsonlite)
library(httr)

flask_url <- "http://localhost:5000/"
response <- GET(flask_url, path="query", query=list(origin = "ATL", dest="LAX", year=2007))
df <- fromJSON(content(response, "text"), simplifyDataFrame = TRUE)
View(df)
