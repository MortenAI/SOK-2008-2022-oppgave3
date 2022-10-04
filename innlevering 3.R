library(jsonlite)
library(httr)
library(jsonstat)
library(rjstat)

url <- "https://data.ssb.no/api/v0/no/table/11155/"

data <- '{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "0",
          "1",
          "2"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "15-74",
          "20-64",
          "20-66",
          "15-24",
          "25-39",
          "40-54",
          "55-74"
        ]
      }
    },
    {
      "code": "UtdNivaa",
      "selection": {
        "filter": "item",
        "values": [
          "TOT",
          "1-2",
          "3-5",
          "6-8"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'


d <- POST(url , body = data, encode = "json", verbose())


gruppenivå <- fromJSONstat(content(d, "text"))
gruppenivå <- as_tibble(gruppenivå)
