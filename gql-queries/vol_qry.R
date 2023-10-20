library(glue)
vol_qry <- function(id, from, to) {
  query <- glue('{
    trafficData(trafficRegistrationPointId: "<<id>>") {
      volume {
        byHour(from: "<<from>>", to: "<<to>>") {
          edges {
            node {
              from
              to
              total {
                volumeNumbers {
                  volume
                }
              }
            }
          }
        }
      }
    }
  }', 
                .open = "<<", 
                .close = ">>"
  )
  
  return(query)
}



test_query <- vol_qry("97411V72313", "2022-05-01T06:55:47Z", "2022-05-08T06:55:47Z")
print(test_query)


GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)



