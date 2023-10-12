transform_metadata_to_df <- function(stations_metadata) { # transforming data to dataframe
  stations_metadata[[1]] %>% # Extracting the first element in the list
    map(as_tibble) %>% # converting each element into a tibble
    list_rbind() %>% # binding the list of tibbles to single tibble
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>%
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% # coverting to datetime and setting UTC timezone
    unnest_wider(location) %>% # separating the the contents of location column into latlon
    unnest_wider(latLon) # Further unnesting the latlon column into latitude lat and longitude lon
}


to_iso8601 <- function(datetime, offset_days) {
  datetime %>% # Add the offset measured in days using days() to the date
    add(days(offset_days)) %>%     # Convert date to UTC timezone
    with_tz(tzone = "UTC") %>%
    format_ISO8601() %>%   # Convert to ISO8601 format with 'Z' at the end
    paste0(., "Z")
}


print(to_iso8601(as_datetime("2016-09-01 10:11:12"), -4))
print(to_iso8601(as_datetime("2016-09-01 10:11:12"), 0))


# Did not manage this one -----------------------
transform_volumes <- function(data) {
  
  # Convert the nested json_data to a tibble
  df <- tibble(data = list(stations_metadata_df))
  
  # Unnesting the structure step-by-step
  df <- df %>% 
    unnest(cols = data) %>% 
  
  return(df)
}



