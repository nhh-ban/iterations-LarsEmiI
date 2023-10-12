# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

test_stations_metadata_colnames <- # this function checks if the column names are correct
  function(df) { # Accepting a single argument, the df in question
    
    expected_colnames <- c("id", "name", "latestData", "lat", "lon") # Creating a vector of the expected column names
    
    if (all(colnames(df) == expected_colnames) == TRUE) { 
      print("PASS: Data has the correct columns") # If all column names in the dataframe matches the list of expected column names, print PASS
    } else{
      print("FAIL: Columns do not match the correct specification") # If one or more column names does not match, then FAIL
    }
  }

test_stations_metadata_nrows <- # this function checks if the dataframe has the correct amount of rows
  function(df) { # Accepting a single argument, the df in question
    
    min_expected_rows <- 5000 # Creating a minimum and maximum expected rows in the dataframe
    max_expected_rows <- 10000
    
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) { 
      print("PASS: Data has a reasonable number of rows") # if the number of rows in the df is more than the minimum expected rows and less than max, print PASS
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows") # if number of rows in df is less than or equal to minimum expected rows then print FAIL
    } else {
      print("FAIL: Data has suspiciously many rows") # else if number of rows is more than the max expected rows print FAIL
    }
  }

test_stations_metadata_coltypes <- # This function checks whether the df has the correct column types
  function(df) { # Accepting a single argument, the df in question
    expected_coltypes <-
      c("character", "character", "double", "double", "double") # Creating a vector of expected column types
    
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) { # Checks if all columns in the df has the correct column type with respect to the expected coltype vector
      print("PASS: All cols have the correct specifications") 
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }
  
test_stations_metadata_nmissing <- # This function tests if the amount of missing values in the df is reasonable
  function(df) { # Accepting a single argument, the df in question
    max_miss_vals <- 200 # This is the max amount to be accepted
    
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) { # if the sum of NAs is less than the max_miss_vals = 200, then PASS
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set") # If NAs succeed the amount of accepted missing values then FAIL
    }
  }

test_stations_metadata_latestdata_timezone <- # This function tests if the timezone in the df is correctly set to UTC
  function(df) { # Accepting a single argument, the df in question
    
    if (attr(df$latestData,"tzone")=="UTC") { # retrieves the tzone attribute and checks if this is equal to UTC
      print("PASS: latestData has UTC-time zone") # print this if tzone == UTC
    } else {
      print("FAIL: latestData does not have expected UTC-time zone") # print this if tzone != UTC
    }
  }


test_stations_metadata <- # This function wraps all the previous functions into one, and summarizes all the tests onto one df without having to call each function individually
  function(df){ # Accepting a single argument, the df in question
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }





