#' Label a US location data set with a GEOID column
#'
#' @description Label a US location data set with a GEOID column with appropriate
#' state/county names
#'
#' @usage label_geoid(data)
#'
#' @param data a data.frame with a GEOID column that is of class character
#'
#' @return data.frame like object with values labeled
#'
#' @examples
#' label_geoid(state_total_df)
#'
#' @import dplyr tibble
#'
#' @export

label_geoid <- function(data){
    if(all(stringr::str_length(data$GEOID) == 2)){
        merge_df <- state_fips_df
    }
    else{
        merge_df <- bind_rows(state_fips_df, county_fips_df)
    }

    left_join(data, merge_df, by = "GEOID")
}
