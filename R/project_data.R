#' Project a long yearly data set forward holding last value observed constant
#'
#' @description Project a long yearly data set forward holding last value
#' observed constant
#'
#' @usage project_data(data, future_years)
#'
#' @param data a data.frame with a YEAR column which is long
#' @param future_years int vector years to project for
#'
#' @return data.frame like object with projected values
#'
#' @examples
#' project_data(state_total_df, 2019)
#'
#' @import dplyr tibble
#'
#' @export

project_data <- function(data, future_years){
    lapply(future_years, function(f){
        data %>%
            filter(YEAR == max(YEAR)) %>%
            mutate(YEAR = f)}) %>%
        bind_rows() %>%
        {bind_rows(data, .)}
}
