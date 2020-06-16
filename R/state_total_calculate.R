#' Calculate State Level Police Homicide Deaths
#'
#' @description Calculates state level police homicide counts and rates
#' (for the total population) from the fatal the encounters data set and
#' population data from the Census Beauru.
#'
#' @usage state_total_calculate(project_pop = TRUE)
#'
#' @param project_pop use projected populations for the denominator in later
#' homicide counts where population data is not yet available
#'
#' @return data.frame like object with police homicide death counts and rates
#'
#' @examples
#' state_total_calculate()
#'
#' @import dplyr tibble
#'
#' @export

state_total_calculate <- function(project_pop = TRUE){
    pop_df <- state_age_df %>%
        group_by(GEOID, YEAR) %>%
        summarize(Population = sum(value)) %>%
        ungroup() %>%
        label_geoid()

    current <- (as.integer(format(Sys.Date(), "%Y"))-1)
    maxyear <- max(pop_df$YEAR, na.rm=TRUE)

    if(project_pop & !(current %in% pop_df$YEAR)){
        pop_df <- project_data(pop_df,(maxyear + 1):current)
    }

    fe_df_clean %>%
        filter(YEAR <= max(pop_df$YEAR, na.rm = TRUE)) %>%
        group_by(state_abb, State, YEAR) %>%
        summarise(deaths=n()) %>%
        ungroup() %>%
        right_join(pop_df, by = c("state_abb", "YEAR", "State")) %>%
        mutate(deaths = ifelse(is.na(deaths), 0, deaths)) %>%
        mutate(death_rate = deaths / Population * 100000)
}
