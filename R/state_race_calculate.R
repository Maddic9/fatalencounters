#' Calculate State Level Race specific Police Homicide Deaths
#'
#' @description Calculates state level race specific police homicide counts 
#' and rates from the fatal the encounters data set and population data from
#' the Census Beauru.
#'
#' @usage state_race_calculate(project_pop = TRUE)
#'
#' @param project_pop use projected populations for the denominator in later
#' homicide counts where population data is not yet available
#'
#' @return data.frame like object with police homicide death counts and rates
#'
#' @examples
#' state_race_calculate()
#'
#' @import dplyr tibble
#'
#' @export

state_race_calculate <- function(project_pop = TRUE){
    pop_df <- state_race_df %>%
        group_by(GEOID, YEAR, Race) %>%
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
        group_by(state_abb, State, YEAR, Race) %>%
        summarise(deaths=n()) %>%
        filter(!is.na(YEAR)) %>%
        ungroup() %>%
        right_join(pop_df, by = c("state_abb", "YEAR", "Race", "State")) %>%
        mutate(deaths = ifelse(is.na(deaths), 0, deaths)) %>%
        mutate(death_rate = deaths / Population * 100000) %>%
        bind_rows(
            fe_df_clean %>%
                filter(YEAR <= max(pop_df$YEAR, na.rm = TRUE)) %>%
                group_by(state_abb, State, YEAR, Race) %>%
                summarise(deaths=n()) %>%
                filter(!is.na(YEAR)) %>%
                filter(Race == "Missing")) %>%
        mutate(Race = factor(Race)) %>%
        mutate(Race = fct_relevel(Race, "Missing", after = Inf))
    
}
