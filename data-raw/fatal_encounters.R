rm(list=ls())
library(rmapshaper)
library(devtools)
library(readxl)
library(httr)
library(XML)
library(stringr)
library(tibble)
library(sf)
library(tigris)
library(dplyr)

state_translate <- function(x){
    snames <- c(state.name, "District of Columbia")
    names(snames) <- c(state.abb, "DC")
    snames[x]
}

state_sf <- ms_simplify(input = as(states(class="sf"), 'Spatial')) %>%
    st_as_sf()

fe_df <- read_excel(
    "~/Downloads/FATAL ENCOUNTERS DOT ORG SPREADSHEET (See Read me tab).xlsx")

state_fips_df <- "https://en.wikipedia.org/wiki/" %>%
    str_c("Federal_Information_Processing_Standard_state_code") %>%
    GET() %>%
    content("text") %>%
    {readHTMLTable(doc=.)} %>%
    .[[1]] %>%
    as_tibble() %>%
    filter(V1 != "Name") %>%
    select(state_abb = V2, GEOID = V3) %>%
    mutate(State = state_translate(state_abb)) %>%
    filter(!is.na(State) & as.numeric(GEOID) <= 56)

county_fips_df <- maps::county.fips %>%
    as_tibble() %>%
    mutate(GEOID = sprintf("%05d", fips)) %>%
    mutate(State = str_to_title(str_split_fixed(polyname, ",", 2)[,1])) %>%
    mutate(County = str_to_title(str_split_fixed(polyname, ",", 2)[,2])) %>%
    select(GEOID, State, County) %>%
    left_join(select(state_fips_df, -GEOID), by = c("State"))

use_data(fe_df, state_fips_df, county_fips_df, state_sf, overwrite = TRUE)
