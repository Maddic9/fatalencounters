rm(list=ls())
library(devtools)
library(readxl)
library(httr)
library(XML)
library(stringr)
library(tibble)
library(tigris)
library(dplyr)
library(forcats)
library(maps)

state_translate <- function(x){
    snames <- c(state.name, "District of Columbia")
    names(snames) <- c(state.abb, "DC")
    snames[x]
}

fe_df <- read_excel(
    "~/Downloads/FATAL ENCOUNTERS DOT ORG SPREADSHEET (See Read me tab).xlsx")

fe_df_clean <- fe_df %>%
    mutate(Race = case_when(
        `Subject's race with imputations` == "NA" ~ "Missing",
        is.na(`Subject's race with imputations`) ~ "Missing",
        `Subject's race with imputations` == "HIspanic/Latino" ~ "Hispanic/Latino",
        `Subject's race with imputations` == "Middle Eastern" ~ "Other Race",
        `Subject's race with imputations` == "European American/White" ~ "European-American/White",
        `Subject's race with imputations` == "Race unspecified" ~ "Missing",
        TRUE ~ `Subject's race with imputations`
    )) %>%
    mutate(Race = fct_relevel(
        Race, "Missing", after = Inf
    )) %>%
    mutate(`Sex` = case_when(
        is.na(`Subject's gender`) ~ "Missing",
        `Subject's gender` == "White" ~ "Missing",
        `Subject's gender` == "Transexual" ~ "Transgender",
        TRUE ~ `Subject's gender`
    )) %>%
    mutate(`Sex` = fct_relevel(`Sex`, "Missing", after = Inf)) %>%
    mutate(Age = cut(
        as.numeric(`Subject's age`),
        c(seq(0, 30, by = 5), seq(35, 85, by = 10), Inf),
        right = FALSE)) %>%
    mutate(Age = fct_explicit_na(Age, "Missing")) %>%
    filter(
        !(
            `Dispositions/Exclusions INTERNAL USE, NOT FOR ANALYSIS` %in%
                c("Drug overdose",
                  "Murder-suicide",
                  "Murder/suicide",
                  "Murder/Suicide",
                  "Ruled an overdose",
                  "Ruled natural causes",
                  "Ruled suicide",
                  "Ruled Suicide",
                  "Substance use",
                  "Suicide"))
    ) %>%
    mutate(`Dispositions/Exclusions INTERNAL USE, NOT FOR ANALYSIS` = ifelse(
        is.na(`Dispositions/Exclusions INTERNAL USE, NOT FOR ANALYSIS`),
        "Other",
        `Dispositions/Exclusions INTERNAL USE, NOT FOR ANALYSIS`
    )) %>%
    mutate(disposition = fct_collapse(
        `Dispositions/Exclusions INTERNAL USE, NOT FOR ANALYSIS`,
        Unreported = "Unreported", Justified = "Justified",
        `Pending investigation` = "Pending investigation", Criminal = "Criminal",
        other_level = "Other"
    )) %>%
    mutate(mental_illness = ifelse(
        `Symptoms of mental illness? INTERNAL USE, NOT FOR ANALYSIS` == "Unknown",
        NA,
        `Symptoms of mental illness? INTERNAL USE, NOT FOR ANALYSIS`)) %>%
    mutate(mental_illness = case_when(
        mental_illness == "Yes" ~ "Mental Illness",
        mental_illness == "No" ~ "Nothing Reported",
        TRUE ~ mental_illness
    )) %>%
    mutate(mental_illness = fct_explicit_na(mental_illness, "Missing")) %>%
    mutate(cod = case_when(
        is.na(`Cause of death`) ~ "Missing",
        `Cause of death` == "Undetermined" ~ "Missing",
        `Cause of death` == "Unknown" ~ "Missing",
        `Cause of death` == "Other" ~ "Missing",
        TRUE ~ `Cause of death`, 
    )) %>%
    mutate(cod = fct_relevel(cod, "Missing", after = Inf)) %>%
    mutate(mental_illness = fct_relevel(mental_illness, "Missing", after = Inf)) %>%
    mutate(state_abb = `Location of death (state)`) %>%
    mutate(State = state_translate(state_abb)) %>%
    select(
        `Unique ID`, Age, `Subject's name`, Race, Sex, YEAR = `Date (Year)`,
        state_abb, State, County = `Location of death (county)`,
        disposition, mental_illness, cod)

state_fips_df <- "https://en.wikipedia.org/wiki/" %>%
    str_c("Federal_Information_Processing_Standard_state_code") %>%
    GET() %>%
    content("text") %>%
    {readHTMLTable(doc=.)} %>%
    .[[1]] %>%
    as_tibble() %>%
    filter(V1 != "Name") %>%
    select(state_abb = V2, GEOID = V3) %>%
    mutate(state_abb = as.character(state_abb)) %>%
    mutate(State = state_translate(state_abb)) %>%
    filter(!is.na(State) & as.numeric(GEOID) <= 56)

county_fips_df <- county.fips %>%
    as_tibble() %>%
    mutate(GEOID = sprintf("%05d", fips)) %>%
    mutate(State = str_to_title(str_split_fixed(polyname, ",", 2)[,1])) %>%
    mutate(County = str_to_title(str_split_fixed(polyname, ",", 2)[,2])) %>%
    select(GEOID, State, County) %>%
    left_join(select(state_fips_df, -GEOID), by = c("State"))

use_data(fe_df, fe_df_clean, state_fips_df, county_fips_df, overwrite = TRUE)
