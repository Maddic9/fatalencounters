rm(list=ls())
library(tidyverse)
library(tidycensus)
library(ipumsr)
library(httr)
library(devtools)

# These age groups are  the most granular ones that cover all years of data
age_vec <- c(seq(0, 30, by = 5), seq(35, 85, by = 10), Inf)
race_vec <- c(
    h = "Hispanic or Latino",
    nhba = "Black or African American",
    nhaa = "Asian",
    nhia = "American Indian and Alaska Native", # indegineous?
    nhna = "Native Hawaiian and Other Pacific Islander", # indegionus?
    nhtom = "Two or More Races", # other
    nhwa = "White",
    tot = ""
    )

# Source reference
# https://data.nber.org/census/popest/www.census.gov/popest/data/intercensal/county/files/CO-EST00INT-ALLDATA.pdf
#DF <- "https://data.nber.org/census/popest/coest00intalldata.csv" %>%
DF <- "~/Downloads/coest00intalldata.csv" %>%
    read_csv() %>%
    # lets keep the data sources consistenet
    filter(yearref != 1 & yearref != 12) %>%
    mutate(agegrp = ifelse(agegrp == 0, 1, agegrp)) %>%
    select(
        county, year, agegrp, tot_male, tot_female,
        nhwa_male:nhtom_female, h_male:h_female) %>%
    # age group 99 is teh aggregate age group here for the data
    filter(agegrp != 0 & agegrp != 99) %>%
    mutate(Age = cut((agegrp - 1) * 5, age_vec, right = FALSE)) %>%
    pivot_longer(tot_male:h_female) %>%
    mutate(Sex = str_to_title(str_split_fixed(name, "_", 2)[,2])) %>%
    mutate(Race = race_vec[str_split_fixed(name, "_", 2)[,1]]) %>%
    select(-name, -agegrp)

# Source reference
# https://data.nber.org/census/popest/www.census.gov/popest/data/counties/asrh/2015/files/CC-EST2015-ALLDATA.pdf
#DF2 <- "https://data.nber.org/census/popest/countypopmonthasrh.csv" %>%
DF2 <- "~/Downloads/countypopmonthasrh.csv" %>%
    read_csv() %>%
    filter(year > 2010 & year < 2016) %>%
    rename_all(tolower) %>%
    select(
        county, year, agegrp, tot_male, tot_female,
        nhwa_male:nhtom_female, h_male:h_female) %>%
    # age group 99 is teh aggregate age group here for the data
    filter(agegrp != 0 & agegrp != 99) %>%
    mutate(Age = cut((agegrp - 1) * 5, age_vec, right = FALSE)) %>%
    pivot_longer(tot_male:h_female) %>%
    mutate(Sex = str_to_title(str_split_fixed(name, "_", 2)[,2])) %>%
    mutate(Race = race_vec[str_split_fixed(name, "_", 2)[,1]]) %>%
    select(-name, -agegrp)

ipums_key <- Sys.getenv("IPUMS_API_KEY")

## wait a bit cause this could take a while for IPUMS to do its thing
data_extract_status_res <- GET(
    "https://api.ipums.org/extracts?product=nhgis&version=v1",
    add_headers(Authorization = ipums_key))
de_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = T)

if(nrow(filter(
    de_df, description  == "Fatal Encounters Denom" &
    !is.null(de_df$download_links$table_data))) == 0){

    himeta_df <- "https://api.ipums.org/metadata/nhgis/datasets?version=v1" %>%
        GET(add_headers(Authorization = ipums_key)) %>%
        content("parsed", simplifyDataFrame = TRUE) %>%
        as_tibble()

    acsdetail_df <- bind_rows(lapply(2016:2018, function(y){
        "https://api.ipums.org/metadata/nhgis/datasets/" %>%
            str_c(y, "_ACS1?version=v1") %>%
            GET(add_headers(Authorization = ipums_key)) %>%
            content("parsed", simplifyDataFrame = TRUE) %>%
            .[["data_tables"]] %>%
            as_tibble() %>%
            mutate(year = y)
    }))

    mybody_json <- list(
        datasets = sapply(str_c(2016:2018, "_ACS1"), function(x){
            list(
                geog_levels = list("county", "state"),
                data_tables = as.list(
                    str_c("B01001", c("", "B", "C", "D", "E", "G", "H", "I"))
                )
            )
        }, simplify = FALSE),
        data_format = "csv_no_header",
        description = "Fatal Encounters Denom",
        breakdown_and_data_type_layout = "single_file"
    )

    ## send in the data request

    result <- POST(
        "https://api.ipums.org/extracts/?product=nhgis&version=v1",
        add_headers(Authorization = ipums_key),
        body = mybody_json, encode = "json", verbose())
    res_df <- content(result, "parsed", simplifyDataFrame = T)
    my_number <- res_df$number

    ## wait a bit cause this could take a while for IPUMS to do its thing
    data_extract_status_res <- GET(
        "https://api.ipums.org/extracts?product=nhgis&version=v1",
        add_headers(Authorization = ipums_key))
    de_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = T)

}

my_number <- filter(de_df,
       description  == "Fatal Encounters Denom" &
            !is.null(de_df$download_links$table_data))$number[1]

tf <- tempfile(fileext = ".zip")

download.file(
    de_df[de_df$number == my_number,c("download_links")]$table_data,
    tf
)

county_acs_df <- bind_rows(lapply(2016:2018, function(y){
    sty <- str_c(y, "_county")

    ipums_year_df <- tf %>%
        read_nhgis(data_layer = contains(sty)) %>%
        set_ipums_var_attributes(
            read_ipums_codebook(
                tf,
                contains(sty)))

    year_var_df <- ipums_year_df %>%
        select(-(GISJOIN:NAME_E), -NAME_M) %>%
        {tibble(
            name = names(sapply(., attr, which = "label")),
            desc = sapply(., attr, which = "label"),
            var = sapply(., attr, which = "var_desc"))} %>%
        mutate(var = str_sub(var, 1, -8)) %>%
        filter(desc != "Total" & desc != "Female" & desc != "Male") %>%
        filter(str_sub(name, -4, -4) == "E") %>%
        mutate(Age = str_split(desc, ": ", simplify = TRUE)[,2]) %>%
        mutate(Sex = str_split(desc, ": ", simplify = TRUE)[,1]) %>%
        mutate(`Min Age` = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
        mutate(`Max Age` = as.numeric(str_split_fixed(Age, " ", 4)[,3])) %>%
        mutate(`Min Age` = ifelse(
            is.na(`Min Age`) & is.na(`Max Age`), 0, `Min Age`)) %>%
        mutate(Age = cut(`Min Age`, age_vec, right = F)) %>%
        mutate(var = str_remove(var, "Sex by Age")) %>%
        mutate(var = str_remove(var, " \\(")) %>%
        mutate(var = str_remove(var, "\\)")) %>%
        mutate(var = str_remove(var, " Alone")) %>%
        mutate(Race = str_remove(var, ", Not Hispanic or Latino")) %>%
        select(name, Age, Sex, Race)

    ipums_year_df %>%
        mutate(GEOID = str_c(STATEA, COUNTYA)) %>%
        select(YEAR, GEOID, STATE, COUNTY, !!year_var_df$name) %>%
        pivot_longer(!!year_var_df$name) %>%
        left_join(year_var_df, by = "name") %>%
        select(-name) %>%
        group_by(YEAR, GEOID, Age, Sex, Race) %>%
        summarize(value = sum(value))}))

state_acs_df <- bind_rows(lapply(2016:2018, function(y){
    sty <- str_c(y, "_state")

    ipums_year_df <- tf %>%
        read_nhgis(data_layer = contains(sty)) %>%
        set_ipums_var_attributes(
            read_ipums_codebook(
                tf,
                contains(sty)))

    year_var_df <- ipums_year_df %>%
        select(-(GISJOIN:NAME_E), -NAME_M) %>%
        {tibble(
            name = names(sapply(., attr, which = "label")),
            desc = sapply(., attr, which = "label"),
            var = sapply(., attr, which = "var_desc"))} %>%
        mutate(var = str_sub(var, 1, -8)) %>%
        filter(desc != "Total" & desc != "Female" & desc != "Male") %>%
        filter(str_sub(name, -4, -4) == "E") %>%
        mutate(Age = str_split(desc, ": ", simplify = TRUE)[,2]) %>%
        mutate(Sex = str_split(desc, ": ", simplify = TRUE)[,1]) %>%
        mutate(`Min Age` = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
        mutate(`Max Age` = as.numeric(str_split_fixed(Age, " ", 4)[,3])) %>%
        mutate(`Min Age` = ifelse(
            is.na(`Min Age`) & is.na(`Max Age`), 0, `Min Age`)) %>%
        mutate(Age = cut(`Min Age`, age_vec, right = F)) %>%
        mutate(var = str_remove(var, "Sex by Age")) %>%
        mutate(var = str_remove(var, " \\(")) %>%
        mutate(var = str_remove(var, "\\)")) %>%
        mutate(var = str_remove(var, " Alone")) %>%
        mutate(Race = str_remove(var, ", Not Hispanic or Latino")) %>%
        select(name, Age, Sex, Race)

    ipums_year_df %>%
        mutate(GEOID = STATEA) %>%
        select(YEAR, GEOID, STATE, !!year_var_df$name) %>%
        pivot_longer(!!year_var_df$name) %>%
        left_join(year_var_df, by = "name") %>%
        select(-name) %>%
        group_by(YEAR, GEOID, Age, Sex, Race) %>%
        summarize(value = sum(value))}))

full_df <- bind_rows(DF, DF2) %>%
    rename(GEOID = county, YEAR = year) %>%
    group_by(GEOID, YEAR, Age, Race, Sex) %>%
    summarise_all(sum) %>%
    ungroup() %>%
    bind_rows(ungroup(county_acs_df)) %>%
    arrange(GEOID, YEAR, Age, Race, Sex)

full_df_state <- bind_rows(DF, DF2) %>%
    rename(GEOID = county, YEAR = year) %>%
    mutate(GEOID = str_sub(GEOID, 1, 2)) %>%
    group_by(GEOID, YEAR, Age, Race, Sex) %>%
    summarise_all(sum) %>%
    ungroup() %>%
    bind_rows(ungroup(state_acs_df)) %>%
    arrange(GEOID, YEAR, Age, Race, Sex)

race_df <- full_df %>%
    filter(Race != "")

total_df <- full_df %>%
    filter(Race == "") %>%
    select(-Race)

state_race_df <- full_df_state %>%
    filter(Race != "")

state_total_df <- full_df_state %>%
    filter(Race == "") %>%
    select(-Race)

use_data(race_df, total_df, state_race_df, state_total_df, overwrite = TRUE)
