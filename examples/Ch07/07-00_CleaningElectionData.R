# Make data-frames for the 07-03 models

library("dplyr")
library("tidyr")
library("readr")
library("stringr")

# Load and prepare a single .asc file
read_asc_year <- function(path) {
  # Get year from filename
  year <- path %>% basename %>% str_extract("\\d{4}") %>% as.numeric

  df <- path %>%
    # It looks like -9 was used to code missing values.
    read_table(col_names = FALSE, na = "-9") %>%
    # Make sure all values are numbers so division works
    mutate_each(funs(as.numeric)) %>%
    rename(Incumbent = X3, DemVote = X4, RepVote = X5) %>%
    mutate(year = year,
           DemShare = DemVote / (DemVote + RepVote),
           Contested = 0.1 < DemShare & DemShare < 0.9,
           # Impute uncontested values: Create an adjusted share to show what
           # might have happened if the race had been contested.
           DemShare_Adj = ifelse(DemShare < 0.1, .25, DemShare),
           DemShare_Adj = ifelse(0.9 < DemShare_Adj, .75, DemShare_Adj)) %>%
    select(year, everything())
  df
}


# Find all .asc files
paths <- list.files(path = "examples/Ch07/cong3/", full.names = TRUE)

# Combine all .asc files into a single data-frame, then keep just the elections
# the chapter uses
congress <- paths %>%
  lapply(read_asc_year) %>%
  bind_rows %>%
  filter(year %in% c(1986, 1988, 1990))

year_by_year <- congress %>%
  filter(year %in% c(1986, 1988, 1990)) %>%
  select(-RepVote, -DemVote) %>%
  gather(Key, Value, -year, -X1, -X2) %>%
  unite(Key_Year, Key, year) %>%
  spread(Key_Year, Value)

write_csv(congress, "examples/Ch07/congress_long.csv")
write_csv(year_by_year, "examples/Ch07/congress_wide.csv")
