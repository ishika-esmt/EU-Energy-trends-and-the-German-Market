
# SMARD Data Merge — Actual Electricity Generation (Daily)

library(tidyverse)
library(lubridate)
library(stringr)
library(readr)

# base directory 
base_dir <- "~/Desktop/ESMT Berlin/Term 1/Data Visualisation/Session Material/Group project/SMARD"
sub_path <- "electricity generation/actual"   # this dataset

in_dir  <- file.path(base_dir, sub_path)
out_dir <- file.path(base_dir, gsub(" ", "_", paste0(sub_path, "_cleaned")))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# find all CSV files
files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No CSV files found in: ", in_dir)

# helpers
# Detect comma or semicolon delimiter
detect_delim <- function(path) {
  l1 <- read_lines(path, n_max = 1)
  commas <- str_count(l1, ",")
  semis  <- str_count(l1, ";")
  if (semis > commas) ";" else ","
}

# renaming map for Actual Generation columns
generation_cols_map <- list(
  biomass_mwh               = regex("^\\s*biomass.*\\[mwh\\]", ignore_case = TRUE),
  hydropower_mwh             = regex("^\\s*hydropower.*\\[mwh\\]", ignore_case = TRUE),
  wind_offshore_mwh          = regex("^\\s*wind.*offshore.*\\[mwh\\]", ignore_case = TRUE),
  wind_onshore_mwh           = regex("^\\s*wind.*onshore.*\\[mwh\\]", ignore_case = TRUE),
  photovoltaics_mwh          = regex("^\\s*photovoltaics.*\\[mwh\\]", ignore_case = TRUE),
  other_renewable_mwh        = regex("^\\s*other.*renewable.*\\[mwh\\]", ignore_case = TRUE),
  nuclear_mwh                = regex("^\\s*nuclear.*\\[mwh\\]", ignore_case = TRUE),
  lignite_mwh                = regex("^\\s*lignite.*\\[mwh\\]", ignore_case = TRUE),
  hard_coal_mwh              = regex("^\\s*hard.*coal.*\\[mwh\\]", ignore_case = TRUE),
  fossil_gas_mwh             = regex("^\\s*fossil.*gas.*\\[mwh\\]", ignore_case = TRUE),
  hydro_pumped_storage_mwh   = regex("^\\s*hydro.*pumped.*storage.*\\[mwh\\]", ignore_case = TRUE),
  other_conventional_mwh     = regex("^\\s*other.*conventional.*\\[mwh\\]", ignore_case = TRUE)
)

# Simple renaming function
fuzzy_rename <- function(df, patterns_map) {
  cn <- names(df)
  for (new_nm in names(patterns_map)) {
    pat <- patterns_map[[new_nm]]
    hit <- which(str_detect(cn, pat))
    if (length(hit) == 1) names(df)[hit] <- new_nm
  }
  df
}

# Read and clean a single file
read_one <- function(path) {
  # find where the header starts
  first_lines <- read_lines(path, n_max = 15)
  hdr_idx <- which(str_detect(first_lines, regex("start\\s*date", ignore_case = TRUE)))[1]
  skip_n <- ifelse(is.na(hdr_idx), 0, hdr_idx - 1)
  delim <- detect_delim(path)
  
  df <- read_delim(
    file   = path,
    delim  = delim,
    skip   = skip_n,
    locale = locale(grouping_mark = ","),
    show_col_types = FALSE,
    trim_ws = TRUE
  )
  
  # clean column names
  names(df) <- tolower(trimws(names(df)))
  
  # rename and parse dates
  if ("start date" %in% names(df)) df <- df %>% rename(start_date = `start date`)
  if ("end date"   %in% names(df)) df <- df %>% rename(end_date   = `end date`)
  
  if ("start_date" %in% names(df)) {
    df <- df %>%
      mutate(
        start_date = mdy(start_date),
        end_date   = mdy(end_date),
        date       = start_date
      ) %>%
      filter(!is.na(start_date))
  }
  
  # convert numeric-like strings
  chrs <- names(df)[vapply(df, is.character, logical(1))]
  keep_chr <- setdiff(chrs, c("start_date", "end_date", "date"))
  if (length(keep_chr) > 0) {
    df <- df %>%
      mutate(across(all_of(keep_chr),
                    ~ parse_number(.x, locale = locale(grouping_mark = ","))))
  }
  
  df
}

# Merge all files
merged_df <- map_dfr(files, read_one)

# remove overlapping duplicates
if (all(c("start_date", "end_date") %in% names(merged_df))) {
  merged_df <- merged_df %>%
    mutate(.key = paste(start_date, end_date)) %>%
    distinct(.key, .keep_all = TRUE) %>%
    select(-.key) %>%
    arrange(start_date)
} else {
  warning("Columns 'start_date' or 'end_date' missing in merged data.")
}

# Apply fuzzy renaming
merged_df <- fuzzy_rename(merged_df, generation_cols_map)

# save cleaned file
out_file <- file.path(out_dir, paste0("merged_", gsub("/", "_", sub_path), ".csv"))
write_csv(merged_df, out_file)
cat("✅ Merged & cleaned Actual Generation saved at:\n", out_file, "\n")

# check
print(dim(merged_df))
print(head(merged_df, 3))
