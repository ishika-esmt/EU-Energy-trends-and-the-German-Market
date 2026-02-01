
## ACTUAL ELECTRICITY CONSUMPTION DATA MERGE


library(tidyverse)
library(lubridate)
library(stringr)
library(readr)

# base directory
base_dir <- "~/Desktop/ESMT Berlin/Term 1/Data Visualisation/Session Material/Group project/SMARD"
sub_path <- "electricity consumption/actual"   # <— change when needed

# in/out dirs
in_dir  <- file.path(base_dir, sub_path)
out_dir <- file.path(base_dir, gsub(" ", "_", paste0(sub_path, "_cleaned")))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No CSV files found in: ", in_dir)

# helpers
detect_delim <- function(path) {
  l1 <- read_lines(path, n_max = 1)
  # count commas/semicolons
  commas <- str_count(l1, ",")
  semis  <- str_count(l1, ";")
  if (semis > commas) ";" else ","
}

consume_cols_map <- list(
  grid_load_mwh               = regex("^\\s*grid\\s*load.*\\[mwh\\].*calculated", ignore_case = TRUE),
  grid_load_incl_pumped_mwh   = regex("^\\s*grid\\s*load.*incl.*pumped.*\\[mwh\\].*calculated", ignore_case = TRUE),
  hydro_pumped_storage_mwh    = regex("^\\s*hydro\\s*pumped\\s*storage.*\\[mwh\\].*calculated", ignore_case = TRUE),
  residual_load_mwh           = regex("^\\s*residual\\s*load.*\\[mwh\\].*calculated", ignore_case = TRUE)
)

fuzzy_rename <- function(df, patterns_map) {
  cn <- names(df)
  for (new_nm in names(patterns_map)) {
    pat <- patterns_map[[new_nm]]
    hit <- which(str_detect(cn, pat))
    if (length(hit) == 1) {
      names(df)[hit] <- new_nm
    }
  }
  df
}

# read + clean one file
read_one <- function(path) {
  # find header row (first line containing "Start date" case-insensitively)
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
  
  # normalize names
  names(df) <- tolower(trimws(names(df)))
  
  # standardize date columns if present
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
  
  # parse numeric-looking character columns (keep date cols as-is)
  chrs <- names(df)[vapply(df, is.character, logical(1))]
  keep_chr <- setdiff(chrs, c("start_date", "end_date", "date"))
  if (length(keep_chr) > 0) {
    df <- df %>%
      mutate(across(all_of(keep_chr),
                    ~ parse_number(.x, locale = locale(grouping_mark = ","))))
  }
  
  df
}

# merge all 
merged_df <- map_dfr(files, read_one)

if (all(c("start_date","end_date") %in% names(merged_df))) {
  merged_df <- merged_df %>%
    mutate(.key = paste(start_date, end_date)) %>%
    distinct(.key, .keep_all = TRUE) %>%
    select(-.key) %>%
    arrange(start_date)
} else {
  warning("Columns 'start_date' or 'end_date' not found after read. Names:\n",
          paste(names(merged_df), collapse = ", "))
}

# fuzzy renaming
if (grepl("electricity consumption/actual", sub_path, fixed = TRUE)) {
  merged_df <- fuzzy_rename(merged_df, consume_cols_map)
}

# save
out_file <- file.path(out_dir, paste0("merged_", gsub("/", "_", sub_path), ".csv"))
write_csv(merged_df, out_file)
cat("✅ Saved:", out_file, "\n")

# quick peek
print(dim(merged_df))
print(head(merged_df, 3))
