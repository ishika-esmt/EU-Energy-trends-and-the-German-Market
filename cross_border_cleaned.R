
# Cross-border Physical Flows (Daily) Merge
library(tidyverse)
library(lubridate)
library(readr)
library(stringr)

# 0) Configure paths
base_dir <- "~/Desktop/ESMT Berlin/Term 1/Data Visualisation/Session Material/Group project/SMARD"
# Adjust ONLY this if your folder name differs
sub_path <- "market/cross border"

in_dir  <- file.path(base_dir, sub_path)
out_dir <- file.path(base_dir, paste0(sub_path, "_cleaned"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Sanity checks 
cat("Looking in:\n", normalizePath(in_dir, mustWork = FALSE), "\n\n")

files <- list.files(in_dir, pattern = "(?i)\\.csv$", full.names = TRUE)  # case-insensitive .csv
cat("Found", length(files), "CSV file(s).\n\n")
if (length(files) == 0) {
  cat("Tip: check the exact folder name. Common variants:\n",
      "  'market/cross-border physical flows'\n",
      "  'market/cross border physical flows'\n",
      "  'market/Cross-border physical flows'\n",
      "Use list.dirs(in_dir, recursive = FALSE) from the parent to verify.\n")
  stop("No CSV files found. Fix sub_path and rerun.")
}

# helpers
detect_delim <- function(path) {
  l1 <- read_lines(path, n_max = 1)
  if (str_count(l1, ";") >= str_count(l1, ",")) ";" else ","
}

detect_header_skip <- function(path) {
  first <- read_lines(path, n_max = 30)
  hit <- which(str_detect(first, regex("^\\s*Start\\s*date\\b", ignore_case = TRUE)))[1]
  if (is.na(hit)) 0 else hit - 1
}

parse_dates_safe <- function(x) {
  suppressWarnings(parse_date_time(
    x,
    orders = c("b d, Y", "d b Y", "Y-m-d", "d.m.Y", "m/d/Y"),
    tz = "UTC"
  ))
}

# read & clean one file
read_one <- function(path) {
  cat("📂 Reading:", basename(path), "\n")
  delim  <- detect_delim(path)
  skip_n <- detect_header_skip(path)
  
  df <- read_delim(
    path,
    delim = delim,
    skip = skip_n,
    locale = locale(grouping_mark = ","),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  
  # normalize headers
  names(df) <- tolower(trimws(names(df)))
  # guard for header variants
  if ("start date" %in% names(df)) df <- df |> rename(start_date = `start date`)
  if ("end date"   %in% names(df)) df <- df |> rename(end_date   = `end date`)
  
  if (!all(c("start_date","end_date") %in% names(df))) {
    stop("File missing Start/End date columns: ", basename(path))
  }
  
  # replace dashes with NA but KEEP them
  df[df == "-" | df == "–"] <- NA
  
  # parse dates (keep invalids count for info, then drop those rows only)
  df <- df |>
    mutate(
      start_date = parse_dates_safe(start_date),
      end_date   = parse_dates_safe(end_date)
    )
  
  bad <- sum(is.na(df$start_date) | is.na(df$end_date))
  if (bad > 0) cat("  Skipping", bad, "row(s) with invalid dates in", basename(path), "\n")
  df <- df |> filter(!is.na(start_date) & !is.na(end_date))
  
  # convert numeric-like character columns (leave date cols alone)
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  chr_cols <- setdiff(chr_cols, c("start_date","end_date"))
  if (length(chr_cols) > 0) {
    df <- df |>
      mutate(across(all_of(chr_cols),
                    ~ parse_number(.x, locale = locale(grouping_mark=","))))
  }
  
  df
}

# merge
merged_df <- purrr::map_dfr(files, read_one)

# dedupe overlapping date ranges
merged_df <- merged_df |>
  mutate(.key = paste(start_date, end_date)) |>
  distinct(.key, .keep_all = TRUE) |>
  select(-.key) |>
  arrange(start_date)

# save 
out_file <- file.path(out_dir, "merged_cross_border_flows.csv")
write_csv(merged_df, out_file)
cat("\n Saved:\n", normalizePath(out_file), "\n")
cat("Rows/Cols:", paste(dim(merged_df), collapse=" x "), "\n")
