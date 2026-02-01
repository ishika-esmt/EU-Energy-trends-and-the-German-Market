
# GERMANY CROSS-BORDER ELECTRICITY FLOWS — TWO-DAY CONTRAST

# packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(scales)
library(lubridate)

# LOAD DATA
df <- readr::read_csv("/Users/Isha/Desktop/ESMT Berlin/Term 1/Data Visualisation/Session Material/Group project/SMARD_data_merge/merged_data/market/cross border_cleaned/merged_cross_border_flows.csv")

df <- df %>%
  mutate(
    start_date = as.Date(str_sub(start_date, 1, 10)),
    end_date   = as.Date(str_sub(end_date,   1, 10))
  )

# RESHAPE DATA
long <- df %>%
  pivot_longer(
    cols = -c(start_date, end_date, `net export [mwh] calculated resolutions`),
    names_to = "key", values_to = "mwh"
  ) %>%
  filter(!is.na(mwh)) %>%
  mutate(
    country   = str_to_lower(str_trim(str_remove(key, "\\s*\\[mwh\\].*$"))),
    country   = str_remove(country, "\\s*\\(export\\)|\\s*\\(import\\)"),
    direction = if_else(str_detect(key, "\\(export\\)"), "export", "import")
  ) %>%
  select(start_date, end_date, country, direction, mwh)

# STANDARDIZE COUNTRY NAMES
name_map <- tribble(
  ~country_raw,          ~ne_admin,
  "czech republic",      "Czechia",
  "netherlands",         "Netherlands",
  "united kingdom",      "United Kingdom",
  "sweden",              "Sweden",
  "norway",              "Norway",
  "denmark",             "Denmark",
  "poland",              "Poland",
  "france",              "France",
  "austria",             "Austria",
  "switzerland",         "Switzerland",
  "belgium",             "Belgium",
  "luxembourg",          "Luxembourg"
)

long <- long %>%
  mutate(country = str_trim(country)) %>%
  left_join(name_map, by = c("country" = "country_raw")) %>%
  mutate(ne_admin = coalesce(ne_admin, str_to_title(country)))

# PICK DATES
manual_mode <- TRUE  # <<< change to TRUE if we want to pick our own dates

if (!manual_mode) {
  # Auto mode: find 2015 max import & 2025 max export
  exp_cols <- grep("\\(export\\).*\\[mwh\\]", names(df), ignore.case = TRUE, value = TRUE)
  imp_cols <- grep("\\(import\\).*\\[mwh\\]", names(df), ignore.case = TRUE, value = TRUE)
  row_sum <- function(m) rowSums(m, na.rm = TRUE)
  
  daily_net <- df %>%
    mutate(
      year             = year(start_date),
      total_export_mwh = row_sum(across(all_of(exp_cols))),
      total_import_mwh = row_sum(across(all_of(imp_cols))),
      net_export_mwh   = total_export_mwh - total_import_mwh
    ) %>%
    select(start_date, year, total_export_mwh, total_import_mwh, net_export_mwh)
  
  ext_2015 <- daily_net %>%
    filter(year == 2015) %>%
    summarise(
      max_export_day = start_date[which.max(net_export_mwh)],
      max_import_day = start_date[which.min(net_export_mwh)]
    )
  
  ext_2025 <- daily_net %>%
    filter(year == 2025) %>%
    summarise(
      max_export_day = start_date[which.max(net_export_mwh)],
      max_import_day = start_date[which.min(net_export_mwh)]
    )
  
  date_a <- ext_2015$max_import_day
  date_b <- ext_2025$max_export_day
  
} else {
  # Manually type your own dates here
  date_a <- as.Date("2025-06-20")  # e.g. import-heavy
  date_b <- as.Date("2017-12-30")  # e.g. export-heavy
}

# GET GEOMETRIES
world <- ne_countries(scale = "medium", returnclass = "sf") %>% st_transform(4326)
deu   <- world %>% filter(iso_a3 == "DEU") %>% st_point_on_surface() %>% mutate(admin = "Germany")
partners <- world %>% filter(admin %in% unique(long$ne_admin)) %>%
  st_point_on_surface() %>% select(admin, geometry) %>% rename(partner_admin = admin)

deu_xy <- st_coordinates(deu)[1, ]
partners_xy <- partners %>%
  mutate(lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2]) %>%
  st_drop_geometry()

# BUILD EDGE LIST
sel <- long %>% filter(start_date %in% c(date_a, date_b))
edges <- sel %>%
  inner_join(partners_xy, by = c("ne_admin" = "partner_admin")) %>%
  mutate(
    from_x   = if_else(direction == "export", deu_xy[1], lon),
    from_y   = if_else(direction == "export", deu_xy[2], lat),
    to_x     = if_else(direction == "export", lon, deu_xy[1]),
    to_y     = if_else(direction == "export", lat, deu_xy[2]),
    dir_lab  = if_else(direction == "export", "Export (DE → partner)", "Import (partner → DE)"),
    mwh_abs  = abs(mwh)
  ) %>%
  drop_na(from_x, from_y, to_x, to_y)

# PLOTTING
plot_day <- function(day_date) {
  edges_day <- edges %>% filter(start_date == day_date)
  total_flow <- sum(edges_day$mwh, na.rm = TRUE)
  
  ggplot() +
    #  base map with black borders
    geom_sf(data = world, fill = "grey95", color = "black", size = 0.3) +
    
    # curved trade lines
    geom_curve(
      data     = edges_day,
      aes(x = from_x, y = from_y, xend = to_x, yend = to_y,
          linewidth = mwh_abs, color = dir_lab),
      curvature = 0.15, alpha = 0.8
    ) +
    
    # Germany centroid
    geom_point(aes(x = deu_xy[1], y = deu_xy[2]), size = 3, color = "black") +
    
    # partner country labels
    geom_text(
      data      = partners_xy,
      aes(x = lon, y = lat, label = partner_admin),
      size      = 3,
      color     = "black",
      fontface  = "bold",
      nudge_y   = 0.4
    ) +
    
    # scales & colors
    scale_linewidth_continuous(name = "Flow (MWh)", range = c(0.3, 3)) +
    scale_color_manual(
      name   = "",
      values = c("Export (DE → partner)" = "#2C7FB8",
                 "Import (partner → DE)" = "#F16913")
    ) +
    
    # zoomed region 
    coord_sf(xlim = c(-10, 25), ylim = c(40, 65)) +
    
    # labels & theme
    labs(
      title    = paste0("Germany’s Cross-Border Electricity Flows (", day_date, ")"),
      subtitle = paste0("Curves show direction; total flow: ", format(total_flow, big.mark = ","), " MWh"),
      caption  = "Data: merged cross-border flows"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.title = element_blank(),
      axis.text = element_text(size = 9)
    )
}

# CREATE & SAVE MAPS
p_a <- plot_day(date_a)
p_b <- plot_day(date_b)

print(p_a) # import heavy day
print(p_b) # export heavy day

ggsave(paste0("map_", date_a, ".png"), p_a, width = 9, height = 6, dpi = 300)
ggsave(paste0("map_", date_b, ".png"), p_b, width = 9, height = 6, dpi = 300)
