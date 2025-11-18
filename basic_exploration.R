library(tidyverse)
library(lubridate)

message("Reading data...")
tows <- read_csv("data/Snow_Emergency_Diamond_Lake_Tows_2019.csv") %>%
  mutate(
    Call_Taken = mdy_hms(Call_Taken),
    date = as_date(Call_Taken),
    dow  = wday(Call_Taken, label = TRUE, week_start = 1),
    hour = hour(Call_Taken)
  )

message("\n--- Basic structure ---")
glimpse(tows)

message("\n--- Row count ---")
cat("Number of rows:", nrow(tows), "\n")

message("\n--- Date range ---")
cat(
  "From", as.character(min(tows$date, na.rm = TRUE)),
  "to", as.character(max(tows$date, na.rm = TRUE)), "\n"
)

message("\n--- Tows by day of week ---")
print(tows %>% count(dow, sort = TRUE))

message("\n--- Top 10 neighborhoods ---")
print(tows %>% count(Neighborho, sort = TRUE) %>% slice_head(n = 10))

message("\n--- Tows by ward ---")
print(tows %>% count(Ward, sort = TRUE))

message("\n--- Tows by tow zone ---")
print(tows %>% count(Tow_Zone, sort = TRUE))

message("\n--- Creating plots in Analysis/plots ---")
dir.create("Analysis/plots", showWarnings = FALSE)

# Tows by day of week
p_dow <- tows %>%
  count(dow) %>%
  ggplot(aes(dow, n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Tows by day of week",
    x = "Day of week",
    y = "Number of tows"
  )
ggsave("Analysis/plots/tows_by_dow.png", p_dow, width = 6, height = 4, dpi = 300)

# Tows by hour of day
p_hour <- tows %>%
  count(hour) %>%
  ggplot(aes(hour, n)) +
  geom_col(fill = "tomato") +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Tows by hour of day",
    x = "Hour (0–23)",
    y = "Number of tows"
  )
ggsave("Analysis/plots/tows_by_hour.png", p_hour, width = 6, height = 4, dpi = 300)

# Top 10 neighborhoods
p_neighborhood <- tows %>%
  count(Neighborho, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(reorder(Neighborho, n), n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 10 neighborhoods by tows",
    x = "Neighborhood",
    y = "Number of tows"
  )
ggsave("Analysis/plots/tows_by_neighborhood_top10.png", p_neighborhood, width = 7, height = 4, dpi = 300)

# Tows by ward
p_ward <- tows %>%
  count(Ward, sort = TRUE) %>%
  ggplot(aes(reorder(as.factor(Ward), n), n)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(
    title = "Tows by ward",
    x = "Ward",
    y = "Number of tows"
  )
ggsave("Analysis/plots/tows_by_ward.png", p_ward, width = 6, height = 4, dpi = 300)

# Tow locations: simple point map
p_points <- ggplot(tows, aes(x = Longitude, y = Latitude)) +
  geom_point(alpha = 0.4, size = 0.8, color = "steelblue") +
  coord_equal() +
  labs(
    title = "Tow locations (Diamond Lake snow emergency)",
    x = "Longitude",
    y = "Latitude"
  )
ggsave("Analysis/plots/tow_locations_points.png", p_points, width = 6, height = 6, dpi = 300)

# Tow locations colored by ward
p_points_ward <- ggplot(tows, aes(x = Longitude, y = Latitude, color = as.factor(Ward))) +
  geom_point(alpha = 0.5, size = 0.8) +
  coord_equal() +
  labs(
    title = "Tow locations by ward",
    x = "Longitude",
    y = "Latitude",
    color = "Ward"
  )
ggsave("Analysis/plots/tow_locations_by_ward.png", p_points_ward, width = 6, height = 6, dpi = 300)

# Tow locations by ward on a basemap (if ggmap is available)
if (requireNamespace("ggmap", quietly = TRUE)) {
  message("\n--- Creating basemap plot with ggmap (Stadia Maps) ---")

  bbox <- tows %>%
    summarize(
      lon_min = min(Longitude, na.rm = TRUE),
      lon_max = max(Longitude, na.rm = TRUE),
      lat_min = min(Latitude, na.rm = TRUE),
      lat_max = max(Latitude, na.rm = TRUE)
    )

  margin <- 0.01
  basemap_bbox <- c(
    left = bbox$lon_min - margin,
    bottom = bbox$lat_min - margin,
    right = bbox$lon_max + margin,
    top = bbox$lat_max + margin
  )

  basemap <- tryCatch(
    ggmap::get_stadiamap(
      bbox = basemap_bbox,
      zoom = 12,
      maptype = "stamen_toner_lite"
    ),
    error = function(e) {
      message(
        "\n--- Could not download basemap: ", conditionMessage(e),
        "\nSkipping basemap plot; points-only maps are still available."
      )
      NULL
    }
  )

  if (!is.null(basemap)) {
    p_ward_basemap <- ggmap::ggmap(basemap) +
      geom_point(
        data = tows,
        aes(x = Longitude, y = Latitude, color = as.factor(Ward)),
        alpha = 0.6,
        size = 0.7
      ) +
      labs(
        title = "Tow locations by ward (with basemap)",
        x = "Longitude",
        y = "Latitude",
        color = "Ward"
      )

    ggsave(
      "Analysis/plots/tow_locations_by_ward_basemap.png",
      p_ward_basemap,
      width = 6,
      height = 6,
      dpi = 300
    )
  }
} else {
  message(
    "\n--- Package 'ggmap' not installed; skipping basemap plot.\n",
    "Install with install.packages('ggmap') to enable the basemap."
  )
}
