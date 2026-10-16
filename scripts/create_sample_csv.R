# scripts/create_sample_csv.R - Generates a realistic sample historical CSV for testing upload
dir.create("sample_data", recursive = TRUE, showWarnings = FALSE)

dates <- seq.Date(from = as.Date("2018-01-01"), to = as.Date("2024-12-31"), by = "day")
n <- length(dates)
d_years <- as.integer(format(dates, "%Y"))
d_months <- as.integer(format(dates, "%m"))
d_yday <- as.integer(format(dates, "%j"))

set.seed(101)

# Mumbai
t_mumbai <- 27.2 + 4.5 * sin(2 * pi * (d_yday - 105) / 365) + rnorm(n, 0, 1.2)
p_mumbai <- rep(0, n)
monsoon_mumbai <- d_months %in% c(6, 7, 8, 9)
p_mumbai[monsoon_mumbai] <- rgamma(sum(monsoon_mumbai), shape = 1.2, scale = 22.0) * (runif(sum(monsoon_mumbai)) > 0.3)
h_mumbai <- 68 + 20 * sin(2 * pi * (d_yday - 120) / 365) + rnorm(n, 0, 4)
h_mumbai <- pmax(30, pmin(98, h_mumbai))

df_mumbai <- data.frame(
  date = as.character(dates),
  station_code = "MUMBAI",
  city = "Mumbai",
  lat = 19.0760,
  lon = 72.8777,
  mean_temp_c = round(t_mumbai, 1),
  total_rain_mm = round(p_mumbai, 1),
  humidity_percent = round(h_mumbai, 1),
  stringsAsFactors = FALSE
)

# Delhi
t_delhi <- 25.0 + 11.5 * sin(2 * pi * (d_yday - 110) / 365) + rnorm(n, 0, 1.8)
p_delhi <- rep(0, n)
monsoon_delhi <- d_months %in% c(7, 8, 9)
p_delhi[monsoon_delhi] <- rgamma(sum(monsoon_delhi), shape = 0.9, scale = 14.0) * (runif(sum(monsoon_delhi)) > 0.45)
h_delhi <- 54 + 25 * sin(2 * pi * (d_yday - 130) / 365) + rnorm(n, 0, 6)
h_delhi <- pmax(15, pmin(95, h_delhi))

df_delhi <- data.frame(
  date = as.character(dates),
  station_code = "DELHI",
  city = "Delhi",
  lat = 28.6139,
  lon = 77.2090,
  mean_temp_c = round(t_delhi, 1),
  total_rain_mm = round(p_delhi, 1),
  humidity_percent = round(h_delhi, 1),
  stringsAsFactors = FALSE
)

df_all <- rbind(df_mumbai, df_delhi)
df_all <- df_all[order(df_all$station_code, df_all$date), ]

write.csv(df_all, "sample_data/sample_weather_data.csv", row.names = FALSE)
cat(sprintf("Created sample_data/sample_weather_data.csv with %d rows across Mumbai and Delhi (2018-2024).\n", nrow(df_all)))
