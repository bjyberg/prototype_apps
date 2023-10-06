library(terra)
library(sf)
library(exactextractr)

level <- "adm1" # Name of admin level for file
countries <- read_sf("www/GADM_adm1.gpkg")[c(1:4)]
country_isos <- unique(countries$GID_0)
# level <- 'adm0' # Name of admin level for file
# countries <- st_make_valid(st_as_sf(geodata::world(path = tempdir())))
# sf_use_s2(FALSE) # fixes a known issue with S2 geoms sometimes

file_dict <- read.csv("www/file_dict.csv")
AC_df <- subset(file_dict, group == "ad_cap" | group == "statistics")
# hazard_df <- file_dict[file_dict$group == "hazard", ]

mean_rasts <- rast(AC_df[AC_df$fn == "mean", "path"])
names(mean_rasts) <- AC_df[AC_df$fn == "mean", "name"]

country_means <- exact_extract(mean_rasts, countries, c("mean")) |>
  cbind(countries) |>
  st_as_sf()

if(any('sum' %in% AC_df$fn)) {
  sum_rasts <- rast(AC_df[AC_df$fn %in% c("sum", "sum_ha"), "path"])
  names(sum_rasts) <- AC_df[AC_df$fn %in% c("sum", "sum_ha"), "name"]
  sum_df <- exact_extract(sum_rasts, countries, "sum", force_df = T,
    full_colnames = T)
  names(sum_df) <- AC_df[AC_df$fn %in% c("sum", "sum_ha"), "name"]
  full_country_data <- sum_df |>
    cbind(country_means) |>
    st_as_sf()
  # full_country_data <- exact_extract(sum_rasts, countries, "sum", force_df = T,
  # full_colnames = T) |>
  #   cbind(country_means) |>
  #   st_as_sf()
} else {
  full_country_data <- country_means
}
# sum_rasts <- rast(AC_df[AC_df$fn %in% c("sum", "sum_ha"), "path"])
# names(sum_rasts) <- AC_df[AC_df$fn %in% c("sum", "sum_ha"), "name"]
# country_extracted <- exact_extract(sum_rasts, countries, "sum", force_df = T,
# full_colnames = T) |>
#   cbind(country_means) |>
#   st_as_sf()

# country_extracted$area_ha <- as.numeric(st_area(country_extracted) / 10000) # convert to ha
# to_ha <- AC_df[AC_df$fn == "sum_ha", "name"] == gsub(".*\\.", "", names(country_extracted))
# sum_ha_calc <- (st_drop_geometry(country_extracted[to_ha])
# / country_extracted[["area_ha"]]) |>
#   data.frame()
# var_name_clean <- gsub(".*\\.", "",
#   names(st_drop_geometry(country_extracted[to_ha])))
# # names(sum_ha_calc) <- paste0("sum_ha.", var_name_clean)
# country_extracted <- cbind(sum_ha_calc, country_extracted) |>
#   st_as_sf()
# afri_mean <- global(mean_rasts, "mean", na.rm = T)
# area <- st_as_sfc(st_bbox(country_extracted))
# region_total <- c(afri_mean[[1]], NA, NA, NA, NA, NA, NA, NA,  "region_avg")
# region_df <- rbind(data.frame(), region_total)
# names(region_df) <- names(st_drop_geometry(country_extracted))
# full_region <- merge(area, region_df) |>
#   st_as_sf()
# full_country_data <- rbind(country_extracted, full_region) |>
#   st_as_sf()

#afri_mean$names <- paste0("region_mean.", row.names(afri_mean))
# for (i in 1:nrow(afri_mean)) {
#   name <- paste0('region_mean.', row.names(afri_mean)[i])
#   country_extracted[name] <- afri_mean[i, ]
# }
write_sf(full_country_data,
  paste0("www/aggregated_data_", level, ".gpkg"),
  append = F)

# Now for admin 0

level <- 'adm0' # Name of admin level for file
countries <- st_make_valid(st_as_sf(geodata::world(path = tempdir())))
countries <- countries[countries$GID_0 %in% country_isos, ]
# sf_use_s2(FALSE) # fixes a known issue with S2 geoms sometimes

mean_rasts <- rast(AC_df[AC_df$fn == "mean", "path"])
names(mean_rasts) <- AC_df[AC_df$fn == "mean", "name"]
a0_means <- exact_extract(mean_rasts, countries, c("mean")) |>
  cbind(countries) |>
  st_as_sf()
afri_mean <- global(mean_rasts, "mean", na.rm = T)

if (any("sum" %in% AC_df$fn)) {
  sum_rasts <- rast(AC_df[AC_df$fn %in% c("sum", "sum_ha"), "path"])
  names(sum_rasts) <- AC_df[AC_df$fn %in% c("sum", "sum_ha"), "name"]
  sum_df <- exact_extract(sum_rasts, countries, "sum", force_df = T,
    full_colnames = T)
  names(sum_df) <- AC_df[AC_df$fn %in% c("sum", "sum_ha"), "name"]
  full_country_data <- sum_df |>
    cbind(a0_means) |>
    st_as_sf()
  
  # full_country_data <- exact_extract(sum_rasts, countries, "sum", force_df = T,
  #   full_colnames = TRUE) |>
  #   cbind(a0_means) |>
  #     st_as_sf()
  afri_sum <- global(sum_rasts, "sum", na.rm = T)
} else {
  full_country_data <- country_means
}

area <- st_as_sfc(st_bbox(countries))
if (any("sum" %in% AC_df$fn)) {
  region_total <- c(afri_sum[[1]], afri_mean[[1]], "region_avg", "region_avg")
} else {
  region_total <- c(afri_mean[[1]], "region_avg", "region_avg")
}

region_df <- rbind(data.frame(), region_total)
names(region_df) <- names(st_drop_geometry(full_country_data))
full_region <- merge(area, region_df) |>
  st_as_sf()
full_country_data <- rbind(full_country_data, full_region) |>
  st_as_sf()

# afri_mean$names <- paste0("region_mean.", row.names(afri_mean))
# for (i in 1:nrow(afri_mean)) {
#   name <- paste0('region_mean.', row.names(afri_mean)[i])
#   country_extracted[name] <- afri_mean[i, ]
# }

write_sf(full_country_data,
  paste0("www/aggregated_data_", level, ".gpkg"),
  append = F)
