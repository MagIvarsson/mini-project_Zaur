
# ============================================================
# 01_read_gis_2022.R
# Swedish voting districts - 2022
# Source: Valmyndigheten
# ============================================================

# 1. Load packages
library(sf)
library(dplyr)

# 2. Set folder paths
input_dir <- "Data/Raw/GIS"
output_dir <- "Data/Processed"
figure_dir <- "Outputs/Figures"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# 3. Find all JSON files
json_files <- list.files(
  path = input_dir,
  pattern = "\\.(geo)?json$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

cat("Number of JSON files found:", length(json_files), "\n")

if (length(json_files) == 0) {
  stop("No JSON files found. Check the input folder path.")
}

# 4. Read each county file
gis_list <- lapply(json_files, function(file) {

  cat("Reading:", basename(file), "\n")

  gis <- st_read(file, quiet = TRUE)

  # Valmyndigheten's 2022 coordinates use SWEREF99 TM
  # (EPSG:3006). Some GeoJSON readers may assume WGS84.
  bbox <- st_bbox(gis)

  projected_coordinates <- (
    abs(bbox[["xmin"]]) > 1000 ||
    abs(bbox[["ymin"]]) > 1000
  )

  current_crs <- st_crs(gis)

  if (is.na(current_crs)) {

    if (!projected_coordinates) {
      stop("Unknown coordinate system in: ", basename(file))
    }

    st_crs(gis) <- 3006

  } else if (
    identical(current_crs$epsg, 4326L) &&
    projected_coordinates
  ) {

    # Correct a wrongly assumed geographic CRS
    st_crs(gis) <- 3006

  } else if (current_crs != st_crs(3006)) {

    # Transform genuinely different coordinate systems
    gis <- st_transform(gis, 3006)
  }

  # Preserve the source filename
  gis$source_file <- basename(file)

  return(gis)
})

# 5. Combine all counties
districts_2022 <- bind_rows(gis_list)

cat("\nTotal voting districts:", nrow(districts_2022), "\n")
cat("Coordinate system:", st_crs(districts_2022)$epsg, "\n")

# 6. Inspect available variables
cat("\nGIS ATTRIBUTE COLUMNS:\n")
print(names(districts_2022))

cat("\nFIRST 3 DISTRICTS:\n")
print(
  head(st_drop_geometry(districts_2022), 3)
)

# 7. Check geometry validity
invalid_n <- sum(
  !st_is_valid(districts_2022),
  na.rm = TRUE
)

cat("\nInvalid geometries:", invalid_n, "\n")

# 8. Save combined geographical dataset
output_file <- file.path(
  output_dir,
  "districts_2022.gpkg"
)

st_write(
  districts_2022,
  output_file,
  delete_dsn = TRUE,
  quiet = TRUE
)

cat("\nSaved GIS dataset to:", output_file, "\n")

# 9. Create file inventory
inventory <- data.frame(
  source_file = basename(json_files),
  districts = sapply(gis_list, nrow)
)

write.csv(
  inventory,
  file.path(output_dir, "gis_inventory_2022.csv"),
  row.names = FALSE
)

print(inventory)

# 10. Plot all voting district boundaries
png(
  filename = file.path(
    figure_dir,
    "voting_district_boundaries_2022.png"
  ),
  width = 1600,
  height = 2200,
  res = 180
)

plot(
  st_geometry(districts_2022),
  col = "grey90",
  border = "grey45",
  lwd = 0.15,
  main = "Swedish Voting Districts, 2022"
)

dev.off()

cat("\nGIS import completed successfully!\n")

