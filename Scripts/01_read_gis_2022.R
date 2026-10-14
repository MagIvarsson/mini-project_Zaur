
# ============================================================
# 01_read_gis_2022.R
# Swedish Voting Districts - 2022
# ============================================================

library(sf)
library(dplyr)

# 1. FOLDERS
input_dir  <- "Data/Raw/GIS"
output_dir <- "Data/Processed"
figure_dir <- "Outputs/Figures"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# 2. FIND ALL JSON FILES
json_files <- list.files(
  input_dir,
  pattern = "\\.(geo)?json$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

cat("JSON files found:", length(json_files), "\n")

if (length(json_files) == 0) {
  stop("No JSON files found!")
}

# 3. READ AND COMBINE ALL COUNTY FILES
gis_list <- lapply(json_files, function(file) {

  cat("Reading:", basename(file), "\n")

  gis <- st_read(file, quiet = TRUE)

  # Check coordinates and assign/transform CRS
  bbox <- st_bbox(gis)

  if (abs(bbox[["ymin"]]) > 1000) {

    # Projected coordinates: SWEREF99 TM
    st_crs(gis) <- 3006

  } else {

    if (is.na(st_crs(gis))) {
      st_crs(gis) <- 4326
    }

    gis <- st_transform(gis, 3006)
  }

  gis$Lkfv <- as.character(gis$Lkfv)
  gis$source_file <- basename(file)

  gis
})

districts_2022 <- bind_rows(gis_list)

cat("\nTotal voting districts:", nrow(districts_2022), "\n")
cat("CRS:", st_crs(districts_2022)$epsg, "\n")

# 4. CHECK DISTRICT IDENTIFIERS
cat("\n========== ID CHECKS ==========\n")

cat("Identifier type:\n")
print(class(districts_2022$Lkfv))

cat("\nDuplicate IDs:",
    sum(duplicated(districts_2022$Lkfv)), "\n")

cat("Missing IDs:",
    sum(is.na(districts_2022$Lkfv) |
        trimws(districts_2022$Lkfv) == ""), "\n")

cat("\nIdentifier lengths:\n")
print(table(nchar(districts_2022$Lkfv), useNA = "ifany"))

cat("\nFirst 5 voting districts:\n")
print(
  head(
    st_drop_geometry(districts_2022)[, c("Lkfv", "Vdnamn")],
    5
  )
)

# 5. CHECK INVALID GEOMETRIES
cat("\n========== GEOMETRY CHECKS ==========\n")

valid_before <- st_is_valid(districts_2022)

invalid_rows <- which(
  is.na(valid_before) | !valid_before
)

cat("Invalid geometries before repair:",
    length(invalid_rows), "\n")

if (length(invalid_rows) > 0) {

  cat("\nProblematic districts:\n")

  print(
    data.frame(
      district_id = districts_2022$Lkfv[invalid_rows],
      district_name = districts_2022$Vdnamn[invalid_rows],
      problem = st_is_valid(
        districts_2022[invalid_rows, ],
        reason = TRUE
      )
    )
  )

  # Repair geometries
  districts_2022 <- st_make_valid(districts_2022)
}

valid_after <- st_is_valid(districts_2022)

cat("\nInvalid geometries after repair:",
    sum(is.na(valid_after) | !valid_after), "\n")

if (any(is.na(valid_after) | !valid_after)) {
  stop("Some geometries remain invalid.")
}

# 6. SAVE CLEANED GIS DATASET
st_write(
  districts_2022,
  file.path(output_dir, "districts_2022.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

cat("\nSaved: Data/Processed/districts_2022.gpkg\n")

# 7. SAVE COUNTY INVENTORY
inventory <- data.frame(
  source_file = basename(json_files),
  districts = sapply(gis_list, nrow)
)

write.csv(
  inventory,
  file.path(output_dir, "gis_inventory_2022.csv"),
  row.names = FALSE
)

cat("\n========== COUNTY INVENTORY ==========\n")
print(inventory)

# 8. CREATE MAP
png(
  file.path(
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

cat("\n========== IMPORT COMPLETE ==========\n")
cat("Total districts:", nrow(districts_2022), "\n")
cat("Unique IDs:", n_distinct(districts_2022$Lkfv), "\n")
cat("Invalid geometries:", sum(!valid_after), "\n")

# 9. DISPLAY WARNINGS, IF ANY
cat("\n========== WARNINGS ==========\n")
print(warnings())
