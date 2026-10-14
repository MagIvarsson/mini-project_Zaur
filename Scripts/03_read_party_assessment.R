
# ============================================================
# 03_read_party_assessment.R
# CHES 2024 - Swedish Political Parties
# Immigration Restrictiveness
# ============================================================

library(dplyr)

# 1. FILE PATHS
input_file <- "Data/Raw/Party_assesment/CHES_2024_final_v2.csv"
output_dir <- "Data/Processed"

if (!file.exists(input_file)) {
  matches <- list.files(
    "Data/Raw",
    pattern = "^CHES_2024_final_v2\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(matches) != 1) {
    stop("CHES file not found or multiple copies found.")
  }

  input_file <- matches[1]
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 2. READ CHES
ches_raw <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM"
)

cat("\n========== CHES DATA ==========\n")
cat("Rows:", nrow(ches_raw), "\n")
cat("Columns:", ncol(ches_raw), "\n")

# Inspect country codes in the actual file
cat("\nAvailable country codes:\n")
print(sort(unique(as.character(ches_raw$country))))

# 3. CHECK REQUIRED VARIABLES
required_vars <- c(
  "country", "party_id", "party",
  "electionyear", "immigrate_policy",
  "immigrate_salience", "multiculturalism",
  "galtan", "lrgen"
)

missing_vars <- setdiff(required_vars, names(ches_raw))

if (length(missing_vars) > 0) {
  stop(
    "Missing variables: ",
    paste(missing_vars, collapse = ", ")
  )
}

# 4. VERIFIED SWEDISH PARTY IDENTIFIERS
party_lookup <- data.frame(
  party_id = c(
    1601, 1602, 1603, 1604,
    1605, 1606, 1607, 1610
  ),
  expected_code = c(
    "V", "SAP", "C", "L",
    "M", "KD", "MP", "SD"
  ),
  election_party = c(
    "Vänsterpartiet",
    "Arbetarepartiet-Socialdemokraterna",
    "Centerpartiet",
    "Liberalerna (tidigare Folkpartiet)",
    "Moderaterna",
    "Kristdemokraterna",
    "Miljöpartiet de gröna",
    "Sverigedemokraterna"
  )
)

# 5. SELECT SWEDISH PARTIES BY NUMERIC ID
ches_sweden <- ches_raw %>%
  mutate(
    party_id = as.integer(party_id)
  ) %>%
  filter(
    party_id %in% party_lookup$party_id
  ) %>%
  transmute(
    country = as.character(country),
    party_id,
    ches_party = as.character(party),
    electionyear,
    immigrate_policy = as.numeric(immigrate_policy),
    immigrate_salience = as.numeric(immigrate_salience),
    multiculturalism = as.numeric(multiculturalism),
    galtan = as.numeric(galtan),
    lrgen = as.numeric(lrgen)
  )

cat("\n========== SWEDISH CHES RECORDS ==========\n")

print(as.data.frame(ches_sweden), row.names = FALSE)

cat("\nCountry codes for matched parties:\n")
print(unique(ches_sweden$country))

# 6. VALIDATE THE MATCHES
if (nrow(ches_sweden) != 8) {
  stop(
    "Expected 8 Swedish CHES parties; found ",
    nrow(ches_sweden)
  )
}

if (anyDuplicated(ches_sweden$party_id)) {
  stop("Duplicate CHES party IDs.")
}

party_assessment <- party_lookup %>%
  left_join(
    ches_sweden,
    by = "party_id"
  )

# Verify that numeric IDs correspond to the
# expected Swedish party abbreviations.
if (any(
  is.na(party_assessment$ches_party) |
  party_assessment$ches_party !=
    party_assessment$expected_code
)) {
  stop("Party ID and party abbreviation mismatch.")
}

if (any(is.na(party_assessment$immigrate_policy))) {
  stop("Missing immigration-policy scores.")
}

if (any(
  party_assessment$immigrate_policy < 0 |
  party_assessment$immigrate_policy > 10
)) {
  stop("Immigration scores outside 0-10.")
}

if (length(unique(party_assessment$country)) != 1) {
  stop("Matched parties have inconsistent country codes.")
}

cat("\n========== IMMIGRATION RESTRICTIVENESS ==========\n")

print(
  as.data.frame(
    party_assessment %>%
      select(
        election_party,
        ches_party,
        immigrate_policy,
        immigrate_salience,
        multiculturalism
      ) %>%
      arrange(immigrate_policy)
  ),
  row.names = FALSE
)

# 7. VALIDATE AGAINST ELECTION RESULTS
votes_file <- file.path(
  output_dir,
  "election_2022_party_shares.csv"
)

if (!file.exists(votes_file)) {
  stop("Run script 02 before script 03.")
}

party_votes <- read.csv(
  votes_file,
  colClasses = c(Lkfv = "character"),
  stringsAsFactors = FALSE
)

# Attach actual CHES expert scores to party votes
votes_with_ches <- party_votes %>%
  left_join(
    party_assessment %>%
      select(election_party, immigrate_policy),
    by = c("party" = "election_party")
  )

# Check that the join did not duplicate rows
if (nrow(votes_with_ches) != nrow(party_votes)) {
  stop("Party join duplicated election observations.")
}

# 8. CALCULATE CHES COVERAGE
coverage <- votes_with_ches %>%
  group_by(Lkfv) %>%
  summarise(
    total_votes = sum(votes),
    assessed_votes = sum(
      votes[!is.na(immigrate_policy)]
    ),
    ches_coverage = assessed_votes / total_votes,
    .groups = "drop"
  )

cat("\n========== CHES VOTE COVERAGE ==========\n")

print(summary(coverage$ches_coverage))

cat(
  "\nDistricts with coverage below 90%:",
  sum(coverage$ches_coverage < 0.90),
  "\n"
)

cat(
  "Districts with coverage below 95%:",
  sum(coverage$ches_coverage < 0.95),
  "\n"
)

cat(
  "Districts with coverage below 99%:",
  sum(coverage$ches_coverage < 0.99),
  "\n"
)

# 9. SAVE VERIFIED FILES
write.csv(
  ches_sweden,
  file.path(output_dir, "ches_sweden_2024.csv"),
  row.names = FALSE
)

write.csv(
  party_assessment,
  file.path(output_dir, "party_assessment_2024.csv"),
  row.names = FALSE
)

write.csv(
  coverage,
  file.path(output_dir, "ches_vote_coverage_2022.csv"),
  row.names = FALSE
)

cat("\n========== SCRIPT 03 COMPLETE ==========\n")
cat("Matched CHES parties:", nrow(party_assessment), "\n")
cat("Districts assessed:", nrow(coverage), "\n")
cat("Files saved in:", output_dir, "\n")
install.packages(c("ggplot2", "leaflet", "htmlwidgets"))

# ============================================================
# 10. CALCULATE DISTRICT RESTRICTIVE VOTING INDEX
# ============================================================

library(sf)
library(ggplot2)
library(leaflet)
library(htmlwidgets)

figure_dir <- "Outputs/Figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Weighted average of CHES immigration policy scores
# using votes for the 8 assessed parliamentary parties.

district_scores <- votes_with_ches %>%
  group_by(Lkfv) %>%
  summarise(
    municipality = first(municipality),
    district_name = first(district_name),

    total_votes = sum(votes),

    assessed_votes = sum(
      votes[!is.na(immigrate_policy)]
    ),

    weighted_sum = sum(
      votes * immigrate_policy,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%
  mutate(
    ches_coverage = assessed_votes / total_votes,

    restrictive_index = if_else(
      assessed_votes > 0,
      weighted_sum / assessed_votes,
      NA_real_
    ),

    coverage_ok = !is.na(ches_coverage) &
      ches_coverage >= 0.90
  )

# Check results
cat("\n========== RESTRICTIVE VOTING INDEX ==========\n")

print(summary(district_scores$restrictive_index))

cat("Total districts:", nrow(district_scores), "\n")

cat(
  "Districts with CHES coverage >= 90%:",
  sum(district_scores$coverage_ok), "\n"
)

cat(
  "Districts below 90% coverage:",
  sum(!district_scores$coverage_ok), "\n"
)

# Validate scores
if (any(
  district_scores$restrictive_index < 0 |
  district_scores$restrictive_index > 10,
  na.rm = TRUE
)) {
  stop("Restrictive index outside the 0-10 range.")
}

if (nrow(district_scores) != 6264 ||
    anyDuplicated(district_scores$Lkfv)) {
  stop("Unexpected number of districts or duplicate IDs.")
}

# Save the index
write.csv(
  district_scores,
  file.path(output_dir, "restrictive_index_2022.csv"),
  row.names = FALSE
)

# ============================================================
# 11. MERGE RESTRICTIVE INDEX WITH GIS
# ============================================================

gis <- st_read(
  "Data/Processed/districts_2022.gpkg",
  quiet = TRUE
)

gis$Lkfv <- as.character(gis$Lkfv)

map_data <- gis %>%
  left_join(
    district_scores,
    by = "Lkfv"
  ) %>%
  mutate(
    # Primary map shows districts with >=90% coverage
    map_score = if_else(
      coverage_ok %in% TRUE,
      restrictive_index,
      NA_real_
    )
  )

if (nrow(map_data) != nrow(gis)) {
  stop("GIS join unexpectedly changed district count.")
}

cat("\n========== GIS MATCH ==========\n")

cat("District polygons:", nrow(map_data), "\n")
cat(
  "Matched restrictive scores:",
  sum(!is.na(map_data$restrictive_index)), "\n"
)

# Save map-ready spatial data
st_write(
  map_data,
  file.path(output_dir, "restrictive_voting_2022.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

# ============================================================
# 12. STATIC MAP OF RESTRICTIVE VOTING
# ============================================================

static_map <- ggplot(map_data) +
  geom_sf(
    aes(fill = map_score),
    color = NA
  ) +
  scale_fill_gradient(
    low = "#fee8c8",
    high = "#b30000",
    na.value = "grey75",
    name = "Restrictiveness\nindex"
  ) +
  labs(
    title = "Immigration-Restrictive Voting in Sweden",
    subtitle = "2022 Riksdag Election",
    caption = paste(
      "Vote-weighted CHES 2024 party scores.",
      "Grey: CHES vote coverage below 90%."
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 8)
  )

ggsave(
  file.path(
    figure_dir,
    "restrictive_voting_map_2022.png"
  ),
  plot = static_map,
  width = 9,
  height = 12,
  dpi = 250
)

# ============================================================
# 13. INTERACTIVE RESTRICTIVE VOTING MAP
# ============================================================

# Simplify polygons to improve loading speed
interactive_data <- st_simplify(
  map_data,
  dTolerance = 30,
  preserveTopology = TRUE
)

# Transform to WGS84 for Leaflet
interactive_data <- st_transform(
  interactive_data,
  4326
)

# Color scale
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = interactive_data$map_score,
  na.color = "#bdbdbd"
)

# District information displayed when clicked
interactive_data$popup <- paste0(
  "<b>Voting district:</b> ",
  interactive_data$Vdnamn,

  "<br><b>Municipality:</b> ",
  interactive_data$municipality,

  "<br><b>District ID:</b> ",
  interactive_data$Lkfv,

  "<br><b>Restrictiveness index:</b> ",
  sprintf("%.2f", interactive_data$restrictive_index),

  "<br><b>CHES vote coverage:</b> ",
  sprintf("%.1f%%", 100 * interactive_data$ches_coverage),

  "<br><b>Total party votes:</b> ",
  interactive_data$total_votes,

  "<br><b>Coverage:</b> ",
  ifelse(
    interactive_data$coverage_ok,
    "Included",
    "Below 90% threshold"
  )
)

# Create interactive map
restrictive_map <- leaflet(interactive_data) %>%
  addProviderTiles(
    providers$CartoDB.Positron
  ) %>%
  addPolygons(
    fillColor = ~pal(map_score),
    fillOpacity = 0.85,
    color = "#555555",
    weight = 0.2,
    popup = ~popup,
    highlightOptions = highlightOptions(
      color = "black",
      weight = 1.5,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~map_score,
    title = "Restrictiveness index",
    opacity = 0.9
  ) %>%
  addControl(
    html = paste0(
      "<b>Swedish Election 2022</b>",
      "<br>Immigration-restrictive voting",
      "<br><small>Grey: coverage below 90%</small>"
    ),
    position = "topright"
  )

# Save interactive map
saveWidget(
  restrictive_map,
  file = file.path(
    figure_dir,
    "interactive_restrictive_voting_2022.html"
  ),
  selfcontained = FALSE
)

cat("\n========== ANALYSIS COMPLETED ==========\n")
cat("Restrictive index calculated.\n")
cat("Static map generated.\n")
cat("Interactive map generated.\n")
