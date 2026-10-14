
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



# ============================================================
# 15. SALIENCE-WEIGHTED RESTRICTIVE VOTING INDEX
# ============================================================

# Check the CHES variables
if (anyNA(party_assessment$immigrate_policy) ||
    anyNA(party_assessment$immigrate_salience)) {
  stop("Missing CHES policy or salience scores.")
}

if (any(party_assessment$immigrate_salience < 0 |
        party_assessment$immigrate_salience > 10)) {
  stop("CHES salience scores outside 0-10.")
}

# Add policy restrictiveness and policy salience to votes
votes_salience <- party_votes %>%
  left_join(
    party_assessment %>%
      select(
        election_party,
        immigrate_policy,
        immigrate_salience
      ),
    by = c("party" = "election_party")
  )

if (nrow(votes_salience) != nrow(party_votes)) {
  stop("CHES join duplicated vote observations.")
}

# Calculate the salience-weighted index per district
salience_scores <- votes_salience %>%
  group_by(Lkfv) %>%
  summarise(
    salience_weighted_votes = sum(
      votes * immigrate_salience,
      na.rm = TRUE
    ),

    salience_weighted_score_sum = sum(
      votes * immigrate_policy * immigrate_salience,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%
  left_join(
    district_scores %>%
      select(
        Lkfv,
        municipality,
        district_name,
        total_votes,
        assessed_votes,
        ches_coverage,
        coverage_ok,
        restrictive_index
      ),
    by = "Lkfv"
  ) %>%
  mutate(
    salience_weighted_index = if_else(
      salience_weighted_votes > 0,
      salience_weighted_score_sum /
        salience_weighted_votes,
      NA_real_
    ),

    # Difference between the two indices
    salience_effect = salience_weighted_index -
      restrictive_index,

    # Apply the same coverage threshold
    salience_map_score = if_else(
      coverage_ok %in% TRUE,
      salience_weighted_index,
      NA_real_
    )
  )

# Validate results
if (nrow(salience_scores) != 6264 ||
    anyDuplicated(salience_scores$Lkfv)) {
  stop("Unexpected number of districts or duplicate IDs.")
}

if (anyNA(salience_scores$salience_weighted_index)) {
  stop("Some districts have no salience-weighted score.")
}

if (any(
  salience_scores$salience_weighted_index < 0 |
  salience_scores$salience_weighted_index > 10
)) {
  stop("Salience-weighted index outside 0-10.")
}

# ============================================================
# 16. COMPARE ORIGINAL AND SALIENCE-WEIGHTED INDICES
# ============================================================

cat("\n========== INDEX COMPARISON ==========\n")

print(
  summary(
    salience_scores %>%
      select(
        restrictive_index,
        salience_weighted_index,
        salience_effect
      )
  )
)

cat(
  "\nCorrelation between indices:",
  cor(
    salience_scores$restrictive_index,
    salience_scores$salience_weighted_index
  ),
  "\n"
)

cat(
  "Districts included in primary maps:",
  sum(salience_scores$coverage_ok),
  "\n"
)

# Save the new dataset
write.csv(
  salience_scores,
  file.path(
    output_dir,
    "salience_weighted_restrictive_index_2022.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 17. JOIN THE NEW INDEX WITH GIS
# ============================================================

salience_map_data <- map_data %>%
  left_join(
    salience_scores %>%
      select(
        Lkfv,
        salience_weighted_index,
        salience_effect,
        salience_map_score
      ),
    by = "Lkfv"
  )

if (nrow(salience_map_data) != 6264) {
  stop("GIS join changed the number of districts.")
}

# Save spatial dataset
st_write(
  salience_map_data,
  file.path(
    output_dir,
    "salience_weighted_voting_2022.gpkg"
  ),
  delete_dsn = TRUE,
  quiet = TRUE
)

# ============================================================
# 18. STATIC MAP
# ============================================================

salience_static_map <- ggplot(salience_map_data) +
  geom_sf(
    aes(fill = salience_map_score),
    color = NA
  ) +
  scale_fill_gradient(
    low = "#fee8c8",
    high = "#b30000",
    limits = c(0, 10),
    na.value = "grey75",
    name = "Restrictiveness\nindex"
  ) +
  labs(
    title = "Salience-Weighted Restrictive Voting",
    subtitle = "Sweden, 2022 Riksdag Election",
    caption = paste(
      "CHES 2024 immigration-policy restrictiveness",
      "weighted by party votes and policy salience.",
      "Grey: CHES coverage below 90%."
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
    "salience_weighted_restrictive_map_2022.png"
  ),
  plot = salience_static_map,
  width = 9,
  height = 12,
  dpi = 250
)

# ============================================================
# 19. INTERACTIVE SALIENCE-WEIGHTED MAP
# ============================================================

salience_interactive <- st_simplify(
  salience_map_data,
  dTolerance = 30,
  preserveTopology = TRUE
)

salience_interactive <- st_transform(
  salience_interactive,
  4326
)

# Fixed 0-10 scale
salience_pal <- colorNumeric(
  palette = "YlOrRd",
  domain = c(0, 10),
  na.color = "#bdbdbd"
)

# Popup comparing the two indices
salience_interactive$popup_salience <- paste0(
  "<b>District:</b> ",
  salience_interactive$district_name,

  "<br><b>Municipality:</b> ",
  salience_interactive$municipality,

  "<br><b>District ID:</b> ",
  salience_interactive$Lkfv,

  "<br><b>Original index:</b> ",
  sprintf(
    "%.2f",
    salience_interactive$restrictive_index
  ),

  "<br><b>Salience-weighted index:</b> ",
  sprintf(
    "%.2f",
    salience_interactive$salience_weighted_index
  ),

  "<br><b>Difference:</b> ",
  sprintf(
    "%+.2f",
    salience_interactive$salience_effect
  ),

  "<br><b>CHES vote coverage:</b> ",
  sprintf(
    "%.1f%%",
    100 * salience_interactive$ches_coverage
  )
)

# Create interactive map
salience_map <- leaflet(salience_interactive) %>%
  addProviderTiles(
    providers$CartoDB.Positron
  ) %>%
  addPolygons(
    fillColor = ~salience_pal(salience_map_score),
    fillOpacity = 0.85,
    color = "#555555",
    weight = 0.2,
    popup = ~popup_salience,
    highlightOptions = highlightOptions(
      color = "black",
      weight = 1.5,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = salience_pal,
    values = c(0, 10),
    title = "Salience-weighted index",
    opacity = 0.9
  ) %>%
  addControl(
    html = paste0(
      "<b>Swedish Riksdag Election 2022</b>",
      "<br>Salience-weighted restrictive voting",
      "<br><small>Grey: CHES coverage below 90%</small>"
    ),
    position = "topright"
  )

# Save HTML
saveWidget(
  salience_map,
  file = file.path(
    figure_dir,
    "interactive_salience_weighted_2022.html"
  ),
  selfcontained = FALSE
)

cat("\n========== SALIENCE ANALYSIS COMPLETED ==========\n")
cat("Salience-weighted index saved.\n")
cat("Static map saved.\n")
cat("Interactive map saved.\n")



# ============================================================
# 20. COMPARE VARIANCE OF THE TWO INDICES
# ============================================================

# Compare the SAME districts for both indices.
# Use the primary sample: CHES coverage >= 90%.

variance_data <- salience_scores %>%
  filter(
    coverage_ok %in% TRUE,
    is.finite(restrictive_index),
    is.finite(salience_weighted_index)
  )

if (nrow(variance_data) < 2) {
  stop("Not enough districts to compare variance.")
}

# Prepare data in long format
variance_long <- bind_rows(
  variance_data %>%
    transmute(
      index_type = "Original index",
      score = restrictive_index
    ),

  variance_data %>%
    transmute(
      index_type = "Salience-weighted index",
      score = salience_weighted_index
    )
)

# 21. DESCRIPTIVE STATISTICS

variance_summary <- variance_long %>%
  group_by(index_type) %>%
  summarise(
    n = n(),
    mean = mean(score),
    variance = var(score),
    sd = sd(score),
    iqr = IQR(score),
    p05 = as.numeric(quantile(score, 0.05)),
    p95 = as.numeric(quantile(score, 0.95)),
    min = min(score),
    max = max(score),
    .groups = "drop"
  )

cat("\n========== VARIANCE COMPARISON ==========\n")

print(as.data.frame(variance_summary), row.names = FALSE)

# Extract the two variances
var_original <- var(
  variance_data$restrictive_index
)

var_salience <- var(
  variance_data$salience_weighted_index
)

sd_original <- sd(
  variance_data$restrictive_index
)

sd_salience <- sd(
  variance_data$salience_weighted_index
)

# Relative difference in variance
variance_ratio <- var_salience / var_original

variance_change_pct <- 100 * (
  var_salience - var_original
) / var_original

cat("\n========== MAIN RESULTS ==========\n")

cat(
  "Number of districts:",
  nrow(variance_data), "\n"
)

cat(
  "Original variance:",
  round(var_original, 4), "\n"
)

cat(
  "Salience-weighted variance:",
  round(var_salience, 4), "\n"
)

cat(
  "Original SD:",
  round(sd_original, 4), "\n"
)

cat(
  "Salience-weighted SD:",
  round(sd_salience, 4), "\n"
)

cat(
  "Variance ratio:",
  round(variance_ratio, 4), "\n"
)

cat(
  "Change in variance:",
  round(variance_change_pct, 2), "%\n"
)

if (variance_ratio > 1) {
  cat("\nRESULT: Salience weighting INCREASES variance.\n")
} else if (variance_ratio < 1) {
  cat("\nRESULT: Salience weighting DECREASES variance.\n")
} else {
  cat("\nRESULT: Variance is unchanged.\n")
}

# ============================================================
# 22. VISUALIZE DISTRIBUTIONS
# ============================================================

distribution_plot <- ggplot(
  variance_long,
  aes(
    x = score,
    fill = index_type
  )
) +
  geom_density(
    alpha = 0.4,
    color = NA
  ) +
  labs(
    title = "Distribution of Restrictive Voting Indices",
    subtitle = "Swedish voting districts, Riksdag election 2022",
    x = "Restrictive voting index (0-10)",
    y = "Density",
    fill = "Index",
    caption = "Only districts with CHES vote coverage >= 90%"
  ) +
  coord_cartesian(xlim = c(0, 10)) +
  theme_minimal(base_size = 12)

print(distribution_plot)

ggsave(
  file.path(
    figure_dir,
    "original_vs_salience_distribution_2022.png"
  ),
  plot = distribution_plot,
  width = 10,
  height = 6,
  dpi = 250
)

# ============================================================
# 23. SCATTERPLOT: ORIGINAL VS SALIENCE-WEIGHTED
# ============================================================

comparison_plot <- ggplot(
  variance_data,
  aes(
    x = restrictive_index,
    y = salience_weighted_index
  )
) +
  geom_point(
    alpha = 0.15,
    size = 0.8
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  coord_equal(
    xlim = c(0, 10),
    ylim = c(0, 10)
  ) +
  labs(
    title = "Does Policy Salience Change Restrictive Voting?",
    subtitle = "Each point represents one voting district",
    x = "Original restrictive voting index",
    y = "Salience-weighted restrictive voting index"
  ) +
  theme_minimal(base_size = 12)

print(comparison_plot)

ggsave(
  file.path(
    figure_dir,
    "original_vs_salience_scatter_2022.png"
  ),
  plot = comparison_plot,
  width = 8,
  height = 8,
  dpi = 250
)

# ============================================================
# 24. WITHIN-MUNICIPALITY VARIANCE COMPARISON
# ============================================================

municipality_variance <- variance_data %>%
  group_by(municipality) %>%
  summarise(
    n_districts = n(),

    original_variance = if (n() >= 2) {
      var(restrictive_index)
    } else {
      NA_real_
    },

    salience_variance = if (n() >= 2) {
      var(salience_weighted_index)
    } else {
      NA_real_
    },

    .groups = "drop"
  ) %>%
  filter(n_districts >= 5) %>%
  mutate(
    variance_difference =
      salience_variance - original_variance,

    variance_ratio = if_else(
      original_variance > 0,
      salience_variance / original_variance,
      NA_real_
    ),

    variance_increased =
      salience_variance > original_variance
  )

cat("\n========== MUNICIPALITY COMPARISON ==========\n")

cat(
  "Municipalities with at least 5 districts:",
  nrow(municipality_variance), "\n"
)

cat(
  "Municipalities with increased variance:",
  sum(municipality_variance$variance_increased),
  "\n"
)

cat(
  "Municipalities with decreased variance:",
  sum(
    municipality_variance$salience_variance <
      municipality_variance$original_variance
  ),
  "\n"
)

# Municipalities with largest absolute increases
cat("\nTOP 10: LARGEST INCREASES IN VARIANCE\n")

print(
  municipality_variance %>%
    arrange(desc(variance_difference)) %>%
    select(
      municipality,
      n_districts,
      original_variance,
      salience_variance,
      variance_difference,
      variance_ratio
    ) %>%
    head(10),
  n = 10
)

# Municipalities with largest absolute decreases
cat("\nTOP 10: LARGEST DECREASES IN VARIANCE\n")

print(
  municipality_variance %>%
    arrange(variance_difference) %>%
    select(
      municipality,
      n_districts,
      original_variance,
      salience_variance,
      variance_difference,
      variance_ratio
    ) %>%
    head(10),
  n = 10
)

# ============================================================
# 25. SAVE VARIANCE RESULTS
# ============================================================

write.csv(
  variance_summary,
  file.path(
    output_dir,
    "index_variance_comparison_2022.csv"
  ),
  row.names = FALSE
)

write.csv(
  municipality_variance,
  file.path(
    output_dir,
    "municipality_index_variance_2022.csv"
  ),
  row.names = FALSE
)

cat("\n========== VARIANCE ANALYSIS COMPLETE ==========\n")


# ============================================================
# 26. DOES SALIENCE INCREASE VARIANCE OR ONLY THE MEAN?
# ============================================================

comparison <- variance_data %>%
  mutate(
    difference = salience_weighted_index -
      restrictive_index
  )

# Numerical comparison
results <- data.frame(
  measure = c(
    "Mean",
    "Standard deviation",
    "Variance",
    "Minimum",
    "Maximum"
  ),
  original = c(
    mean(comparison$restrictive_index),
    sd(comparison$restrictive_index),
    var(comparison$restrictive_index),
    min(comparison$restrictive_index),
    max(comparison$restrictive_index)
  ),
  salience_weighted = c(
    mean(comparison$salience_weighted_index),
    sd(comparison$salience_weighted_index),
    var(comparison$salience_weighted_index),
    min(comparison$salience_weighted_index),
    max(comparison$salience_weighted_index)
  )
)

results$change <- results$salience_weighted -
  results$original

cat("\n========== COMPARISON ==========\n")
print(round(results[-1], 4))

cat("\n========== DIFFERENCE ==========\n")

cat("Mean shift:",
    round(mean(comparison$difference), 4), "\n")

cat("SD of differences:",
    round(sd(comparison$difference), 4), "\n")

cat("Correlation:",
    round(cor(
      comparison$restrictive_index,
      comparison$salience_weighted_index
    ), 5), "\n")

cat("Variance ratio:",
    round(
      var(comparison$salience_weighted_index) /
        var(comparison$restrictive_index),
      4
    ), "\n")

# ============================================================
# 27. PLOT THE DIFFERENCE AGAINST ORIGINAL INDEX
# ============================================================

difference_plot <- ggplot(
  comparison,
  aes(
    x = restrictive_index,
    y = difference
  )
) +
  geom_point(
    alpha = 0.15,
    size = 0.7
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    color = "#c0392b"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "How Much Does Salience Change the Index?",
    subtitle = "Swedish voting districts, 2022",
    x = "Original restrictive voting index",
    y = "Salience-weighted minus original",
    caption = "Positive values indicate higher scores after salience weighting"
  ) +
  theme_minimal(base_size = 12)

print(difference_plot)

ggsave(
  file.path(
    figure_dir,
    "salience_index_difference_2022.png"
  ),
  plot = difference_plot,
  width = 9,
  height = 6,
  dpi = 250
)

# ============================================================
# 28. CHECK WHETHER SALIENCE CHANGES THE SPREAD
# ============================================================

variance_ratio <- var(
  comparison$salience_weighted_index
) / var(
  comparison$restrictive_index
)

cat("\n========== VARIANCE INTERPRETATION ==========\n")

cat(
  "Variance change (%):",
  round(100 * (variance_ratio - 1), 2),
  "\n"
)

if (variance_ratio > 1) {
  cat("Salience weighting increases geographical variance.\n")
} else if (variance_ratio < 1) {
  cat("Salience weighting decreases geographical variance.\n")
} else {
  cat("Geographical variance remains unchanged.\n")
}
