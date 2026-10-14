
# ============================================================
# 02_read_votes_2022.R
# Swedish Parliamentary Election 2022
# Source: Valmyndigheten
# ============================================================

library(readxl)
library(dplyr)
library(sf)

# 1. FILE PATHS
input_file <- "Data/Raw/Election_result/election_district_2022.xlsx"
gis_file <- "Data/Processed/districts_2022.gpkg"
output_dir <- "Data/Processed"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(input_file))
stopifnot(file.exists(gis_file))

# 2. READ THE CORRECT ELECTION WORKSHEET
cat("\n========== READING ELECTION DATA ==========\n")

votes_raw <- read_excel(
  input_file,
  sheet = "roster_RD",
  col_types = "text",
  .name_repair = "unique_quiet"
)

cat("Election rows:", nrow(votes_raw), "\n")
cat("Columns:", ncol(votes_raw), "\n")

# 3. VERIFY REQUIRED VARIABLES
required_columns <- c(
  "Val",
  "Valdistriktskod",
  "Valdistriktnamn",
  "Kommun",
  "Parti",
  "Röster",
  "Röstberättigade"
)

missing_columns <- setdiff(
  required_columns,
  names(votes_raw)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# 4. CLEAN IDENTIFIERS AND NUMERIC VARIABLES

clean_number <- function(x) {
  x <- gsub("[[:space:]]", "", as.character(x))
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

clean_district_id <- function(x) {

  x <- trimws(as.character(x))
  x <- gsub("[[:space:]]", "", x)
  x <- sub("\\.0+$", "", x)

  # Restore a leading zero if a normal 8-digit
  # district code was imported with only 7 digits.
  seven_digits <- !is.na(x) & grepl("^[0-9]{7}$", x)
  x[seven_digits] <- paste0("0", x[seven_digits])

  # Do not pad 6-digit collection-district codes.
  x
}

votes_all <- votes_raw %>%
  transmute(
    election = trimws(Val),
    Lkfv = clean_district_id(Valdistriktskod),
    district_name = trimws(Valdistriktnamn),
    municipality = trimws(Kommun),
    party = trimws(Parti),
    votes = clean_number(`Röster`),
    eligible_voters = clean_number(`Röstberättigade`)
  ) %>%
  filter(election == "RD")

cat("\nUnique election district codes:",
    n_distinct(votes_all$Lkfv), "\n")

cat("\nDistrict identifier lengths:\n")
print(table(nchar(votes_all$Lkfv), useNA = "ifany"))

# 5. READ GIS DISTRICTS
cat("\n========== GIS MATCHING ==========\n")

gis <- st_read(gis_file, quiet = TRUE)

gis_ids <- as.character(gis$Lkfv)

cat("GIS districts:", length(gis_ids), "\n")

# Unique district records in the election data
election_districts <- votes_all %>%
  distinct(Lkfv, district_name, municipality)

# Matched district identifiers
matched_ids <- intersect(
  election_districts$Lkfv,
  gis_ids
)

cat("Matched district IDs:", length(matched_ids), "\n")

cat(
  "GIS districts without matching election codes:",
  length(setdiff(gis_ids, matched_ids)),
  "\n"
)

if (length(matched_ids) == 0) {
  stop("No election districts match the GIS identifiers.")
}

# 6. IDENTIFY ELECTION DISTRICTS WITHOUT GIS POLYGONS
unmatched <- election_districts %>%
  filter(!(Lkfv %in% gis_ids))

cat("\nUnmatched election districts:",
    nrow(unmatched), "\n")

cat("\nFirst 15 unmatched districts:\n")
print(head(unmatched, 15))

write.csv(
  unmatched,
  file.path(
    output_dir,
    "election_2022_unmatched_districts.csv"
  ),
  row.names = FALSE
)

# 7. SEPARATE PARTY VOTES FROM ADMINISTRATIVE CATEGORIES

# These categories are not valid votes for
# registered parties and must not enter the
# restrictive-voting index.

excluded_categories <- c(
  "blanka röster",
  "övriga ogiltiga",
  "ogiltiga röster",
  "ej anmält deltagande",
  "summa giltiga röster",
  "summa röster",
  "totalsumma",
  "valdeltagande",
  "röstberättigade"
)

# Print all distinct labels for inspection
party_labels <- sort(unique(
  votes_all$party[!is.na(votes_all$party)]
))

cat("\n========== PARTY LABELS ==========\n")
print(party_labels)

# Check for administrative labels that might
# need to be added to the exclusion list.
suspected_categories <- party_labels[
  grepl(
    "ogiltig|blanka|ej anmält|summa|totalt|valdeltagande|röstberättigade",
    tolower(party_labels)
  ) &
  !(tolower(party_labels) %in% excluded_categories)
]

if (length(suspected_categories) > 0) {
  stop(
    "Review these possible non-party categories: ",
    paste(suspected_categories, collapse = "; ")
  )
}

# Keep valid party rows in mapped districts
votes_2022 <- votes_all %>%
  filter(
    Lkfv %in% gis_ids,
    !is.na(party),
    party != "",
    !(tolower(party) %in% excluded_categories)
  )

if (any(is.na(votes_2022$votes)) ||
    any(votes_2022$votes < 0, na.rm = TRUE)) {
  stop("Missing or negative party vote counts found.")
}

cat("\nParty-vote observations in mapped districts:",
    nrow(votes_2022), "\n")

# 8. PREPARE DISTRICT METADATA
district_meta <- votes_2022 %>%
  distinct(
    Lkfv,
    municipality,
    district_name
  )

if (anyDuplicated(district_meta$Lkfv) > 0) {
  stop("Some district IDs have conflicting names or municipalities.")
}

# 9. CALCULATE TOTAL VALID PARTY VOTES PER DISTRICT
district_totals <- votes_2022 %>%
  group_by(Lkfv) %>%
  summarise(
    valid_votes = sum(votes),
    eligible_voters = if (all(is.na(eligible_voters))) {
      NA_real_
    } else {
      max(eligible_voters, na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  left_join(
    district_meta,
    by = "Lkfv"
  )

cat("\n========== DISTRICT VOTE TOTALS ==========\n")
print(summary(district_totals$valid_votes))

# 10. CALCULATE PARTY VOTE SHARES
party_votes <- votes_2022 %>%
  group_by(Lkfv, party) %>%
  summarise(
    votes = sum(votes),
    .groups = "drop"
  ) %>%
  left_join(
    district_totals %>%
      select(Lkfv, valid_votes, municipality, district_name),
    by = "Lkfv"
  ) %>%
  mutate(
    vote_share = if_else(
      valid_votes > 0,
      votes / valid_votes,
      NA_real_
    )
  )

# 11. VALIDATE PARTY VOTE SHARES
share_check <- party_votes %>%
  group_by(Lkfv) %>%
  summarise(
    total_share = sum(vote_share),
    .groups = "drop"
  )

cat("\n========== VOTE SHARE VALIDATION ==========\n")

print(summary(share_check$total_share))

if (any(
  abs(share_check$total_share - 1) > 0.000001,
  na.rm = TRUE
)) {
  warning("Some district vote shares do not sum to 1.")
}

cat(
  "Districts with zero valid party votes:",
  sum(district_totals$valid_votes == 0),
  "\n"
)

# 12. SAVE CLEANED DATASETS

write.csv(
  votes_2022,
  file.path(
    output_dir,
    "election_2022_votes_long.csv"
  ),
  row.names = FALSE
)

write.csv(
  district_totals,
  file.path(
    output_dir,
    "election_2022_district_totals.csv"
  ),
  row.names = FALSE
)

write.csv(
  party_votes,
  file.path(
    output_dir,
    "election_2022_party_shares.csv"
  ),
  row.names = FALSE
)

# 13. DISPLAY MAJOR PARTIES
cat("\n========== LARGEST PARTIES ==========\n")

national_party_summary <- party_votes %>%
  group_by(party) %>%
  summarise(
    votes = sum(votes),
    .groups = "drop"
  ) %>%
  arrange(desc(votes))

print(head(national_party_summary, 12))

# 14. FINAL SUMMARY
cat("\n========== IMPORT COMPLETE ==========\n")

cat("GIS districts:", length(gis_ids), "\n")

cat(
  "Districts with valid party votes:",
  n_distinct(party_votes$Lkfv),
  "\n"
)

cat(
  "Unmatched election district codes:",
  nrow(unmatched),
  "\n"
)

cat(
  "Number of parties/categories retained:",
  n_distinct(party_votes$party),
  "\n"
)

cat("\nFiles saved in:", output_dir, "\n")
