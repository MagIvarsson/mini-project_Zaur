# Swedish Election 2022: Immigration-Restrictive Voting

This project examines geographical variation in voting for parties with different immigration-policy positions in the **2022 Swedish parliamentary election**. It combines election results for **6,264 voting districts**, voting-district boundaries, and party positions from the **2024 Chapel Hill Expert Survey (CHES)**.

## Data

- **Valmyndigheten:** 2022 Riksdag election results by voting district and district boundaries — https://www.val.se/
- **CHES 2024:** Expert assessments of Swedish parties' immigration-policy positions and issue salience — https://www.chesdata.eu/

The eight parliamentary parties are matched to CHES ratings. Special vote-collection districts without matching geographical boundaries are excluded from district maps.

## Analysis

The main outcome is a **vote-weighted immigration restrictiveness index** (0–10), based on each party's CHES `immigrate_policy` score. A sensitivity analysis additionally weights party positions by `immigrate_salience`. Maps generally display districts with **at least 90% of votes covered by CHES ratings**.

The scripts produce district-level datasets, an interactive map of **Sverigedemokraterna (SD)** vote share, restrictive-voting maps, and comparisons between the two indices.

## Reproduce the analysis

1. Download the data from the sources above. Place the 21 county GIS files in `Data/Raw/GIS/`, the district election workbook at `Data/Raw/Election_result/election_district_2022.xlsx`, and CHES at `Data/Raw/Party_assesment/CHES_2024_final_v2.csv`.
2. Open the repository as your working directory in Positron or RStudio.
3. Install packages and run the scripts in order:

```r
install.packages(c("sf", "dplyr", "readxl", "ggplot2", "leaflet", "htmlwidgets", "scales", "purrr"))

source("Scripts/01_read_gis_2022.R")
source("Scripts/02_read_votes_2022.R")
source("Scripts/03_read_party_assessment.R")
```

Processed datasets are saved in `Data/Processed/`, and maps in `Outputs/Figures/`.

**Interpretation:** These are district-level electoral measures, not individual voters' immigration attitudes. CHES ratings were collected in 2024 and applied retrospectively to 2022 votes.