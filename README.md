# Spatial Social Polarization (SSP) Maps

An interactive Shiny map of the Index of Concentration at the Extremes (ICE) for US counties, ZCTAs and census tracts, 2010 to 2019. It is a companion to the [spatial social polarization database](https://github.com/samjaros-stanford/spatial_social_polarization_database). The app's "Details and Methodology" tab explains the measures.

**Authors:** Lisa Frueh, Sam Jaros, Dr. Hoda Abdel Magid, Dr. Gina Lovasi
**Contact:** Lisa Frueh, <lisa.frueh@drexel.edu>

## How the project is organised

```
app.R               The Shiny app (UI and server)
www/                Images, favicon and ssp-map.js (recolours map polygons in the browser)
CSS/, HTML/         Styles and page text included by app.R
data/               App data (not in git): parquet files built by data-raw/, plus two small xlsx tables
data-raw/           Scripts that build data/ from the original sources
deploy.R            Deploys the app to shinyapps.io
renv.lock           Package versions (restore with renv::restore())
CODE_REVIEW.md      Review notes and progress
```

## Run the app locally

You need R 4.5 and the `data/` folder (see "Rebuild the data" if you do not have it).

1. Open the project in RStudio. renv starts automatically; run `renv::restore()` once to install the packages.
2. Get a free CARTO basemap key at <https://carto.com/basemaps/apikey/> and put it in a project-level `.Renviron` file (this file is not in git):

   ```
   CARTO_API_KEY=your_key_here
   ```

   Restart R afterwards. Without a key the basemap tiles carry a watermark.
3. Run `shiny::runApp()`.

## Rebuild the data

`data/` is not stored in git because it is large and can be rebuilt. The scripts in `data-raw/` do the rebuild, in order:

| Script | What it does |
|---|---|
| `00_get_data.R` | Downloads the ICE tables, Census crosswalks and 2010 boundaries |
| `01_prep_data.R` | Adds state and county names, picks one primary county per ZCTA, simplifies boundaries |
| `02_write_app_data.R` | Writes the parquet files the app reads into `data/` |
| `03_check_data.R` | Checks the result (unique IDs, expected states and years, value ranges) |
| `run_all.R` | Runs all four in order |

From the project root:

```
Rscript data-raw/run_all.R
```

Expect a long run the first time (the Census downloads are large). Reruns skip files that were already downloaded.

**Safety:** `02_write_app_data.R` refuses to replace an existing `data/` unless you set `SSP_OVERWRITE=yes`.

**Settings** (environment variables, all optional):

| Variable | Default | Meaning |
|---|---|---|
| `SSP_RAW_DIR` | `data-raw/cache` | Where downloads and intermediate files go. Not in git, not deployed. Can be deleted after a rebuild. |
| `SSP_OUT_DIR` | `data` | Where the finished app data are written |
| `SSP_OVERWRITE` | (unset) | Set to `yes` to replace existing app data |

To build somewhere else for testing: `SSP_RAW_DIR=/tmp/ssp_raw SSP_OUT_DIR=/tmp/ssp_out Rscript data-raw/run_all.R`

### What the build needs

- R 4.5 and the packages in `renv.lock`. On macOS and Windows CRAN provides binary packages. If a package has to compile from source on macOS and fails with `libintl.h file not found`, install gettext (`brew install gettext`).
- `sf` needs GDAL, GEOS and PROJ. The CRAN binaries on macOS and Windows include them. On Linux install the system libraries first.
- No Node or mapshaper is needed. Boundaries are simplified with the `V8` package, one state at a time.
- Internet access to `census.gov` and `raw.githubusercontent.com`.

### Data sources

| Data | Source |
|---|---|
| ICE values by county, ZCTA and tract, ACS 5-year estimates 2010 to 2019 | [spatial_social_polarization_database](https://github.com/samjaros-stanford/spatial_social_polarization_database), **pinned to commit `d26cf5e`** (`data-raw/config.R`) |
| 2010 county, ZCTA and tract boundaries | US Census Bureau TIGER/Line, through the `tigris` package |
| State and county names | Census `national_county.txt` |
| ZCTA to county relationships (2010) | Census `zcta_county_rel_10.txt` |

`data-raw/source_log.csv` records the hash of every downloaded table. `00_get_data.R` warns if a source file no longer matches it. To update the ICE data, change `ice_commit` in `data-raw/config.R`, rebuild, and review the check report.

### Choices to know about

- **ZCTAs and counties:** a ZCTA can span several counties. Each ZCTA is assigned to the county holding most of its population, so it appears once on the map. The county filter uses that county.
- **Boundaries are 2010 vintage and simplified** to 20% of their detail so maps draw quickly. Simplifying state by state means shared state borders are simplified separately.
- **Some areas have data but no boundary** (for example counties and tracts whose codes changed after 2010). They are reported by `03_check_data.R` and do not appear on the map.
- **Puerto Rico** has county and ZCTA data but no tract data.

## Deploy

`deploy.R` uploads only the files the app needs (`app.R`, `CSS/`, `HTML/`, `www/`, `data/`, `renv.lock`), so `data-raw/` and its cache are never sent. Check the account and app name at the top of the script, make sure `CARTO_API_KEY` is set in your session, then run it from the project root.

## Update a data year

1. Add the year to `data_years` (and `zcta_years` if relevant) in `data-raw/config.R`, and make sure the ICE source has that year.
2. Update the year choices in `app.R` and the expected years in `data-raw/03_check_data.R`.
3. Rebuild, check, and update the availability table (`data/data_avail.xlsx`).
