---
editor_options: 
  markdown: 
    wrap: sentence
---

# Code Review Report: SSP Maps

**Branch:** `rework/code-review-and-perf`
**Files reviewed:** `app.R`, `build_sf_data.R`, `HTML/*`, `CSS/*`, `renv.lock`, `renv/settings.json`, `.gitignore`, `.Rprofile`, `rsconnect/*.dcf`, the `data/` layout, and `.claude/` (project conventions and reviewer rubric).
**Focus this round:** efficiency and reproducibility.
The earlier findings are kept and merged in below.
**How reviewed:** static read, plus file-size checks on `data/` and one attempted `renv::restore()`.
The app has **not** been profiled, because the restore failed (see R3).
Profile before acting on the speed ranking (`profvis`, `shiny.reactlog`).

Per `.claude/CLAUDE.md`, existing code does not have to follow the new conventions yet.
These are suggestions only, and nothing has been refactored.

------------------------------------------------------------------------

## Progress: efficiency pass 1

Done on this branch, then tested on all 1,500 geography/state/year combinations (no join errors) and in the running app.

| Item | Status | Notes |
|------------------|------------------|------------------------------------|
| E1 `Sys.sleep(1)` | Done | Spinner now hides after the first map draw. |
| E2 map rebuilds | Done (pass 2) | Base map is built once. Polygons are sent **once** per (geography, state, counties) with `layerId = GEOID10`, and the view refits only when that area changes. Year, measure and histogram brush send only colors, popup text and a dim flag (a few hundred KB) to `www/ssp-map.js`, which restyles the polygons already in the browser. Areas outside the brush are **faded, not removed**. Areas with no data for the year (for example ZCTA 2010) are hidden. |
| E3 geometry per year | Done | `geom_data()` depends on state and geography only, and is cached with `bindCache()`. Year changes read only the small attribute files. County choices now come from the geometry, so they no longer reset on a year change. |
| E4 `open_dataset()` | Done, small win | Opened once at startup. Opening was already cheap (0.01 to 0.1 s), so the real cost was elsewhere (see E16). |
| E6 duplicated ZCTAs | Done | One primary county per ZCTA (most population, land area breaks ties). Rows: attributes 399,690 to 298,080, geometry 44,410 to 33,120. PA ZCTAs 2,184 to 1,798. `build_sf_data.R` is updated, and the existing data was migrated. **ZCTA download columns changed:** the Census relationship columns are gone. |
| E16 (new) `read_sf_dataset()` ignores the state filter | Done | With arrow 26 and sfarrow 0.4.1, the state filter is silently ignored, so every geometry read loaded the whole country (3,221 counties, and all tracts and ZCTAs). The old code did this too. Fixed with `read_state_geom()`: filter and collect with arrow, then rebuild sf from WKB. Output matches the old reader (same IDs and areas). |

**Measured, PA, browser seconds from change to finished draw** (old `app.R` from git vs new, same data):

| Step | Old | New |
|------------------------------|--------------|--------------|
| Geography to Tracts | 2.2 | 1.4 |
| Year change | 2.1 | 1.3 |
| Measure change | 1.0 | 1.2 |
| Geography to ZCTA | 2.0 | 1.4 |
| Year change (ZCTA) | 1.9 | 1.4 |

Data layer only (R, seconds): old 0.2 to 0.8 per change, new 0.02 to 0.09 on first load and 0.01 to 0.05 on a year change.

**After pass 2 (recolor in the browser), PA, browser seconds:**

| Step | Old | Pass 1 | Pass 2 |
|------------------------------|--------------|--------------|--------------|
| Geography to Tracts | 2.2 | 1.4 | 1.4 |
| Year change (tracts) | 2.1 | 1.3 | 0.4 |
| Measure change (tracts) | 1.0 | 1.2 | 0.4 |
| Geography to ZCTA | 2.0 | 1.4 | 1.4 |
| Year change (ZCTA) | 1.9 | 1.4 | 0.4 |

CA tracts (8,057 polygons): year change 0.5 s, measure change 0.35 s. Geography or state changes still send the polygons, so they stay around 1.4 s for PA tracts (serializing about 8 MB, plus the histogram).

**What is left:**
- Geography, state and county changes still serialize and send the polygons (PA tracts: about 8 MB, roughly 0.75 s of server time). Rounding coordinates only cut this to 7.2 MB, so more simplification (E5) or vector tiles are the next levers.
- The histogram (`ggplotly`) is only about 0.24 s for PA tracts (E8).

**Side effects to know about:**
- About 170 tract rows (NY 108, AZ 56, CA 8, VA 6, summed over years) have data but no 2010 boundary, because the tract ID changed after 2010. They used to be counted in the histogram with no polygon, and are now dropped by the inner join.
- The histogram brush now **fades** areas outside the selection instead of removing them. The map does not zoom to the brush. It zooms only when state, geography or counties change.
- The polygon layer in the layer control is now labeled "ICE values" (it used to carry the measure name), because the polygons are no longer redrawn when the measure changes.
- Popups are built in R as text (`popup_html()`), not as a formula evaluated by leaflet. The text is the same as before.
- `air` is not installed on RStudio, so the edited files have not been auto-formatted.

## Progress: quick wins (batch A)

Tested in the running app and by fetching real downloads.

| Item | Status | Notes |
|------------------|------------------|------------------------------------|
| CRITICAL #4 invisible errors | Done | The white `.shiny-output-error` CSS is gone. The histogram uses `validate()`, and the help text explains why a selection has no data. No red error elements appear on load or on any combination I tried. |
| CRITICAL #3 shapefile download | Done | Delivered as a zip (`.shp .shx .dbf .prj`). Read back with `sf`: 3,218 PA tracts, WGS84. |
| Correctness #5, #6 downloads | Done | CSV has no row-number column. File names are `County_PA_All_Counties_2019.csv`, with one county named, or `2_Counties` for several. GeoJSON overwrites its temp file cleanly. |
| Correctness #4 download vs brush | Resolved by design | The brush now only fades areas, so the map shows every area in the selection and the download matches it. |
| Correctness #1, #2 | Done | `&&`, numeric year, and a generic "no data" message for any empty combination. |
| Correctness #7, #9 | Done earlier | Join keeps sf class. Geography lookup replaced the `if` chain. |
| Correctness #8 DC and PR | Done, with gaps | Both added to the state list. County and ZCTA work for both. **Tract boundaries are missing for DC** (the tract data has 178 DC values, but `build_sf_data.R` only downloads the 50 states), and **Puerto Rico has no tract data**. Both show the "no data" message. |
| E9 Excel per session | Done | Read once at startup. |
| E13 Font Awesome kit | Done | Removed. `HTML/FontAwesomeLoader.html` also held the favicon link, which now lives in `app.R`. The file is deleted on disk (not committed). |
| E14 unused libraries | Done | `readr` and `stringr` dropped, `ggplot2` loaded explicitly. |
| `includeHTML` warnings | Done | Removed the `<html>/<head>/<body>` wrappers from the three `.Rhtml` files. No warnings at startup. |
| Typos | Done | "Department" (twice) in `details.Rhtml`, and the formula sentence now reads "P_i is the number of people ... in the deprived group". The old `README.md` copy of this text was replaced in batch B. |

## Progress: reproducibility (batch B)

Verified by a full rebuild from scratch into temp folders (about 4.5 minutes, 1.3 GB of raw downloads), then compared with the existing data.

| Item | Status | Notes |
|------------------|------------------|------------------------------------|
| R1 layout | Done | `data-raw/` holds `config.R`, `00_get_data.R`, `01_prep_data.R`, `02_write_app_data.R`, `03_check_data.R`, `run_all.R` and `helper_functions/`. It is `data-raw/` and not `R/` because Shiny runs everything in `R/` at app start. |
| R2 unpinned sources | Done | ICE data pinned to commit `d26cf5e`. `source_log.csv` records a hash for every download, and `00_get_data.R` warns when one changes. |
| R3 R and renv | Done | renv status is consistent on R 4.5.2. |
| R4 system dependencies | Done | Documented in the README. Node and mapshaper are no longer needed (V8, one state at a time). |
| R5 entry point | Done | `Rscript data-raw/run_all.R`. `02_write_app_data.R` refuses to overwrite `data/` unless `SSP_OVERWRITE=yes`. |
| R6 data checks | Done | `03_check_data.R` runs 18 checks (unique IDs, expected states and years, value ranges) and reports unmatched areas. It found the missing DC tracts and no-state rows in the old data. |
| R7 deployment | Partly done | `deploy.R` uploads an explicit file list. **Left for you:** `.DS_Store` and `rsconnect/shinyapps.io/lisa-frueh/ssp_maps.dcf` are still tracked (`git rm --cached`), and the account name needs confirming (header links to `drexel-uhc`). |
| R8 README | Done | Replaced with setup, rebuild, deploy and data-source notes. |
| R9 build loops | Done | `purrr` maps and one `simplify_by_state()` function replace the loops and the three copies of the chain. |
| Old `build_sf_data.R` | Removed | Replaced by `data-raw/`. The last committed version is in git history. |
| DC tracts | Done | Added to the build and patched into the existing `data/`. |

**Rebuild vs existing data:** ICE values are identical (maximum difference 0) for every area and year. The rebuild drops 30 attribute rows with no state (10 county, 20 tract; renamed counties and changed tract codes), which the app never showed, and adds 179 DC tracts. Existing no-state partitions were removed from `data/`, and `03_check_data.R` passes on it.

------

------------------------------------------------------------------------

## CRITICAL

1.  **The app cannot be rebuilt from a fresh clone** (`.gitignore:7`: `data/**`).
    The whole `data/` folder is ignored, and only the two xlsx files are tracked.
    Nothing in the repo says how to regenerate it.
    `build_sf_data.R` is at the repo root, not in `R/`, has no header or run order, and pulls from unpinned sources (R2).
    Fix: see R1 to R5.

2.  **The package environment cannot be restored on a current machine** (`renv.lock`).
    `renv::restore()` on this Mac (R 4.5.2) stopped at `Matrix`: `fatal error: 'libintl.h' file not found`.
    The lockfile pins R 4.3.2-era versions, so packages are compiled from source, and a missing system library breaks the build.
    Fix: see R3 and R4.

3.  **Shapefile download cannot work** (`app.R:434`).
    `st_write()` makes a multi-file set (`.shp`, `.dbf`, `.shx`, `.prj`), but `downloadHandler` returns one temp file.
    Write to a temp dir and return a zip.

4.  **All app errors are invisible** (`app.R:122`).
    The CSS sets `.shiny-output-error` to white text, so any failure looks like a blank panel.
    It also hides the empty-data crashes in Correctness #2.

## HIGH: Efficiency

Ranked by expected payoff.
File sizes come from `du` on `data/`.

| \# | Where | Problem | Suggested fix |
|------------------|------------------|-------------------|------------------|
| E1 | `app.R:159` | `Sys.sleep(1)` runs for every session. The spinner also hides before anything has rendered. | Remove it. Call `waiter_hide()` after the first map render. |
| E2 | `app.R:362-395` | The whole Leaflet map is rebuilt on every input change (measure, county, histogram brush), including the tile requests. | Build the base map once. Update polygons with `leafletProxy()` (`clearShapes()` and `addPolygons()`). Better still, send geometry once per (geography, state) and recolor from a vector of values. |
| E3 | `app.R:164-205` | Geometry is re-read and re-joined every time the **year** changes, but geometry does not depend on year. | `geom_data()` depends only on state and geography, cached with `bindCache()`. `attr_data()` depends on year too, and is cheap. |
| E4 | `app.R:167-197` | `open_dataset()` is called inside each reactive. The non-spatial datasets are about 500 small files each (county 525, ZCTA 468, tract 515), so every call re-lists a big directory tree. | Open each dataset once at startup. Arrow datasets are lazy, so this is cheap. |
| E5 | `data/*_sf_partitioned` | Per-state geometry is large (TX ZCTA 14 MB, TX tract 9.3 MB, CA tract 8.5 MB). PA ZCTA is 6.3 MB while PA tract is 3.4 MB, even though PA has fewer ZCTAs than tracts, which is consistent with the duplicated ZCTA rows in E6. Leaflet turns all of it into JSON for the browser. | Simplify ZCTA and tract more (`keep` of 0.05 to 0.1). Round coordinates at build time (`st_snap_to_grid` or `st_set_precision`). If still slow, move to vector tiles (tippecanoe plus PMTiles). |
| E6 | `build_sf_data.R:41` | The many-to-many ZCTA-to-county join gives **one geometry row per ZCTA-county pair**. That inflates the files, the polygons drawn, and the histogram counts. | Keep one geometry row per ZCTA. Store the county as a primary-county column or a separate lookup. Pick the primary county by largest area or population share. |
| E7 | `app.R:208-224` | `county_choices()` always uses `county_data()`, so county data loads even when the geography is ZCTA or tract. The `renderUI` also re-renders on year change and resets the user's selection. | Make the county list depend on state only. Use a static `selectizeInput` plus `updateSelectizeInput(server = TRUE)`. |
| E8 | `app.R:346` | `ggplotly()` is slow and builds a large widget for a histogram. | Compute the bins once and use `plot_ly(type = "bar")`. Cache with `bindCache()`. |
| E9 | `app.R:459-462` | `read_excel()` runs per session for two tiny tables. | Read once at startup, or convert to CSV or static HTML at build time. |
| E10 | `app.R:389` | `popup = hovertext()` builds an HTML string for every polygon up front. | Use `label =` with a precomputed column, or build only on click. |
| E11 | `app.R` (all inputs) | Five inputs each trigger a full recompute, so quick successive changes queue up work. | Debounce the inputs, or add an "Update map" button for the heavy geographies (ZCTA, tract). |
| E12 | `www/` | `howto.png` is 463 KB and `logo.png` is 188 KB, loaded twice in `Header.html`. The how-to is a screenshot of text. | Resize, convert to WebP, add `loading="lazy"`. Replace the screenshot with real HTML. |
| E13 | `HTML/FontAwesomeLoader.html` | Unused, render-blocking external script (the app only uses Shiny's bundled `icon("download")`). | Delete it. |
| E14 | `app.R:4-14` | `readr` and `stringr` are loaded but not used. `ggplot2` is only loaded through plotly. | Drop the unused ones. Load `ggplot2` explicitly if kept. |
| E15 | `data/` (about 380 MB) | The deploy bundle is mostly geometry, which slows deploys and cold starts. | Do E5 and E6 first, then re-measure. Also check the shinyapps.io instance size and idle settings. |

## HIGH: Reproducibility

| \# | Problem | Suggested fix |
|------------------------------------|------------------------------------|------------------------------------|
| R1 | **Repo layout does not match the convention** in `.claude/CLAUDE.md`. `build_sf_data.R` is at the root, and there is no `R/`, `documentation/` or `results/` folder. Raw and derived data are mixed in `data/`. | `R/00_get_data.R` (download raw to `data/raw/`), `R/01_prep_data.R` (simplify, join), `R/02_write_app_data.R` (write the app datasets). Put helpers in `R/helper_functions/`. Add the standard script header and section fences. |
| R2 | **Unpinned external sources** (`build_sf_data.R:10-16, 76, 91, 105`). The ICE CSVs come from the `main` branch of another GitHub repo. The Census crosswalks and TIGER files come from live URLs. The data can change under you. | Pin the ICE repo to a commit or tag. Save raw downloads to `data/raw/` with the retrieval date and source URL (and a checksum if possible). Set `options(tigris_use_cache = TRUE)`. |
| R3 | **R version drift and a restore that fails.** `renv.lock` says R 4.3.2, this machine has R 4.5.2, and `CLAUDE.md` says 4.5.3. The lockfile pins old versions (for example arrow 12.0.1.1, shiny 1.7.4.1). `renv::restore()` failed on `Matrix` (see CRITICAL #2). | Pick one R version. Update the packages on it (`renv::update()`), fix any build failures, then re-snapshot. Per your "renv last" approach, do this after the code changes settle. Until then, develop with an ordinary library, not the old lockfile. |
| R4 | **Undocumented system dependencies.** The failed restore shows one: gettext headers (`libintl.h`). `ms_simplify(sys = TRUE)` also needs the system `mapshaper` (Node) binary. `sf` needs GDAL, GEOS and PROJ. | List them in the README, with install commands for macOS and Linux. Prefer binary packages (Posit Package Manager) so users compile less. |
| R5 | **No run order or entry point.** Nothing says to run the build script before the app, or how long it takes. | One script, or a `Makefile` or `targets` pipeline, with the order in the README. |
| R6 | **No data checks after the build.** Nothing confirms every year, state and geography the UI offers actually exists. | Add checks at the end of the build: unique GEOIDs, row counts per year, and a (geography, year, state) completeness table. The UI can then be generated from the real data, which also fixes the empty-data crashes in Correctness #2. |
| R7 | **Deployment config is untracked in practice.** `rsconnect/*.dcf` is tracked even though ignored, and `.DS_Store` is tracked. Nothing says how to deploy or where secrets go. | `git rm --cached` both. Document the deploy steps and how `CARTO_API_KEY` reaches the server. |
| R8 | **README** is a copy of the intro text in HTML. It has no setup, data, or deploy steps. | Replace with a real README: purpose, script flow, data sources and licenses, system requirements, how to run, how to deploy, known gaps. Move the intro text to `documentation/`. |
| R9 | **Build script loops** (`build_sf_data.R:47-115`) grow objects with `rbind` and `bind_rows`, use magic indices (`state.name[2:50]`, `data_years[2:10]`), and repeat the same simplify chain three times. | One function for the simplify chain. `map() |> list_rbind()` for the loops. Name the intent (`years_zcta <- 2011:2019`). |

## HIGH: Correctness

| \# | Where | Problem |
|------------------------|------------------------|------------------------|
| 1 | `app.R:254-266` | `&` instead of `&&`. `input$year_input` is a **character**, so `< 2012` compares as strings and only works by accident. Use `as.integer()`. |
| 2 | `app.R:254-266` | When the help text says a combination is unavailable (ZCTA 2010, ICEedu before 2012), the outputs still run on empty data and fail. Use `validate(need(...))`. |
| 3 | `app.R:364-368` | Histogram brush: uses the min and max of selected **bar centers**, so values in the outer half-bins are dropped. It also **persists** when state, geography or measure changes, which can blank the map. Reset it on those inputs and use bin edges. |
| 4 | Download vs. map | The download ignores the histogram brush the map applies, so they can differ. Use one shared `filtered_data()` for both. |
| 5 | `app.R:432` | `write.csv(data, file)` writes a row-number column. Use `row.names = FALSE`. |
| 6 | `app.R:418-420` | File name gives `County_PA_All_Counties_2019_.csv` (stray `_`). Many selected counties make a huge file name. |
| 7 | `app.R:175,188,201` | `left_join(data, data_sf, ...)` on a plain data frame, then `st_as_sf()` (the code comment asks why the class is lost). Put the sf object on the left of the join. Joining on `county.name` too is fragile for punctuation and name changes. |
| 8 | `app.R:30` | `state.abb` leaves out **DC and Puerto Rico**, which are in the county data. |
| 9 | `app.R:226-236` | `data2()` has no fallback branch for an unexpected geography. |

## MEDIUM

-   **Style (new code only, per CLAUDE.md):** native pipe `|>` instead of `%>%`. `join_by()` instead of `by = c(...)`. `.data[[...]]` instead of `get(...)` (`app.R:274-282, 320-340`). `map()` instead of loops. `air` formatting.
-   **Joins:** add `relationship =` and `unmatched =` checks (build script and app) so duplicated or missing keys fail loudly.
-   **Structure:** `app.R` is about 470 lines, with a stray `{ ... }` block at the top. The reviewer rubric asks for files under 400 lines and functions under 50. Split into `global.R`, `R/` helpers, and modules (map, histogram, download).
-   **Lookups:** replace the two `case_when` reactives (`disadvantaged`, `privileged`) and `ice_val` with one lookup table (label, disadvantaged, privileged, first year).
-   **HTML fragments:** `HTML/*.Rhtml` are full `<html><head><body>` documents included mid-page, which is invalid. Use fragments or Markdown. `Header.html` has a hidden duplicate logo and five `<br>`s for spacing.
-   **Typos:** "Departmetn" and "Departmnet" (`HTML/details.Rhtml:23-24`). The formula paragraph is missing "P_i" ("and is the number of people ... P_i deprived group").
-   **CSS:** `.navbar` rules are duplicated in `Home.css` and `NavbarPage.css`, and `.salurbal_hr` in `Header.css` and `NavbarPage.css`. Merge into one file in `www/` so the browser can cache it. Optionally move to `bslib` theming.
-   **Accessibility:** no alt text on the logo and how-to images. Check the contrast of the white mid-gradient on the map.
-   **Tests:** none. Add `testthat` for the data-prep functions and one `shinytest2` smoke test.
-   **Docs:** no function docs. Add short comments on *why* choices were made, per CLAUDE.md.
-   **Dependencies:** check that `sfarrow` is still maintained. Newer `sf` (GDAL 3.5+) may read and write GeoParquet directly.

## LOW

-   Naming: reactive `data` shadows `base::data`.
-   `HTML(paste("..."))` appears many times. Use `HTML("...")` or `tags$h3()`.
-   The header links to `drexel-uhc.shinyapps.io/SSP_Maps`, but the deploy file points at the `lisa-frueh` account. Confirm the canonical URL.

------------------------------------------------------------------------

## Verdict: NEEDS CHANGES

**Blocking:** 4 CRITICAL (not rebuildable from a clone, environment will not restore, broken shapefile download, hidden errors).
**Security:** no hardcoded secrets in tracked files.
The CARTO key is read from the environment, and `.Renviron` is gitignored.

## Suggested order

1.  **Reproducibility first**, so every later change can be tested against a rebuildable dataset and a working environment: R3, R4, R1, R2, R5, R8.
2.  **Quick wins in the app:** E1, E13, the error CSS (CRITICAL #4), the download fixes (CRITICAL #3, Correctness #5 and #6).
3.  **Data size and ZCTA duplication:** E6, E5, R6, then re-measure E15. This changes the data files, so do it once.
4.  **Data layer:** E4, E3, E7, E9, then Correctness #1, #2, #7 and #8.
5.  **Map and histogram:** E2, E8, E10, E11, then Correctness #3 and #4.
6.  **Cleanup:** assets (E12), structure and modules, tests, then re-snapshot renv (R3).

## Questions for you

1.  **Target R version:** 4.5.3 (as in CLAUDE.md), or stay on the lockfile's 4.3.2?
2.  **ZCTA across counties:** is it acceptable to give each ZCTA one primary county for the county filter, or do you need ZCTAs shown under every county they touch?
3.  **Where should the rebuilt data live?** Rebuild locally only (gitignored, documented), or also keep a versioned copy somewhere (for example a release asset or shared drive)?
