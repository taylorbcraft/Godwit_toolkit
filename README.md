# Godwit Movement & Environment Explorer

The Godwit Movement & Environment Explorer connects Black-tailed Godwit (*Limosa limosa limosa*) tracking data with satellite-derived environmental layers across the East Atlantic flyway.

Application: <https://tbcraft.shinyapps.io/flyway_tracking_summary_app/>

Satellite Index Explorer: <https://ee-tbcraft.projects.earthengine.app/view/satellite-index-explorer>

## Features

- filter locations by migration year, date and multiple tagging sites
- view daily locations or all tracking fixes across the flyway
- focus the analysis using a country, uploaded boundary or drawn polygon
- inspect individual movement patterns and visits to selected areas
- create GPI, NDVI, EVI, NDWI, NDMI, SAVI and SWIR layers in Google Earth Engine
- sample an uploaded GeoTIFF at filtered daily bird locations
- review overlap maps, selection statistics and encountered-value distributions

## Environment workflow

1. Open the **Environment** tab.
2. Define a satellite, index, period and search location in the embedded Satellite Index Explorer.
3. Draw a rectangle or polygon around the required area. Clipping keeps the image small enough to download and upload.
4. Download the single-band GeoTIFF and upload it to the Shiny application.
5. Review the satellite layer, sampled daily bird locations, summary metrics and value distribution.

The application identifies the index from the GeoTIFF band name or Earth Engine filename. Raster maps use fixed index-specific display ranges, while statistics retain the original sampled values. Uploaded rasters remain temporary session files and are not committed to the repository.

```text
Google Earth Engine ──► clipped single-band GeoTIFF
                                  │
                                  ▼
Movebank telemetry ──► filtered bird-day extraction ──► maps and summaries
```

## Repository structure

```text
Flyway_Tracking_Summary_App/       Active Shiny application and required data
GEE_code/                          Maintained Satellite Index Explorer source
archive/legacy_toolkit/            Retired application and utility source code
.github/workflows/                 Data refresh and Shiny deployment workflow
import_location_data.R             Movebank import and quality control
renv.lock                          Reproducible R package versions
```

Large retired rasters, regional movement extracts, generated geospatial files and local deployment metadata are excluded from the active repository.

## Run locally

Restore the R environment from the repository root, then launch the application:

```r
renv::restore()
shiny::runApp("Flyway_Tracking_Summary_App")
```

The application reads `Flyway_Tracking_Summary_App/allLocations.rds` and `Flyway_Tracking_Summary_App/countries_sf.rds` at startup.

## Automated deployment

The GitHub Actions workflow validates the active R and JavaScript source and deploys the application to shinyapps.io:

- pushes affecting the active app, import script, dependency lockfile or workflow deploy the checked-in movement dataset
- scheduled and manually dispatched runs retrieve current Movebank data before deployment
- scheduled refreshes run every Monday at 00:00 UTC

Deployment requires the `MOVEBANK_USER`, `MOVEBANK_PASSWORD`, `SHINYAPPS_ACCOUNT`, `SHINYAPPS_TOKEN` and `SHINYAPPS_SECRET` repository secrets.

## Contact

Taylor B. Craft

<taylor.craft.mail@gmail.com>
