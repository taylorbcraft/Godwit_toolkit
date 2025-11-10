# Godwit Landscape Toolkit

The Godwit Landscape Toolkit is an open-source suite of interactive applications designed to support spatial and movement ecology, with a focus on migratory shorebirds such as the Black-tailed Godwit (*Limosa limosa limosa*).

It includes:

- R Shiny Viewers for exploring telemetry data in ecological context  
- Google Earth Engine (GEE) Tools for generating and exporting environmental raster layers  

The toolkit enables conservation researchers and practitioners to interact with large-scale telemetry and satellite datasets in an intuitive, browser-based format — without requiring advanced geospatial coding expertise.

---

## R Shiny Applications

Interactive viewers for exploring godwit movement data in relation to environmental variables:

- **Flyway Movement Viewer**  
  https://tbcraft.shinyapps.io/flyway_tracking_summary_app/

- **Friesland Grassland Productivity Viewer**  
  https://tbcraft.shinyapps.io/Friesland_GPI_App/

- **Doñana Wetland Viewer**  
  https://tbcraft.shinyapps.io/Donana_Wetland_Viewer/

- **Senegal Delta Habitat Viewer**  
  https://tbcraft.shinyapps.io/Senegal_Delta_Habitat_Use_App/

---

## Google Earth Engine Applications

Browser-based tools for generating and exporting environmental raster layers:

- **Grassland Productivity Tool**  
  https://ee-tbcraft.projects.earthengine.app/view/grasslandproductionintensity

- **Seasonal Water Mapping Tool**  
  https://ee-tbcraft.projects.earthengine.app/view/floodmapping

- **Land Cover Classification Tool**  
  https://ee-tbcraft.projects.earthengine.app/view/landcoverclassificationapp

### Earth Engine Code Editor Scripts

To use the GEE scripts, sign up for a free GEE acount at:
[Google Earth Engine registration](https://code.earthengine.google.com/)

Access the underlying GEE scripts for customization:

- **Grassland Productivity Tool Script**  
  https://code.earthengine.google.com/a0eb88e3af93be5d39d911b2f4b18bf3

- **Seasonal Water Mapping Tool Script**  
  https://code.earthengine.google.com/e24a603dabcf0c22e4431e72d2522af6

- **Land Cover Classification Tool Script**  
  https://code.earthengine.google.com/a7832e9efc471371b71fed6546b72413

---

## Repository Contents

- `app/` – Source code for R Shiny viewers  
- `GEE_scripts/` – Optional exports of Earth Engine scripts  
- `location_data.R` – Movement data processing scripts  
- `daily_update.yml` – GitHub Actions workflow for weekly updates and deployments  
- `README.md` – Project overview and documentation

---

## Citation and Archiving

An archived version of this toolkit is available on Zenodo:  
https://doi.org/10.5281/zenodo.123456 *(latest version)*

Please cite this DOI in publications referencing the toolkit. Version-specific DOIs are also available for reproducibility.


## Contact

**Taylor B. Craft**  
`taylor.craft.mail@gmail.com`

Please reach out with questions or feedback.
