###############################################################################
# Download and prepare data for `MT_cd_2010` analysis
# ``COPYRIGHT``
###############################################################################

suppressMessages({
    library(dplyr)
    library(readr)
    library(sf)
    library(redist)
    library(geomander)
    library(cli)
    library(here)
    devtools::load_all() # load utilities
})

# Download necessary files for analysis -----
cli_process_start("Downloading files for {.pkg MT_cd_2010}")

path_data <- download_redistricting_file("MT", "data-raw/MT", year = 2010)

# download the enacted plan.
# TODO try to find a download URL at <https://redistricting.lls.edu/state/``state_name``/>
url <- "https://leg.mt.gov/content/Committees/Interim/2011-2012/Districting/Maps/Adopted-Plan/House_shape_adopted021213.zip"
path_enacted <- "data-raw/MT/MT_enacted.zip"
download(url, here(path_enacted))
unzip(here(path_enacted), exdir = here(dirname(path_enacted), "MT_enacted"))
file.remove(path_enacted)
path_enacted <- "data-raw/MT/MT_enacted/House_shape_adopted021213.shp"

cli_process_done()

# Compile raw data into a final shapefile for analysis -----
shp_path <- "data-out/MT_2010/shp_vtd.rds"
perim_path <- "data-out/MT_2010/perim.rds"

if (!file.exists(here(shp_path))) {
    cli_process_start("Preparing {.strong MT} shapefile")
    # read in redistricting data
    MT_shp <- read_csv(here(path_data)) %>%
        join_vtd_shapefile(year = 2010) %>%
        st_transform(EPSG$MT)  %>%
        rename_with(function(x) gsub("[0-9.]", "", x), starts_with("GEOID"))

    # add municipalities
    d_muni <- make_from_baf("MT", "INCPLACE_CDP", "VTD", year = 2010)  %>%
        mutate(GEOID = paste0(censable::match_fips("2010"), vtd)) %>%
        select(-vtd)
    d_cd <- make_from_baf("MT", "CD", "VTD", year = 2010)  %>%
        transmute(GEOID = paste0(censable::match_fips("MT"), vtd),
                  cd_2000 = as.integer(cd))
    MT_shp <- left_join(MT_shp, d_muni, by = "GEOID") %>%
        left_join(d_cd, by="GEOID") %>%
        mutate(county_muni = if_else(is.na(muni), county, str_c(county, muni))) %>%
        relocate(muni, county_muni, cd_2000, .after = county)

    # add the enacted plan
    cd_shp <- st_read(here(path_enacted))
    MT_shp <- MT_shp %>%
        mutate(cd_2010 = as.integer(cd_shp$DISTRICT)[
            geo_match(MT_shp, cd_shp, method = "area")],
            .after = cd_2000)

    # Create perimeters in case shapes are simplified
    redistmetrics::prep_perims(shp = MT_shp,
                               perim_path = here(perim_path)) %>%
        invisible()

    # simplifies geometry for faster processing, plotting, and smaller shapefiles
    # TODO feel free to delete if this dependency isn't available
    if (requireNamespace("rmapshaper", quietly = TRUE)) {
        MT_shp <- rmapshaper::ms_simplify(MT_shp, keep = 0.05,
                                          keep_shapes = TRUE) %>%
            suppressWarnings()
    }

    # create adjacency graph
    MT_shp$adj <- redist.adjacency(MT_shp)

    MT_shp <- MT_shp %>%
        fix_geo_assignment(muni)

    write_rds(MT_shp, here(shp_path), compress = "gz")
    cli_process_done()
} else {
    MT_shp <- read_rds(here(shp_path))
    cli_alert_success("Loaded {.strong MT} shapefile")
}
