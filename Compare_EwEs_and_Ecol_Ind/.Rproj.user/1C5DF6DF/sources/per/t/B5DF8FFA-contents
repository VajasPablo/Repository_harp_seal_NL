
NAFO_map_study_area <- function(){

# Check whether ‘map_study_area’ already exists in the environment
if (file.exists(here("DFO_shapefile", "map_study_area.rds"))) {
  # If ‘map_study_area’ exists, load it from the RDS file
  map_study_area <<- readRDS(here("DFO_shapefile", "map_study_area.rds"))

  cat("########## yes, map_study_area exists and is loaded from .rds\n")
} else {
  cat("########## not running script, map_study_area does not exist\n")
  
  library(raster)    # For handling raster data
  library(sf)        # For spatial data manipulation
  library(marmap)    # For downloading bathymetric data (getNOAA.bathy)
  library(ggspatial) # for annotate scale and north arrow
  
  # Step 1: Download bathymetric data for the Newfoundland region
  # NOAA bathymetric data: longitude from -65 to -42, latitude from 41 to 57
  # Resolution is set to 15 for a good balance between detail and performance
  NL <- getNOAA.bathy(lon1 = -65, lon2 = -42, lat1 = 41, lat2 = 57, resolution = 1)
  
  # Step 2: Load the NAFO Divisions shapefile
  # Load shapefile of NAFO divisions (Northwest Atlantic Fisheries Organization zones)
  nafo_divisions <- read_sf(here("DFO_shapefile", "NAFO_Divisions_2021_poly_clipped.shp"))
  
  # Step 3: Convert bathymetric data into an XYZ dataframe
  # Convert bathymetric matrix to XYZ format (longitude, latitude, depth)
  NL_xyz <- as.xyz(NL)
  NL_df <- as.data.frame(NL_xyz)  # Convert to a dataframe
  colnames(NL_df) <- c("lon", "lat", "depth")  # Rename columns for clarity
  
  # Step 4: Create a raster from the XYZ data and set CRS to WGS84
  # Transform the bathymetric data into a raster and define its coordinate reference system (CRS)
  bathymetry_raster <- rasterFromXYZ(NL_df, crs = "+proj=longlat +datum=WGS84")
  
  # Step 5 (Optional): Reduce raster resolution to improve performance
  # Downsample the raster by a factor of 2 for faster processing
  bathymetry_raster <- aggregate(bathymetry_raster, fact = 2)
  
  # Step 6: Ensure NAFO divisions have the same CRS as the raster
  # Transform the NAFO shapefile's CRS to match the bathymetric raster
  nafo_divisions <- st_transform(nafo_divisions, crs = "+proj=longlat +datum=WGS84")
  
  # Step 7: Convert raster into polygons for areas shallower than -1000m
  # Extract areas with depths >= -1000m and convert the raster to polygons
  bathymetry_polygons <- rasterToPolygons(bathymetry_raster, fun = function(x) x >= -1000, dissolve = TRUE)
  
  # Convert polygons into an sf object and set CRS explicitly
  bathymetry_polygons <- st_as_sf(bathymetry_polygons)
  st_crs(bathymetry_polygons) <- "+proj=longlat +datum=WGS84"
  
  # Step 8: Simplify NAFO Divisions by grouping them into zones
  # Group divisions into broader zones (e.g., "2J3K" and "3LNO") and add labels for key zones
  nafo_divisions <- nafo_divisions %>%
    mutate(zone = case_when(
      Division %in% c("2J", "3K") ~ "2J3K",  # Group 2J and 3K into one zone
      Division %in% c("3L", "3O", "3N") ~ "3LNO",  # Group 3L, 3O, and 3N into another zone
      TRUE ~ NA_character_  # Exclude other divisions
    )) %>%
    mutate(label = ifelse(Division %in% c("2J", "3K", "3L", "3O", "3N"), Division, NA))
  
  # Step 9: Merge grouped zones to create external contours
  # Merge geometries of grouped zones (e.g., "2J3K", "3LNO") into single polygons
  zone_contours <- nafo_divisions %>%
    filter(!is.na(zone)) %>%  # Keep only the grouped zones
    group_by(zone) %>%
    summarise(geometry = st_union(geometry))  # Merge geometries
  
  # Step 10: Perform an intersection between NAFO zones and bathymetry polygons
  # Extract portions of NAFO zones that are shallower than -1000m
  zones_upper_1000m <- st_intersection(zone_contours, bathymetry_polygons)
  
  # Step 11: Group by zone and merge geometries
  # Combine geometries of areas within each zone
  zones_upper_1000m_fused <- zones_upper_1000m %>%
    group_by(zone) %>%
    summarise(geometry = st_union(geometry)) %>%
    ungroup()
  
  # Step 12: Clean the "3LNO" zone by keeping only the largest sub-polygon
  # Filter to focus on the "3LNO" zone
  zone_3LNO <- zones_upper_1000m_fused %>%
    filter(zone == "3LNO")
  
  # Decompose "3LNO" into individual sub-polygons
  zone_3LNO_split <- zone_3LNO %>%
    st_cast("POLYGON") %>%  # Separate into individual polygons
    mutate(subpolygon_id = row_number())  # Add a unique ID for each sub-polygon
  
  # Calculate the area of each sub-polygon
  zone_3LNO_split <- zone_3LNO_split %>%
    mutate(area = st_area(geometry))  # Measure area
  
  # Keep only the largest sub-polygon
  largest_polygon <- zone_3LNO_split %>%
    slice_max(order_by = area, n = 1)  # Select the largest sub-polygon
  
  # Replace the original "3LNO" zone with the cleaned version
  zones_upper_1000m_fused_cleaned <- zones_upper_1000m_fused %>%
    filter(zone != "3LNO") %>%  # Remove the original "3LNO"
    bind_rows(largest_polygon)  # Add the cleaned "3LNO" geometry
  
  # Step 13: Assign colors to each zone for plotting
  zones_upper_1000m_fused_cleaned <- zones_upper_1000m_fused_cleaned %>%
    mutate(color = case_when(
      zone == "2J3K" ~ "tomato",  # Red for "2J3K"
      zone == "3LNO" ~ "cyan2",   # Cyan for "3LNO"
      TRUE ~ "gray"  # Gray for other zones (if any)
    ))
  
  # Step 14: Define the fjord exclusion polygon
  fjord_exclusion <- st_sf(
    geometry = st_sfc(
      st_polygon(list(matrix(
        c(
          -61.0, 50.0,          # Bottom-left corner
          -57.75, 50.0,         # Bottom-right corner
          -57.75, 54.40,        # Top-right corner
          -61.0, 54.40,         # Top-left corner
          -61.0, 50.0           # Return to starting point
        ),
        ncol = 2,
        byrow = TRUE
      )))
    ),
    crs = "+proj=longlat +datum=WGS84"
  )
  
  # Step 15: Subtract the fjord exclusion polygon from "2J3K"
  zone_2J3K_without_fjord <- zones_upper_1000m_fused_cleaned %>%
    filter(zone == "2J3K") %>%
    st_difference(fjord_exclusion)
  
  # Step 16: Replace the original "2J3K" with the modified version
  zones_upper_1000m_fused_cleaned <- zones_upper_1000m_fused_cleaned %>%
    filter(zone != "2J3K") %>%
    bind_rows(zone_2J3K_without_fjord)
  
  # Save the processed spatial data for future use
  saveRDS(zones_upper_1000m_fused_cleaned, here("DFO_shapefile", "study_area.rds"))
  st_write(zones_upper_1000m_fused_cleaned, here("DFO_shapefile", "study_area.shp"), delete_dsn = TRUE)
  
  # Step 17: Create the map using ggplot2
  map_study_area <- ggplot() +
    # Layer for NAFO divisions (ungrouped zones)
    geom_sf(data = nafo_divisions %>% filter(is.na(zone)),
            aes(geometry = geometry),
            fill = "#caf0f1", color = "gray45", alpha = 0.6, linewidth = 0.5) +
    # Layer for grouped zones
    geom_sf(data = nafo_divisions %>% filter(!is.na(zone)),
            aes(geometry = geometry),
            fill = "#caf0f1", color = "gray45", alpha = 0.6, linewidth = 0.5) +
    # Layer for bathymetry zones with different colors
    geom_sf(data = zones_upper_1000m_fused_cleaned,
            aes(fill = color, color = color),
            alpha = 0.5, linewidth = 0.7) +
    # Add labels for NAFO divisions
    geom_sf_text(data = nafo_divisions, aes(label = label),
                 color = "black", size = 5, na.rm = TRUE) +
    # Add labels for Newfoundland and Labrador
    geom_text(data = data.frame(label = c("Newfoundland", "Labrador"),
                                x = c(-56, -58),
                                y = c(48.5, 53)),
              aes(x = x, y = y, label = label),
              size = 4, color = "black", fontface = "italic") +
    # General theme and layout
    theme_bw() +
    xlab("Longitude") + ylab("Latitude") +
    theme(
      panel.background = element_rect(fill = "gray99"),
      panel.grid.major = element_line(color = "gray99", linetype = "dashed"),
      axis.text = element_text(size = 10),
      legend.position = "none"
    ) +
    # Add a scale bar and north arrow
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_fancy_orienteering) +
    # Set coordinate limits
    coord_sf(
      xlim = c(-60, NA),  # Longitude limits
      ylim = c(38, 57),   # Latitude limits
      expand = FALSE
    )
  
  # Save the map for future use
  saveRDS(map_study_area, here("DFO_shapefile", "map_study_area.rds"))


detach("package:raster", unload = TRUE)
detach("package:sf", unload = TRUE)
detach("package:marmap", unload = TRUE)
detach("package:ggspatial", unload = TRUE)

}
  
}
  
