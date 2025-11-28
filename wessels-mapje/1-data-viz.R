
# LOADING DATA  -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(amt)

## dingo
dingo_data <- read_csv("data/tanami_collars.csv") %>% filter(x > 0)
dingo_data_sf <- st_as_sf(dingo_data, coords = c("x", "y"), crs = 4326)

# shapefiles
artificial_food <- st_read("spatial_layers/artificial_food/Art_Food_W84.shp") 
artificial_water <- st_read("spatial_layers/artificial_water/ArtWater_W84v2.shp")
roads <- st_read("spatial_layers/roads/RM_rds_trks_clip.shp") %>% 
  st_transform(crs = st_crs(artificial_food))

### plotting
ggplot() +
  geom_sf(data = roads, color = "black") +
  geom_sf(data = dingo_data_sf, aes(color = as.factor(dog_id)), size = 0.25, alpha = 0.25) +
  geom_sf(data = artificial_water, color = "blue") +
  geom_sf(data = artificial_food, color = "red") +
  theme_bw()


# DINGO DATA --------------------------------------------------------------
# Function to create rectangle from coordinate bounds
create_rectangle_shapefile <- function(df, x_col = "x", y_col = "y", 
                                       buffer = 0.05, crs = 4326, 
                                       output_path = "rectangle") {
  
  # Calculate bounds with buffer
  min_x <- min(df[[x_col]], na.rm = TRUE) - buffer
  max_x <- max(df[[x_col]], na.rm = TRUE) + buffer
  min_y <- min(df[[y_col]], na.rm = TRUE) - buffer
  max_y <- max(df[[y_col]], na.rm = TRUE) + buffer
  
  # Create rectangle coordinates (clockwise from bottom-left)
  rectangle_coords <- matrix(c(
    min_x, min_y,  # bottom-left
    max_x, min_y,  # bottom-right
    max_x, max_y,  # top-right
    min_x, max_y,  # top-left
    min_x, min_y   # close the polygon
  ), ncol = 2, byrow = TRUE)
  
  # Create polygon geometry
  rectangle_poly <- st_polygon(list(rectangle_coords))
  
  # Create sf object
  rectangle_sf <- st_sf(
    id = 1,
    area = st_area(rectangle_poly),
    geometry = st_sfc(rectangle_poly, crs = crs)
  )
  
  rectangle_projected <- st_transform(rectangle_sf, crs = 4326) 
  
  # Export as shapefile
  st_write(rectangle_projected, paste0(output_path, ".shp"), 
           delete_dsn = TRUE, quiet = TRUE)
  
  # Also export as GeoJSON (alternative format)
  st_write(rectangle_projected, paste0(output_path, ".geojson"), 
           delete_dsn = TRUE, quiet = TRUE)
  
  # Print summary
  cat("Rectangle created with bounds:\n")
  cat("X range:", min_x, "to", max_x, "\n")
  cat("Y range:", min_y, "to", max_y, "\n")
  cat("Files saved:", paste0(output_path, ".shp"), "and", paste0(output_path, ".geojson"), "\n")
  
  return(rectangle_projected)
}

# Create the rectangle shapefile
dingo_extent <- create_rectangle_shapefile(
  df = dingo_data,
  x_col = "x",             # column name for easting/longitude
  y_col = "y",             # column name for northing/latitude
  buffer = 0.1,           # buffer distance in coordinate units (degrees or m)
  crs = 4326,              # coordinate reference system of GPS locations (will output in 4326)
  output_path = "spatial_layers/extent/dingo_extent" # without extension
)

# View the result
print(dingo_extent)

# Create a sf object
dingo_extent_sf <- st_geometry(dingo_extent)

# Optional: Add your original points to the plot
plot(dingo_extent_sf, main = "Rectangle with Original Points")
plot(st_geometry(dingo_data_sf), add = TRUE, col = "red")


# ENVIRONMENTAL LAYERS ----------------------------------------------------
# read in the DEM
dem <- terra::rast("spatial_layers/topography/Hydro_Enforced_1_Second_DEM.tif") # hydro-enforced DEM
dem
plot(dem)

# slope layer
# # create slope layer
# slope <- terra::terrain(dem, v = "slope", unit = "degrees", neighbors = 8)
# plot(slope)
# # save the layer
# terra::writeRaster(slope, "spatial_layers/topography/slope.tif", overwrite = T)

# load slope
slope <- terra::rast("spatial_layers/topography/slope.tif")
plot(slope)

# clip artificial food layer
artificial_food_clipped <- st_intersection(artificial_food, dingo_extent_sf)
# clip artificial water layer
artificial_water_clipped <- st_intersection(artificial_water, dingo_extent_sf)
# clip the roads layer
roads_clipped <- st_intersection(roads, dingo_extent_sf)

ggplot() +
  geom_sf(data = roads_clipped, color = "black") +
  geom_sf(data = dingo_data_sf, aes(color = as.factor(dog_id)), size = 0.5, alpha = 0.25) +
  geom_sf(data = artificial_water_clipped, color = "blue") +
  geom_sf(data = artificial_food_clipped, color = "red") +
  scale_colour_viridis_d("Dingo ID") +
  theme_bw() +
  theme(legend.position = "none")

### DISTANCE LAYERS
# # create distance to artificial food layer
# artificial_food_raster <- terra::rasterize(vect(artificial_food_clipped), dem, field = 1, background = NA)
# dist_artificial_food <- terra::distance(artificial_food_raster)
# plot(dist_artificial_food, main = "Distance to Artificial Food")
# terra::writeRaster(dist_artificial_food, "spatial_layers/artificial_food/dist_artificial_food.tif", overwrite = T)
# 
# # create distance to artificial water layer
# artificial_water_raster <- terra::rasterize(vect(artificial_water_clipped), dem, field = 1, background = NA)
# dist_artificial_water <- terra::distance(artificial_water_raster)
# plot(dist_artificial_water, main = "Distance to Artificial Water")
# terra::writeRaster(dist_artificial_water, "spatial_layers/artificial_water/dist_artificial_water.tif", overwrite = T)
# 
# # create distance to roads layer
# roads_raster <- terra::rasterize(vect(roads_clipped), dem, field = 1, background = NA)
# dist_roads <- terra::distance(roads_raster)
# plot(dist_roads, main = "Distance to Roads")
# terra::writeRaster(dist_roads, "spatial_layers/roads/dist_roads.tif", overwrite = T)


# load distance to artificial food layer
dist_artificial_food <- terra::rast("spatial_layers/artificial_food/dist_artificial_food.tif")
plot(dist_artificial_food, main = "Distance to Artificial Food")


# load distance to artificial water layer
dist_artificial_water <- terra::rast("spatial_layers/artificial_water/dist_artificial_water.tif")
plot(dist_artificial_water, main = "Distance to Artificial Water")

# load distance to roads layer
dist_roads <- terra::rast("spatial_layers/roads/dist_roads.tif")
plot(dist_roads, main = "Distance to Roads")

# load NDVI layer
ndvi <- terra::rast("spatial_layers/ndvi/MODIS_mean_NDVI_2008_2010.tif")
plot(ndvi, main = "Mean NDVI 2008-2010 - MODIS")

ndvi <- terra::project(ndvi, crs(dem))
plot(ndvi, main = "Mean NDVI 2008-2010 - MODIS")
