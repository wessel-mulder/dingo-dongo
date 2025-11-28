rm(list=ls())
# LOADING DATA  -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(amt)
library(ctmm)

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


# DINGO WORKING DATASET ---------------------------------------------------
library(dplyr)
dingo_sub <- dingo_data %>%
  #remove irrelevant data
  mutate(
    timestamp = as.POSIXct(
      paste(cst_date,cst_time),
      format = '%d/%m/%Y %I:%M:%S',
      tz = 'Australia/Darwin'
    )
  ) %>%
  transmute(
    ID = dogname,
    timestamp = timestamp,
    longitude = x,
    latitude = y
  )

# grab one individual
#dingo_sub_jumbuck <- dingo_sub %>%
#  filter(dogname == 'jumbuck')

dingos <- as.telemetry(dingo_sub)

plot(dingos)

level = 0.95
xlim = c(0,30 %#% "day")

# wild dingos 
away_names <- unique(dingo_data$dogname[dingo_data$mine_away=='away'])

lapply(away_names,function(dingo){
  subset <- dingos[[dingo]]
  SVF <- variogram(subset)
  par(mfrow = c(1,2))
  plot(SVF, fraction = 1, level = level,
       main = dingo)
  plot(SVF, xlim = xlim, level = level,
       main = dingo)
})

# range for species 
dingo <- 'alistair'
all <- names(dingos)
dingo_MLs<-lapply(all,function(dingo){
  # get individual 
  dingo_telem <- dingos[[dingo]] 
  # best guesstimate
  GUESS1 <- ctmm.guess(dingo_telem, interactive = FALSE)
  #summary(GUESS1)
  #dingo_verbose <- ctmm.select(dingo_telem, GUESS1, verbose = TRUE)
  # model selection fit
  fit_ML <- ctmm.select(dingo_telem, GUESS1, method = 'ML')
  # akde estimation
  dingo_ML <- akde(dingo_telem, fit_ML)
  dingo_ML
})
saveRDS(dingo_MLs,
        'dingoMLS.rds')

areas <- lapply(dingo_MLs, \(x) summary(x)$CI[2])
areas_df <- data.frame(areas = unlist(areas),
                       dogname=names)

dogs_unique <- dingo_data %>%
  select(dogname, sex, mine_away) %>%
  distinct()

df <- left_join(dogs_unique,areas_df,by='dogname')
write_csv(df,'df.csv')

ggplot(data=df,
       aes(x=mine_away,
           y=areas,
           col=sex))+
  geom_boxplot()

shapes <- lapply(dingo_MLs,function(ML){
  shape <- as.sf(ML,level.UD=0.95, level=0.95)
  middle_polygon <- shape[2,]  #selects the second row which is the 95% est middle polygon
  polygon <- st_transform(middle_polygon) # match crs to base data so everything is aligned
  polygon
})

plot(shapes)

plot(st_geometry(shapes[[1]]), col = 'lightgrey')

for(i in 1:length(shapes)) {
  plot(st_geometry(shapes[[i]]), add = TRUE, col = NA, border = 'red')
}
