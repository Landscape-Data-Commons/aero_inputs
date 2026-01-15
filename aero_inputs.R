library(sf)
library(raster)
library(terradactyl)
library(tidyverse)



############################## AERO
folder_location <- "C:/yourfoldername/" # the folder where the aero data will be saved to
lpi_tall <- read.csv("D:/yourfoldername/Tall/lpi_tall.csv") # lpi from trex
gap_tall <- read.csv("D:/yourfoldername/Tall/gap_tall.csv") # gap from trex
height_tall <- read.csv("D:/yourfoldername/Tall/height_tall.csv") #height from trex
header <- read.csv("D:/yourfoldername/header.csv") # header from trex
geoind <- read.csv("D:/yourfoldername/geoIndicators.csv") # indicators from trex


# texture_raster <- raster::stack("C:/Users/Brandi.Wheeler/Box/LDC_Data/AERO_input_script/AERO_nonaim_files/Processing/projects_todo/aero_practice/soil_texture_all.tif")
# t <- texture_raster[[c("sandco_0_cm_p","sandfine_0_cm_p" ,"sandmed_0_cm_p", "sandtotal_0_cm_p","claytotal_0_cm_p")]]

texture_clay <- raster::raster("D:/yourfoldername/soil_grids_sand_clay/soil_grids_sand_clay/clay_M_sl1_100m_reproject.tif")
texture_sand <- raster::raster("D:/yourfoldername/soil_grids_sand_clay/soil_grids_sand_clay/sand_M_sl1_100m_reproject.tif")

texture_raster <- raster::stack(texture_clay, texture_sand)

#texture_raster <- raster::stack("C:/Users/bwheeler.ACN/Documents/LDC_Data/Processing/projects_todo/aero_practice/soil_texture_w_sand_frac.tif")

#texture_raster = setNames(texture_raster[[texture_raster$sandtotal_0_cm_p]], "sand")
#texture_raster = setNames(texture_raster = c("sand", "clay"))
names(texture_raster) = c("clay", "sand")

# Remove NAs from coordinates
header <- header %>% subset(!is.na(Longitude_NAD83) &
                              !is.na(Latitude_NAD83))

# Limit tall tables to only PrimaryKeys found in header
lpi_tall <- subset(lpi_tall, PrimaryKey %in% header$PrimaryKey)
gap_tall <- subset(gap_tall, PrimaryKey %in% header$PrimaryKey)
height_tall <- subset(height_tall, PrimaryKey %in% header$PrimaryKey)

# if (grepl(x = texture_file,
#           pattern = ".csv$")){
#   texture <- read.csv(texture_file) %>% dplyr::select(PrimaryKey, SoilTexture)
#   plots_texture <- texture %>% dplyr::left_join(header) %>%
#     dplyr::left_join(terradactyl::texture_class)
#   
#   
# } else if (grepl(x = texture_file,
#                  pattern = ".rdata$")) {
#   texture_raster <- readRDS(texture_file)
proj <- terra::crs(texture_raster)
plots<-sp::SpatialPointsDataFrame(data=header,
                                  coords=cbind(y=header$Longitude_NAD83,
                                               x=header$Latitude_NAD83))

new_f <- sf::st_as_sf(plots) # convert from sp to sf package format

# tells object new f that it is in same crs as texture file, without changing any value
new_f <- sf::st_set_crs(new_f, 4269) 

new_f <- sf::st_transform(new_f, proj)

# # projects new f from existing CRS
# # coordinate values are changed
# new_f <- st_transform(new_f, proj)
# 
# saves new_f in the old {sp} format
plots <- as(new_f, "Spatial")

#extract soil texture values to plots
plots_texture <- raster::extract( x=texture_raster,y=plots, df=TRUE, sp=TRUE)

# if(all(is.na(plots_texture$sand) & all(is.na(plots_texture$clay)))){
#   stop("No raster values extracted. Plots and raster do not overlap.")
# }
# 
# Remove any plots without sand texture
plots_texture <- subset(plots_texture,!is.na(sand))

# Convert texture to fraction
plots_texture$sand <- plots_texture$sand/100
plots_texture$clay <- plots_texture$clay/100
# plots_texture$sand_fine <- plots_texture$sand_fine/100
# plots_texture$sand_coarse <- plots_texture$sand_coarse/100
# plots_texture$sand_med <- plots_texture$sand_med/100
# 
#AERO requires WGS84
new_f <- sf::st_as_sf(plots_texture) # convert from sp to sf package format

# projects new f from existing CRS 
# 
proj <- "+proj=longlat +datum=WGS84"
plots_texture <- sf::st_transform(new_f, proj)

# plots_texture<-sp::spTransform(plots_texture,
#                                CRSobj=sp::CRS("+proj=longlat +datum=WGS84"))
# 
#Add a SoilTexture field, just as an identifier
plots_texture$SoilTexture <- NA
plots_texture <- as(plots_texture, "Spatial")

plots_texture <- plots_texture@data



# Calculate mean maximum height for each plot
max_height <- terradactyl::mean_height(
  height_tall = height_tall,
  method = "max",
  omit_zero = TRUE,
  by_line = FALSE,
  tall = TRUE
) %>%
  # convert to meters
  dplyr::mutate(max_height = max_height/100)

# Calculate bare soil from LPI data
# bare_soil<-pct_cover_bare_soil(lpi_tall = lpi_tall,
#                                tall = FALSE,
#                                by_line = FALSE)

bare_soil <- geoind %>% dplyr::select(PrimaryKey,BareSoil)

# subset gap_tall to only Canopy gaps
canopy_gap <- subset(gap_tall, RecType == "C")

# Find out which plots have bare soil, gap,  and height data
common_PK <- Reduce(intersect, (list(
  unique(canopy_gap$PrimaryKey),
  unique(plots_texture$PrimaryKey),
  unique(max_height$PrimaryKey),
  unique(bare_soil$PrimaryKey)
)))

# because there may be multiple textures per plot, make a new identifier of common_pk + SoilTexture
plots_texture <- plots_texture %>%
  dplyr::mutate(
    SoilTexture = SoilTexture %>% stringr::str_replace(" ", "_")) %>%
  subset(PrimaryKey %in% common_PK)

# If there is soil texture data, append it to the primary keys.
if(!all(is.na(plots_texture$SoilTexture))){
  plots_texture$PK_texture = paste(PrimaryKey,SoilTexture, sep = "_")
  
  # Otherwise, pass primary key to PK_texture
} else {
  plots_texture$PK_texture <- plots_texture$PrimaryKey
}

# Remove restricted character (/) from keys
plots_texture$PK_texture <- gsub("///", "-", plots_texture$PK_texture)

# Write Gap txt files of the raw gap observations
# Create the gap folder location
gap_location <- paste(folder_location, "gap/", sep = "")
dir.create(gap_location)

# Convert gaps to meters - # do we know they'll always need this conversion?
canopy_gap <- canopy_gap %>% dplyr::mutate(Gap = Gap/100)

# Write files to gap location
lapply(
  plots_texture$PK_texture,
  function(X) write.table(canopy_gap[canopy_gap$PrimaryKey == plots_texture$PrimaryKey[plots_texture$PK_texture == X], "Gap"],
                          file = paste(folder_location, "gap/", X, ".txt", sep = ""),
                          col.names = F, row.names = F, sep = "/t"
  )
)



# Write the ini files out to folder and compile the list of files for the combo .bat files
lapply(
  X = plots_texture$PK_texture,
  function(X) {
    cat(
      file = paste(folder_location, X, ".ini", sep = ""),
      "[INPUT_VALUES]",
      paste("wind_location:",
            plots_texture$Latitude[plots_texture$PK_texture == X] %>% unique(),
            plots_texture$Longitude[plots_texture$PrimaryKey == plots_texture$PrimaryKey[plots_texture$PK_texture == X]]%>% unique(),
            sep = " "
      ),
      paste("soil_sand_fraction: ",
            plots_texture$sand[plots_texture$PK_texture == X] %>% unique(),
            sep = ""),
      paste("soil_clay_fraction: ",
            plots_texture$clay[plots_texture$PK_texture == X] %>% unique(),
            sep = ""),
      paste("veg_cover_fraction: ",
            (100 - bare_soil$BareSoil[bare_soil$PrimaryKey == plots_texture$PrimaryKey[plots_texture$PK_texture == X]]) %>% unique() / 100,
            sep = ""),
      paste("veg_mean_height: ",
            max_height$max_height[max_height$PrimaryKey == plots_texture$PrimaryKey[plots_texture$PK_texture == X]] %>% unique(),
            sep = ""),
      paste("gap_obsv: ", "./gap/", X, ".txt", sep = ""),
      sep = "/n", append = FALSE
      
    )
  }
)
## write combined input data to single file
input_data <- dplyr::left_join(plots_texture, bare_soil) %>%
  dplyr::left_join(max_height) %>%
  dplyr::left_join( canopy_gap, by = "PrimaryKey")

write.csv(input_data, file = paste(folder_location, "input_data.csv", sep = ""))




