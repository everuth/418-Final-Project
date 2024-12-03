# 418 Final Project 
Regression analysis of precipitation and fire data in BC 2023

---


## Introduction
Fire is an increasing problem in British Columbia and In a heavily forested province wildfires are of extreme concern. In the last decade we have seen increasingly severe wildfire seasons  which have damaged communities, destroyed wildlife habitat and had devastating economic impacts (Cohen and Westhaven, 2022; Parisien et al. 2023). Being able to predict weather patterns associated with fire is useful for allocating resources and preparing communities. A more comprehensive understanding of what drives bad fire seasons is especially important as climate change shifts weather patterns changing the parameters of what we can expect from year to year. 

Intuitively it makes sense that areas with higher precipitation will experience fewer fires but is this really the case? Multiple studies both within and outside of BC have found that dryer summers produce more severe fire seasons but less research has been done on what influences the spatial variability of fires within a given year (Holden et al., 2018; Meyn et al.,2013; Vore et al., 2020; Zhao, 2015). Two 2012 papers from Europe and China examined the causes of the spatial distribution of fire and found that fire location was influenced by precipitation but human impacts to the landscape also played a significant role (Liu et al., 2012; Oliveira et al., 2012). There does not appear to be any more recent analysis of the spatial relationship between fire and precipitation nor is there any research on that subject from within British Columbia.

In this tutorial we will walk through the steps needed to examine the spatial relationship between fire density and mean daily precipitation values in the 2023 fire season in British columbia. We will discuss the different statistical tests available to better understand the relationship between the two variables and we will learn how to apply them. I will also share tips and tricks for troubleshooting potential issues that may come up during this analysis. The hypothesis for this analysis is the following:

**The null hypothesis**:Average daily precipitation from May - October in 2023 does not explain fire density in 2023 in British Columbia. 

**The Alternate hypothesis**: Average daily precipitation from May - October in 2023 does explain fire density in 2023 in British columbia.

The fire data used in this analysis comes from the BC open data catalogue and is titled “BC Wildfire Incident Locations - Historical” you can find this and many more amazing free datasets on the BC data catalogue website: https://catalogue.data.gov.bc.ca/datasets 

The precipitation data used in this analysis comes from the Pacific Climate Impacts Consortium open data portal. The data I downloaded from the PCIC website came from the following station networks: BC Hydro, BC Ministry of Environment and Climate Change Strategy, BC Ministry of Forests - Wildfire Service Network, Ministry of Transportation and Infrastructure - Automated stations. You can access the PCIC open data portal here: https://www.pacificclimate.org/data/bc-station-data-disclaimer 



## Data cleaning 
### Precipitation data
The first step to any data analysis is cleaning the data. It is an onerous process but an especially necessary one when working with datasets large enough to represent an entire province. When you initially download the precipitation data from PCIC you will have 618 stations each saved as a CSV file. Getting precipitation averages from that much data will require some automation. I will walk you through the steps below. 
First we will load the needed libraries, set our working directory and create an empty data frame to hold our results
```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
library(sf)
library(lubridate)
library(dplyr)
library(gstat)
library(maps)
library(hms)
#Set working directory
dir <- "~/Documents/418Final"
setwd(dir)

empty_data <- data.frame(Native.ID = character(), AvgPrecip = numeric(),
                         Longitude = numeric(), Latitude = numeric(), stringsAsFactors = FALSE)
csv_file_name <- "BC_Precip.csv"
# Write the empty data frame to a CSV file
write.csv(empty_data, file = csv_file_name, row.names = FALSE)
```
Next we will read in all of the CSV files that we need and start a for loop to process them. The for loop will standardise the names of all the different 
columns referring to daily precipitation, adjust the formting of date and time columns, select only for data collected at midnight, calculate daily average precipitation
from May to October for each station and store all of this data in a new CSV file. The reason we only select the data recorded at midnight is thats when the weather stations
report daily precipitation and there is erroneous data outside of this time. 
```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Loop through each CSV file
for (file in csv_files) {
    # Read the file without header (header = FALSE) to capture all rows
    df <- read.csv(file, skip = 1, header = TRUE)
    colnames(df) <- trimws(colnames(df))
    
    # Standardise Names of Daily Precipitation columns 
    if (any(c("pcpn_amt_pst24hrs", "rnfl_amt_pst24hrs", "ONE_DAY_PRECIPITATION") %in% colnames(df))) {
      # Find the first matching column name and rename it
      target_column <- intersect(c("pcpn_amt_pst24hrs", "rnfl_amt_pst24hrs", "ONE_DAY_PRECIPITATION"), colnames(df))[1]
      df <- df %>% rename(ONE_DAY_PRECIP = all_of(target_column))
      message("Column renamed successfully in file: ", basename(file), " (", target_column, " -> ONE_DAY_PRECIP)")
    } else {
      message("Skipping file: ", file, " (no matching columns for precipitation found).")
      next
    }
    # Overwrite the file with the cleaned version
    write.csv(df, file, row.names = FALSE)

# Process the data
Daily_data <- df
if (!"time" %in% colnames(Daily_data) || !"ONE_DAY_PRECIP" %in% colnames(Daily_data)) {
  message("Skipping file: ", file, " (missing required columns).")
  next
}
#Adjust the date/time column so that it is usable in calculations
Daily_data$time <- lubridate::ymd_hms(Daily_data$time)# Adjust format as needed
class(Daily_data$time)

#Seperate date and time into seperate columns
Daily_data <- Daily_data %>%
  mutate(
    date = as.Date(time),
    time = format(time, "%H:%M:%S") )

Daily_data$time <- hms::as_hms(Daily_data$time)

# Define the target time as a string
target_time <- "00:00:00"  # Change this to the time you want to filter by

# Filter Daily_data based on the time column
Daily_data <- Daily_data %>%
  filter(format(time, "%H:%M:%S") == target_time)

#convert ONE_DAY_PRECIP to numeric
Daily_data$ONE_DAY_PRECIP <- as.numeric(Daily_data$ONE_DAY_PRECIP)

# Replace NAs with 0 in a specific column, e.g., 'value'
Daily_data$ONE_DAY_PRECIP[is.na(Daily_data$ONE_DAY_PRECIP)] <- 0

# Calculate average Precip
monthly_avg_precip <- Daily_data %>%
  group_by(year = year(date), month = month(date)) %>%
  summarize(monthly_avg_precip = mean(ONE_DAY_PRECIP, na.rm = TRUE)) %>%
  ungroup()  # Ungroup for any future modifications

# Display daily average precip for each month
print(monthly_avg_precip)

# Filter for months between May (5) and October (10) and calculate the mean precipitation
average_precip_may_october <- Daily_data %>%
  filter(month(date) >= 5 & month(date) <= 10) %>%
  summarize(AvgPrecip = mean(ONE_DAY_PRECIP, na.rm = TRUE))  # Calculate mean, ignoring NA values

#Assigning the filename to an object
#Extract the filename (with extension)
file_name <- basename(file_name)

#Remove the file extension
file_name_no_ext <- sub("\\.[^.]*$", "", file_name)

# Display the result
print(file_name_no_ext)

#Read the existing CSV file
file_path <- csv_file_name
data <- read.csv(file_path)

#Round the temperature values to two decimals
Roundedprecip <- round(average_precip_may_october$AvgPrecip, 2)

#Convert the weather station ID column to character
data$Native.ID <- as.character(data$Native.ID)

# Append new rows
new_values <- data.frame(Native.ID = file_name_no_ext, 
                         AvgPrecip = Roundedprecip, 
                         stringsAsFactors = FALSE)

data <- bind_rows(data, new_values)
#Save the updated data frame back to a new CSV file
output_file_path <- csv_file_name
write.csv(data, file = output_file_path, row.names = FALSE)
}
```
Next will combine our new CSV file with the station metadata we downloaded earlier to the coordinates for each weather station. 
Because in the for loop we assigned sation ID names to each row we able to combine them smoothly
```
#Merge the climate data for each station with the location data found in the metadata file
metadata <- read.csv("station-metadata-by-history_Bigdata.csv")
climatedata <- read.csv("BC_Precip.csv")

merged_data <- merge(metadata, climatedata, by = "Native.ID")

#Remove the last two columns which are duplicate Latitude and Longitude
merged_data <- merged_data[, -((ncol(merged_data)-1):ncol(merged_data))]

#Change column names for Latitude and Longitude to remove the x
colnames(merged_data)[colnames(merged_data) %in% c("Latitude.x", "Longitude.x")] <- c("Longitude", "Latitude")

merged_data <- na.omit(merged_data)

# Remove specific columns by name
merged_data <- merged_data[, !colnames(merged_data) %in% c("Variables", "Obs.Freq", "Record.End", "Record.Start")]

#There are erroneous Precip values. Filter data to remove these
PrecipDataClean<- merged_data[merged_data$AvgPrecip < 6.00, ]

write.csv(PrecipDataClean, file = "PrecipDataClean.csv", row.names = FALSE)
```
### Fire Data
Cleaning the fire data is much more straight forward. All you need to do is filter for fires that began between May and and October 2023 using the Ignition date column. 
Even if you were looking at a whole year of data it is important to use the ignition date column no the Fire year column as "FIRE_YEAR" represents the fiscal year starting in April.
```
# Read the shapefile
shapefile_path <- "H_FIRE_PNT_point.shp"  # Replace with the path to your shapefile
fire_data <- st_read(shapefile_path)

# Check the structure of the data to identify the date column
str(fire_data)

# Convert IGN_DATE to a proper datetime object (POSIXct) using the correct format
fire_data$IGN_DATE <- as.POSIXct(fire_data$IGN_DATE, format = "%Y%m%d%H%M%S")
# Filter data for May to October 2023
fire_data_2023 <- fire_data[format(fire_data$IGN_DATE, "%Y") == "2023" & 
                    as.numeric(format(fire_data$IGN_DATE, "%m")) >= 5 & 
                    as.numeric(format(fire_data$IGN_DATE, "%m")) <= 10, ]
# Remove rows with NA in the 'IGN_DATE' column
fire_data_2023 <- subset(fire_data_2023, !is.na(IGN_DATE))

# Optionally, save the filtered data to a new shapefile
st_write(fire_data_2023, "fire_data_2023.shp")
```
And thats it! Your data is now clean so you can begin your analysis. 
## Evaluating spatial distribution of fires in 2023
Point pattern analysis is an umbrella term used to describe multiple statistical tests that measure the distribution of points in space (Boots & Getis, 1988). Points can either be clustered, randomly distributed or dispersed. The three tests we will be using to measure the distribution of fires in BC in the summer of 2023 are: Nearest Neighbour distance analysis, Quadrat analysis and Kernel Density Analysis. 

In a contiguous quadrat analysis we place a grid over our study area and measure the frequency of points within each grid cell (Boots & Getis, 1988). From here we can describe the mean number of points in each grid cell and the variation in the number of points per cell. Our Quadrat analysis will produce two important numbers: the variance Mean ratio (VMR) and a chi square value. Large VMR values occur when the variance is very different from the mean indicating that the points do not follow a random distribution(Boots & Getis, 1988). Chi square values are used to calculate p values if p is less than 0.05 the data distribution is significantly different than random. 

Two dimensional nearest neighbor analysis measures the distance between one point and the next nearest point. Average nearest neighbor distance within a study area is calculated by measuring the distance between each point and its nearest neighbor, adding all of these values together and then dividing the final value by the number of points within the study area (Boots & Getis, 1988). The resulting value can then be compared to the average values for spatially random, dispersed and clustered distributions (Boots & Getis, 1988).

The K-function also known as Ripleys K-function is a second order analysis of point patterns (Haase, 1995). Like other point pattern analysis methods it helps determine what distribution a point pattern follows but unlike other methods it can be used to visualize whether the pattern of point distribution changes at different scales (Haase, 1995). For example at the scale of a watershed vehicle fires may be randomly distributed but if we are to zoom out to the scale of the province we might find clustering. 
Inorder to conduct any point pattern analysis we must first create a ppp object which is required by spatstat to conduct analysis. 
```
shapefile_path <- "fire_data_2023.shp"  # Replace with the path to your shapefile
fire_data <- st_read(shapefile_path)

fire_data <- st_difference(fire_data)

fire_data_ext <- as.matrix(st_bbox(fire_data))
window1 <- as.owin(list(xrange = c(fire_data_ext[1], fire_data_ext[3]), 
                        yrange = c(fire_data_ext[2], fire_data_ext[4])))

coords <- st_coordinates(fire_data)
# Create the ppp object
Fire.ppp <- ppp(x = coords[, 1], y = coords[, 2], window = window1)
```
Now you can use the ppp to conduct a Nearest Neighbour Analysis
```
nearestNeighbour <- nndist(Fire.ppp)
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))
colnames(nearestNeighbour) = "Distance"
nnd = sum(nearestNeighbour$Distance)/nrow(nearestNeighbour)
bc_boundary <- st_read("BC_Boundary.shp")
studyArea <- area(bc_boundary)
pointDensity <- nrow(nearestNeighbour) / studyArea
r.nnd = 1 / (2 * sqrt(pointDensity))
d.nnd = 1.07453 / sqrt(pointDensity)
R = nnd / r.nnd

#Calculate the standard deviation
SE.NND <- .26136 / sqrt(nrow(nearestNeighbour) * pointDensity)

#Calculate the Z score
z = (nnd-r.nnd)/SE.NND

#Create a dataframe of the results
nndResults <- data.frame(StudyArea = round(studyArea, 2),
                         NNDd = round(d.nnd, 2), 
                         NNDr = round(r.nnd, 2), 
                         NND = round(nnd, 2), 
                         Zscore = round(z, 2), 
                         Ratio = round(R, 2))

#Crate a table of the results using the kable function.
kable(nndResults, caption = "NND results for 'Fire locations May-October 2023")
```
The code above creates this table. We can see that the NND score is below the NNDr score meaning that on average 
the fires are closer together than we would expect for a random distribution. This is supported by the Z score of -26.39 which indicates a strongly clustered distribution.
**Table 1**: NND results for Fire locations May-October 2023

|    StudyArea|     NNDd|    NNDr|     NND| Zscore| Ratio|
|------------:|--------:|-------:|-------:|------:|-----:|
| 944918258759| 22345.49| 10397.8| 7328.99| -26.39|   0.7|

Next We will conduct a quadrat analysis. Because we are working with such a large area we will set quads to 20 which will give us 400 total quadrats. 
```
quads <- 20
qcount <- quadratcount(Fire.ppp, nx = quads, ny = quads)
qcount.df <- as.data.frame(qcount)
qcount.df <- plyr::count(qcount.df,'Freq')
colnames(qcount.df) <- c("x","f")
sum.f.x2 <- sum(qcount.df$f * (qcount.df$x^2))
M <- sum(qcount.df$f)
N <- sum(qcount.df$x * qcount.df$f)
sum.fx.2 <- (sum(qcount.df$x * qcount.df$f)) ^ 2
VAR <- ((sum.f.x2) - (sum.fx.2 / M)) / (M - 1)
MEAN <- (N/M)
VMR <- (VAR/MEAN)

##Finally, perform the test statistic to test for the existence of a random spatial pattern.
chi.square = VMR * (M - 1)
p = 1 - pchisq(chi.square, (M - 1))

quadResults <- data.frame(Quadrats = quads * quads, 
                          Variance = round(VAR, 2), 
                          Mean = round(MEAN, 2), 
                          VMR = round(VMR, 2), 
                          Chisquare = round(chi.square, 2))

#Print a table of the results.
kable(quadResults, caption = "Quadrat analysis results for Fire in British Columbia in 2023 May -October.")
```
This creates the table below. The relatively high VMR indicates that points do not likely follow a random spatial distribution 
and the chi square value of 5918.43 can be used to calculate a p-value < .00001. The result is significant at p < .05. this tells 
us that the fire distribution is significatly different than random. 
**Table 2**: Quadrat analysis results for Fire in British Columbia in 2023 May -October.

| Quadrats| Variance| Mean|   VMR| Chisquare|
|--------:|--------:|----:|-----:|---------:|
|      400|    81.03| 5.46| 14.83|   5918.43|

Finally we calculate our K function and create a map that allows us to visualize how the distribution of fires changes a different scales. 
```
k.fun <- Kest(Fire.ppp, correction = "Ripley")
#use simulation to test the point pattern against CSR
k.fun.e <- envelope(Fire.ppp, Kest, nsim = 99, correction = "Ripley", verbose = FALSE)
plot(k.fun.e, main = "")
# Save the plot to a file
png("k_function_envelope_plot.png", width = 800, height = 600)  # Specify file name and dimensions
plot(k.fun.e, main = "")  # Your plot code
dev.off()  # Close the graphical device
```
This creates this map. We can see that the observed K value stays well out of the gray range of randomness and becomes increasingly clustered at larger distances.
![k_function_envelope_plot](https://github.com/user-attachments/assets/19a98f0c-6c7f-45cc-9fad-c85fab2c4360)

## Creating a Predictive surface for precipitation
Predictive surfaces are created through the mathematical interpolation of existing data points. Rainfall is notoriously challenging to interpolate due to its high degree of spatial variability (Dirks et al., 1998). The right approach to modeling it depends on the trends in the data and the density of weather station coverage. We will examine two methods of spatial interpolation, compare their results and determine which method we should move forward with. 

Inverse distance weighting (IDW) is a simple method of spatial interpolation whereby unknown precipitation values are predicted based on nearby values. Each cell has a weight that decreases with distance so cells are more influenced by other nearby cells than those further away. It is important to note that IDW assumes a constant distance decay, this is no the case for kriging (Choi & Chong 2022). Kriging is more computationally involved and takes into consideration the overall spatial arrangement of data (Choi & Chong 2022). Kriging is often considered superior because of its ability to work with non linear data (Choi & Chong 2022). However some studies have found that IDW works just as well given the right conditions (Dirks et al., 1998).

First we will create an interpolated surface using IDW

```
library(sf)       # For handling shapefiles
library(gstat)    # For geostatistical methods
library(ggplot2)  # For plotting
library(viridis)  # For color scales
PrecipData <- st_read("PrecipData.shp")
PrecipData <- st_transform(PrecipData, coords = c("Longitude", "Latitude"), crs = 3005)
BC_Boundary_polygon <- st_read("BC_Boundary.shp")
                               
# Create a grid for the interpolation
bbox <- st_bbox(BC_Boundary_polygon)
grid <- st_make_grid(st_as_sfc(bbox), cellsize = c(50000, 50000))  # Adjust the cell size

# Interpolate using IDW
idw_result <- gstat::idw(AvgPrcp ~ 1, 
                         locations = PrecipData, 
                         newdata = st_as_sf(grid), 
                         idp = 2)

# Convert idw_result to an sf object
idw_sf <- st_as_sf(idw_result)

# Extract coordinates 
idw_sf <- st_as_sf(idw_result)

# Save the result to a shapefile if needed
st_write(idw_sf, "IDW_Result.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)

# Check the CRS of both objects
crs_idw <- st_crs(idw_sf)  # CRS of IDW result
crs_polygon <- st_crs(BC_Boundary_polygon)  # CRS of the polygon shapefile

print(crs_idw)
print(crs_polygon)

# Now attempt the intersection
idw_clipped <- st_intersection(idw_sf, BC_Boundary_polygon)

print(st_geometry(idw_clipped))  # Check geometry to ensure it's clipped correctly

# Step 3: Create the map of the clipped results
ggplot(data = idw_clipped) +
  geom_sf(aes(fill = var1.pred), color = NA) +  # Fill based on predicted temperature values
  scale_fill_viridis(option = "mako", direction= -1) +  # Use viridis color scale for better readability
  labs(title = "IDW Interpolation of Precipitation (May - October)",
       fill = "Avg Precip (mm/day)",  # Change label as appropriate
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")

# Step 4: Save the map as an image file (optional)
ggsave("Clipped_IDW_Interpolation_Map.png", width = 10, height = 8, dpi = 300)
```
This should create the map we see below. As you can see IDW did a good job representing the 
spatial variability of precipitation in the province but it is surprising that predited 
rainfall is so small in the North west of the and on the central coast. These areas are typically famous 
for being very wet. While it is posible they experienced and unusually dry summer in 2023 it seems 
more likely that the sparse coverage of weather stations in that region was unable to capture sufficient data.
![Clipped_IDW_Interpolation_Map3](https://github.com/user-attachments/assets/396e0ec6-e971-4bf8-a821-a524dd1330d3)

Next we will attempt a an interpolation using the kriging method. The first step is to create a semivariogram that 
can be used to inform our results. Kriging has 3 different variogram models spherical, gaussian, and exponential. In this example I use the 
spherical model. It is possible to specify values for the nugget psill and range before hand but I found the model struggled to preform unless it created its own values.
```
library(raster)
library(sp)
library(tmap)
library(st)
library(sda)
library(entropy)
library(corpcor)
library(fdrtool)
library(sf)
library(gstat)
#Set working directory
dir <- "~/Documents/418Final"
setwd(dir)
PrecipData <- st_read("PrecipData.shp")
f.0 <- as.formula(AvgPrcp ~ 1) 
coords <- st_coordinates(PrecipData)  # Extract coordinates as matrix (lon, lat)
# Calculate pairwise distances using spDists
distances <- spDists(coords, longlat = TRUE)
hist(distances, breaks = 30, main = "Histogram of Pairwise Distances")

# Create variogram. Be sure to test out the three different models.
var.smpl <- variogram(f.0, PrecipData, cloud = FALSE,) 
dat.fit  <- fit.variogram(var.smpl,fit.ranges = TRUE, fit.sills = TRUE,
                          vgm(model="Sph"))
plot(var.smpl, dat.fit)
# Check results
print(dat.fit)
```
This produces a semivariogram that you can see below. The value for psill is 1.377695 and the range is 32717.65. 
![variogram_plot](https://github.com/user-attachments/assets/33baeae6-3d43-41ed-85b4-24855721c699)
```
bbox_pre_data <- st_bbox(PrecipData)
bbox_pre_data
# Get bounding box values
xmin <- bbox_pre_data["xmin"]
xmax <- bbox_pre_data["xmax"]
ymin <- bbox_pre_data["ymin"]
ymax <- bbox_pre_data["ymax"]
# Create a regular grid within this bounding box
n <- 1000  # Number of points for the grid (adjust as needed)
grd <- st_as_sf(expand.grid(x = seq(xmin, xmax, length.out = sqrt(n)),
                            y = seq(ymin, ymax, length.out = sqrt(n))),
                coords = c("x", "y"), crs = st_crs(PrecipData))
# Define the grid
xmin <- st_bbox(PrecipData)$xmin
xmax <- st_bbox(PrecipData)$xmax
ymin <- st_bbox(PrecipData)$ymin
ymax <- st_bbox(PrecipData)$ymax
# Create a regular grid
n <- 1000  # Number of points
grd <- st_as_sf(expand.grid(x = seq(xmin, xmax, length.out = sqrt(n)),
                            y = seq(ymin, ymax, length.out = sqrt(n))),
                coords = c("x", "y"), crs = st_crs(PrecipData))
dat.krg <- krige(f.0, PrecipData, grd, dat.fit, debug.level=0)

kriging_results_sf <- st_as_sf(dat.krg)
coords_df <- as.data.frame(st_coordinates(kriging_results_sf))
coords_df$PrecipData <- kriging_results_sf$var1.pred  # Replace with your prediction column

predicted_raster <- rasterFromXYZ(coords_df)

crs(predicted_raster) <- CRS("EPSG:3005")
# Step 1: Load the polygon shapefile for clipping
BC_Boundary_polygon <- st_read("~/Documents/418Final/Lab3data/BC_Boundary.shp")  # Ensure the path is correct
map <- tm_shape(predicted_raster) +
  tm_raster(palette = "viridis", title = "Precipitation") +
  tm_shape(BC_Boundary_polygon) +
  tm_borders(col = "black", lwd = 2) +  # Overlay the boundary as black lines
  tm_layout(
    title = "Kriging Results",
    legend.position = c("right", "top")  # Adjust legend position
  ) +
  tm_scale_bar()
tmap_save(map, filename = "kriging_results.png", width = 10, height = 8, dpi = 300)
```
The interpolated surface looks like this we see that in comparison to the IDW surface it is much smoother with most of BC predicted to have an average of _mm per day in the summer
of 2023, with a few outlier locations. Knowning that BC contains so much variablility in its summer precipitation the Kriging surface seems unrealisticly smooth. We will continue
to the next steps using the IDW results 
![kriging_results](https://github.com/user-attachments/assets/2197ba51-1c0d-47a7-bc7e-196f38e18fa7)

## Creating a wildfire density surface 
This is a relatively simple process. You work with the fire data from 2023 that you cleaned earlier and use it to create a 
raster where each cell has value representing the number of fires found within it. I set the raster resolution to 50000 meters to achieve a decent 
resolution while keeping the computational time needed relatively low. You can choose to raise or lower this value but make sure that you match 
the resolution to the resoluton of your interpolated surface. 
```
library(sf)
library(dplyr)
library(ggplot2)
library(raster)
#Set working directory
dir <- "~/Documents/418Final"
setwd(dir)
# Load your point data 
C_FIRE_PNT_point <- st_read("fire_data_2023.shp")
abms_prov_polygon <- st_read("BC_Boundary.shp")  # Ensure the path is correct

bbox2 <- st_bbox(abms_prov_polygon)# Ensure bbox2 is valid and formatted correctly

raster_res <- 50000   # This resolution in 50000 meters 
raster_template <- raster(extent(bbox2), res = c(raster_res, raster_res))

density_raster <- raster::rasterize(st_as_sf(C_FIRE_PNT_point), raster_template, fun = "count", field = 1)# Estimate density using kernel density estimate

density_raster[is.na(density_raster)] <- 0

# Convert the raster to a data frame and replace any potential NAs with zeros
density_df <- as.data.frame(density_raster, xy = TRUE)
density_df[is.na(density_df)] <- 0  # Replace NAs in the data frame with zeros

# Step to rename the 'layer' column to 'fires' if applicable
colnames(density_df)[colnames(density_df) == "layer"] <- "fires"

# Convert to a spatial points data frame using sf (if needed later)
density_sf <- st_as_sf(density_df, coords = c("x", "y"), crs = st_crs(abms_prov_polygon))

# Plotting the density map with the polygon boundary
ggplot() +
  geom_raster(data = density_df, aes(x = x, y = y, fill = fires)) +  # Use 'fires' from the data frame
  geom_sf(data = abms_prov_polygon, fill = NA, color = "black") + # Boundary polygon
  scale_fill_viridis_c(option = "plasma") +  # Using a color scale
  theme_minimal() +
  labs(title = "Density Map of Fire Points",
       x = "Longitude",
       y = "Latitude",
       fill = "Density")
# Optional: Save the plot if desired
ggsave("FireDensity_map2023.png", width = 10, height = 8, dpi = 300)

# Convert the raster to a data frame
density_df <- as.data.frame(density_raster, xy = TRUE)

# Rename the 'layer' column to 'fires'
colnames(density_df)[colnames(density_df) == "layer"] <- "fires"

# Replace NA values with zeros
density_df[is.na(density_df$fires), "fires"] <- 0

# Convert to a spatial points data frame using sf
density_sf <- st_as_sf(density_df, coords = c("x", "y"), crs = st_crs(abms_prov_polygon))

# Write to a shapefile
st_write(density_sf, "density_points.shp", delete_dsn = TRUE)
```
The map you created should look like the one seen below. As we might expect the southern interior has the highest density of fires. 
![FireDensity_mapReal2023](https://github.com/user-attachments/assets/0339497f-7987-420b-87de-f2cfb090aa4d)

## Determining if Precipitation explains fire locations

Regression analysis is used to determine if one process can explain another. You have a independent variable and a dependent variable 
and you use regression analysis to measure how much of the variance of the dependent variable can be explained by the independent variable. 
In this project fire is the dependent variable and we are trying to determine how much fire location can be explained by the amount of precipitation
which is the independent variable. The first regression test we will conduct is Ordinary Least Squares Regression (OLS)

Ordinary Least Squares is a form of multiple linear regression analysis. It fits a line using vertical residuals and assumes that independent variables 
were measured perfectly and that any variation from the line is due to the dependent variable (Kilmer & Rodríguez, 2017). It also assumes Normality and 
linearity meaning that the dependent variable must be normally distributed and the relationship between the dependent and independent variables must be 
linear (Kilmer & Rodríguez, 2017). These assumptions must be met to create a reliable model. 

First you need to conduct a spatial join of your fire density and IDW data and create a CSV file that contains the data from both of them. 
```
# Read the shapefile
shapefile_path <- "density_points.shp"
density_sf <- st_read(shapefile_path)
 
# Perform the spatial join
joined_data <- st_join(idw_clipped, density_sf, join = st_intersects)

# Select needed columns
final_data <- joined_data[, c("var1.pred", "fires")]

# Rename column
final_data <- final_data %>%
  rename(precipitation = var1.pred)

# Replace NA values in the fires column with 0
final_data <- final_data %>%
  mutate(fires = ifelse(is.na(fires), 0, fires))

# Save final_data as a shapefile
st_write(final_data, "final_data.shp", delete_dsn = TRUE)

# Convert final_data to a data frame
final_data_df <- st_drop_geometry(final_data)

# Write as CSV
write.csv(final_data_df, "final_data.csv", row.names = FALSE)
```
Now that your data is joined you can preform an Ordinary Least Squares regression analysis
```
library(sp)
library(ggplot2)
#Set working directory
dir <- "~/Documents/418Final"
setwd(dir)
# Read the shapefile
final_data_sf <- st_read("final_data.shp")

colnames(final_data_sf)
head(final_data_sf)
# Fit the OLS regression model on the entire spatial data
ols_model <- lm(fires ~ prcpttn, data = final_data_sf)

# Add residuals to the original spatial data frame
final_data_sf$residuals <- resid(ols_model)

# Inspect the updated spatial object to verify residuals are added
print(head(final_data_sf))

# (Optional) Save the updated shapefile with residuals
st_write(final_data_sf, "final_data_with_residuals.shp", delete_dsn = TRUE)

# Create a map of residuals from the OLS regression
ggplot(data = final_data_sf) +
  geom_sf(aes(fill = residuals)) + # Map the residuals to fill color
  scale_fill_viridis_c(option = "C", name = "Residuals") + # Use a color scale
  theme_minimal() +
  labs(title = "Map of Residuals from OLS Regression",
       fill = "Residuals") +
  theme(legend.position = "right")

# Optional: Save the plot if desired
ggsave("residuals_map.png", width = 10, height = 8, dpi = 300)
```

If you compare this map to the fire desity map you will notice a striking similarity this indicates that our model is likely not very reliable. 
Lets conduct further analysis to better understand these results. 

```
  # Check residuals
  summary(final_data_sf$residuals)
  # Check fires
  summary(final_data_sf$fires)
  # Check fires
  summary(final_data_sf$prcpttn)

# Explicitly check if residuals are different from original fire data
cor(final_data_sf$fires, final_data_sf$residuals)

# Detailed model summary
summary(ols_model)

# Check model diagnostics
par(mfrow=c(2,2))
plot(ols_model)

# Create a scatter plot of residuals
ggplot(final_data_sf, aes(x = prcpttn, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linestyle = "dashed", color = "red") +
  labs(title = "Residuals vs Precipitation",
       x = "Precipitation",
       y = "Residuals")

# Check data distribution
plot(fires ~ prcpttn, data = final_data_sf)
png("scatterplot.png", width = 1200, height = 800, res = 150)
plot(fires ~ prcpttn, data = final_data_sf)
dev.off()

```
When we check the correlation between the values for the residuals and the fire data we get a value of 0.9960845 which indicates a 99% correlation. The fact that our 
regression model is so closely correlated to our fire density data indicates that this is not successful model for this data set likely because the data violated one 
of the primary assumptions which is that the independent variable should be measured perfectly. We know that our interpolated surface was not a perfect measure of 
precitptation in the summer of 2023. When we look at the scatter plot below we can see that while some of the highest desenstieis of fire occur in 
areas with low precipitation many other areas with low precipitation have no fires at all.
![scatterplot](https://github.com/user-attachments/assets/dd2967ca-d66d-4256-a638-85ca01aa1462)
Given what we have just learned about our regression model we know that conducting a geographically weighted regression with the same residuals is not likely
to produce useable results. However for the sake of learning more about regression models and how to create them in R we will continue on. 
Next we will conduct a geographically weighted regression to see whether the correlation is stronger in some areas.
```
install.packages("spgwr")
library(sf)
library(sp)
library(spdep)
library(spgwr)
library(ggplot2)

#Set working directory
dir <- "~/Documents/418Final"
setwd(dir)
# Read the shapefile (with residuals included)
final_data_sf <- st_read("final_data_with_residuals.shp")

# Preview the data to check variable names and content
print(head(final_data_sf))
print(colnames(final_data_sf))

# Convert the sf object to Spatial object
final_data_sp <- as_Spatial(final_data_sf)

# Create neighborhood structure
neighbors <- poly2nb(final_data_sp, queen = TRUE)

# Check neighbors for any issues
print(summary(neighbors))

# Check for any empty neighbors
if (any(sapply(neighbors, length) == 0)) {
  warning("Some polygons have no neighbors. This may cause issues for GWR.")
}

# Prepare the dependent and independent variables
dependent_var <- final_data_sp@data$fires
independent_vars <- final_data_sp@data$prcpttn

# Check if both variables are numeric
if (!is.numeric(dependent_var) || !is.numeric(independent_vars)) {
  stop("Dependent and independent variables must be numeric.")
}

# Run GWR with a fixed bandwidth of 200 km
fixed_bandwidth <- 200000  # Bandwidth in meters (200 km)

gwr_model_fixed <- gwr(dependent_var ~ independent_vars, 
                       data = final_data_sp, 
                       bandwidth = fixed_bandwidth, 
                       se.fit = TRUE,
                       hatmatrix = TRUE)  # Add this parameter

# Validate that the model ran successfully
if (is.null(gwr_model_fixed)) {
  stop("The GWR model did not return any results.")
}

if (is.null(gwr_model_fixed$SDF)) {
  stop("The GWR model SDF is NULL, indicating it might not have calculated properly.")
}

# Print GWR summary
print(summary(gwr_model_fixed))

# Extract coefficients and create a dataframe for visualization
gwr_results_fixed <- as.data.frame(gwr_model_fixed$SDF)

# Extract coordinates from the original spatial data
coordinates_fixed <- st_coordinates(st_centroid(final_data_sf))

# Combine results
gwr_results_fixed <- cbind(gwr_results_fixed, 
                           X = coordinates_fixed[, 1], 
                           Y = coordinates_fixed[, 2])

# Create sf object for visualization
gwr_output_sf_fixed <- st_as_sf(gwr_results_fixed, 
                                coords = c("X", "Y"), 
                                crs = st_crs(final_data_sf))

ggplot(data = gwr_output_sf_fixed) +
  geom_sf(aes(colour = localR2)) +
  scale_color_viridis_c(option = "plasma") +
  labs(title = "GWR Coefficients with Fixed Bandwidth of 200 km",
       colour = "localR2") +
  theme_minimal()

# Optional: Save the plot
ggsave("gwr_coefficients_fixed_bandwidth.png", width = 10, height = 8, dpi = 300)
```
![gwr_coefficients_fixed_bandwidth](https://github.com/user-attachments/assets/3bd9653f-aa6e-4627-9f80-b70cdbf38ab5)
The map we have created here migh look nice but given how many assumptions we know we have violated we know that its results are ultimately meaningless. 
Under different circumstances a map like this would indicate that the southern interior and coast are well represented by the regression model because 
these areas have values at and close to 0. 

## Disscussion  
Ultimately our results fail to disprove the null hypothesis that Average daily precipitation from May - October in 2023 
does not explain fire density in 2023 in British Columbia. The scatterplot of fire density and precipitation is prehaps the most powerful piece of 
evidence in support of the Null hypothesis as it clearly demonstrates the lack of relationship between the two variables. It is likely that some of this lack of relationship
is due to insufficient coverage of weather stations leading to an unreliable interpolated surface especially in north western B.C.. 

## Conclusion 
Through the course of this tutorial we cleaned processed and analysed fire and precipitation data using techniques like point pattern analysis, 
interpolation and regression analysis.

