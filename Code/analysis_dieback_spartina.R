###Load libraries
library(raster)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(glmulti)
library(randomForest)
library(colorRamps)
library(sf)
library(rgdal)
library(Rmisc)
library(emmeans)
library(multcomp)

#set seed
set.seed(1989)

#Set wd
setwd(wd)

NDVI_raster_2009 <- raster(paste0(wd, "Aerial_Imagery/NAIP/2009_04/Merged_2009_NAIP_WAshley_NDVI.tif"))
Blue_raster_2009 <- raster(paste0(wd, "Aerial_Imagery/NAIP/2009_04/Merged_2009_NAIP_WAshley_Blue.tif"))
Green_raster_2009 <- raster(paste0(wd, "Aerial_Imagery/NAIP/2009_04/Merged_2009_NAIP_WAshley_Green.tif"))
Red_raster_2009 <- raster(paste0(wd, "Aerial_Imagery/NAIP/2009_04/Merged_2009_NAIP_WAshley_Red.tif"))
NIR_raster_2009 <- raster(paste0(wd, "Aerial_Imagery/NAIP/2009_04/Merged_2009_NAIP_WAshley_NIR.tif"))

dieback_points_2009 <- read_sf('Aerial_Imagery/NAIP/Dieback_points/Dieback_points_2009.shp')
healthy_points_2009 <- read_sf('Aerial_Imagery/NAIP/Dieback_points/Healthy_points_2009.shp')

dieback_2009_vector <- as.data.frame(extract(x = NDVI_raster_2009, y = dieback_points_2009))
names(dieback_2009_vector)[1] <- "NDVI"
dieback_2009_vector$Condition <- "Dieback"
healthy_2009_vector <- data.frame(extract(x = NDVI_raster_2009, y = healthy_points_2009))
names(healthy_2009_vector)[1] <- "NDVI"
healthy_2009_vector$Condition <- "Healthy"

dieback_2009_vector$Red <- (extract(x = Red_raster_2009, y = dieback_points_2009))
healthy_2009_vector$Red <- (extract(x = Red_raster_2009, y = healthy_points_2009))

dieback_2009_vector$Green <- (extract(x = Green_raster_2009, y = dieback_points_2009))
healthy_2009_vector$Green <- (extract(x = Green_raster_2009, y = healthy_points_2009))

dieback_2009_vector$Blue <- (extract(x = Blue_raster_2009, y = dieback_points_2009))
healthy_2009_vector$Blue <- (extract(x = Blue_raster_2009, y = healthy_points_2009))

dieback_2009_vector$NIR <- (extract(x = NIR_raster_2009, y = dieback_points_2009))
healthy_2009_vector$NIR <- (extract(x = NIR_raster_2009, y = healthy_points_2009))

df_dieback_2009 <- rbind(dieback_2009_vector, healthy_2009_vector)

df_dieback_2009$Condition <- as.factor(df_dieback_2009$Condition) 

model <- randomForest(Condition ~ NDVI +  Blue + Green +
                        Red + NIR, data = df_dieback_2009,
                      ntree = 500, mtry = 2)

model
varImpPlot(model)


Dieback_stack <- stack(NDVI_raster_2009, Blue_raster_2009, Green_raster_2009,
                       Red_raster_2009, NIR_raster_2009)

names(Dieback_stack) <- c("NDVI", "Blue", "Green",
                          "Red", "NIR")

Dieback_prob <- raster::predict(model = model, object = Dieback_stack, type="prob")

writeRaster(Dieback_prob, "Aerial_Imagery/NAIP/Dieback_probability/Dieback_prob_2009",
            format='GTiff', overwrite = TRUE)

###Creeks were removed from rasters (-1 value) based on 2009 to 2020 MSL - 10 cm, and MHW + 30 cm
###This was done in QGIS
###Load new rasters

Dieback_nocreek_raster_2009 <- raster(paste0(wd, "Aerial_Imagery/NAIP/Dieback_probability/Dieback_prob_2009_nocreek.tif"))


#2009
f_dieback_nocreek_2009 <- freq(Dieback_nocreek_raster_2009, digits = 3)
f_dieback_nocreek_no_NA_2009 <- f_dieback_nocreek_2009[!is.na(f_dieback_nocreek_2009[,1]),]
f_dieback_nocreek_no_NA_2009 <- as.data.frame(f_dieback_nocreek_no_NA_2009)
f_dieback_nocreek_no_NA_2009 <- f_dieback_nocreek_no_NA_2009[f_dieback_nocreek_no_NA_2009$value > -0.5,]

f_dieback_nocreek_no_NA_2009$Dieback <- "Uncertain"
f_dieback_nocreek_no_NA_2009$Dieback[f_dieback_nocreek_no_NA_2009$value  > 0.75] <- "Dieback"
f_dieback_nocreek_no_NA_2009$Dieback[f_dieback_nocreek_no_NA_2009$value  < 0.25] <- "Vegetated"

Dieback_95_2009 <- as.data.frame(aggregate(f_dieback_nocreek_no_NA_2009$count,
                                           by = list(Category=f_dieback_nocreek_no_NA_2009$Dieback),
                                           FUN=sum))

Dieback_95_2009$Year <- 2009
Dieback_95_2009$Percentage <- 100*(Dieback_95_2009$x / sum(Dieback_95_2009$x))  

#####Repeat for all years and for Louisiana to make Fig 1 and Fig 2

#Figure for dieback in LA
setwd(wd)

palmer_df <- read.csv("../Field data/Drought_NOAA_data_LA_98_21.csv", header = TRUE,
                      stringsAsFactors = FALSE)

palmer_df <- palmer_df[palmer_df$Year_continuous < 2019 &
                         palmer_df$Year_continuous > 1998,]

PDSI_fig <- ggplot(palmer_df, aes(x = Year_continuous, y = PDSI)) + geom_point() + 
  geom_line() + theme_bw() + geom_hline(yintercept = -4, linetype = "dashed") +
  geom_hline(yintercept = -2, linetype = "dotted") +
  scale_x_continuous(breaks = c(1998, 2001, 2004, 2006, 2008, 2010, 2012, 2015, 2017, 2019)) + 
  theme(axis.text.x = element_text(size = 11), axis.title = element_text(size = 16)) +
  labs(x = "Year")
PDSI_fig

unvegetated_df <- read.csv("Summary_unvegetated.csv", header = TRUE,
                           stringsAsFactors = FALSE)

Unvegetated_fig <- ggplot(unvegetated_df, aes(x = Year, y = Unveg....)) + 
  geom_point() +
  geom_line() + theme_bw() + theme(axis.text = element_text(size = 11),
                                   axis.title.x = element_blank(), 
                                   axis.title.y = element_text(size = 16)) +
  labs(x = "Year", y = "Unvegetated area (%)") + expand_limits(y = 0) + 
  scale_x_continuous(breaks = c(1998, 2001, 2004, 2006, 2008, 
                                2010, 2012, 2015, 2017, 2019)) 
Unvegetated_fig

tiff("../Paper/Figures/Fig_LA_R2.tiff", res = 300, width = 1600, height = 2000)
ggarrange(Unvegetated_fig, PDSI_fig, nrow = 2, ncol = 1, align = "v")
dev.off()




######Correct LIDAR based on RTK measurement

set.seed(20)

setwd(wd)

DEM_raster_Feb2017 <- raster(paste0(wd, "NOAA_DEM/dem_m_WGS84.tif"))
NDVI_raster_Feb2017 <- raster(paste0(wd, "../Multispectral/Planet/charleston_Feb20_2017_1351_psscene4band_analytic/files/PSScene4Band/20170220_135127_0d06/analytic/ndvi_toa_reflectance_27.tif"))
Red_raster_Feb2017 <- raster(paste0(wd, "../Multispectral/Planet/charleston_Feb20_2017_1351_psscene4band_analytic/files/PSScene4Band/20170220_135127_0d06/analytic/red_toa_reflectance_27.tif"))
Green_raster_Feb2017 <- raster(paste0(wd, "../Multispectral/Planet/charleston_Feb20_2017_1351_psscene4band_analytic/files/PSScene4Band/20170220_135127_0d06/analytic/green_toa_reflectance_27.tif"))
Blue_raster_Feb2017 <- raster(paste0(wd, "../Multispectral/Planet/charleston_Feb20_2017_1351_psscene4band_analytic/files/PSScene4Band/20170220_135127_0d06/analytic/blue_toa_reflectance_27.tif"))
NIR_raster_Feb2017 <- raster(paste0(wd, "../Multispectral/Planet/charleston_Feb20_2017_1351_psscene4band_analytic/files/PSScene4Band/20170220_135127_0d06/analytic/nir_toa_reflectance_27.tif"))

tidal_distance_raster <- raster(paste0(wd, "../Distance_Creek/distance_raster_modified_m_WAshley.tif"))

field_data <- read.csv("../Field data/summary_data.csv")
field_data_coord <- field_data[,c(9,8)]

field_data_coord_sp <- SpatialPoints(field_data_coord, proj4string = CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"))

field_data$NDVI_Feb2017 <- extract(x = NDVI_raster_Feb2017, y = field_data_coord_sp)

field_data$Red_Feb2017 <- extract(x = Red_raster_Feb2017, y = field_data_coord_sp)
field_data$Green_Feb2017 <- extract(x = Green_raster_Feb2017, y = field_data_coord_sp)
field_data$Blue_Feb2017 <- extract(x = Blue_raster_Feb2017, y = field_data_coord_sp)
field_data$NIR_Feb2017 <- extract(x = NIR_raster_Feb2017, y = field_data_coord_sp)

field_data$MSAVI2_Feb2017 <- (2 * field_data$NIR_Feb2017 + 1 - sqrt((2 * field_data$NIR_Feb2017 + 1)^2 - 8 * (field_data$NIR_Feb2017 - field_data$Red_Feb2017))) / 2

field_data$DEM_LIDAR <- extract(x = DEM_raster_Feb2017, y = field_data_coord_sp)

field_data$delta_DEM <- field_data$DEM_LIDAR - field_data$RTK_elevation
field_data$distance_creek <- extract(x = tidal_distance_raster, y = field_data_coord_sp)

#Remove town1q and town1g because they are tidal creeks
#Remove orange1a, carrmarsh30 and oldtown7a because they are outliers.

field_data_curated <- field_data[field_data$RTK_name != "town1q" &
                                   field_data$RTK_name != "town1g" &
                                   field_data$RTK_name != "orange1a" &
                                   field_data$RTK_name != "carrmarsh30" &
                                   field_data$RTK_name != "2oldtown7a",]


##Make training and testing datasets (70% - 30%)

testing_numbers <- sample(length(field_data_curated$Transect),
                          0.3*length(field_data_curated$Transect), 
                          replace = FALSE)

field_data_curated_training <- field_data_curated[-testing_numbers,]

res<-glmulti(log(delta_DEM+1)~ DEM_LIDAR + NDVI_Feb2017 + 
               MSAVI2_Feb2017 + Red_Feb2017 + Green_Feb2017 +
               Blue_Feb2017 + NIR_Feb2017,
             data = field_data_curated_training, fitfunc=lm, level = 1, method = "h")

print(res)
plot(res, type="s")
summary(res@objects[[1]])

#Estimate correction based on the glmulti formula...

field_data_curated$DEM_corrected_test <- 1 + field_data_curated$DEM_LIDAR - 
  exp(0.22620*field_data_curated$DEM_LIDAR +
        105.96724*field_data_curated$MSAVI2_Feb2017 +
        177.40841*field_data_curated$Red_Feb2017 -  
        179.50405*field_data_curated$NIR_Feb2017 +  
        0.06880)

#Adj-R2: 41.17%, p-value: 4.33e-14. All factors are significant


field_data_curated$delta_DEM_test <- field_data_curated$DEM_corrected_test - 
  field_data_curated$RTK_elevation


field_data_curated_training <- field_data_curated[-testing_numbers,]
field_data_curated_testing <- field_data_curated[testing_numbers,]

summary(field_data_curated_testing$delta_DEM)
summary(field_data_curated_testing$delta_DEM_test)

#RMSE for testing dataset

field_data_curated_testing$delta_DEM_square <- field_data_curated_testing$delta_DEM^2
sqrt(sum(field_data_curated_testing$delta_DEM_square)/length(field_data_curated_testing$delta_DEM_square))
#0.08207517

field_data_curated_testing$delta_DEM_test_square <- field_data_curated_testing$delta_DEM_test^2
sqrt(sum(field_data_curated_testing$delta_DEM_test_square, na.rm = T)/length(field_data_curated_testing$delta_DEM_test_square))
#0.05726391


sd(field_data_curated_testing$delta_DEM)
sd(field_data_curated_testing$delta_DEM_test)


summary(field_data_curated_training$delta_DEM)
summary(field_data_curated_training$delta_DEM_test)

#RMSE for training dataset

field_data_curated_training$delta_DEM_square <- field_data_curated_training$delta_DEM^2
sqrt(sum(field_data_curated_training$delta_DEM_square)/length(field_data_curated_training$delta_DEM_square))
#0.1054615

field_data_curated_training$delta_DEM_test_square <- field_data_curated_training$delta_DEM_test^2
sqrt(sum(field_data_curated_training$delta_DEM_test_square)/length(field_data_curated_training$delta_DEM_test_square))
#0.08043353


sd(field_data_curated_training$delta_DEM)
sd(field_data_curated_training$delta_DEM_test)


write.table(field_data_curated, "../Field data/field_data_curated.txt", 
            quote = FALSE, sep = '\t')




####Create and save a MSAVI2 raster

rast_stack_msavi2 <- stack(Red_raster_Feb2017, NIR_raster_Feb2017)


fun_msavi2 <- function(x) {((2*x[2] + 1 - sqrt((2*x[2] + 1)^2 -8*(x[2] - x[1])))/2)}

MSAVI2_raster <- calc(rast_stack_msavi2, fun_msavi2)

# write to a new geotiff file (depends on rgdal)
if (require(rgdal)) {
  rf <- writeRaster(MSAVI2_raster,
                    "../Multispectral/Planet/charleston_Feb20_2017_1351_psscene4band_analytic/files/PSScene4Band/20170220_135127_0d06/analytic/msavi2_toa_reflectance_27.tif",
                    format="GTiff", overwrite=TRUE)
}


####Load rasters and build the corrected DEM


DEM_raster_Feb2017_WAshley <- raster(paste0(wd, "NOAA_DEM/dem_m_WGS84_WAshley.tif"))
MSAVI2_raster_Feb2017_WAshley <- raster(paste0(wd, "../Multispectral/Planet/charleston_Feb20_2017_1351_psscene4band_analytic/files/PSScene4Band/20170220_135127_0d06/analytic/msavi2_toa_reflectance_27.tif"))
NDVI_raster_Feb2017_WAshley <- raster(paste0(wd, "../Multispectral/Planet/charleston_Feb20_2017_1351_psscene4band_analytic/files/PSScene4Band/20170220_135127_0d06/analytic/ndvi_toa_reflectance_27.tif"))
Red_raster_Feb2017_WAshley <- raster(paste0(wd, "../Multispectral/Planet/charleston_Feb20_2017_1351_psscene4band_analytic/files/PSScene4Band/20170220_135127_0d06/analytic/red_toa_reflectance_27.tif"))
NIR_raster_Feb2017_WAshley <- raster(paste0(wd, "../Multispectral/Planet/charleston_Feb20_2017_1351_psscene4band_analytic/files/PSScene4Band/20170220_135127_0d06/analytic/nir_toa_reflectance_27.tif"))
Blue_raster_Feb2017_WAshley <- raster(paste0(wd, "../Multispectral/Planet/charleston_Feb20_2017_1351_psscene4band_analytic/files/PSScene4Band/20170220_135127_0d06/analytic/blue_toa_reflectance_27.tif"))

NDVI_reproject <- projectRaster(NDVI_raster_Feb2017_WAshley, DEM_raster_Feb2017_WAshley) 
MSAVI_reproject <- projectRaster(MSAVI2_raster_Feb2017_WAshley, DEM_raster_Feb2017_WAshley) 
Red_reproject <- projectRaster(Red_raster_Feb2017_WAshley, DEM_raster_Feb2017_WAshley) 
NIR_reproject <- projectRaster(NIR_raster_Feb2017_WAshley, DEM_raster_Feb2017_WAshley) 
Blue_reproject <- projectRaster(Blue_raster_Feb2017_WAshley, DEM_raster_Feb2017_WAshley) 

rast_stack <- stack(DEM_raster_Feb2017_WAshley, NDVI_reproject, MSAVI_reproject,
                    Red_reproject, NIR_reproject, Blue_reproject)


fun_1 <- function(x) { 1 + x[1] - exp(0.22620*x[1] + 105.96724*x[3] +
                                        177.40841*x[4] - 179.50405*x[5] +
                                        0.06880)}

DEM_corrected_raster_1 <- calc(rast_stack, fun_1)


field_data$DEM_corrected_raster_1 <- extract(x = DEM_corrected_raster_1, y = field_data_coord_sp)

field_data_curated <- field_data[field_data$RTK_name != "town1q" &
                                   field_data$RTK_name != "town1g" &
                                   field_data$RTK_name != "orange1a" &
                                   field_data$RTK_name != "carrmarsh30" &
                                   field_data$RTK_name != "2oldtown7a",]

field_data_curated$delta_DEM_raster_1 <- field_data_curated$RTK_elevation -
  field_data_curated$DEM_corrected_raster_1


summary(field_data_curated$delta_DEM)
summary(field_data_curated$delta_DEM_raster_1)


field_data_curated_testing_R <- field_data_curated[testing_numbers,]


# write to a new geotiff file (depends on rgdal)
if (require(rgdal)) {
  rf <- writeRaster(DEM_corrected_raster_1, "DEM_corrected_level1.tiff", format="GTiff", overwrite=TRUE)
}


####Figure S2!

delta_freq_fig <- ggplot(field_data_curated_testing, aes(y = delta_DEM)) + 
  geom_freqpoly(binwidth = 0.025) +
  expand_limits(y = 0.6) + expand_limits(y = -0.25) +  theme_bw()  + 
  labs(y = "DEM error (m)", x = "Frequency") + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_blank()) +
  expand_limits(x = 13)
delta_freq_fig

delta_freq_test_fig <- ggplot(field_data_curated_testing, aes(y = delta_DEM_test)) + 
  geom_freqpoly(binwidth = 0.025) + 
  expand_limits(y = 0.6) + expand_limits(y = -0.25) +  theme_bw()  + 
  labs(y = "DEM error corrected (m)", x = "Frequency") + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  expand_limits(x = 13)
delta_freq_test_fig

field_data_curated_testing$Dataset <- "Testing"
field_data_curated_training$Dataset <- "Training"
field_data_curated_all <- rbind(field_data_curated_testing, 
                                field_data_curated_training)

biplot_fig <- ggplot(field_data_curated_all,
                     aes(x = RTK_elevation, y = DEM_LIDAR,
                         color = Dataset)) + 
  geom_point(size = 1) + theme_bw() + geom_abline(intercept = 0, slope = 1) +
  expand_limits(x = 0.1, y = 0.1) + expand_limits(x = 1, y = 1) +
  labs(y = "DEM (m)", x = "RTK elevation (m)") + coord_fixed() + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) + 
  guides(color = guide_legend(override.aes = list(size=2)))
biplot_fig

biplot_corrected_fig <- ggplot(field_data_curated_all, 
                               aes(x = RTK_elevation, y = DEM_corrected_test,
                                   color = Dataset)) + 
  geom_point(size = 1) + theme_bw() + geom_abline(intercept = 0, slope = 1) +
  expand_limits(x = 0.1, y = 0.1) + expand_limits(x = 1, y = 1) +
  labs(y = "DEM corrected (m)", x = "RTK elevation (m)") + coord_fixed() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) + 
  guides(color = guide_legend(override.aes = list(size=2)))

biplot_corrected_fig

DEM_corrected_raster_1 <- raster(paste0(wd, "DEM_corrected_level1.tif"))

freq_DEM_corrected <- data.frame(freq(DEM_corrected_raster_1, digits = 3))
freq_DEM_original <- data.frame(freq(DEM_raster_Feb2017_WAshley, digits = 3))

freq_DEM_original_curated <- freq_DEM_original[freq_DEM_original$value > -0.06 &
                                                 freq_DEM_original$value < 1.1,]

freq_DEM_original_curated <- freq_DEM_original_curated[complete.cases(freq_DEM_original_curated),]

freq_DEM_corrected_curated <- freq_DEM_corrected[freq_DEM_corrected$value > -0.06 &
                                                   freq_DEM_corrected$value < 1.1,]

freq_DEM_corrected_curated <- freq_DEM_corrected_curated[complete.cases(freq_DEM_corrected_curated),]

freq_plot_original <- ggplot(freq_DEM_original_curated, aes(x = value, y = count)) + geom_point() + theme_bw() +
  labs(y = "Frequency", x = "DEM NAVD88 (m)") + theme(axis.title = element_text(size = 14),
                                                      axis.text = element_text(size = 12),
                                                      axis.title.x = element_blank()) +
  scale_x_continuous(breaks = round(seq(-0.1, 1.1,
                                        by = 0.2),1)) +
  geom_vline(xintercept = 0.04, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 0.80, linetype = "dashed", color = "red") +
  expand_limits(y = 16000)
freq_plot_original

freq_plot_corrected <- ggplot(freq_DEM_corrected_curated, aes(x = value, y = count)) + geom_point() + theme_bw() +
  labs(y = "Frequency corrected", x = "DEM NAVD88 (m)") + theme(axis.title = element_text(size = 14),
                                                                axis.text = element_text(size = 12)) +
  scale_x_continuous(breaks = round(seq(-0.1, 1.1,
                                        by = 0.2),1)) +
  geom_vline(xintercept = 0.04, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 0.80, linetype = "dashed", color = "red") +
  expand_limits(y = 16000)
freq_plot_corrected

jpeg("../Paper/Figures/FigS2.jpeg", res = 300, width = 3000, height = 2000)
ggarrange(biplot_fig, delta_freq_fig, freq_plot_original, 
          biplot_corrected_fig, delta_freq_test_fig, freq_plot_corrected, 
          nrow = 2, ncol = 3, legend = "top", common.legend = TRUE, align = "hv")
dev.off()



##Figure 1 and 3 / West Ashley Dieback Characterization


set.seed(1989)

setwd(wd)

DEM_raster_corrected <- raster(paste0(wd, "DEM_corrected_level1.tif"))
tidal_distance_raster <- raster(paste0(wd, "../Distance_Creek/distance_raster_modified_m_WAshley.tif"))

#2021

dieback_raster_2021 <- raster(paste0(wd, "../Multispectral/Aerial_Imagery/NAIP/Dieback_probability/Dieback_prob_2021_nocreek.tif"))

DEM_LIDAR_corrected_reproject <- projectRaster(DEM_raster_corrected, dieback_raster_2021) 
tidal_distance_raster_reproject <- projectRaster(tidal_distance_raster, dieback_raster_2021) 

DEM_values <- values(DEM_LIDAR_corrected_reproject)
distance_creek_values <- values(tidal_distance_raster_reproject)
dieback_2021_values <- values(dieback_raster_2021)

dieback_2021_df  <- tibble(DEM = DEM_values, 
                           distance_creek = distance_creek_values,
                           dieback_2021 = dieback_2021_values,
                           Year = 2021)

dieback_2021_df <- dieback_2021_df[complete.cases(dieback_2021_df),]
dieback_2021_df <- dieback_2021_df[dieback_2021_df$DEM >= -0.06,]
dieback_2021_df <- dieback_2021_df[dieback_2021_df$DEM <= 1.1,]
dieback_2021_df <- dieback_2021_df[dieback_2021_df$dieback_2021 > -0.5,]

dieback_2021_df$dieback_2021_factor <- ""
dieback_2021_df$dieback_2021_factor[dieback_2021_df$dieback_2021 >= 0.75] <- "Dieback"
dieback_2021_df$dieback_2021_factor[dieback_2021_df$dieback_2021 <= 0.25] <- "Vegetated"

dieback_2021_df$dieback_2021_factor <- as.factor(dieback_2021_df$dieback_2021_factor)

dieback_2021_df_midmarsh <- dieback_2021_df[dieback_2021_df$distance_creek > 7.5,]
dieback_2021_df_creekbank <- dieback_2021_df[dieback_2021_df$distance_creek <= 7.5,]


#2019

dieback_raster_2019 <- raster(paste0(wd, "../Multispectral/Aerial_Imagery/NAIP/Dieback_probability/Dieback_prob_2019_nocreek.tif"))

DEM_LIDAR_corrected_reproject <- projectRaster(DEM_raster_corrected, dieback_raster_2019) 
tidal_distance_raster_reproject <- projectRaster(tidal_distance_raster, dieback_raster_2019) 

DEM_values <- values(DEM_LIDAR_corrected_reproject)
distance_creek_values <- values(tidal_distance_raster_reproject)
dieback_2019_values <- values(dieback_raster_2019)

dieback_2019_df  <- tibble(DEM = DEM_values,
                           distance_creek = distance_creek_values,
                           dieback_2019 = dieback_2019_values,
                           Year = 2019)

dieback_2019_df <- dieback_2019_df[complete.cases(dieback_2019_df),]
dieback_2019_df <- dieback_2019_df[dieback_2019_df$DEM >= -0.06,]
dieback_2019_df <- dieback_2019_df[dieback_2019_df$DEM <= 1.1,]
dieback_2019_df <- dieback_2019_df[dieback_2019_df$dieback_2019 > -0.5,]

dieback_2019_df$dieback_2019_factor <- ""
dieback_2019_df$dieback_2019_factor[dieback_2019_df$dieback_2019 >= 0.75] <- "Dieback"
dieback_2019_df$dieback_2019_factor[dieback_2019_df$dieback_2019 <= 0.25] <- "Vegetated"

dieback_2019_df$dieback_2019_factor <- as.factor(dieback_2019_df$dieback_2019_factor)

dieback_2019_df_midmarsh <- dieback_2019_df[dieback_2019_df$distance_creek > 7.5,]
dieback_2019_df_creekbank <- dieback_2019_df[dieback_2019_df$distance_creek <= 7.5,]


#2017

dieback_raster_2017 <- raster(paste0(wd, "../Multispectral/Aerial_Imagery/NAIP/Dieback_probability/Dieback_prob_2017_nocreek.tif"))

DEM_LIDAR_corrected_reproject <- projectRaster(DEM_raster_corrected, dieback_raster_2017) 
tidal_distance_raster_reproject <- projectRaster(tidal_distance_raster, dieback_raster_2017) 

DEM_values <- values(DEM_LIDAR_corrected_reproject)
distance_creek_values <- values(tidal_distance_raster_reproject)
dieback_2017_values <- values(dieback_raster_2017)

dieback_2017_df  <- tibble(DEM = DEM_values,
                           distance_creek = distance_creek_values,
                           dieback_2017 = dieback_2017_values,
                           Year = 2017)

dieback_2017_df <- dieback_2017_df[complete.cases(dieback_2017_df),]
dieback_2017_df <- dieback_2017_df[dieback_2017_df$DEM >= -0.06,]
dieback_2017_df <- dieback_2017_df[dieback_2017_df$DEM <= 1.1,]
dieback_2017_df <- dieback_2017_df[dieback_2017_df$dieback_2017 > -0.5,]

dieback_2017_df$dieback_2017_factor <- ""
dieback_2017_df$dieback_2017_factor[dieback_2017_df$dieback_2017 >= 0.75] <- "Dieback"
dieback_2017_df$dieback_2017_factor[dieback_2017_df$dieback_2017 <= 0.25] <- "Vegetated"

dieback_2017_df$dieback_2017_factor <- as.factor(dieback_2017_df$dieback_2017_factor)

dieback_2017_df_midmarsh <- dieback_2017_df[dieback_2017_df$distance_creek > 7.5,]
dieback_2017_df_creekbank <- dieback_2017_df[dieback_2017_df$distance_creek <= 7.5,]


#2015

dieback_raster_2015 <- raster(paste0(wd, "../Multispectral/Aerial_Imagery/NAIP/Dieback_probability/Dieback_prob_2015_nocreek.tif"))

DEM_LIDAR_corrected_reproject <- projectRaster(DEM_raster_corrected, dieback_raster_2015) 
tidal_distance_raster_reproject <- projectRaster(tidal_distance_raster, dieback_raster_2015) 

DEM_values <- values(DEM_LIDAR_corrected_reproject)
distance_creek_values <- values(tidal_distance_raster_reproject)
dieback_2015_values <- values(dieback_raster_2015)

dieback_2015_df  <- tibble(DEM = DEM_values,
                           distance_creek = distance_creek_values,
                           dieback_2015 = dieback_2015_values,
                           Year = 2015)

dieback_2015_df <- dieback_2015_df[complete.cases(dieback_2015_df),]
dieback_2015_df <- dieback_2015_df[dieback_2015_df$DEM >= -0.06,]
dieback_2015_df <- dieback_2015_df[dieback_2015_df$DEM <= 1.1,]
dieback_2015_df <- dieback_2015_df[dieback_2015_df$dieback_2015 > -0.5,]

dieback_2015_df$dieback_2015_factor <- ""
dieback_2015_df$dieback_2015_factor[dieback_2015_df$dieback_2015 >= 0.75] <- "Dieback"
dieback_2015_df$dieback_2015_factor[dieback_2015_df$dieback_2015 <= 0.25] <- "Vegetated"

dieback_2015_df$dieback_2015_factor <- as.factor(dieback_2015_df$dieback_2015_factor)

dieback_2015_df_midmarsh <- dieback_2015_df[dieback_2015_df$distance_creek > 7.5,]
dieback_2015_df_creekbank <- dieback_2015_df[dieback_2015_df$distance_creek <= 7.5,]



#2013

dieback_raster_2013 <- raster(paste0(wd, "../Multispectral/Aerial_Imagery/NAIP/Dieback_probability/Dieback_prob_2013_nocreek.tif"))

DEM_LIDAR_corrected_reproject <- projectRaster(DEM_raster_corrected, dieback_raster_2013) 
tidal_distance_raster_reproject <- projectRaster(tidal_distance_raster, dieback_raster_2013) 

DEM_values <- values(DEM_LIDAR_corrected_reproject)
distance_creek_values <- values(tidal_distance_raster_reproject)
dieback_2013_values <- values(dieback_raster_2013)

dieback_2013_df  <- tibble(DEM = DEM_values,
                           distance_creek = distance_creek_values,
                           dieback_2013 = dieback_2013_values,
                           Year = 2013)

dieback_2013_df <- dieback_2013_df[complete.cases(dieback_2013_df),]
dieback_2013_df <- dieback_2013_df[dieback_2013_df$DEM >= -0.06,]
dieback_2013_df <- dieback_2013_df[dieback_2013_df$DEM <= 1.1,]
dieback_2013_df <- dieback_2013_df[dieback_2013_df$dieback_2013 > -0.5,]

dieback_2013_df$dieback_2013_factor <- ""
dieback_2013_df$dieback_2013_factor[dieback_2013_df$dieback_2013 >= 0.75] <- "Dieback"
dieback_2013_df$dieback_2013_factor[dieback_2013_df$dieback_2013 <= 0.25] <- "Vegetated"

dieback_2013_df$dieback_2013_factor <- as.factor(dieback_2013_df$dieback_2013_factor)

dieback_2013_df_midmarsh <- dieback_2013_df[dieback_2013_df$distance_creek > 7.5,]
dieback_2013_df_creekbank <- dieback_2013_df[dieback_2013_df$distance_creek <= 7.5,]



#2011 

dieback_raster_2011 <- raster(paste0(wd, "../Multispectral/Aerial_Imagery/NAIP/Dieback_probability/Dieback_prob_2011_nocreek.tif"))

DEM_LIDAR_corrected_reproject <- projectRaster(DEM_raster_corrected, dieback_raster_2011) 
tidal_distance_raster_reproject <- projectRaster(tidal_distance_raster, dieback_raster_2011) 

DEM_values <- values(DEM_LIDAR_corrected_reproject)
distance_creek_values <- values(tidal_distance_raster_reproject)
dieback_2011_values <- values(dieback_raster_2011)

dieback_2011_df  <- tibble(DEM = DEM_values,
                           distance_creek = distance_creek_values,
                           dieback_2011 = dieback_2011_values,
                           Year = 2011)

dieback_2011_df <- dieback_2011_df[complete.cases(dieback_2011_df),]
dieback_2011_df <- dieback_2011_df[dieback_2011_df$DEM >= -0.06,]
dieback_2011_df <- dieback_2011_df[dieback_2011_df$DEM <= 1.1,]
dieback_2011_df <- dieback_2011_df[dieback_2011_df$dieback_2011 > -0.5,]

dieback_2011_df$dieback_2011_factor <- ""
dieback_2011_df$dieback_2011_factor[dieback_2011_df$dieback_2011 >= 0.75] <- "Dieback"
dieback_2011_df$dieback_2011_factor[dieback_2011_df$dieback_2011 <= 0.25] <- "Vegetated"

dieback_2011_df$dieback_2011_factor <- as.factor(dieback_2011_df$dieback_2011_factor)

dieback_2011_df_midmarsh <- dieback_2011_df[dieback_2011_df$distance_creek > 7.5,]
dieback_2011_df_creekbank <- dieback_2011_df[dieback_2011_df$distance_creek <= 7.5,]




#2009 

dieback_raster_2009 <- raster(paste0(wd, "../Multispectral/Aerial_Imagery/NAIP/Dieback_probability/Dieback_prob_2009_nocreek.tif"))

DEM_LIDAR_corrected_reproject <- projectRaster(DEM_raster_corrected, dieback_raster_2009) 
tidal_distance_raster_reproject <- projectRaster(tidal_distance_raster, dieback_raster_2009) 

DEM_values <- values(DEM_LIDAR_corrected_reproject)
distance_creek_values <- values(tidal_distance_raster_reproject)
dieback_2009_values <- values(dieback_raster_2009)

dieback_2009_df  <- tibble(DEM = DEM_values,
                           distance_creek = distance_creek_values,
                           dieback_2009 = dieback_2009_values,
                           Year = 2009)

dieback_2009_df <- dieback_2009_df[complete.cases(dieback_2009_df),]
dieback_2009_df <- dieback_2009_df[dieback_2009_df$DEM >= -0.06,]
dieback_2009_df <- dieback_2009_df[dieback_2009_df$DEM <= 1.1,]
dieback_2009_df <- dieback_2009_df[dieback_2009_df$dieback_2009 > -0.5,]

dieback_2009_df$dieback_2009_factor <- ""
dieback_2009_df$dieback_2009_factor[dieback_2009_df$dieback_2009 >= 0.75] <- "Dieback"
dieback_2009_df$dieback_2009_factor[dieback_2009_df$dieback_2009 <= 0.25] <- "Vegetated"

dieback_2009_df$dieback_2009_factor <- as.factor(dieback_2009_df$dieback_2009_factor)

dieback_2009_df_midmarsh <- dieback_2009_df[dieback_2009_df$distance_creek > 7.5,]
dieback_2009_df_creekbank <- dieback_2009_df[dieback_2009_df$distance_creek <= 7.5,]


#Merge all years
names(dieback_2009_df) = names(dieback_2011_df) = names(dieback_2013_df) =
  names(dieback_2015_df) = names(dieback_2017_df) = names(dieback_2019_df) =
  names(dieback_2021_df) = c("DEM", "distance_creek",
                             "dieback_prob", "Year", "dieback_factor")

dieback_df <- rbind(dieback_2009_df, dieback_2011_df, dieback_2013_df,
                    dieback_2015_df, dieback_2017_df, dieback_2019_df,
                    dieback_2021_df)

dieback_df$marsh_location <- "Midmarsh"
dieback_df$marsh_location[dieback_df$distance_creek < 7.5] <- "Creekbank"


dieback_df$Year <- factor(dieback_df$Year)

dieback_df_curated <- dieback_df[dieback_df$dieback_factor != "",]




###Check DEM error between dieback and vegetated marsh areas

DEM_raster_corrected <- raster(paste0(wd, "DEM_corrected_level1.tif"))

field_data <- read.csv("../Field data/summary_data.csv")
field_data_coord <- field_data[,c(9,8)]
field_data_coord_sp <- SpatialPoints(field_data_coord,
                                     proj4string = CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"))
field_data$DEM_LIDAR_corrected <- extract(x = DEM_raster_corrected,
                                          y = field_data_coord_sp)

field_data$delta_DEM <- field_data$DEM_LIDAR_corrected - field_data$RTK_elevation

##Remove creekbank points and elevation outliers (most likely human/technical error during measurement)
field_data_curated <- field_data[field_data$RTK_name != "town1q" &
                                   field_data$RTK_name != "town1g" &
                                   field_data$RTK_name != "orange1a" &
                                   field_data$RTK_name != "carrmarsh30" &
                                   field_data$RTK_name != "2oldtown7a",]

field_data_curated <- field_data_curated[!is.na(field_data_curated$biomass_total),]

field_data_curated$dieback_factor <- "Vegetated"
field_data_curated$dieback_factor[field_data_curated$biomass_total == 0] <- "Dieback"
field_data_curated$dieback_factor  <- factor(field_data_curated$dieback_factor)

ggplot(field_data_curated, aes(x = dieback_factor, y = delta_DEM)) + 
  geom_boxplot() + expand_limits(y = -0.3) + expand_limits(y = 0.5) 

summary(field_data_curated[field_data_curated$dieback_factor == "Dieback",]$delta_DEM)
summary(field_data_curated[field_data_curated$dieback_factor == "Vegetated",]$delta_DEM)


###Fig 1

dieback_df_summary_DEM <- summarySE(dieback_df_curated, 
                                    measurevar = "DEM", 
                                    groupvars = c("Year", 
                                                  "dieback_factor",
                                                  "marsh_location"))

dieback_df_summary_DEM_group <- dieback_df_summary_DEM %>%
  dplyr::group_by(Year) %>% dplyr::mutate(N_total = sum(N))

dieback_df_summary_DEM_group$Percetage <- 100*dieback_df_summary_DEM_group$N/dieback_df_summary_DEM_group$N_total

dieback_df_summary_DEM_raw <- summarySE(dieback_df, 
                                        measurevar = "DEM", 
                                        groupvars = c("Year", 
                                                      "dieback_factor",
                                                      "marsh_location"))

dieback_df_summary_DEM_group_raw <- dieback_df_summary_DEM_raw %>%
  dplyr::group_by(Year) %>% dplyr::mutate(N_total = sum(N))

dieback_df_summary_DEM_group_raw$Percetage <- 100*dieback_df_summary_DEM_group_raw$N/dieback_df_summary_DEM_group_raw$N_total

Dieback_percentage_plot <- ggplot(dieback_df_summary_DEM_group_raw[dieback_df_summary_DEM_group_raw$dieback_factor == "Dieback",],
                                  aes(fill=marsh_location, y=Percetage, x=Year)) + 
  geom_bar(position="stack", stat="identity") + theme_bw() +
  labs(y = "Unvegetated area (%)") + theme(legend.position = "top", legend.title = element_blank(), 
                                           axis.title.x = element_blank(), legend.text = element_text(size = 8),
                                           axis.text.x = element_text(size = 8)) +
  guides(fill = guide_legend(override.aes = list(size = 1.5)))

write.table(dieback_df_summary_DEM_group_raw, "../unvegetated_percentage_Charleston_raw.txt", 
            quote = F, sep = "\t")



##Drought index

Drought_index_df <- read.csv("../Field data/Drought_NOAA_data_Charleston_99_21.csv")

Drought_index_df <- Drought_index_df[Drought_index_df$Year_continuous > 2008,]

PDSI_fig <- ggplot(Drought_index_df, aes(x = Year_continuous, y = PDSI)) + geom_point() + 
  geom_line() + theme_bw() + geom_hline(yintercept = -4, linetype = "dashed") +
  geom_hline(yintercept = -2, linetype = "dotted") +
  scale_x_continuous(breaks = c(2009, 2011, 2013, 2015, 2017, 2019, 2021)) + 
  theme(axis.text.x = element_text(size = 8)) +
  labs(x = "Year")

##Partial Fig 1
tiff("../Paper/Figures/Fig_1_R.tiff", res = 300 , width = 1000, height = 1450)
ggarrange(Dieback_percentage_plot, PDSI_fig, ncol = 1, nrow = 2, align = "hv")
dev.off()



##Dieback characterization - Fig3
dieback_df_curated$dieback_factor <- gsub("Dieback", "Unvegetated", dieback_df_curated$dieback_factor)

fig_distance <- ggplot(dieback_df_curated, aes(x = Year, y = distance_creek, fill = dieback_factor)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5) + theme_bw() +
  facet_wrap(~marsh_location, scales = "free") + 
  expand_limits(y = 0) + labs(y = "Distance from tidal creek (m)") +
  theme(legend.title = element_blank(), axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        axis.text = element_text(size = 14), legend.text = element_text(size = 14))

fig_elevation <- ggplot(dieback_df_curated, aes(x = Year, y = DEM, fill = dieback_factor)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5) + theme_bw() +
  facet_wrap(~marsh_location, scales = "free") + 
  labs(y = "2017 Elevation NAVD88 (m)") +
  theme(legend.title = element_blank(), axis.title = element_text(size = 16),
        strip.text = element_text(size = 16), axis.title.x = element_blank(),
        axis.text = element_text(size = 14), legend.text = element_text(size = 14))

median(dieback_df_curated[dieback_df_curated$Year == 2021 &
                            dieback_df_curated$marsh_location == "Midmarsh" &
                            dieback_df_curated$dieback_factor == "Vegetated",]$DEM)

tiff("../Paper/Figures/Fig3.tiff", res = 300, width = 3000, height = 2000)
ggarrange(fig_elevation, fig_distance, ncol = 1, nrow = 2, align = "hv",
          common.legend = TRUE, legend = "top")
dev.off()

save.image("../dieback_characterization.Rdata")



###Biomass estimation and figure 4

set.seed(1989)

setwd(wd)

field_data <- read.csv("../Field data/summary_data.csv")
field_data_coord <- field_data[,c(9,8)]
field_data_coord_sp <- SpatialPoints(field_data_coord, proj4string = CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"))

DEM_raster_corrected <- raster(paste0(wd, "DEM_corrected_level1.tif"))
NDVI_raster_2021 <- raster(paste0(wd, "../Multispectral/Planet/charleston_Aug01_2021_15_26_psscene4band_analytic_sr_udm2/20210801_154236_NDVI_WAshley.tif"))
MSAVI2_raster_2021 <- raster(paste0(wd, "../Multispectral/Planet/charleston_Aug01_2021_15_26_psscene4band_analytic_sr_udm2/20210801_154236_MSAVI2_WAshley.tif"))
Red_raster_2021 <- raster(paste0(wd, "../Multispectral/Planet/charleston_Aug01_2021_15_26_psscene4band_analytic_sr_udm2/20210801_154236_Red_WAshley.tif"))
Green_raster_2021 <- raster(paste0(wd, "../Multispectral/Planet/charleston_Aug01_2021_15_26_psscene4band_analytic_sr_udm2/20210801_154236_Green_WAshley.tif"))
Blue_raster_2021 <- raster(paste0(wd, "../Multispectral/Planet/charleston_Aug01_2021_15_26_psscene4band_analytic_sr_udm2/20210801_154236_Blue_WAshley.tif"))
NIR_raster_2021 <- raster(paste0(wd, "../Multispectral/Planet/charleston_Aug01_2021_15_26_psscene4band_analytic_sr_udm2/20210801_154236_NIR_WAshley.tif"))
tidal_distance_raster <- raster(paste0(wd, "../Distance_Creek/distance_raster_modified_m_WAshley.tif"))

field_data_coord <- field_data[,c(9,8)]
field_data_coord_sp <- SpatialPoints(field_data_coord, proj4string = CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"))

field_data$NDVI_2021 <- extract(x = NDVI_raster_2021, y = field_data_coord_sp)
field_data$MSAVI2_2021 <- extract(x = MSAVI2_raster_2021, y = field_data_coord_sp)
field_data$DEM_LIDAR_corrected <- extract(x = DEM_raster_corrected, y = field_data_coord_sp)
field_data$distance_creek <- extract(x = tidal_distance_raster, y = field_data_coord_sp)
field_data$Red_2021 <- extract(x = Red_raster_2021, y = field_data_coord_sp)
field_data$Green_2021 <- extract(x = Green_raster_2021, y = field_data_coord_sp)
field_data$Blue_2021 <- extract(x = Blue_raster_2021, y = field_data_coord_sp)
field_data$NIR_2021 <- extract(x = NIR_raster_2021, y = field_data_coord_sp)

##Remove tidal creek and outliers as before
field_data_curated <- field_data[field_data$RTK_name != "town1q" &
                                   field_data$RTK_name != "town1g" &
                                   field_data$RTK_name != "orange1a" &
                                   field_data$RTK_name != "carrmarsh30" &
                                   field_data$RTK_name != "2oldtown7a",]

#Keep only July datapoints
field_data_curated_July <- field_data_curated[field_data_curated$Date != "5/27/2021" &
                                                field_data_curated$Date != "5/28/2021",]

field_data_curated_July <- field_data_curated_July[-grep(pattern = "rep", x = field_data_curated_July$Transect),]

#divide testing and training datasets
testing_numbers <- sample(length(field_data_curated_July$Transect),
                          0.2*length(field_data_curated_July$Transect), 
                          replace = FALSE)
field_data_curated_training <- field_data_curated_July[-testing_numbers,]
field_data_curated_testing <- field_data_curated_July[testing_numbers,]

#Build model to predict biomass 
#Training model
model_training <- randomForest(biomass_total ~  MSAVI2_2021 + NDVI_2021 +
                                 Green_2021 + Red_2021 + NIR_2021 +
                                 DEM_LIDAR_corrected ,
                               data = field_data_curated_training,
                               ntree = 5000, mtry = 2)
model_training
varImpPlot(model_training)


#Testing

field_data_curated_testing$biomass_predicted <- predict(model_training, newdata = field_data_curated_testing)
field_data_curated_testing$biomass_error <- field_data_curated_testing$biomass_total - field_data_curated_testing$biomass_predicted
plot(field_data_curated_testing$biomass_total, field_data_curated_testing$biomass_predicted)

#RMSE testing
field_data_curated_testing$biomass_error_square <- field_data_curated_testing$biomass_error^2
sqrt(sum(field_data_curated_testing$biomass_error_square)/length(field_data_curated_testing$biomass_error_square))
#268.0715 g m-2


##Training dataset

field_data_curated_training$biomass_predicted <- predict(model_training, newdata = field_data_curated_training)
field_data_curated_training$biomass_error <- field_data_curated_training$biomass_total - field_data_curated_training$biomass_predicted

field_data_curated_training$biomass_error_square <- field_data_curated_training$biomass_error^2
sqrt(sum(field_data_curated_training$biomass_error_square)/length(field_data_curated_training$biomass_error_square))
#174.3009 g m-2

##Merge testing and training
field_data_curated_testing$Dataset <- "Testing"
field_data_curated_training$Dataset <- "Training"

field_data_curated_all <- rbind(field_data_curated_testing, field_data_curated_training)

# write to a new geotiff file (depends on rgdal)

DEM_LIDAR_corrected_reproject <- projectRaster(DEM_raster_corrected, NDVI_raster_2021) 
tidal_distance_raster_reproject <- projectRaster(tidal_distance_raster, NDVI_raster_2021) 

rast_stack <- stack(MSAVI2_raster_2021, Green_raster_2021,
                    NDVI_raster_2021, Red_raster_2021, NIR_raster_2021,
                    DEM_LIDAR_corrected_reproject)

names(rast_stack) <- c("MSAVI2_2021", "Green_2021",
                       "NDVI_2021", "Red_2021",
                       "NIR_2021", "DEM_LIDAR_corrected")

#Predict raster from study area
predicted_biomass_raster <- raster::predict(model = model_training, object = rast_stack, type='response',
                                            progress='window')

#Save biomass raster
if (require(rgdal)) {
  rf <- writeRaster(predicted_biomass_raster, "../predicted_biomass_2021_nodistance", format="GTiff", overwrite=TRUE)
}

## IMPORTANT!! In QGIS removed all creeks and set dieback areas as 0 biomass, read new raster

predicted_biomass_raster_nocreek <- raster(paste0(wd, "../predicted_biomass_2021_nodistance_nocreek_dieback.tif"))

predicted_biomass_raster_nocreek <- projectRaster(predicted_biomass_raster_nocreek,
                                                  DEM_LIDAR_corrected_reproject) 

DEM_values <- values(DEM_LIDAR_corrected_reproject)
biomass_values <- values(predicted_biomass_raster_nocreek)
MSAVI2_values <- values(MSAVI2_raster_2021)
distance_creek_values <- values(tidal_distance_raster_reproject)

biomass_df  <- tibble(DEM = DEM_values, biomass = biomass_values, 
                      MSAVI2 = MSAVI2_values, distance_creek = distance_creek_values)

biomass_df <- biomass_df[biomass_df$biomass >= 0,]
biomass_df <- biomass_df[complete.cases(biomass_df),]
biomass_df <- biomass_df[biomass_df$DEM >= -0.06,]
biomass_df <- biomass_df[biomass_df$DEM <= 1.1,]

biomass_df$distance_creek_factor <- "Midmarsh"
biomass_df$distance_creek_factor[biomass_df$distance_creek <= 7.5] <- "Creekbank"

biomass_df_midmarsh <- biomass_df[biomass_df$distance_creek_factor == "Midmarsh",]
biomass_df_creekbank <- biomass_df[biomass_df$distance_creek_factor == "Creekbank",]

###Do Fig 5 

#Biomass prediction vs measured
fig_prediction <- ggplot(field_data_curated_all, aes(x = biomass_total, y = biomass_predicted, color = Dataset)) +
  geom_point() + theme_bw() + geom_abline(intercept = 0, slope = 1, linetype =  "dashed") +
  labs(x = bquote('Aboveground biomass ('*g~m^-2*')'), y = bquote('Predicted aboveground biomass ('*g~m^-2*')')) +
  theme(legend.position = "top") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)) 


#Allometric equation
allometric_df <- read.csv("../Field data/July_2021/biomass_shoots_height.csv", 
                          header = TRUE)

allometric.mod <- lm(Mass ~ 0 + Height + I(Height^2), allometric_df)

eq <- paste0('paste(Biomass, " = ",~',  round(allometric.mod$coefficients[1], 5), '*italic(Height)~+~',
             round(allometric.mod$coefficients[2], 5), '*italic(Height)^2~', 
             '~R^2~ "="~', round(100*summary(allometric.mod)$r.squared, 2), ")")


fig_allometric <- ggplot(allometric_df, aes(x = Height, y = Mass)) + geom_point(size = 1) +
  theme_bw() + labs(x = "Shoot height (cm)", y = "Shoot biomass (g)") +
  stat_smooth(method = "lm", formula = y ~ 0 + x + I(x^2),
              size = 1, se = FALSE, show.legend = TRUE) +
  geom_text(aes(x = min(Height) - 0.0 * diff(range(Height)), 
                y = max(Mass) + 0.1 * diff(range(Mass)), label = eq), 
            parse = TRUE, size = 3.5, check_overlap = TRUE, hjust = 0) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) 
fig_allometric

##Biomass ~ Distance creek relation. Calculate 25, and 75 percentiles
k <- (max(biomass_df$biomass) - min(biomass_df$biomass))/100
median_table_distance_creek <- tibble(id = 1:100, distance_creek = 1:100, biomass = 1:100,
                                      distance_creek_25 = 1:100, distance_creek_75 = 1:100)

for(i in 1:100){
  temp_df <- biomass_df[(biomass_df$biomass>(min(biomass_df$biomass)+k*(i-1)) &
                           biomass_df$biomass<(min(biomass_df$biomass)+k*(i))),]
  median_table_distance_creek$biomass[i] <- median(temp_df$biomass)
  median_table_distance_creek$distance_creek_25[i] <- quantile(temp_df$distance_creek, probs = 0.25)
  median_table_distance_creek$distance_creek[i] <- median(temp_df$distance_creek)
  median_table_distance_creek$distance_creek_75[i] <- quantile(temp_df$distance_creek, probs = 0.75)
}


fig_distance <- ggplot() + geom_point(aes(x = median_table_distance_creek$distance_creek,
                                          y = median_table_distance_creek$biomass),
                                      alpha = 0.5, size = 1) +
  theme_bw() + geom_errorbar(aes(x = median_table_distance_creek$distance_creek, 
                                 y = median_table_distance_creek$biomass,
                                 xmin = median_table_distance_creek$distance_creek_25,
                                 xmax = median_table_distance_creek$distance_creek_75),
                             width = 0.001, alpha = 0.5) +
  labs(x = "Distance Tidal Creek (m)", y = bquote('Predicted aboveground biomass ('*g~m^-2*')')) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) 
fig_distance


##Biomass ~ Elevation relation. Calculate 25, and 75 percentiles

k <- (max(biomass_df$biomass) - min(biomass_df$biomass))/100
median_table_DEM <- tibble(id = 1:100, DEM = 1:100, biomass = 1:100,
                           DEM_25 = 1:100, DEM_75 = 1:100)

for(i in 1:100){
  temp_df <- biomass_df[(biomass_df$biomass>(min(biomass_df$biomass)+k*(i-1)) &
                           biomass_df$biomass<(min(biomass_df$biomass)+k*(i))),]
  median_table_DEM$biomass[i] <- median(temp_df$biomass)
  median_table_DEM$DEM_25[i] <- quantile(temp_df$DEM, probs = 0.25)
  median_table_DEM$DEM[i] <- median(temp_df$DEM)
  median_table_DEM$DEM_75[i] <- quantile(temp_df$DEM, probs = 0.75)
}

fig_DEM <- ggplot() + geom_point(aes(x = median_table_DEM$DEM,
                                     y = median_table_DEM$biomass),
                                 alpha = 0.5, size = 1) +
  theme_bw() + geom_errorbar(aes(x = median_table_DEM$DEM, 
                                 y = median_table_DEM$biomass,
                                 xmin = median_table_DEM$DEM_25,
                                 xmax = median_table_DEM$DEM_75),
                             width = 0.001, alpha = 0.5) +
  labs(x = "DEM NAVD88 (m)", y = bquote('Predicted aboveground biomass ('*g~m^-2*')')) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) 


tiff("../Paper/Figures/Fig_4_R.tiff", res = 300, width = 3000, height = 3000)
ggarrange(fig_allometric, fig_prediction, fig_distance, fig_DEM, 
          nrow = 2, ncol = 2, align = "hv", common.legend = T, legend = "top")
dev.off()


##Density plot. Frequency of predicted AG biomass

tiff("../Paper/Figures/Fig_4_R2.tiff", res = 300, height = 900, width = 2000)
ggplot(biomass_df, aes(x = biomass)) + geom_density() + 
  theme_bw() + theme(axis.title = element_text(size = 20), 
                     axis.text = element_text(size = 16)) +
  labs(y = "Density", x = bquote('Predicted aboveground biomass ('*g~m^-2*')'))
dev.off()



####Figure of restoration /grass planting in Maryville, SC

setwd(wd)

tidal_distance_raster <- raster(paste0(wd, "Distance_Creek/distance_raster_modified_m_WAshley.tif"))

field_data <- read.csv("Field data/summary_data.csv")

field_data_coord <- field_data[,c(9,8)]
field_data_coord_sp <- SpatialPoints(field_data_coord, proj4string = CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"))

field_data$distance_creek <- extract(x = tidal_distance_raster, y = field_data_coord_sp)

field_data$Treatment <- "Midmarsh"
field_data$Treatment[field_data$distance_creek < 7.5] <- "Creek Bank"
field_data$Treatment[grep(pattern = "2020rep", x = field_data$Transect)] <- "Planted - 2020"
field_data$Treatment[grep(pattern = "2021rep", x = field_data$Transect)] <- "Planted - 2021"

#Remove points without biomass assessment
field_data_curated <- field_data[!is.na(field_data$biomass_total),]

#Remove dieback areas
field_data_curated <- field_data_curated[field_data_curated$biomass_total > 0,]

#Remove May sampling
field_data_curated <- field_data_curated[field_data_curated$Date != "5/27/2021" &
                                           field_data_curated$Date != "5/28/2021",]



#Plot july!
Fig_July <- ggplot(field_data_curated, aes(x = Treatment, y = biomass_total, group = Treatment)) + 
  geom_boxplot() + theme_bw() + labs(y = bquote('Aboveground biomass ('*g~m^-2*')')) + 
  geom_jitter(size = 1, alpha = 0.5) +
  theme(axis.title = element_text(size= 16), axis.text = element_text(size = 14),
        axis.title.x = element_blank(), title = element_text(size = 14), 
        axis.text.x = element_text(angle = 15, vjust = 0.7))  + expand_limits(y = 3100) + 
  labs(title = "July 2021")
Fig_July

Rmisc::summarySE(field_data_curated, measurevar = "biomass_total", groupvars = "Treatment")

#Model for letters

model_July <- lm(biomass_total~Treatment, data = field_data_curated)
anova(model_July)

ref_grid(model_July)
July.mlsm <- emmeans(model_July, ~ "Treatment")
July.mlsm
sign.July<-cld(July.mlsm, alpha=0.05)
sign.July

##Work on September sampling now

data_september <- read.csv("Field data/September_2021/data_summary_september.csv",
                           header = TRUE, skip = 1, stringsAsFactors = FALSE)

data_september$Year <- as.factor(data_september$Year)
data_september$Year <- factor(data_september$Year,
                              levels(data_september$Year)[c(4,5,1,2,3)])

Fig_September <- ggplot(data_september, aes(x = Year, y = Biomass_total, group = Year)) + 
  geom_boxplot() + theme_bw() + labs(y = bquote('Aboveground biomass ('*g~m^-2*')')) + 
  geom_jitter(size = 1, alpha = 0.5) +
  theme(axis.title = element_blank(), axis.text = element_text(size = 14),
        title = element_text(size = 14), axis.text.x = element_text(angle = 15, vjust = 0.7)) +
  expand_limits(y = 3100) + labs(title = "September 2021")

Fig_September

model_Sept <- lm(Biomass_total~Year, data = data_september)
anova(model_Sept)

ref_grid(model_Sept)
Sept.mlsm <- emmeans(model_Sept, ~ "Year")
Sept.mlsm
sign.Sept<-cld(Sept.mlsm, alpha=0.05)
sign.Sept

#Plot!
tiff("Paper/Figures/Fig5.tiff", res = 300, width = 3000, height = 1500)
ggarrange(Fig_July, Fig_September, ncol = 2, nrow = 1)
dev.off()


####MEM modelling plot / Fig. 6

setwd(wd)


mem_df <- read.csv("mem_results_summary_TLP_122.csv")
names(mem_df)[1] <- "Year"
mem_df$Optimal_biomass <- as.factor(mem_df$Optimal_biomass)
mem_df$Optimal_biomass <- factor(mem_df$Optimal_biomass,
                                 levels = c("0", "500", "1000", "2000", "Mean Sea Level"))

mem_df$Treatment <- gsub("None", "No~intervention", mem_df$Treatment)
mem_df$Treatment <- gsub("TLP", "Thin~Layer~Placement", mem_df$Treatment)

mem_df$SLR_100 <- gsub("y-1", "y^-1", mem_df$SLR_100)
mem_df$SLR_100 <- gsub(" ", "~", mem_df$SLR_100)
mem_df$SLR_100 <- as.factor(mem_df$SLR_100)
mem_df$SLR_100 <- factor(mem_df$SLR_100, 
                         levels = c("60~cm~100~y^-1", "122~cm~100~y^-1"))


#Figure elevation
fig_elevation <- ggplot(mem_df, aes(x = Year, y = marshE.cm.NAVD, color = Optimal_biomass)) +
  facet_wrap(~Treatment*SLR_100, nrow = 1, ncol = 4, labeller = label_parsed) + 
  labs(y = expression(atop("Marsh elevation", paste("NAVD88 cm"))), 
       color = bquote('Optimal biomass'~(g~m^-2~y^-1)))  + 
  theme_bw() + geom_line() +
  theme(legend.title = element_text(size= 12), legend.text = element_text(size = 10),
        axis.title = element_text(size = 12), axis.title.x = element_blank(), 
        strip.text.x = element_text(margin = ggplot2::margin(0,0,0,0, "cm"), size = 10),
        axis.text = element_text(size = 10))
fig_elevation

#Fig elevation MSL

fig_elevation_MLS <- ggplot(mem_df, aes(x = Year, y = Marsh.E.rel.MSL..cm., color = Optimal_biomass)) +
  facet_wrap(~Treatment*SLR_100, nrow = 1, ncol = 4, labeller = label_parsed) + 
  labs(y = expression(atop("Marsh elevation", paste("MSL cm"))), 
       color = bquote('Optimal biomass'~(g~m^-2~y^-1)))  + 
  theme_bw() + geom_line() +
  theme(legend.title = element_text(size= 12), legend.text = element_text(size = 10),
        axis.title = element_text(size = 12), axis.title.x = element_blank(), 
        strip.text.x = element_text(margin = ggplot2::margin(0,0,0,0, "cm"), size = 10),
        axis.text = element_text(size = 10))
fig_elevation_MLS


#Figure biomass
fig_biomass <- ggplot(mem_df, aes(x = Year, y = Standing.Bio.g.m2, color = Optimal_biomass)) + 
  facet_wrap(~Treatment*SLR_100, nrow = 1, ncol = 4, labeller = label_parsed) +
  labs(y = expression(atop("Aboveground biomas", paste(g~m^-2~y^-1))),
       color = bquote('Optimal biomass'~(g~m^-2~y^-1))) +
  theme_bw() + geom_line() + 
  theme(legend.title = element_text(size= 12), legend.text = element_text(size = 10),
        strip.text.x = element_text(margin = ggplot2::margin(0,0,0,0, "cm"), size = 10),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10))
fig_biomass


tiff("Fig6.tiff", res = 300, height = 1850, width = 2500)
ggarrange(fig_elevation, fig_elevation_MLS, fig_biomass, ncol = 1, nrow = 3, align = "v",
          common.legend = TRUE, legend = "top")
dev.off()