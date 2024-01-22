setwd("/Users/liuzhao/Desktop/code/hubei_landuse/data") 
library(terra)
library(openxlsx)
library(ggplot2)
library(ggalluvial)
library(dplyr)
landuse_2000 <- rast(("CLCD_v01_2000_albert_hubei.tif"))
landuse_2005 <- rast(("CLCD_v01_2005_albert_hubei.tif"))
landuse_2010 <- rast(("CLCD_v01_2010_albert_hubei.tif"))
landuse_2015 <- rast(("CLCD_v01_2015_albert_hubei.tif"))
landuse_2020 <- rast(("CLCD_v01_2020_albert_hubei.tif"))
# 地物类别映射
land_cover_types <- c("1" = "Cropland", "2" = "Forest", "3" = "Shrub",
                      "4" = "Grassland", "5" = "Water",
                      "7" = "Barren", "8" = "Impervious", "9" = "Wetland")

calculate_transition_matrix_area <- function(raster1, raster2, pixel_area, land_cover_types) {
  values1 <- values(raster1)
  values2 <- values(raster2)
  
  unique_values <- sort(unique(c(values1, values2)))
  unique_values <- unique_values[!is.na(unique_values)]
  
  transition_matrix <- matrix(0, nrow = length(unique_values), ncol = length(unique_values),
                              dimnames = list(land_cover_types[as.character(unique_values)], 
                                              land_cover_types[as.character(unique_values)]))
  
  for (i in seq_along(values1)) {
    if (!is.na(values1[i]) && !is.na(values2[i])) {
      row_index <- which(names(land_cover_types) == as.character(values1[i]))
      col_index <- which(names(land_cover_types) == as.character(values2[i]))
      transition_matrix[row_index, col_index] <- transition_matrix[row_index, col_index] + pixel_area
    }
  }
  
  return(transition_matrix)
}

pixel_area <- 30  # 数据是30m

# 计算转移矩阵
matrix_2000_2005 <- calculate_transition_matrix_area(landuse_2000, landuse_2005, pixel_area, land_cover_types)
matrix_2005_2010 <- calculate_transition_matrix_area(landuse_2005, landuse_2010, pixel_area, land_cover_types)
matrix_2010_2015 <- calculate_transition_matrix_area(landuse_2010, landuse_2015, pixel_area, land_cover_types)
matrix_2015_2020 <- calculate_transition_matrix_area(landuse_2015, landuse_2020, pixel_area, land_cover_types)
print(matrix_2000_2005)
# 保存转移矩阵为Excel文件
write.xlsx(list("2000-2005" = matrix_2000_2005,"2005-2010" = matrix_2005_2010,
                "2010-2015" = matrix_2010_2015,"2015-2020" = matrix_2015_2020
                ), file = "TransitionMatrix.xlsx")

