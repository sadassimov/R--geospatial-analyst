setwd("/Users/liuzhao/Desktop/code/hubei_landuse") 
library(terra)
library(ggplot2)

landuse_stack <- rast(dir("./data", full.names = TRUE, pattern = '.tif$'))
plot(landuse_stack)

calculateChangeCount <- function(stack) {
  app(stack, fun = function(x) {
    changes = 0
    for (i in 1:(length(x) - 1)) {
      changes <- changes + (x[i] != x[i + 1])
    }
    return(changes)
  })
}
#算每个像素在栅格堆栈中的变化次数
change_count <- calculateChangeCount(landuse_stack)
plot(change_count)
writeRaster(change_count, filename = "landuse_change_count.tif",  overwrite = TRUE)