setwd("/Users/liuzhao/Desktop/code/trans_heavy/data") 

library(raster)
library(sp)
library(ggplot2)

read_mresi_data <- function(year) {
  # 构建文件名
  filename <- sprintf("MRSEI%d.tif", year)
  # 检查文件是否存在
  if (!file.exists(filename)) {
    print(paste("文件不存在:", filename))
    return(NULL)
  }
  # 读取raster数据
  raster(filename)
}
calculate_centroid <- function(raster_data) {
  # 提取有效值的索引
  valid_cells <- which(raster_data[] > 0, arr.ind = TRUE)
  
  # 如果有有效值，则计算重心
  if (length(valid_cells) > 0) {
    # 提取有效单元格的坐标
    coords <- xyFromCell(raster_data, valid_cells)
    
    # 计算加权重心
    values <- raster_data[valid_cells]
    weighted_mean_x <- sum(coords[,1] * values) / sum(values)
    weighted_mean_y <- sum(coords[,2] * values) / sum(values)
    
    return(c(weighted_mean_x, weighted_mean_y))
  } else {
    return(NA)
  }
}

# 初始化保存重心坐标的数据框
centroids <- data.frame(year=integer(), x=numeric(), y=numeric())
# 循环处理2000年至2005年的数据
for (year in 2000:2005) {
  # 读取数据
  r <- read_mresi_data(year)
  
  # 如果数据有效，则计算重心
  if (!is.null(r)) {
    centroid <- calculate_centroid(r)
    
    # 检查重心是否有效
    if (!all(is.na(centroid))) {
      # 添加到数据框
      centroids <- rbind(centroids, data.frame(year=year, x=centroid[1], y=centroid[2]))
    } else {
      print(paste("数据无效或缺失于年份:", year))
    }
  }
}

# 导出重心坐标
write.csv(centroids, "centroids.csv", row.names=FALSE)
centroids

# 绘制重心转移图
ggplot(centroids, aes(x=x, y=y)) +
  geom_point() +
  geom_text(aes(label=year), hjust=-0.1, vjust=0) +
  geom_path(aes(group=1)) +
  theme_minimal() +
  labs(title="MRESI_centroids", x="X", y="Y")
