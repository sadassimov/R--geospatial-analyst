# 设置工作目录
setwd("/Users/liuzhao/Desktop/code/partial_corr") 
library(terra)
library(ppcor)
# 读取栅格数据
hfp <- rast(dir("./data/hfp", full.names = TRUE, pattern = '.tif$'))
MRSEI <- rast(dir("./data/MRSEI", full.names = TRUE, pattern = '.tif$'))
pre <- rast(dir("./data/pre", full.names = TRUE, pattern = '.tif$'))
tem <- rast(dir("./data/tem", full.names = TRUE, pattern = '.tif$'))
# 合并处理后的栅格数据
z <- c(MRSEI, pre, tem, hfp)
# 定义计算偏相关的函数
fun_cor = function(cell_values) {
  # 如果任何一个值是 NA，则返回 NA
  if (any(is.na(cell_values))) {
    return(c(NA, NA)) 
  } else {
    # 尝试执行偏相关计算，处理可能的错误
    tryCatch({
      Rs = ppcor::pcor.test(cell_values[1:6], cell_values[7:12], list(cell_values[13:18], cell_values[19:24]))
      Rx = Rs$estimate
      Px = Rs$p.value
      return(c(Rx, Px))
    }, error = function(e) {
      return(c(NA, NA))  # 如果出现错误，返回 NA
    })
  }
}
# 应用函数
MRSEI_pre <- terra::app(z, fun_cor, cores=8)
# 输出
writeRaster(MRSEI_pre[[1]], filename="/Users/liuzhao/Desktop/code/partial_corr/MRSEI_pre_pcor_coefficient.tif", overwrite=TRUE)
writeRaster(MRSEI_pre[[2]], filename="/Users/liuzhao/Desktop/code/partial_corr/MRSEI_pre_p_value.tif", overwrite=TRUE)
# 分别绘制每个层
plot(MRSEI_pre[[1]], main="MRSEI_pre_Partial Correlation Coefficient")
plot(MRSEI_pre[[2]], main="MRSEI_pre_P-value of Partial Correlation")
