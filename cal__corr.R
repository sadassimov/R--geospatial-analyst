# 设置工作目录
setwd("/Users/liuzhao/Desktop/code/partial_corr") 
library(terra)
library(Hmisc)
hfp <- rast(dir("./data/hfp",full.names = T,pattern = '.tif$'))
MRSEI <- rast(dir("./data/MRSEI",full.names = T,pattern = '.tif$'))
pre <- rast(dir("./data/pre",full.names = T,pattern = '.tif$'))
tem <- rast(dir("./data/tem",full.names = T,pattern = '.tif$'))

z = c(MRSEI,tem)
names(z)
nlyr(z)
fun_cor =  function(x) {
  Rs = Hmisc::rcorr(x[1:6], x[7:12], type = "spearman")
  Rx = Rs$r[2]
  Px = Rs$P[2]
  return(c(Rx, Px))
}
r_MRSEI_tem = app(z, fun_cor, cores=4)
names(r_MRSEI_tem) <- c('coefficient','p_value')
plot(r_MRSEI_tem)

# 保存相关系数和P值到文件
writeRaster(r_MRSEI_tem$coefficient, "coefficient.tif", overwrite=TRUE)
writeRaster(r_MRSEI_tem$p_value, "p_value.tif", overwrite=TRUE)