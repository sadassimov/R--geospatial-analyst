setwd("/Users/liuzhao/Desktop/code/hubei_landuse/data") 
library(openxlsx)
library(ggplot2)
library(ggalluvial)
library(dplyr)
library(reshape2)
land_cover_types <- c("1" = "Cropland", "2" = "Forest", "3" = "Shrub",
                      "4" = "Grassland", "5" = "Water",
                      "6" = "Barren", "7" = "Impervious", "8" = "Wetland")
# 从Excel文件中读取转移矩阵
matrix_2000_2005 <- read.xlsx("TransitionMatrix.xlsx", sheet = "2000-2005")
matrix_2005_2010 <- read.xlsx("TransitionMatrix.xlsx", sheet = "2005-2010")
matrix_2010_2015 <- read.xlsx("TransitionMatrix.xlsx", sheet = "2010-2015")
matrix_2015_2020 <- read.xlsx("TransitionMatrix.xlsx", sheet = "2015-2020")
# 定义一个函数来转换宽格式矩阵为长格式
long_format <- function(matrix, period, land_cover_types) {
  # 将矩阵转换为数据框，以便使用melt函数
  matrix_df <- as.data.frame(matrix)
  # 将行名作为新列（源地物类型）
  matrix_df$from <- rownames(matrix_df)
  # 将行名（源地物类型）从数字转换为对应的土地利用类型
  matrix_df$from <- land_cover_types[matrix_df$from]
  # 使用melt函数将宽格式转换为长格式
  df_long <- melt(matrix_df, id.vars = "from", variable.name = "to", value.name = "freq")
  # 添加时间段标签
  df_long$period <- period
  return(df_long)
}

df_2000_2005 <- long_format(matrix_2000_2005, "2000-2005",land_cover_types)
df_2005_2010 <- long_format(matrix_2005_2010, "2005-2010",land_cover_types)
df_2010_2015 <- long_format(matrix_2010_2015, "2010-2015",land_cover_types)
df_2015_2020 <- long_format(matrix_2015_2020, "2015-2020",land_cover_types)

# 绘制每个时间段的 Sankey 图，减少左右两侧空白
plot_sankey <- function(data, title) {
  ggplot(data, aes(axis1 = from, axis2 = to, y = freq)) +
    geom_alluvium(aes(fill = from), width = 0) +  # 减少流的宽度
    geom_stratum(width = 0) +  # 减少分层的宽度
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "lines"),  # 减少图表边距
      legend.position = "bottom"
    ) +
    ggtitle(title) +
    scale_fill_discrete(name = "Land Cover Type")
}

# 绘制四个 Sankey 图
p1 <- plot_sankey(df_2000_2005, "Land Cover Transition (2000-2005)")
p2 <- plot_sankey(df_2005_2010, "Land Cover Transition (2005-2010)")
p3 <- plot_sankey(df_2010_2015, "Land Cover Transition (2010-2015)")
p4 <- plot_sankey(df_2015_2020, "Land Cover Transition (2015-2020)")

print(p1)
print(p2)
print(p3)
print(p4)