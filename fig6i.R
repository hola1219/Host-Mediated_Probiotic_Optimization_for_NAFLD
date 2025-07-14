library(ggplot2)
library(viridis)

# 设置工作目录
setwd("/Users/olaaaa/desktop")

# 读取数据
aa = read.csv("1.txt", header=TRUE, sep="\t")

# 合并 gene 和 product 信息
aa$gene_product <- paste(aa$gene, ":", aa$product)

# 保持原始顺序
aa$gene_product <- factor(aa$gene_product, levels = unique(aa$gene_product))
aa$group <- factor(aa$group, levels = unique(aa$group))  # 确保分组顺序

# 创建分面气泡图
p_bubble <- ggplot(aa, aes(x = group, y = gene_product)) +  # 重点修改：x轴设为分组
  geom_point(aes(size = Log2FC, color = pvalue), alpha = 0.7) +  # 颜色映射Log2FC值
  facet_grid(cols = vars(group), scales = "free_x", space = "free_x") +  # 三列分面
  scale_color_viridis(option = "magma", direction = -1) +
  theme_bw() +
  scale_y_discrete(position = "right") +
  theme(
    axis.text.y = element_text(hjust = 0),
    axis.title.x = element_blank(),  # 隐藏x轴标题
    strip.background = element_blank(),  # 去除分面标签背景
    panel.spacing = unit(0, "lines")  # 减少分面间距
  ) +
  labs(size = "rich factor", color = "-Log10 pvalue")

# 显示图形
p_bubble


library(ggplot2)

# 设置工作目录
setwd("/Users/olaaaa/desktop")

# 读取数据
aa = read.csv("1.txt", header=TRUE, sep="\t")

# 合并 gene 和 product 信息
aa$gene_product <- paste(aa$gene, ":", aa$product)

# 保持原始顺序
aa$gene_product <- factor(aa$gene_product, levels = unique(aa$gene_product))
aa$group <- factor(aa$group, levels = unique(aa$group))

# 创建分面气泡图（关键修改：使用手动配色方案）
p_bubble <- ggplot(aa, aes(x = group, y = gene_product)) +
  geom_point(aes(size = Log2FC, color = pvalue), alpha = 0.7) +
  facet_grid(cols = vars(group), scales = "free_x", space = "free_x") +
  
  # 修改颜色配置（新增颜色梯度）
  scale_color_gradientn(
    colours = c("#6DCCDD","#EDCAE0","#F494BE","#F9B26C"),
    guide = "colourbar"
  ) +
  
  theme_bw() +
  scale_y_discrete(position = "right") +
  theme(
    axis.text.y = element_text(hjust = 0),
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(0, "lines")
  ) +
  labs(size = "rich factor", color = "-Log10 pvalue")

# 显示图形
p_bubble

