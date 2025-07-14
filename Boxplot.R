
library(ggplot2)

setwd("/Users/desktop")

df <- read.table("1.txt", header = TRUE, sep = "\t", na.strings = "NA")

df$group <- factor(df$group, levels = unique(df$group))

p <- ggplot(df, aes(x = group, y = value)) +
  geom_boxplot(aes(fill = "#cccccc"), outlier.shape = NA, alpha = 0.4) +  
  scale_fill_manual(values = "#cccccc") + 
  geom_jitter(color = "#cccccc", size = 2.5, width = 0.2, alpha = 0.85) +  
  facet_wrap(~index, scales = "free") + 
  scale_y_continuous(limits = c(0.3, 2.3)) +
  
  theme_bw() +  
  theme(
    legend.position = "none",  
    strip.background = element_blank(),  
    strip.text = element_text(size = 12)  
  )
   
p