library(ggplot2)
library(ggsignif)

setwd("/Users/desktop")

df <- read.table("1.txt", header = T, sep = "\t", na.strings = "NA")


custom_order <- c("group1", "group2", "group3", "group4")
df$group <- factor(df$group, levels = custom_order)  

compaired <- list(c("group4", "group1"),c("group4", "group2"),c("group3", "group4"),c("group3", "group1"),c("group3", "group2"),c("group1", "group2"))


p <- ggplot(df, aes(x = group, y = value)) +
  geom_boxplot(aes(fill = as.numeric(group)), outlier.shape = NA, alpha = 0.6) +  
  geom_jitter(color = "black", size = 2.5, width = 0.2) +  
  facet_wrap(~index, scales = "free") + 
  geom_signif(comparisons = compaired, step_increase = 0.1, map_signif_level = F, test = t.test) + 
  scale_fill_gradientn(colors = c("#EB7E60", "#7AC3DF", "#8FB4DC", "#70CDBE")) + 
  theme_bw() +  
  theme(
    legend.position = "none",  
    strip.background = element_blank(),  
    strip.text = element_text(size = 12)  
  )

p
p <- ggplot(df, aes(x = group, y = value)) +
  geom_boxplot(aes(fill = as.numeric(group)), outlier.shape = NA, alpha = 0.4) +
  
  geom_jitter(aes(color = as.numeric(group)), 
              size = 2.5, width = 0.2) +
  
  facet_wrap(~index, scales = "free") +
  
  geom_signif(
    comparisons = compaired,
    step_increase = 0.1,
    map_signif_level = FALSE,
    test = wilcox.test
  ) +
  
  scale_fill_gradientn(colors = c("#EB7E60", "#7AC3DF", "#8FB4DC", "#70CDBE")) +
  scale_color_gradientn(colors = c("#EB7E60", "#7AC3DF", "#8FB4DC", "#70CDBE")) +
  
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 12)
  )

p
