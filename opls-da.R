rm(list = ls())
library(tidyverse)
library(ropls)
library(openxlsx)
options(scipen = 9999)
#loading and reshaping data
setwd("/Users/olaaaa/desktop")
files.csv <- fs::dir_ls("inputfile",recurse = TRUE,glob = "*.csv")
dat.group <-  map_dfc(files.csv,read_csv) 
files.xlsx <- fs::dir_ls("inputfile",recurse = TRUE,glob = "*.xlsx")
dat <- map_dfc(files.xlsx,openxlsx::read.xlsx) %>% 
  select(c('variables',dat.group$sample)) %>% 
  column_to_rownames(var ='variables') %>% 
  t()
# 创建输出目录（如果不存在）
if (!dir.exists("outputfile")) {
  dir.create("outputfile")
}
# OPLSDA
plsda<-opls(dat, dat.group$group,
            predI = 1, orthoI = 1,#orthoI = 1:opls-da; orthoI = 0:Pls-da
            log10L = F,
            crossvalI = nrow(dat),
            scaleC="pareto",#pareto scaling
            fig.pdfC = c("none", "interactive", "outputfile/plsda.pdf")[3],
            permI=200)

#extracting data of VIP:
vip <-  plsda@vipVn%>% as.data.frame() %>% 
  rename("VIP"=".") %>% rownames_to_column(var = 'variables')

dat_vip <- map_dfc(files.xlsx,openxlsx::read.xlsx) %>% 
  select(c('variables',dat.group$sample)) %>% 
  merge(vip,by="variables")

dat_vip %>% write_csv("outputfile/opls-daResult.csv")

##散点图可视化
# data and score data
## score data
plsdaScore <- data.frame(
  t1 =plsda@scoreMN,
  to1 =plsda@orthoScoreMN 
) %>% scale(center = T,scale = T) %>% 
  as.data.frame() %>% 
  rename(
    "t1"="p1",
    "to1"="o1"
  ) %>% 
  rownames_to_column(var = 'sample') %>% 
  merge(dat.group,by="sample")

t1Weight=sprintf("%.1f%%", plsda@modelDF[1,1] * 100);t1Weight
to1Weight=sprintf("%.1f%%", plsda@modelDF[2,1] * 100);to1Weight

R2X=plsda@modelDF[1,1]+plsda@modelDF[2,1]
R2Y=plsda@modelDF[1,3]+plsda@modelDF[2,3]
Q2Y=plsda@modelDF[1,6]+plsda@modelDF[2,6]

subTitle <- paste0("R2X=",R2X,"  R2Y=",R2Y,"  Q2Y=",Q2Y)
## opls-da  aes(shape = group),
oplsdaFig <- ggplot(plsdaScore, aes(x = t1, y = to1, color = group)) +
  geom_point(size = 3) +
  stat_ellipse(aes(fill = group), alpha = 0.2, geom = "polygon", level = 0.80) + # 缩小置信区间，level值越小置信区间越窄
  theme_classic() +
  scale_fill_manual(values = c("#78BBD5", "#8CAED4")) +
  scale_color_manual(values = c("#78BBD5", "#8CAED4")) +
  labs(title = "OPLS-DA", 
       subtitle = subTitle, 
       x = paste0("t1(", t1Weight, ")"),
       y = paste0("to1(", to1Weight, ")")) +
  xlim(-3, 3) + # 控制x轴范围
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.2)), 
    plot.subtitle = element_text(hjust = 0.5, size = rel(0.6)) 
  )
#saving
ggsave("outputfile/oplsda.png", width = 4, height = 4)
print(oplsdaFig)
ggsave("outputfile/oplsda.pdf", width = 4, height = 4,onefile=F)
print(oplsdaFig)
dev.off()     

