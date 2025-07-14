setwd("/Users/Desktop/")
library(stringr) 
library(circlize)      
library(grid)    

##命令传递
sample_name <- 'H22B656' 
ref_name <- 'H22B656'    

genome_gff <- 'line.gff'  
snp_vcf <- 'snp_line.vcf'    
depth_base_stat <- 'line.depth_base.txt'    #
seq_split <- 2000    

out_dir = 'output'    
if (!file.exists(out_dir)) dir.create(out_dir)

depth_base <- read.delim(depth_base_stat, stringsAsFactors = FALSE)
genome_size <- sum(depth_base$seq_end - depth_base$seq_start + 1) 
genome_GC <- round(mean(depth_base$GC), 2)

depth_exist <- subset(depth_base, depth != 0)
coverage <- round(100 * sum(depth_exist$seq_end - depth_exist$seq_start + 1) / genome_size, 2)
average_depth <- round(mean(depth_base$depth), 0)

seq_stat <- NULL
for (seq_id in unique(depth_base$seq_ID)) seq_stat <- rbind(seq_stat, c(seq_id, 1, max(subset(depth_base, seq_ID == seq_id)$seq_end)))
seq_stat <- data.frame(seq_stat, stringsAsFactors = FALSE)
colnames(seq_stat) <- c('seq_ID', 'seq_start', 'seq_end')
rownames(seq_stat) <- seq_stat$seq_ID
seq_stat$seq_start <- as.numeric(seq_stat$seq_start)
seq_stat$seq_end <- as.numeric(seq_stat$seq_end)

write.table(seq_stat, str_c(out_dir, '/', sample_name, '.genome_stat.txt'), row.names = FALSE, sep = '\t', quote = FALSE)

#读取 vcf 文件，统计 SNP 类型
snp <- read.delim(snp_vcf, header = FALSE, colClasses = 'character', comment.char = '#')[c(1, 2, 4, 5)]
snp$V2 <- as.numeric(snp$V2)
snp$change <- str_c(snp$V4, snp$V5)

change <- which(snp$change == 'AT')
snp[change,'type1'] <- 'A>T|T>A'; snp[change,'type2'] <- 'tv'
change <- which(snp$change == 'AG')
snp[change,'type1'] <- 'A>G|T>C'; snp[change,'type2'] <- 'ti'
change <- which(snp$change == 'AC')
snp[change,'type1'] <- 'A>C|T>G'; snp[change,'type2'] <- 'tv'

change <- which(snp$change == 'TA')
snp[change,'type1'] <- 'A>T|T>A'; snp[change,'type2'] <- 'tv'
change <- which(snp$change == 'TG')
snp[change,'type1'] <- 'A>C|T>G'; snp[change,'type2'] <- 'tv'
change <- which(snp$change == 'TC')
snp[change,'type1'] <- 'A>G|T>C'; snp[change,'type2'] <- 'ti'

change <- which(snp$change == 'GA')
snp[change,'type1'] <- 'G>A|C>T'; snp[change,'type2'] <- 'ti'
change <- which(snp$change == 'GT')
snp[change,'type1'] <- 'G>T|C>A'; snp[change,'type2'] <- 'tv'
change <- which(snp$change == 'GC')
snp[change,'type1'] <- 'G>C|C>G'; snp[change,'type2'] <- 'tv'

change <- which(snp$change == 'CA')
snp[change,'type1'] <- 'G>T|C>A'; snp[change,'type2'] <- 'tv'
change <- which(snp$change == 'CT')
snp[change,'type1'] <- 'G>A|C>T'; snp[change,'type2'] <- 'ti'
change <- which(snp$change == 'CG')
snp[change,'type1'] <- 'G>C|C>G'; snp[change,'type2'] <- 'tv'

snp_ti <- length(which(snp$type2 == 'ti'))
snp_tv <- length(which(snp$type2 == 'tv'))

snp_at <- length(which(snp$type1 == 'A>T|T>A'))
snp_ag <- length(which(snp$type1 == 'A>G|T>C'))
snp_ac <- length(which(snp$type1 == 'A>C|T>G'))
snp_ga <- length(which(snp$type1 == 'G>A|C>T'))
snp_gt <- length(which(snp$type1 == 'G>T|C>A'))
snp_gc <- length(which(snp$type1 == 'G>C|C>G'))

#统计 SNP 密度
snp <- snp[c(1, 2, 5, 6, 7)]
colnames(snp)[1:2] <- c('seq_ID', 'seq_site')

snp_stat <- NULL
seq_ID <- unique(snp$seq_ID)

for (seq_ID_n in seq_ID) {
  snp_subset <- subset(snp, seq_ID == seq_ID_n)
  seq_end <- seq_split
  snp_num <- 0
  
  for (i in 1:nrow(snp_subset)) {
    if (snp_subset[i,'seq_site'] <= seq_end) snp_num <- snp_num + 1
    else {
      snp_stat <- rbind(snp_stat, c(seq_ID_n, seq_end - seq_split + 1, seq_end, snp_num))
      
      seq_end <- seq_end + seq_split
      snp_num <- 0
      while (snp_subset[i,'seq_site'] > seq_end) {
        snp_stat <- rbind(snp_stat, c(seq_ID_n, seq_end - seq_split + 1, seq_end, snp_num))
        seq_end <- seq_end + seq_split
      }
      snp_num <- snp_num + 1
    }
  }
  
  while (seq_end < seq_stat[seq_ID_n,'seq_end']) {
    snp_stat <- rbind(snp_stat, c(seq_ID_n, seq_end - seq_split + 1, seq_end, snp_num))
    seq_end <- seq_end + seq_split
    snp_num <- 0
  }
  snp_stat <- rbind(snp_stat, c(seq_ID_n, seq_end - seq_split + 1, seq_stat[seq_ID_n,'seq_end'], snp_num))
}

snp_stat <- data.frame(snp_stat, stringsAsFactors = FALSE)
names(snp_stat) <- c('seq_ID', 'seq_start', 'seq_end', 'snp_num')
snp_stat$seq_start <- as.numeric(snp_stat$seq_start)
snp_stat$seq_end <- as.numeric(snp_stat$seq_end)
snp_stat$snp_num <- as.numeric(snp_stat$snp_num)

write.table(snp_stat, str_c(out_dir, '/', sample_name, '.snp_stat.txt'), row.names = FALSE, sep = '\t', quote = FALSE)


pdf(str_c(out_dir, '/', sample_name, '.circlizeL.pdf'), width = 14, height = 8)
circle_size = unit(1, "snpc")
circos.par(gap.degree = 2)
circos.genomicInitialize(seq_stat, plotType = 'axis')

circos.track(
  ylim = c(0, 1), track.height = 0.05, bg.border = NA, bg.col = '#83ddc9',
  panel.fun = function(x, y) {
    xlim = CELL_META$xlim
    ylim = CELL_META$ylim
    seq_ID = CELL_META$sector.index
  } )

circos.genomicTrack(
  depth_base[c(1:3, 5)], track.height = 0.08, bg.col = '#EEEEEE6E', bg.border = NA,
  panel.fun = function(region, value, ...) {
    circos.genomicLines(region, value, col = 'blue', lwd = 0.35, ...)
    circos.lines(c(0, max(region)), c(genome_GC, genome_GC), col = 'blue2', lwd = 0.15, lty = 2)
    circos.yaxis(labels.cex = 0.2, lwd = 0.1, tick.length = convert_x(0.15, 'mm'))
  } )

circos.genomicTrack(
  depth_base[1:4], track.height = 0.08, ylim = c(0, (max(depth_base$depth) + 1)), bg.col = '#EEEEEE6E', bg.border = NA,
  panel.fun = function(region,value, ...) {
    circos.genomicRect(region, value, ytop.column = 1, ybottom = 0, border = '#F8E27C', lwd = 0.02, col = '#F8E27C', ...)
    circos.lines(c(0, max(region)), c(average_depth, average_depth), col = 'red3', lwd = 0.15, lty = 2)
    circos.yaxis(labels.cex = 0.2, lwd = 0.1, tick.length = convert_x(0.15, 'mm'))
  } )

value_max <- max(snp_stat$snp_num)
colorsChoice <- colorRampPalette(c('white', "#26B49D"))
color_assign <- colorRamp2(breaks = c(0:value_max), col = colorsChoice(value_max + 1))

circos.genomicTrackPlotRegion(
  snp_stat, track.height = 0.12, stack = TRUE, bg.border = NA,
  panel.fun = function(region, value, ...) {
    circos.genomicRect(region, value, col = color_assign(value[[1]]), border = NA, ...)
  } )


dev.off()


