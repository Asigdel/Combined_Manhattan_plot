# Combined_Manhattan_plot
This is the one way to merge 6 manhattan plots into one
```
*** REMEMBER TO CHANGE THE NUMBER OF CHROMOSOMES ***


require(ggplot2)
require(gridExtra)
require(grid)
require(cowplot)

setwd('C:/FERNANDA/UFU/EDITAIS/CNPQ/UNIVERSAL/2013/NEOGEN/SUINOS/GWAS/blupf90/VOL/var_airemlf90_comSNP/round3')
SOL=read.table("snp_sol")
colnames(SOL)<-c("trait", "effect", "SNP", "Chromosome", "Position", "SNP solution", "weight", "Variance")

## SOL  = snp_sol File output postGS

SNP = data.frame(Chr = SOL$Chromosome, Pos = SOL$Position, Var = SOL$Variance)
SNP$Chr = as.numeric(as.character(SNP$Chr))
SNP$within = seq(1:nrow(SNP))

gap = 0; xx = as.numeric(); x0 = 0

for(w in 1:18){
x1 = table(SNP$Chr)[[w]]
x2 = gap + 0.5 * x1
x3 = x0 + x2
xx[w] = x3
x0 = x3 + 0.5 * x1
}

name = seq(1:18)

coloR <- c(rep(c("blue2","darkorange2"),14),"blue2")
plot1 <- ggplot(SNP, aes(x = within, y = Var, col = factor(Chr))) +
scale_color_manual(values = coloR) + 
geom_point( ) + 
ylim(0,5.0) +
ylab("Genetic Variance (%)")  + xlab("Chromosome") +
theme(axis.title.x = element_text(size = 14)) + 
theme(axis.title.y = element_text(size = 14)) +
theme(axis.text.x  = element_text(size = 12)) + 
theme(axis.text.y  = element_text(size = 12)) +
scale_x_continuous(breaks = xx, labels = name) + theme_bw() +
theme(legend.position = "none", panel.grid.minor.x=element_blank(),
           panel.grid.major.x=element_blank(), 
panel.grid.minor.y=element_blank(),
           panel.grid.major.y=element_blank(),
axis.title.y = element_text(size=14),
axis.title.x = element_text(size=14),
axis.text.x  = element_text(size=10),
axis.text.y  = element_text(size=12))


setwd('C:/FERNANDA/UFU/EDITAIS/CNPQ/UNIVERSAL/2013/NEOGEN/SUINOS/GWAS/blupf90/CONC/var_airemlf90_comSNP/round3')
SOL=read.table("snp_sol")
colnames(SOL)<-c("trait", "effect", "SNP", "Chromosome", "Position", "SNP solution", "weight", "Variance")

## SOL  = snp_sol File output postGS

SNP = data.frame(Chr = SOL$Chromosome, Pos = SOL$Position, Var = SOL$Variance)
SNP$Chr = as.numeric(as.character(SNP$Chr))
SNP$within = seq(1:nrow(SNP))

gap = 0; xx = as.numeric(); x0 = 0

for(w in 1:18){
x1 = table(SNP$Chr)[[w]]
x2 = gap + 0.5 * x1
x3 = x0 + x2
xx[w] = x3
x0 = x3 + 0.5 * x1
}

name = seq(1:18)

coloR <- c(rep(c("blue2","darkorange2"),14),"blue2")
plot2 <- ggplot(SNP, aes(x = within, y = Var, col = factor(Chr))) +
scale_color_manual(values = coloR) + 
geom_point( ) + 
ylim(0,5.0) +
ylab("Genetic Variance (%)")  + xlab("Chromosome") +
theme(axis.title.x = element_text(size = 14)) + 
theme(axis.title.y = element_text(size = 14)) +
theme(axis.text.x  = element_text(size = 12)) + 
theme(axis.text.y  = element_text(size = 12)) +
scale_x_continuous(breaks = xx, labels = name) + theme_bw() +
theme(legend.position = "none", panel.grid.minor.x=element_blank(),
           panel.grid.major.x=element_blank(), 
panel.grid.minor.y=element_blank(),
           panel.grid.major.y=element_blank(),
axis.title.y = element_text(size=14),
axis.title.x = element_text(size=14),
axis.text.x  = element_text(size=10),
axis.text.y  = element_text(size=12))


setwd("C:/FERNANDA/UFU/EDITAIS/CNPQ/UNIVERSAL/2013/NEOGEN/SUINOS/GWAS/blupf90")
tiff("Figure 1.tiff", width = 14, height = 12, units = 'in', res = 300)
tiff("Figure 2.tiff", width = 14, height = 12, units = 'in', res = 300)
plot_grid(plot1,plot2, align = c("hv"), nrow = 2,  
labels = c("A","B"), label_size= 20, label_colour = "darkgreen")
dev.off()
```
