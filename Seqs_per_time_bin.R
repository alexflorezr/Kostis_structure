setwd()
Dataset <- read.delim(file.choose(), sep = "\t", stringsAsFactors = F)
Dataset <- read.delim(file="~/Desktop/PhD/Thesis/Raw_data/Clean_database/DATABASE.txt", sep = "\t", stringsAsFactors = F)
Species <- unique(Dataset$Species)
Bins <- seq(0, 50000, 10000)
seqs_per_bin <- as.data.frame(matrix(data=as.numeric(), nrow = length(Species), ncol = length(Bins)))
colnames(seqs_per_bin) <- Bins
row.names(seqs_per_bin) <- Species
for(sp in seq_along(Species)){
        Dataset_sp <- Dataset[which(Dataset$Species == Species[sp] & nchar(Dataset$Sequence) > 0),]
        if (dim(Dataset_sp)[1] > 0){
                for (bin in seq_along(Bins)){
                seqs_per_bin[sp,bin]  <- sum(Dataset_sp$Mean_Age > Bins[bin] & Dataset_sp$Mean_Age < Bins[bin+1])
                }
        }else{
                seqs_per_bin[sp,] <- NA
        }      
}
### create a z matrix from the x,y matrix
seqs_per_bin_category <- seqs_per_bin[,-length(Bins)]
        seqs_per_bin_category[is.na(seqs_per_bin_category)] <- -1
        seqs_per_bin_category[seqs_per_bin_category  >=0 & seqs_per_bin_category <= 4] <- -2
        seqs_per_bin_category[seqs_per_bin_category  >=5 & seqs_per_bin_category <= 7] <- -3
        seqs_per_bin_category[seqs_per_bin_category  >=8 & seqs_per_bin_category <= 10] <- -4
        seqs_per_bin_category[seqs_per_bin_category  >10 ] <- -5
        
### plot
par(mar=c(6,10,1,2))
image(t(as.matrix(seqs_per_bin_category)), col=rev(c("#FFFFFF", "#C1CDCD", "#DEB887", "#FF7F00", "#8B7355")), xaxs = "i", axes=F, xlim=c(-0.03,1.3))
unit_x <- 1/length(Bins[-1])
min_x <- unit_x/2
abline(v=seq(0-min_x,1+min_x, by= 1/length(Bins[-c(1,2)])), col="#FFFFFF")
#axis(1, at = seq(0-min_x,(1+min_x + (unit_x)), by= 1/length(Bins[-c(1,2)])), labels = Bins, las=2)
mtext(Bins, side=1, at = seq(0-min_x,(1+min_x + (unit_x)), by= 1/length(Bins[-c(1,2)])), las=2, cex=0.7)
mtext(Species, side=2, at=seq(0,1, by=1/length(Species[-1])), las=2, cex=0.7)
abline(h=seq(0,1, by=1/length(Species[-1]))+(1/length(Species[-1])/2), col="#FFFFFF")
legend("topright", legend = c("0", "<4", "<7", "<10", ">10"), fill = c("#FFFFFF", "#C1CDCD", "#DEB887", "#FF7F00", "#8B7355"), border ="grey", bty = "n", xjust = 1)

