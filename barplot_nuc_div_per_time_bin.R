# Estimate the nucleotide diversity per time bin
setwd("~/Desktop/PhD/Thesis/Chapter3/Test_mp/mp_fasta_al_ed/")
nuc_div_bin <- as.data.frame(matrix(nrow = 12, ncol=4))
colnames(nuc_div_bin) <- c("Bin_starts", "Bin_ends", "ND_pdTRUE", "ND_pdFALSE")
Bins <- seq(2000, 50000, 4000)
nuc_div_bin$Bin_starts <- Bins[1:length(nuc_div_bin$Bin_starts)]
nuc_div_bin$Bin_ends <- Bins[2:(length(nuc_div_bin$Bin_starts)+1)]
for(file in dir()){
        file_time_start <- as.numeric(strsplit(file, split = "_")[[1]][2])
        temp_fasta <- read.dna(file, format = "fasta")
        row <- which(nuc_div_bin$Bin_starts == file_time_start) 
        nuc_div_bin$ND_pdTRUE[row] <- nuc.div(temp_fasta, pairwise.deletion = T)
        nuc_div_bin$ND_pdFALSE[row] <- nuc.div(temp_fasta, pairwise.deletion = F)
}
par(mar=c(4,4,4,4))
barplot(nuc_div_bin$ND_pdTRUE, names.arg = nuc_div_bin$Bin_starts, las=2, col="#838B8B90", border="white", add=T)
barplot(nuc_div_bin$ND_pdFALSE, names.arg = nuc_div_bin$Bin_starts, las=2, col="#8B735590", border="white")

