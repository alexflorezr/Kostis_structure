# Script to extract sequences for each time bin

# Required packages
library(seqinr)
# Upload the database
setwd("~/Desktop/PhD/Thesis/Raw_data/Clean_database/")
Dataset <- read.delim(file="DATABASE_19-05-16.txt", sep = "\t", stringsAsFactors = F)
Species <- unique(Dataset$Species)
# this script is an example for Mammuthus primigenius using 4000 years time bins
setwd("~/Desktop/PhD/Thesis/Chapter3/Test_mp/mp_fasta_NO_aligned/")
Dataset_mp <- Dataset[Dataset$Species == "Mammuthus_primigenius",]
Bins <- seq(2000, 54000, 4000)
sp <- "mp" 
for(bin in 1:(length(Bins)-2)){
        temp_bin <-  subset(Dataset_mp, Dataset_mp$Mean_Age > Bins[bin] & Dataset_mp$Mean_Age <= Bins[bin+1])
        temp_bin_seq <- subset(temp_bin, nchar(temp_bin$Sequence) > 1)
        temp_fasta_names <- paste(sp, temp_bin_seq$Accession_GB, sprintf("%02d", temp_bin_seq$Repeat), temp_bin_seq$Country, sep = "_")
        # remove non-ACGT nucleotides from the sequences
        for(seq in seq_along(temp_bin_seq$Sequence)){
                temp_seq <- temp_bin_seq$Sequence[seq]
                list_seq <- strsplit(temp_seq, split = "")
                temp_nuc <- sapply(list_seq, unique)
                if (is.element(FALSE, is.element(temp_nuc, c("A", "C", "G", "T")))){
                        bad_nuc <- temp_nuc[which(!is.element(temp_nuc, c("A", "C", "G", "T")))]
                        for (bad in seq_along(bad_nuc)){
                                list_seq[[1]][which(list_seq[[1]] == bad_nuc[bad])] <- ""
                        }
                        temp_bin_seq$Sequence[seq] <- paste(as.vector(list_seq[[1]]), collapse="")
                }
        }
        write.fasta(as.list(temp_bin_seq$Sequence), names = temp_fasta_names, file.out = paste(sp, "_", Bins[bin],"_",Bins[bin+1], ".fasta", sep=""), open="w")
}