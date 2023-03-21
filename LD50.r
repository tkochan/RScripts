#Load drc package
library(drc)
filename <- commandArgs(trailingOnly = TRUE)[1]
output <- commandArgs(trailingOnly = TRUE)[2]
#Read in input data for all experiments (including dose, starting mice, surviving mice)
input <- read.csv(filename, header = TRUE)

#make a list of all unique strains
strains <- unique(input$Strain)

#Create a data frame to hold a summary of all experiments for each strain
mort <- as.data.frame(matrix(nrow=length(strains),ncol=3))
rownames(mort) <- strains
colnames(mort) <- c("dose","total","dead")

#Initialize empty lists for dose, total mice, and mortalities.  This will be a list of vectors, where each vector is the corresponding data for all experiments with a given strain
d <- list()
tot <- list()
m <- list()

# Iterate through each unique strain
# Create a sub-table including just the experiments with that strain
# extract all dose, total mice, and mortality data from these strains to the above vectors
for(strain in strains){
  print(strain)
 # print(which(input$Strain == strain))
  t <- input[which(input$Strain == strain),]
  d[[strain]] <- t[,2]
  tot[[strain]] <- t[,5]
  m[[strain]] <- t[,4]
}

# Use the dose, total mice, and mortality lists to fill the summary data frame
mort$dose <- d
mort$total <- tot
mort$dead <- m

#Create a data frame to hold LD50 and SD for each strain
#also create a data frame to hold LD50s and SD if calculated using non-log10 transformed doses
LD50s <- as.data.frame(matrix(nrow=length(strains),ncol=2))
colnames(LD50s) <- c("LD50","SD")
LD50s_nonlog <- LD50s

# For each strain extract the dose, total mice, and mortality information from the mort table
# Try to fit to bionomial distribution using drc (drm) - if possible put in LD50s table
# Done with logDose = 10 as well to compare to case when you convert back to raw dose becore fitting to model
for(strain in strains){
  doses <- unlist(mort[strain,"dose"])
  total_mice <- unlist(mort[strain,"total"])
  mortalities <- unlist(mort[strain,"dead"])
  print(strain)
  try({
    model <- drm(mortalities/total_mice ~ doses, weights = total_mice, fct = LL.2(), type = "binomial")
    LD50s[strain,"LD50"] <- ED(model,50)[1]
    LD50s[strain,"SD"] <- ED(model,50)[2]

  })
}

LD50s <- LD50s[!is.na(LD50s$LD50), ]

write.csv(LD50s, file=output)
