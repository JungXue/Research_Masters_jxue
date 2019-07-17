#-------------------------------------Premeables--------------------------------------------------#

set.seed(12345678)

#setwd("~/Desktop/mon")                                                                  # Home
setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/BoxJellyfish") # Uni


# Install Bioconductor (for genetics packages)----------#
# https://www.bioconductor.org/

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
library(BiocManager)

## Installing ------------------------------------------#

### Genetics packages

BiocManager::install("GOpro")
BiocManager::install("GOexpress")
BiocManager::install("goTools")
BiocManager::install("org.Hs.eg.db")

BiocManager::install("biomaRt")
BiocManager::install("clusterProfiler")

source("https://bioconductor.org/biocLite.R")
biocLite("GO.db")
.libPaths()

# GOpro           # Find the most characteristic gene ontology terms for groups of human genes
# GOexpress       # Visualise microarray and RNAseq data using gene ontology annotations
# biomaRt         # BioMart databases 
# clusterProfiler # statistical analysis and visualization of functional profiles for genes and gene clusters
                  # Guangchuang Yu, Li-Gen Wang, Guang-Rong Yan, Qing-Yu He. DOSE: an R/Bioconductor package for Disease Ontology Semantic and Enrichment analysis. Bioinformatics 2015, 31(4):608-609
# org.Hs.eg.db    # data base

### Other packages

Packages <- c("glue","readxl", "tidyverse")
lapply(Packages, install.packages, character.only = TRUE)

## Loading --------------------------------------------#
install(GO.db)
Packages2 <- c("GOpro", "GOexpress", "biomaRt", "clusterProfiler","goTools", "org.Hs.eg.db", 
               "glue", "readxl", "tidyverse",
               "rjags")

lapply(Packages2, library, character.only = TRUE)
library("GOpro")
install.packages("GO.db")

#----------------------------------Data Cleanning-----------------------------------------------------#

# Import data
### https://www.nature.com/articles/s41467-019-09681-1#Sec32

genedata <- read_excel("41467_2019_9681_MOESM2_ESM.xlsx")
#genedata <- genedata[complete.cases(genedata), ] # get rid of NA in Fold change
genedata[1:20,]
genedata <- genedata[order(genedata$Gene), ] #Sort by genename
genedata[1:20,]

dim(genedata)
summary(genedata)

nRNA  = nrow(genedata)                       # number of sgRNAs
nnRNA = unname(unlist(table(genedata$Gene))) # number of sgRNAs per gene
nGene = length(genenames)                    # number of genes
genenames = names(table(genedata$Gene))      # name of genes
location  = cumsum(nnRNA)-5                  # location of first obs of sgRNA of certain gene

### Find avg of Fold change ---------------------#

table(nRNA)

genedata_sum <- genedata %>% 
  group_by(Gene) %>% 
  summarise(avg_fold_changes = mean(`Fold changes`)) #Use mean value of each gene   ##########???use bayes here and finf P??

genedata_sum <- genedata_sum[order(genedata_sum$avg_fold_changes,decreasing=T),] #Order by change decs

write.csv(genedata_sum, 'genedata_sum.csv') 
genedata_sum = read.csv("genedata_sum.csv")
gebedata_sum = genedata_sum [,-1]

dim(gebedata_sum )
head(genedata_sum)


# source("bayes-t-test.R")

############################################################
#    avg of Fold change
############################################################


head(genedata_sum)
summary(genedata_sum)

### Use 5% sample

num = 1:nrow(genedata_sum)
smp1 = sample(num,round(nrow(genedata_sum)*0.05,0))
sample1 = genedata_sum[smp1,]
summary(sample1)

### Create levels
nlevels = 1:5
names  = paste("Level",nlevels,sep="")
name2s = paste("Level",nlevels,"_members",sep="")
names
name2s

level1 = groupGO(sample1$Gene, keyType = "SYMBOL", ont = "BP", level = 1,OrgDb = org.Hs.eg.db, readable = FALSE) # assign levels
level2 = groupGO(sample1$Gene, keyType = "SYMBOL", ont = "BP", level = 2,OrgDb = org.Hs.eg.db, readable = FALSE)
level3 = groupGO(sample1$Gene, keyType = "SYMBOL", ont = "BP", level = 3,OrgDb = org.Hs.eg.db, readable = FALSE)
level4 = groupGO(sample1$Gene, keyType = "SYMBOL", ont = "BP", level = 4,OrgDb = org.Hs.eg.db, readable = FALSE)
level5 = groupGO(sample1$Gene, keyType = "SYMBOL", ont = "BP", level = 5,OrgDb = org.Hs.eg.db, readable = FALSE)

level1_members = as.data.frame(summary(level1)) # output summary as dataframe
level2_members = as.data.frame(summary(level2))
level3_members = as.data.frame(summary(level3))
level4_members = as.data.frame(summary(level4))
level5_members = as.data.frame(summary(level5))

level1_members = level1_members[order(level1_members$Count,decreasing = T),] # order highest count 
level2_members = level2_members[order(level2_members$Count,decreasing = T),]
level3_members = level3_members[order(level3_members$Count,decreasing = T),]
level4_members = level4_members[order(level4_members$Count,decreasing = T),]
level5_members = level5_members[order(level5_members$Count,decreasing = T),]

level1_members = level1_members[!level1_members$Count == 0,] # delete all 0 counts
level2_members = level2_members[!level2_members$Count == 0,]
level3_members = level3_members[!level3_members$Count == 0,]
level4_members = level4_members[!level4_members$Count == 0,]
level5_members = level5_members[!level5_members$Count == 0,]

level1_members$Level = 1
level2_members$Level = 2
level3_members$Level = 3
level4_members$Level = 4
level5_members$Level = 5

nrow(level1_members)
nrow(level2_members)
nrow(level3_members)
nrow(level4_members)
nrow(level5_members)

members = rbind(level1_members,level2_members,level3_members,level4_members,level5_members)
nrow(members)

### for full data ###########################

# Level 1  2   3    4     5 
#       1 32 558 3890 11017   # all

# Level 1  2   3    4     5 
#       1 29 383 2304 6100   # no 0 counts

#############################################

# list containing all member for each level
member_list = list()
for (i in 1:nrow(level1n5_members)){
  member_list[[i]] = unlist(strsplit(as.character(level1n5_members$geneID[i]), "/"))
}
summary(member_list)
member_list[[1]]

# work out average of seleted member groups ???
  
avg_value = c()
for (i in 1:nrow(level1n5_members)){
  pattern = member_list[[i]]
  avg_value[i] = mean(sample1$avg_fold_changes[which(as.logical(rowSums(outer(sample1$Gene,pattern, "=="))))])
}
avg_value


members$avg_fold_value = avg_value
members = members[,-5]
head(members)
summary(members)


# -log10 of P value???
### output CSV






############ top 5 by ratio

members_top5_ratio = rbind(level1_members,level2_members[1:5,],level3_members[1:5,],level4_members[1:5,],level5_members[1:5,])
nrow(members_top5_ratio)


members_top5_ratio_list = list()
for (i in 1:nrow(members_top5_ratio)){
  members_top5_ratio_list[[i]] = unlist(strsplit(as.character(members_top5_ratio$geneID[i]), "/"))
}
summary(members_top5_ratio_list)
members_top5_ratio_list[[1]]

avg_value_top5 = c()
for (i in 1:nrow(level1n5_members_Top5)){
  pattern = members_top5_ratio_list[[i]]
  avg_value_top5[i] = mean(sample1$avg_fold_changes[which(as.logical(rowSums(outer(sample1$Gene,pattern, "=="))))])
}
avg_value_top5


members_top5_ratio$avg_fold_value = avg_value_top5
members_top5_ratio = members_top5_ratio[,-5]
members_top5_ratio
summary(members_top5_ratio)

############ top 5 by avg fold
loc = c(nrow(level1_members),nrow(level2_members),nrow(level3_members),nrow(level4_members),nrow(level5_members))
location = cumsum(rev(loc))-5
location

location_seq = c()
location_seq_list = list()
for(i in 1:5){
  location_seq_list[[i]] = seq(location[i],location[i]+4)
  location_seq = rev(unlist(location_seq_list))
}
location_seq
length(location_seq)



members2 <- members[order(members$Level,members$avg_fold_value,decreasing = T),]
members2[location_seq,]

###how to orde???



#END HERE



###SELECT 10% MOST SIG GROUPS AND COUNT NUMBER OF REPEATED GENE?  MAYBE WE WILL GET SIMILAR RESULT WITH GENE ENRICHMENT ANALYSIS








#########################################################################

gene.df <- bitr(genedata_sum$Gene, fromType = "SYMBOL", toType = c("GOALL"),OrgDb = org.Hs.eg.db)
gene.df <- subset(gene.df, ONTOLOGYALL == 'BP')
head(gene.df)
nrow(gene.df)




plotGOgraph #need enrichment output

data(geneList, package = "DOSE")
de <- names(geneList)[1:100]
yy <- enrichGO(de, 'org.Hs.eg.db', ont="BP", pvalueCutoff=0.01)
head(yy)







####################################################################

# https://www.bioconductor.org/packages/devel/bioc/vignettes/biomaRt/inst/doc/biomaRt.html
# http://jdblischak.github.io/nw/analysis/mouse/go.html
# https://support.bioconductor.org/p/96577/
# mart = useMart('ensembl')
# listDatasets(mart)
listAttributes
# Get ensembl gene ids and GO terms

human = useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl")
test <- getBM(attributes = c("description","ensembl_gene_id","go_id"), values = genedata$Gene, mart = human)
go_id <- getBM(attributes = c("go_id"), values = genedata$Gene, mart = human)

genedata$ensembl_gene_id  = ensembl_gene_id 
genedata$go_id = go_id
genedata [1:50,]


############################################################################


# classify into go term

# draw a go tree

# gene set enrichment???
#https://www.bioconductor.org/help/course-materials/2010/SeattleIntro/Bioconductor-Introduction.pdf

# 2 sample t test, using hierarchical bayes

# get posterior

# comapre theta =0 and theta > 0 
