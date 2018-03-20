rm(list = ls(all=T)) # Command that clears the environment in R studio
library(genoPlotR)  #Command  that loads genoPlotR and ade4 packages into this session of R studio
pRL10 <- try(read_dna_seg_from_fasta("pRL10.fasta")) #DNA sequence data from NCBI data base in fasta format
pRL9 <- try(read_dna_seg_from_fasta("pRL9.fasta")) #from the genome of Rhizobium leguminosarum bv. viciae strain 3841 plasmids pRL 9 and 10
VF391213 <- try(read_dna_seg_from_fasta("VF39cscaffold.fasta")) #Reading in of R.leguminosarum bv. viciae strain VF39SM c plasmid catabolic gene scaffold sequence
VF394041 <- try(read_dna_seg_from_fasta("VF39dscaffold.fasta"))#Reading in of R. leguminosarum bv. viciae strain VF39SM d plasmid catabolic gene scaffold sequence from NCBI
VF39vspRL910 <- try(read_comparison_from_blast("VF39vspRL910G3KJAXJ2114-Alignment.txt"))#Alignment of R.leguminosarum VF39SM genomic sequence compared by blast to pRL9 and 10 of 3841 in Text format.
##################################################################################################################################################################
#Size comparisons of catabolic gene loci on VF39SM scaffolds in comparison to pRL9 and 10 in the related strain 3841
#################################################################################################################################################################
VF391213_vs_pRL9 <-try(read_comparison_from_blast("VF39cscaffoldvspRL9-Alignment.txt")) #Alignment of VF39c plasmid scaffold to pRL9 of 3841 in text format from the NCBI database.
xlims <- list(c(1,352782),c(1,150833))   #sizes of the two DNA sequences being compared so that an appropriate x axis scale can be set in the plot.
plot_gene_map(dna_segs = list(VF391213,pRL9), #list because dna segment data frames being compared contain different classes of data, numeric and character.  
              xlims=xlims, #limit of the x axis in the final figure is as large as the largest dna segment in the figure.  
              main="VF39cscaffold coverage of pRL9 of R.leg. 3841", #Title of the graph
              gene_type = "side_blocks", #The shape of the gene segment in the graphical output.
              dna_seg_scale = TRUE, scale=FALSE) #scales put on each gene segment, not as a scale on the bottom of the figure. 

VF39dscaffold_vs_pRL10 <-try(read_comparison_from_blast("VF39dscaffolvsdpRL10-Alignment.txt")) #Alignment of VF39d plasmid scaffold to pRL10 of 3841 in text format from the NCBI database.
xlims <- list(c(1,12453),c(1,488135))  #sizes of the two DNA sequences being compared so that an appropriate x axis scale can be set in the plot.
plot_gene_map(dna_segs = list(VF394041,pRL10), #list because dna segment data frames being compared contain different classes of data, numeric and character.
              xlims=xlims, #limit of the x axis in the final figure is as large as the largest dna segment in the figure.  
              main="VF39dscaffold coverage of pRL10 of R.leg. 3841", #Figure title
              gene_type = "side_blocks", # Shape of the gene in the DNA segment in the figure
              dna_seg_scale = TRUE, scale=FALSE) #scales put on each gene segment, not as a scale on the bottom of the figure. 

######################################################################################
#Comparison of DNA sequence similarities between  pRLeVF39c and d with pRL 9 and 10
###################################################################################
# In order to graphically display similarities between DNA sequences, DNA sequences are usually aligned using NCBI 
# tblastn function and selecting align two or more sequences.  Accession numbers are entered into the windows on the web
#page and then the alignment is performed using the blast algorithm on this page.  The results are downloaded into the 
#directory that RStudio is in on your computer as a hit table, and then this data can be directly read in to an 
#R session by by using the "try read_comparison_from_blast" command.  Followed by plotting of the compared DNA segments
#using the plot_gene_map function of the DNA segments in the comparison blast file, along with the new function "comparisons"
#which goes through the blast file and finds the regions of similarities found by the Blast alignment, and then plots
#these similar regions on the gene map.
VF391213 <- try(read_dna_seg_from_fasta("VF39cscaffold.fasta"))
pRL9 <- try(read_dna_seg_from_fasta("pRL9.fasta"))
VF391213_vs_pRL9 <-try(read_comparison_from_blast("VF39cscaffoldvspRL9-Alignment.txt")) #Reading in of data from BLAST alignment of the VF39SM c scaffold vs pRL9 plasmid from strain 3841
xlims <- list(c(1, 400000), c(1, 400000)) #limits of the x axis in the final figure.
plot_gene_map(dna_segs = list(VF391213, pRL9), #Asks R to plot a gene map of dna_segments of the VF39SM c plasmid scaffold and plasmid pRL9 from strain 3841.
              comparisons=list(VF391213_vs_pRL9),  #The comparison of the VF39SM c scaffold to pRL9 from 3841.
              xlims = xlims, # The limits of the x axis in the figure is equal to the x lims specified above.  
              main="VF39 scaffold 1213 vs pRL9 3841", # Figure title
              gene_type = "side_blocks",  #gene_type can be a block, a side_block, or a triangle grob, or arrow.
              dna_seg_scale = TRUE, scale = FALSE) # the scale is on the gene segment rather than a scale of base pair size at the bottom of the figure.  

VF394041 <- try(read_dna_seg_from_fasta("VF39dscaffold.fasta"))
pRL10 <- try(read_dna_seg_from_fasta("pRL10.fasta"))
VF39dscaffold_vs_pRL10 <-try(read_comparison_from_blast("VF39dscaffolvsdpRL10-Alignment.txt")) ##Reading in of data from BLAST alignment of the VF39SM d scaffold vs pRL10 plasmid from strain 3841 
xlims <- list(c(1, 200000), c(1, 200000)) #limits of the x axis in the final figure.
plot_gene_map(dna_segs = list(VF394041, pRL10), #Asks R to plot a gene map of dna_segments of the VF39SM d plasmid scaffold and plasmid pRL10 from strain 3841.
              comparisons = list(VF39dscaffold_vs_pRL10), #The comparison of the VF39SM d scaffold to pRL10 from 3841.
              xlims = xlims, # The limits of the x axis in the figure is equal to the x lims specified above
              main = "VF39d scaffold vs pRL10 from 3841", #Figure title
              gene_type = "side_blocks", #Gene_type, can be a block, or arrow, etc.  
              dna_seg_scale = TRUE, scale = FALSE) # the scale is on the gene segment rather than a scale of base pair size at the bottom of the figure.  
###############################################################
#DNA segments
####################################################################
#To mark off specific genes in a large DNA segment, information about the gene names, start and end sites, 
#strand direction either "+" (sendse) or "-" (anti-sense) and colors for each gene segment mapped should be known.  
#Data frames are made with the gene information, and then designated as a dna_seg object, which can then be 
#plotted using the plot_gene_map function.  
df1 <- data.frame(name=c("RlcK","RlcD","RlcB","RlcC","RlcH","RlcA","RlcO","RlcE","RlcF"), #Gene symbols for each gene in this dna_seg object
                  start=c(40333, 116842, 124822, 135571, 127430, 133945, 113133, 105466, 292), #The start coordinates of each corresponding gene in the dna_seg object
                  end=c(40373, 117376, 127423, 150883, 133347, 135513, 113649, 107870, 102982), #The end coordinates of each corresponding gene in the dna_seg object
                  strand=c(1, -1, -1, -1, -1, -1, -1, -1, -1), #The orientation or direction of each gene on the dna_seg object.
                  col=c("red", "blue", "yellow", "green", "black", "purple", "white", "pink", "orange")) #color of each gene in the dna_seg object.  
dna_seg1 <- dna_seg(df1) #designating dna_seg1 as containing information in data frame 1
df1 #print contents of data frame 1
dna_seg1 #print contents of dna_seg1
df2 <- data.frame(name=c("RBTCK", "RBTCD", "RBTCB", "RBTCC", "RBTCH", "RBTCA", "RBTCO", "RBTCE", "RBTCF"), #Gene symbols for each gene in this dna_seg object
                  start=c(307792, 40158, 39595, 29390, 36883, 30955, 40673, 43075, 145781), #The start coordinates of each corresponding gene in the dna_seg object
                  end=c(307832, 39626, 36994, 36994, 14075, 30955, 29392, 40158, 40669), #The end coordinates of each corresponding gene in the dna_seg object
                  strand=c(1, -1, -1, -1, -1, -1, -1, -1, -1), #The orientation or direction of each gene on the dna_seg object.
                  col=c("red", "blue", "yellow", "green", "black", "purple", "white", "pink", "orange")) #color of each gene in the dna_seg object.
dna_seg2 <- dna_seg(df2) #designating dna_seg2 as containing information in data frame 2
plot_gene_map(dna_segs=list(dna_seg1, dna_seg2), #plot a gene map of dna_seg1 and 2
              main = "Gene segments of the c (top) and d (bottom) scaffolds of VF39SM") #With this title

######################################################################################
###############################################################################################################
#Genes in a larger DNA segment-VF39SM c scaffold sequence genes and annotation of these genes with gene symbols
###############################################################################################################
#To mark off specific genes in a large DNA segment, information about the gene names, start and end sites, 
#strand direction either "+" (sendse) or "-" (anti-sense) and colors for each gene segment mapped should be known.  
#Data frames are made with the gene information, and then designated as a dna_seg object, which can then be 
#plotted using the plot_gene_map function.  
name1 <- c("RlcK","RlcD","RlcB","RlcC","RlcH","RlcA","RlcO","RlcE","RlcF")#Gene symbols for each gene in this dna_seg object
start1 <- c(40333, 116842, 124822, 135571, 127430, 133945, 113133, 105466, 292)#The start coordinates of each corresponding gene in the dna_seg object
end1 <- c(40373, 117376, 127423, 150883, 133347, 135513, 113649, 107870, 102982)#The end coordinates of each corresponding gene in the dna_seg object
strand1=c(1, -1, -1, -1, -1, -1, -1, -1, -1)#The orientation or direction of each gene on the dna_seg object.
col1 <- c("red", "blue", "yellow", "green", "black", "purple", "white", "pink", "orange")#color of each gene in the dna_seg object.
df1a <- data.frame(name=name1, start=start1, end=end1, strand=strand1, col=col1)
dna_seg1a <- dna_seg(df1a)
is.dna_seg(dna_seg1a)
dna_seg1a <- dna_seg(df1a) #designating dna_seg1a as containing information in data frame 1a
df1a #print contents of data frame 1a
dna_seg1a #print contents of dna_seg1a
#annotation of dna_seg 1a 
mid <- middle(dna_seg1a) # mid calculates the middle of genes in dna_seg1.

annot1 <- annotation(x1=mid, text=dna_seg1a$name, rot = 0, col = "black") #Places text as an annotation from the name vector in data frame1a in the middle of genes on dna_seg1a. The text is not rotated to any angle and the font color is black.  

#Plotting DNA seg 1a
plot_gene_map(dna_segs = list(dna_seg1a),  # Plot genes in dna_seg 1a, annotating genes in the middle, with a height of 1.0 and annotation text or symbol size of 0.3.
              annotations= annot1, annotation_height=1.0, annotation_cex=0.3,
              limit_to_longest_dna_seg = F, # Do not limit annotation to the longest dna_seg.
              dna_seg_scale = TRUE) # Add a dna scale to the dna_seg.


#############################################################
#Complex gene plot and comparison of four Bartonella genomes
#############################################################
data(barto) #  Sample data set containing comparisons of the genomes of 4 Bartonella strains from the genoPlotR package.
tree <- newick2phylog("(BB:3.0,(BG:1.0,(BH:0.5,BQ:0.2):2):3.5);") #  plots a phylogenetic tree of the four Bartonella genomes being compared.  The decimals next to the abbreviations for the Bartonella genomes being compared indicate the lengths of the branches from the node that they are derived from.  BH and BQ are 2.0 units away from the parental node and BB and BG are 3.5 units away from the parental node.  
xlims2 <- list(c(1445000, 1415000, 1380000, 1412000), c( 10000, 45000, 50000, 83000, 90000, 120000), c( 15000, 36000, 90000, 120000, 74000, 98000), c( 5000, 82000)) #These are the x axis limits for each gene in a dna segment being compared on the plot.   
annots <- lapply(barto$dna_segs, #lapply attaches an annotation to each DNA segment 
                 function(x) #A funtion for applying annotations to function specified gene segments.
  {mid <- middle(x) # defines mid as the middle of the gene segment
  annot <- annotation(x1=mid, text=x$name, rot=30)  #indicates to annotate in the middle of the gene with a character from the name column, with the annotation being rotated at a 30 degree angle. 
  idx <- grep("^[^B]", annot$text, perl=TRUE)  #The vector idx contains gene names which match the pattern of not starting with the letter B in the text column, and are Perl-style regular expressions.  
  annot[idx[idx %% 4 == 0],] }) # Annotate every fourth gene on the dna segment which does not start with the letter B.  
plot_gene_map(barto$dna_segs, barto$comparisons, tree=tree,  #Plot a map of the DNA segments of the comparisons of the four Bartonella genomes in Newick Tree format, as specified above,
              annotations=annots, xlims=xlims2, limit_to_longest_dna_seg=FALSE, dna_seg_scale=TRUE)# annotating every fourth gene in the middle that does not start with the letter B, with x axis limits of each gene subsegment as specified by xlims2. Apply a dna_seg scale to each dna segment on the plot.  

###########################################################################################################
#Annotation simple example that works
###########################################################################################################
names3 <- c("gene1", "gene2", "gene3") #Vector of names for genes on the first DNA segment in this DNA segment comparison
starts1 <- c(1, 1200, 1700) #The start sites of genes 1-3 on DNA segment 1
ends1 <- c(500, 1500, 2500) # The end sites of genes 1-3 on DNA segment 1
strands1 <- c("-", -1, 1) #On DNA segment one, genes one and two go in the reverse orientation, and gene three is in the forward, sense orientation.   
cols1 <- c("red", "grey", "blue") # Vector denoting the colors of genes 1, 2 and 3 on dna segment 1.
df4 <- data.frame(name=names3, start=starts1, end=ends1, #Create a dataframe named data frame 4 with vectors "names3", "starts1", "ends1", "strands1", "cols1".
                  strand=strands1, col=cols1)
dna_seg1 <- dna_seg(df4) #dna_seg1 is equal to the dna_seg specified in data frame 4.  
is.dna_seg(dna_seg1) #Asks R if dna_seg1 is interpreted by R as a dna_seg.  Returns a logical "True" or "False" statement.
str(dna_seg1) #Asks R what the structure of dna_seg1 is.  
#second dna seg
names2 <- c("gene4", "gene5", "gene6") #Vector of gene names for dna_seg2.
starts2 <- c(50, 1400, 2000) #Vector specifying the start sites of genes 4-6 on dna_seg2
ends2 <- c(600, 1700, 3000) #Vector specifying the end sites of genes 4-6 on dna_seg2.
strands2 <- c("+", 1, -1) #Vector specifying the orientation of genes 4-6 on dna_seg2.  Genes 4 and 5 are transcribed or oriented on dna_seg2 in the positive or sense direction, while gene 6 is oriented in the negative direction.  
cols2 <- c("grey", "yellow", "blue") #Vector specifying the colors of genes 4-6 on dna_seg2.  Gene 4 is colored grey, gene 5, yellow and gene 6 is blue in color.  
df5 <- data.frame(name=names2, start=starts2, end=ends2, #Create data frame 5 with vectors names 2, starts 2, ends 2, strands 2 and cols 2.  
                  strand=strands2, col=cols2) 
dna_seg2 <- dna_seg(df5) #dna_seg2 is composed of the dna_seg specified by data frame 5.
is.dna_seg(dna_seg2) #Asks R if dna_seg2 is a dna_seg.  Returns a logical "True" or "False"
str(dna_seg2) #Asks R what the structure of dna_seg2 is.  
#Comaparison of dna_segs1 and 2
comparison1 <- as.comparison(data.frame(start1=starts1, end1=ends1, start2=starts2, end2=ends2)) #Create a comparison between the start and end sites of genes 1-3 on dna_seg1 to those of genes 4-6 on dna_seg2.
str(comparison1) #Asks R what the structure of comparison 1 is.  
#annotation of dna_segs 1 and 2
mid <- middle(dna_seg1) # mid calculates the middle of genes 1-3 in dna_seg1.
mid2 <- middle(dna_seg2)#  mid2 calculates the middle of genes 4-6 in dna_seg2.
annot1 <- annotation(x1=mid, text=dna_seg1$name, rot = 0, col = "black") #Places text as an annotation from the name vector in data frame1 in the middle of genes 1-3 on dna_seg1. The text is not rotated to any angle and the font color is black.  
annots <- annotation(x1=mid2, text=dna_seg2$name, rot = 0, col = "black")  #Places text as an annotation from the name vector in data frame2 in the middle of genes 4-6 on dna_seg2.   The text is not rotated to any angle and the font color is black.  
#Plotting DNA segs 1 and 2
plot_gene_map(dna_segs = list(dna_seg1, dna_seg2),  # Plot genes in dna_seg 1 and 2, annotating genes 1-3 and 4-6 in the middle, with a height of 1.8 and annotation text or symbol size of 1.
              annotations= list(annot1, annots), annotation_height=1.8, annotation_cex=1,
              limit_to_longest_dna_seg = F, # Do not limit annotation to the longest dna_seg.
              dna_seg_scale = TRUE) # Add a dna scale to the bottom right hand side of the gene map plot.  
#######################################################################################################
####End of working code
#######################################################################################################

