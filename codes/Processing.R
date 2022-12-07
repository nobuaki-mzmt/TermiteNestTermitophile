# Data analysis for Termithophile and termite nesting
# This file is for Processing (make rda files for analysis and figures)
# last update: 12/21/2021 N.Mizumoto

# Setup -------------------------------------------------------------------
{
  rm(list = ls())
  PROJHOME = normalizePath(getwd())
  source(file.path(PROJHOME, "/codes/Sources.R"))
}

# Phylogeny ---------------------
{
  tree.file <- "210315_DataS4-tree.tre"
  tree_no3rd <- read.nexus(file.path(PROJHOME, "phylogeny", tree.file))
  
  # drop outgroup
  label <- tree_no3rd$tip.label
  out.group <- is.na(str_locate(label, "Ter")[,1] | str_locate(label, "ter")[,1])
  termite.tree <- drop.tip(tree_no3rd, label[out.group])
  termite.tree <- ladderize(termite.tree)
  
  # fix errors in the label names
  label<- termite.tree$tip.label
  label[label=="Coptotermes_priscus"] <-  "Coptotermes_priscus_fossil"
  label <- str_replace(label, "Homalotermes", "Homallotermes")
  termite.tree$tip.label <- label
  
  # drop fossil species
  fossil <- !is.na(str_locate(label, "fossil")[,1])
  termite.tree <- drop.tip(termite.tree, label[fossil])
  
  # drop Havilanditermes (synonym to Nasutitermes)
  termite.tree <- drop.tip(termite.tree, "Havilanditermes_atripennis")
  
  # label name = genus name
  label<- termite.tree$tip.label
  label <- str_replace(label, "_.*", "")
  termite.tree$tip.label <- label
  
  #plot(termite.tree, cex=0.5)
  
  rm(tree_no3rd)

}

# Data ---------------------------
{
  # read data from Data_S2.xlsx as d.raw and save it as GenusList.Rdata file for further manipuration
  # d.all <- read.delim("clipboard", header =T )            
  # save(d.all, file = file.path(PROJHOME, "data", "rda", "Data_all_genera.Rdata"))
  load(file.path(PROJHOME, "data", "rda", "Data_all_genera.Rdata"))
  
  
}

# Reduce data to match phylogeny ----
{
  d.tree <- NULL
  label<- termite.tree$tip.label
  for(i in 1:length(label)){
    Genus <- label[i]
    d.tree <- rbind(d.tree, d.all[d.all$Genus == Genus,])
  }
  row.names(d.tree) <- 1:dim(d.tree)[1]
}

# Omit not studied termite genus (based on google scholar results) ----
{
  threshold = quantile(d.tree$Scholar)["50%"]
  d.half <- d.tree[d.tree$Scholar>threshold | d.tree$TermitophilePresence==1,]
  termite.tree.half <- drop.tip(termite.tree, na.omit(d.tree[d.tree$Scholar<=threshold&d.tree$TermitophilePresence==0,"Genus"]))
}

# Data with colony size information ----
{
  d.colony.size <- d.tree[!is.na(d.tree$ColonySize),]
  termite.tree.colony.size <- drop.tip(termite.tree, d.tree[is.na(d.tree$ColonySize),"Genus"])
}

# Save all output data
{
  save(d.all, file = file.path(PROJHOME, "data", "rda", "Data_all_genera.Rdata"))
  save(d.tree, file = file.path(PROJHOME, "data", "rda", "Data_tree_genera.Rdata"))
  save(d.half, file = file.path(PROJHOME, "data", "rda", "Data_half_tree_genera.Rdata"))
  save(d.colony.size, file = file.path(PROJHOME, "data", "rda", "Data_colony_size_genera.Rdata"))
  save(termite.tree, file = file.path(PROJHOME, "data", "rda", "Termite_tree.Rdata"))
  save(termite.tree.half, file = file.path(PROJHOME, "data", "rda", "Termite_tree_half.Rdata"))
  save(termite.tree.colony.size, file = file.path(PROJHOME, "data", "rda", "Termite_tree_colony_size.Rdata"))
  
}
