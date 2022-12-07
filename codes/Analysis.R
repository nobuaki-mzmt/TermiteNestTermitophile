# Data analysis for Termithophile and termite nesting
# This file is for Analysis (Statistical tests)
# last update: 12/21/2021 N.Mizumoto

# Setup -------------------------------------------------------------------
{
  rm(list = ls())
  PROJHOME = normalizePath(getwd())
  source(file.path(PROJHOME, "/codes/Sources.R"))
  load(file.path(PROJHOME, "data", "rda", "Data_all_genera.Rdata"))
  load(file.path(PROJHOME, "data", "rda", "Data_tree_genera.Rdata"))
  load(file.path(PROJHOME, "data", "rda", "Data_half_tree_genera.Rdata"))
  load(file.path(PROJHOME, "data", "rda", "Termite_tree.Rdata"))
  load(file.path(PROJHOME, "data", "rda", "Termite_tree_half.Rdata"))
  
  load(file.path(PROJHOME, "data", "rda", "Data_colony_size_genera.Rdata"))
  load(file.path(PROJHOME, "data", "rda", "Termite_tree_colony_size.Rdata"))
  
}

# General Information -------------------------------------------------------------------
{
  # num of termitophile species with one-pice nesters
  sum(d.all$NumOfTermitophileSpecies2[d.all$Forager==0])
  3/670
  
  # num of termitophile species with foragers
  sum(d.all$NumOfTermitophileSpecies2[d.all$Forager==1])
  667/670
  
  # Num of One-pice termite species
  sum(d.all$NumOfTermiteSpecies[d.all$Forager==0])
  567/3039
  
  # Num of Foraging termite species
  sum(d.all$NumOfTermiteSpecies[d.all$Forager==1])
  2472/3039
}

# Phylogenetic Analysis including all species -----------------------
{
  # ----- Preprocess -----
  Forager <- d.tree$Forager
  SoilAccess <- d.tree$SoilAccess
  Termitophile <- d.tree$TermitophilePresence
  NumOfTermiteSpecies <- d.tree$NumOfTermiteSpecies
  NumOfTermitophileSpecies <- d.tree$NumOfTermitophileSpecies2
  names(Forager) = names(SoilAccess) = names(Termitophile) = names(NumOfTermitophileSpecies) =
    names(NumOfTermiteSpecies) = d.tree$Genus
  
  # ----- Pagel test (Nesting type - Termitophile presence)  -----
  fit <- fitPagel(termite.tree,Forager,Termitophile)
  fit
  # Model fit:
  #   log-likelihood      AIC
  # independent      -107.1259 222.2517
  # dependent        -100.5261 217.0522
  
  # Hypothesis test result:
  #   likelihood-ratio:  13.1995 
  # p-value:  0.0103411 
  
  # ----- Pagel test (Soil access - Termitophile presence) -----
  fit <- fitPagel(termite.tree, SoilAccess, Termitophile)
  fit
  # Model fit:
  #   log-likelihood      AIC
  # independent      -108.36735 224.7347
  # dependent        -96.30121 208.6024
  
  # Hypothesis test result:
  #   likelihood-ratio:  24.1323 
  #   p-value:  7.51427e-05 
  
  # ----- Phyloglm'(Termitophile Presence vs Forager)  -----
  d.tree.glm <- d.tree
  row.names(d.tree.glm) <- d.tree.glm$Genus
  r <- phyloglm(TermitophilePresence ~ Forager, phy=termite.tree, data = d.tree.glm, boot = 100,
                method = c("logistic_MPLE"))
  summary(r)
  # Coefficients:
  #   Estimate   StdErr  z.value lowerbootCI upperbootCI   p.value    
  # (Intercept) -2.25986  0.65844 -3.43216    -3.47046     -0.7184 0.0005988 ***
  #   Forager      1.96336  0.70229  2.79567     0.63821      3.7189 0.0051793 ** 
  
  # ----- Phyloglm'(Termitophile Presence vs Soil Access)  -----
  d.tree.glm <- d.tree
  row.names(d.tree.glm) <- d.tree.glm$Genus
  r <- phyloglm(TermitophilePresence ~ SoilAccess, phy=termite.tree, data = d.tree.glm, boot = 100,
                method = c("logistic_MPLE"))
  summary(r)
  # Coefficients:
  #   Estimate   StdErr  z.value lowerbootCI upperbootCI   p.value    
  # (Intercept) -3.7938  1.3634 -2.7825     -4.1612     -1.4949 0.005394 **
  #   SoilAccess    3.5878  1.3866  2.5875      1.6254      4.2416 0.009668 **
}

# Phylogenetic Analysis removing less studied genera -----------------------
{
  # ----- Preprocess -----
  Forager <- d.half$Forager
  SoilAccess <- d.half$SoilAccess
  Termitophile <- d.half$TermitophilePresence
  NumOfTermiteSpecies <- d.half$NumOfTermiteSpecies
  NumOfTermitophileSpecies <- d.half$NumOfTermitophileSpecies2
  names(Forager) = names(SoilAccess) = names(Termitophile) = names(NumOfTermitophileSpecies) =
    names(NumOfTermiteSpecies) = d.half$Genus
  
  # ----- Pagel test (Nesting type - Termitophile presence)  -----
  fit<-fitPagel(termite.tree.half,Forager,Termitophile)
  fit
  # Model fit:
  #   log-likelihood      AIC
  # independent      -55.29326 118.5865
  # dependent        -48.34710 112.6942
  
  # Hypothesis test result:
  #   likelihood-ratio:  13.8923 
  # p-value:  0.00764677 
  
  
  # ----- Pagel test (Soil access - Termitophile presence) -----
  fit <- fitPagel(termite.tree.half, SoilAccess, Termitophile)
  fit
  # Model fit:
  #   log-likelihood      AIC
  # independent      -53.16770 114.33541
  # dependent        -41.60126  99.20251
  
  # Hypothesis test result:
  #   likelihood-ratio:  23.1329 
  #   p-value:  0.000119115 
  
  
  # ----- Phyloglm (Termitophile Presence vs Forager)  -----
  d.half.glm <- d.half
  row.names(d.half.glm) <- d.half.glm$Genus
  r <- phyloglm(TermitophilePresence ~ as.factor(Forager), phy=termite.tree.half, data = d.half.glm, boot = 1000,
                method = c("logistic_MPLE"))
  summary(r)
  # Coefficients:
  #   Estimate   StdErr  z.value lowerbootCI upperbootCI   p.value    
  # (Intercept)         -1.70645  0.71605 -2.38315    -3.43297     -0.6041   0.01717 *  
  #   as.factor(Forager)1  3.18305  0.79427  4.00751     1.88425      5.2922 6.136e-05 ***
  
  
  # ----- Phyloglm (Termitophile Presence vs Soil Access)  -----
  r <- phyloglm(TermitophilePresence ~ SoilAccess, phy=termite.tree.half, data = d.half.glm, boot = 100,
                method = c("logistic_MPLE"))
  summary(r)
  # Coefficients:
  #   Estimate   StdErr  z.value lowerbootCI upperbootCI   p.value    
  # (Intercept) -3.2997  1.4972 -2.2039     -3.3515     -1.4917 0.027532 * 
  # SoilAccess    4.8285  1.5361  3.1433      2.7957      5.6489 0.001671 **
  
  # Note that TermitophilePresence = 1 all belongs to SoilAccess = 1
  
  
  # ----- pgls (Num termitophile species / termite species vs Forager)  -----
  d.half.pgls <- data.frame(d.half[,c("Genus", "Forager")], 
                            Diversity = d.half$NumOfTermitophileSpecies2 / d.half$NumOfTermiteSpecies )
  comp.data<-comparative.data(termite.tree.half, d.half.pgls, names.col="Genus", vcv=T, 
                              vcv.dim=3, warn.dropped=TRUE)
  model<-pgls(Diversity~as.factor(Forager), data=comp.data)
  summary(model)
  anova(model) 
  # Response: Diversity
  # Df  Sum Sq  Mean Sq F value  Pr(>F)  
  # as.factor(Forager)  1 0.10834 0.108340  4.6188 0.03513 *
  #   Residuals          69 1.61848 0.023456   
  
}

# Phylogenetic Analysis for colony size -----------------------
{
  # ----- Preprocess -----
  Forager <- d.colony.size$Forager
  SoilAccess <- d.colony.size$SoilAccess
  Termitophile <- d.colony.size$TermitophilePresence
  ColonySize <- d.colony.size$ColonySize
  names(Forager) = names(SoilAccess) = names(Termitophile) = names(ColonySize) = d.colony.size$Genus
  
  
  # ----- pgls (Colony size vs Forager)  -----
  comp.data<-comparative.data(termite.tree.colony.size, d.colony.size, names.col="Genus", vcv=T, 
                              vcv.dim=3, warn.dropped=TRUE)
  model<-pgls(log10(ColonySize)~as.factor(Forager), data=comp.data)
  summary(model)
  anova(model) 
  # Response: log10(ColonySize)
  # Df  Sum Sq  Mean Sq F value    Pr(>F)    
  # as.factor(Forager)  1 0.12191 0.121907  17.904 0.0001067 ***
  #   Residuals          47 0.32001 0.006809
  
  
  # ----- Phyloglm (Termitophile Presence vs Colony size)  -----
  # ----- pool -----
  d.colony.size.glm <- d.colony.size
  row.names(d.colony.size.glm) <- d.colony.size$Genus
  r <- phyloglm(TermitophilePresence ~ log10(ColonySize), 
                phy=termite.tree.colony.size, 
                data = d.colony.size.glm,
                boot = 1000, method = c("logistic_MPLE"))
  summary(r)
  # Coefficients:
  #   Estimate    StdErr   z.value lowerbootCI upperbootCI   p.value    
  # (Intercept)        -6.96999   2.13152  -3.26996   -10.91832     -3.8921 0.0010756 ** 
  #   log10(ColonySize)   1.62780   0.47730   3.41044     0.88372      2.5610 0.0006486 ***
  
  
  # ----- foragers -----
  termite.tree.colony.size1 <- drop.tip(termite.tree.colony.size, 
                                        d.colony.size.glm[d.colony.size.glm$Forager==0,"Genus"])
  r <- phyloglm(TermitophilePresence ~ log10(ColonySize),
                phy=termite.tree.colony.size1, 
                data = d.colony.size.glm[d.colony.size.glm$Forager==1,],
                boot = 1000, method = c("logistic_MPLE"))
  summary(r)
  # Coefficients:
  #   Estimate    StdErr   z.value lowerbootCI upperbootCI p.value  
  # (Intercept)        -5.15255   3.00046  -1.71726   -10.62862     -0.5851 0.08593 .
  # log10(ColonySize)   1.26452   0.62996   2.00730     0.29248      2.4492 0.04472 *
  
  
  # ----- non-foragers -----
  termite.tree.colony.size2 <- drop.tip(termite.tree.colony.size, 
                                        d.colony.size.glm[d.colony.size.glm$Forager==1,"Genus"])
  r <- phyloglm(TermitophilePresence ~ log10(ColonySize),
                phy=termite.tree.colony.size2, 
                data = d.colony.size.glm[d.colony.size.glm$Forager==0,],
                boot = 1000, method = c("logistic_MPLE"))
  summary(r)
  #Coefficients:
  #  Estimate    StdErr   z.value lowerbootCI upperbootCI p.value
  #(Intercept)        -5.11874   6.08678  -0.84096   -10.52428      2.3990  0.4004
  #log10(ColonySize)   1.00917   1.66975   0.60439    -0.82451      2.8064  0.5456
  
  # ----- soil access -----
  termite.tree.colony.size3 <- drop.tip(termite.tree.colony.size, 
                                        d.colony.size.glm[d.colony.size.glm$SoilAccess==0,"Genus"])
  r <- phyloglm(TermitophilePresence ~ log10(ColonySize),
                phy=termite.tree.colony.size3, 
                data = d.colony.size.glm[d.colony.size.glm$SoilAccess==1,],
                boot = 1000, method = c("logistic_MPLE"))
  summary(r)
  # Coefficients:
  #   Estimate   StdErr  z.value lowerbootCI upperbootCI p.value  
  # (Intercept)       -3.77844  2.57062 -1.46986    -8.79325      0.3413 0.14160  
  # log10(ColonySize)  1.01932  0.54939  1.85536     0.14978      2.0721 0.06354 
  
  
}

