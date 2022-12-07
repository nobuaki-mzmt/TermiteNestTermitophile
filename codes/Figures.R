# Data analysis for Termithophile and termite nesting
# This file is for Figures
# last update: 12/23/2021 N.Mizumoto

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

# Full tree -------------------------------------------------------------------
{
  # ----- Preprocess -----
  Forager <- d.tree$Forager
  SoilAccess <- d.tree$SoilAccess
  Termitophile <- d.tree$TermitophilePresence
  NumOfTermiteSpecies <- d.tree$NumOfTermiteSpecies
  NumOfTermitophileSpecies <- d.tree$NumOfTermitophileSpecies2
  names(Forager) = names(SoilAccess) = names(Termitophile) = names(NumOfTermitophileSpecies) =
    names(NumOfTermiteSpecies) = d.tree$Genus
  
  # ----- plot tree -----
  #pdf(paste0(today,"-Phylogeny-Termitophile-all.pdf"), width=5, height=6, family = "PT Serif", paper = "a4")
  plotTree(termite.tree, fsize=0.5, ftype="i", offset=2 )
  #tiplabels(pch=20, cex=log10(NumOfTermiteSpecies)+1, col=cols[2-Forager], offset=4)
  #tiplabels(pch=20, cex=log10(NumOfTermitophileSpecies)+1, col=cols[1], offset=8)
  #dev.off()
  
  # ----- get tile -----
  is_tip <- termite.tree$edge[,2] <= length(termite.tree$tip.label)
  ordered_tips <- termite.tree$edge[is_tip, 2]
  ordered_tips.label <- termite.tree$tip.label[ordered_tips]
  ordered_forager <- rev(Forager[ordered_tips.label])
  ordered_termitophile <- rev(Termitophile[ordered_tips.label])
  ordered_soilaccess <- rev(SoilAccess[ordered_tips.label])
  
  cols = viridis(2, end=0.5, alpha=0.5)
  col.show <- alpha(c(cols[1], "white"), 0.5)
  data.tile <- c(matrix(c(col.show[2-ordered_forager],  
                          col.show[2-ordered_soilaccess], 
                          col.show[2-ordered_termitophile]), 3, byrow=T))
  #pdf(paste0(today,"-Phylogeny-Termitophile-all-panel.pdf"), width=5, height=6, family = "PT Serif", paper = "a4")
  show_col(data.tile, ncol = 3, labels =F)
  #dev.off()
  
  # ----- Comparison of # termitophile presence genera -----
  dt <- table(d.tree[,c("Forager", "TermitophilePresence")])
  barplot( dt[,2]/(dt[,2]+dt[,1]), ylim=c(0,1))
  mtext(paste(dt[,2], "/", (dt[,2]+dt[,1])), at=c(0.5,1.5))
  
  dtdf <- data.frame(Forager = c(0,1), Pd.treeence = c(2,46), Absence = c(25, 64))
  ggplot(data=dtdf)  + 
    geom_bar(aes(as.factor(Forager), Pd.treeence/(Pd.treeence+Absence)), stat="identity", col="black",
             alpha=0.4, fill=cols[2:1]) +
    scale_y_continuous(limits=c(0,1)) +
    theme_bw() + theme(aspect=1) + ylab("Termitophile records")
  #ggsave(paste0(today, "-FullComparison.pdf"), width = 3, height = 3)
}

# Half tree -------------------------------------------------------------------
{
  # ----- Preprocess -----
  Forager <- d.half$Forager
  SoilAccess <- d.half$SoilAccess
  Termitophile <- d.half$TermitophilePresence
  NumOfTermiteSpecies <- d.half$NumOfTermiteSpecies
  NumOfTermitophileSpecies <- d.half$NumOfTermitophileSpecies2
  names(Forager) = names(SoilAccess) = names(Termitophile) = names(NumOfTermitophileSpecies) =
    names(NumOfTermiteSpecies) = d.half$Genus
  
  # ----- plot tree -----
  #pdf(paste0(today,"-Phylogeny-Termitophile-omit.pdf"), width=5, height=6, family = "PT Serif", paper = "a4")
  plotTree(termite.tree.half, fsize=0.5, ftype="i", offset=2 )
  #tiplabels(pch=20, cex=(log10(NumOfTermiteSpecies)+1)*0.5, col=alpha(cols[2-Forager],1), offset=0)
  #tiplabels(pch=20, cex=(log10(NumOfTermitophileSpecies)+1)*0.5, col=cols[1], offset=4)
  #dev.off()
  
  # ----- get tile -----
  is_tip <- termite.tree.half$edge[,2] <= length(termite.tree.half$tip.label)
  ordered_tips <- termite.tree.half$edge[is_tip, 2]
  ordered_tips.label <- termite.tree.half$tip.label[ordered_tips]
  ordered_forager <- rev(Forager[ordered_tips.label])
  ordered_termitophile <- rev(Termitophile[ordered_tips.label])
  ordered_soilaccess <- rev(SoilAccess[ordered_tips.label])
  cols = viridis(2)
  col.show <- alpha(c(cols[1], "white"), 0.5)
  data.tile <- c(matrix(c(col.show[2-ordered_forager],  
                          col.show[2-ordered_soilaccess], 
                          col.show[2-ordered_termitophile]), 3, byrow=T))
  #pdf(paste0(today,"-Phylogeny-Termitophile-omit-panel.pdf"), width=5, height=6, family = "PT Serif", paper = "a4")
  show_col(data.tile, ncol = 3, labels =F)
  #dev.off()
  
  # ----- Comparison of # termitophile presence genera -----
  dt <- table(d.half[,c("Forager", "TermitophilePresence")])
  barplot( dt[,2]/(dt[,2]+dt[,1]), ylim=c(0,1))
  mtext(paste(dt[,2], "/", (dt[,2]+dt[,1])), at=c(0.5,1.5))
  
  dtdf <- data.frame(Forager = c(0,1), Pd.treeence = c(2,46), Absence = c(25, 64))
  ggplot(data=dtdf)  + 
    geom_bar(aes(as.factor(Forager), Pd.treeence/(Pd.treeence+Absence)), stat="identity", col="black",
             alpha=0.4, fill=cols[2:1]) +
    scale_y_continuous(limits=c(0,1)) +
    theme_bw() + theme(aspect=1) + ylab("Termitophile records")
  #ggsave(paste0(today, "-Comparison.pdf"), width = 3, height = 3)
  
  
}

# Colony Size -------------------------------------------------------------------
{
  # ----- Comparison of colony size -----
  ggplot(d.colony.size, aes(x=as.factor(Forager), y=(ColonySize), fill=as.factor(Forager))) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize=2, binwidth=0.05)+
    scale_fill_viridis(discrete = T, end=0, begin=1, alpha=0.8)+
    scale_color_viridis(discrete = T, end=0, begin=1, alpha=0.8)+
    scale_y_log10(limits=c(100,10000000)) +
    theme_bw() + ylab("Colony size") + xlab("Forager") +
    theme(legend.position = "none", aspect.ratio = 1.5) +
    stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                 geom="pointrange", color="red") 
  ggsave(paste0(today, "_ColonySizeComparison.pdf"), height=3, width=3, family="PT Sans", paper="a4")
  
  # ----- Colony size vs Termitophile presence -----
  x.range <- seq(from=min(log10(d.colony.size$ColonySize)), 
                 to=max(log10(d.colony.size$ColonySize)), by=.01)
  y.logits = -5.152549 +  1.264517 * x.range
  y.prob = exp(y.logits)/(1+exp(y.logits))
  df1 <- data.frame(x.range, y.prob)
  x.range <- seq(from=min(log10(d.colony.size[d.colony.size$Forager==0,]$ColonySize)), 
                 to=max(log10(d.colony.size[d.colony.size$Forager==0,]$ColonySize)), by=.01)
  y.logits = -5.11874 +  1.00917 * x.range
  y.prob = exp(y.logits)/(1+exp(y.logits))
  df2 <- data.frame(x.range, y.prob)
  ggplot(d.colony.size, 
         aes(x=log10(ColonySize), y = TermitophilePresence, col=as.factor(Forager))) +
    geom_point(alpha=0.8)+
    scale_color_viridis(discrete = T, begin=1, end=0) +
    theme_bw() +
    xlab("Colony size")+
    theme(aspect.ratio = 0.75, legend.position = 'none')  +
    geom_line(data=df1, aes(x=x.range, y=y.prob), col=viridis(2)[1])+
    geom_line(data=df2, aes(x=x.range, y=y.prob), col=viridis(2)[2])
  ggsave(paste0(today, "_ColonySizeEffect.pdf"), 
         height=3, width=4, family="PT Sans", paper="a4")
  
  
  
  x.range <- seq(from=min(log10(d.colony.size$ColonySize)), 
                 to=max(log10(d.colony.size$ColonySize)), by=.01)
  y.logits = -5.152549 +  1.264517 * x.range
  y.prob = exp(y.logits)/(1+exp(y.logits))
  df1 <- data.frame(x.range, y.prob)
  ggplot(d.colony.size[d.colony.size$Forager==1,], aes(x=log10(ColonySize), y = TermitophilePresence)) +
    geom_point(col=viridis(1))+
    theme_bw() +
    xlab("Colony size")+
    theme(aspect.ratio = 1, legend.position = 'none')  +
    geom_line(data=df1, aes(x=x.range, y=y.prob))
  ggsave(paste0(today, "_ColonySizeEffect-Forager.pdf"), 
         height=3, width=3, family="PT Sans", paper="a4")
  
  x.range <- seq(from=min(log10(d.colony.size[d.colony.size$Forager==0,]$ColonySize)), 
                 to=max(log10(d.colony.size[d.colony.size$Forager==0,]$ColonySize)), by=.01)
  y.logits = -5.11874 +  1.00917 * x.range
  y.prob = exp(y.logits)/(1+exp(y.logits))
  df1 <- data.frame(x.range, y.prob)
  ggplot(d.colony.size[d.colony.size$Forager==0,], aes(x=log10(ColonySize), y = TermitophilePresence)) +
    geom_point(col=viridis(2)[2])+
    theme_bw() +
    xlim(c(3,5)) +
    xlab("Colony size")+
    theme(aspect.ratio = 1, legend.position = 'none') +
    geom_line(data=df1, aes(x=x.range, y=y.prob))
  ggsave(paste0(today, "_ColonySizeEffect-One-piece.pdf"), 
         height=3, width=3, family="PT Sans", paper="a4")
  
  x.range <- seq(from=min(log10(d.colony.size$ColonySize)), 
                 to=max(log10(d.colony.size$ColonySize)), by=.01)
  y.logits = -3.77844 +  1.01932 * x.range
  y.prob = exp(y.logits)/(1+exp(y.logits))
  df1 <- data.frame(x.range, y.prob)
  ggplot(d.colony.size[d.colony.size$SoilAccess==1,],
         aes(x=log10(ColonySize), y = TermitophilePresence, col=as.factor(Forager))) +
    geom_point()+
    scale_color_viridis(discrete=TRUE, begin=1, end=0) +
    theme_bw() +
    xlab("Colony size")+
    theme(aspect.ratio = 1, legend.position = 'none') +
    geom_line(data=df1, aes(x=x.range, y=y.prob, col="black"))
  ggsave(paste0(today, "_ColonySizeEffect-SoilAccess.pdf"), 
         height=3, width=3, family="PT Sans", paper="a4")
}


# Data analysis -----
{
  # Num of Termite species vs Termitophile presence
  {
    ggplot(d, aes(x=log10(NumOfTermiteSpecies), y=TermitophilePresence, col=as.factor(Forager))) +
      geom_point(alpha = 0.4) +
      scale_color_viridis(discrete=TRUE, begin=1, end=0) +
      theme_bw() +
      theme(aspect.ratio = 1, legend.position = 'none')+
      stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE)
    #ggsave(paste0(today, "-TermiteSpecies.pdf"), width = 4, height = 4)
    
    r <- glm(TermitophilePresence~log10(NumOfTermiteSpecies), 
             data=d[d$Forager == 1,],
             family = "binomial")
    Anova(r)
    
    r <- glm(TermitophilePresence~log10(NumOfTermiteSpecies), 
             data=d[d$Forager == 0,],
             family = "binomial")
    Anova(r)
  }  
  
  # Num of Termite species vs Num of Termitophile species
  {
    ggplot(d, aes(x=(NumOfTermiteSpecies), y=NumOfTermitophileSpecies, col=as.factor(Forager))) +
      geom_point(alpha = 0.4) +
      scale_color_viridis(discrete=TRUE, begin=1, end=0) +
      theme_bw() + coord_fixed()+
      theme(aspect.ratio = 1, legend.position = 'none')+
      stat_smooth(method="lm", se=TRUE)
    #ggsave(paste0(today, "-NumOfSpecies.pdf"), width = 4, height = 4)
    
    r <- glm(NumOfTermitophileSpecies~NumOfTermiteSpecies, 
             data=d[d$Forager == 1,], family ="poisson")
    summary(r)
    Anova(r)
    
    r <- lm(NumOfTermitophileSpecies~NumOfTermiteSpecies, 
            data=d[d$Forager == 0,])
    summary(r)
    Anova(r)
  }  
  
  # Num of Termite researches vs Termitophile presence
  {
    ## Google Scholar
    {
      ggplot(d[d$Genus!="Termes",], aes(x=Scholar, y=TermitophilePresence, col=as.factor(Forager))) + 
        geom_point(alpha=0.5) +
        scale_color_viridis(discrete=TRUE, begin=1, end=0) +
        theme_bw() +
        scale_x_log10()+
        xlab("Search hits in Google Scholar")+
        theme(aspect.ratio = 1, legend.position = 'none')+
        stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE)
      #ggsave(paste0(today, "-ResearchEffort.pdf"), width = 4, height = 4)
      
      r <- glm(TermitophilePresence~Scholar, 
               data=d[d$Forager == 1 & d$Genus!="Termes",],
               family = "binomial")
      Anova(r)
      
      r <- glm(TermitophilePresence~Scholar, 
               data=d[d$Forager == 0,],
               family = "binomial")
      Anova(r)
    }
  }
  .
  
  ggplot(d[d$Genus!="Termes",], aes(x=(NumOfTermiteSpecies), y=(Scholar), col=as.factor(Forager))) +
    geom_point(alpha = 0.4) +
    scale_color_viridis(discrete=TRUE, begin=1, end=0) +
    theme_bw() + scale_x_log10()+ scale_y_log10()+
    theme(aspect.ratio = 1, legend.position = 'none')+
    stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE)
}

