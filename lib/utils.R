
reformatData <- function(sce=NULL,meta.tb=NULL,colSet=NULL,geneDesc.tb=NULL)
{
    if(!is.null(sce)){
        if(!("meta.cluster" %in% colnames(colData(sce))) &&
            ("majorCluster" %in% colnames(colData(sce)))){
            sce[["meta.cluster"]] <- sce[["majorCluster"]]
        }
        return(sce)
    }
    if(!is.null(meta.tb)){
        if(!("meta.cluster" %in% colnames(meta.tb)) && 
            ("majorCluster" %in% colnames(meta.tb))){
            meta.tb[["meta.cluster"]] <- meta.tb[["majorCluster"]]
        }
        return(meta.tb)
    }
    if(!is.null(colSet)){
        if(!("meta.cluster" %in% names(colSet)) &&
            ("majorCluster" %in% names(colSet))){
            colSet[["meta.cluster"]] <- colSet[["majorCluster"]]
        }
        return(colSet)
    }
    if(!is.null(geneDesc.tb)){
        ### required columns:
####        c("geneSymbol", "cluster.name","comb.ES","comb.padj","sig",
####          "comb.ES.rank", "comb.positive.freq",
####          "cancerType.sig.N", "cancerType.sig.freq", "cancerType.total.N",
####          "cancerType.ES.max", "cancerType.ES.max.p.adj",
####          "dataset.sig.N", "dataset.sig.freq", "dataset.total.N",
####          "dataset.ES.max", "dataset.ES.max.p.adj")
        geneDesc.tb <- geneDesc.tb[order(meta.cluster,-comb.ES),]
        .tmp.gene.rank.tb <- geneDesc.tb[,.(geneSymbol=geneSymbol,comb.ES.rank=rank(-comb.ES)),by=c("meta.cluster")]
        geneDesc.tb <- merge(geneDesc.tb,.tmp.gene.rank.tb,by=c("geneSymbol","meta.cluster"))
        geneDesc.tb <- geneDesc.tb[order(meta.cluster,-comb.ES),]
        name.ch.vec <- c("meta.cluster"="cluster.name","bin.freq.comb"="comb.positive.freq",
                         "N.sig.group.2nd"="cancerType.sig.N","freq.sig.group.2nd"="cancerType.sig.freq",
                         "N.total.group.2nd"="cancerType.total.N",
                         "dprime.max.group.2nd"="cancerType.ES.max","dprime.max.adj.P.group.2nd"="cancerType.ES.max.p.adj",
                         "N.sig"="dataset.sig.N","freq.sig"="dataset.sig.freq","N.total"="dataset.total.N",
                         "dprime.max"="dataset.ES.max","dprime.max.adj.P.Val"="dataset.ES.max.p.adj")
        for(i in seq_along(name.ch.vec)){
            n.old <- names(name.ch.vec)[i]
            n.new <- name.ch.vec[i]
            if(!(n.new %in% colnames(geneDesc.tb)) && (n.old %in% colnames(geneDesc.tb))){
                geneDesc.tb[[n.new]] <- geneDesc.tb[[n.old]]
                if(n.old!="meta.cluster"){ geneDesc.tb[[n.old]] <- NULL }
            }
        }
        name.toInt.vec <- c("dataset.sig.N","cancerType.sig.N")
        for(i in seq_along(name.toInt.vec)){
            n.i <- name.toInt.vec[i]
            if(n.i %in% colnames(geneDesc.tb)){ geneDesc.tb[[n.i]] <- as.integer(geneDesc.tb[[n.i]]) }
        }
        return(geneDesc.tb)
    }

}

