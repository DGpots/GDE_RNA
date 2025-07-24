
vsd_km <- function(vsd, k) {
    pca <- DESeq2::plotPCA(
        vsd,
        intgroup = colnames(vsd@colData)[1],
        returnData = TRUE
    )
    set.seed(42)
    km_res <- kmeans(pca[, 1:2], k)
    return(km_res)
}

plot_pca_vsd <- function(vsd, var, pal, dir = 1) {
    pca <- DESeq2::plotPCA(vsd, intgroup = var, returnData = TRUE)
    percentVar <- round(100 * attr(pca, "percentVar"))

    p <- ggplot(pca, aes_string(x = "PC1", y = "PC2", color = var)) +
        geom_point(size = 3) +
        xlab(paste0("PC1: ", percentVar[1], "% variance")) +
        ylab(paste0("PC2: ", percentVar[2], "% variance")) +
        theme_minimal()

    if (is.numeric(vsd@colData[[var]])) {
        p <- p + scale_color_distiller(palette = pal, direction = dir)
    } else {
        p <- p + scale_color_brewer(palette = pal)
    }

    return(p)
}

plot_pca_vsd_km <- function(vsd, km_res, pal) {
    vsd@colData$Kmeans <- LETTERS[km_res$cluster]
    plot_pca_vsd(vsd, "Kmeans", pal)
}


deseq_table <- function(res, p_co, lfc_co) {
    
    res %<>%
        deseq_transform(p_co, lfc_co)
    
    res %<>%
        filter(significant != "Not Sig") %>%
        select(symbol:padj) %>%
        arrange(padj, abs(log2FoldChange)) %>%
        datatable() %>%
        formatRound(columns = c(2:5), digits = 3) %>%
        formatSignif(columns = c(6:7), digits = 3)
    
    return(res)
}
