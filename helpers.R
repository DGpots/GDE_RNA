
df_to_signature <- function(df) {
    
    colnames(df) <- c("pathway", "gene")
    pathways <- unique(df[[1]])
    sig <- list()
    
    for (i in seq_along(pathways)) {
        sig[[i]] <- df %>%
            filter(pathway == pathways[i]) %>%
            pull(gene)
    }
    names(sig) <- pathways
    return(sig)
}


deseq_format_test <- function(rds) {
    if ((class(rds[[1]]) == "DESeqDataSet") & (class(rds[[2]]) == "DESeqResults")) {
        return(TRUE)
    }
}


deseq_transform <- function(res, p_co, lfc_co) {
    res %<>%
        as.data.frame() %>%
        rownames_to_column(var = "symbol") %>%
        as_tibble() %>%
        filter(!is.na(padj)) %>%
        mutate(significant = ifelse(padj <= p_co & log2FoldChange >= lfc_co, 
                                    "Up",
                                    ifelse(padj <= p_co & log2FoldChange <= -lfc_co, 
                                           "Down", 
                                           "Not Sig")))
    return(res)
}


parse_rna_genes <- function(gene_list) {
    gene_list <- gsub("[[:space:]]", "", gene_list)
    genes <- str_split(gene_list, "(,|;)")[[1]]
    return(genes[genes != ""])
}


get_count_message <- function(mtx) {
    msg <- paste("HTSeq count data uploaded:", ncol(mtx), "files,", nrow(mtx), "genes")
    return(msg)
}


filter_mt <- function(mtx, metadata) {
    metadata[match(colnames(mtx), metadata[["Sample"]]),]
}


is_valid_dge <- function(mt, dge_var, g1, g2) {
    if (sum(mt[[dge_var]] == g1) <= 1 || sum(mt[[dge_var]] == g2) <= 1) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

get_dge_message <- function(isvalid, res = NULL) {
    if(isvalid) {
        msg <- paste0("Differential gene expression (DGE) analysis is finished\n\n",
                      res@elementMetadata[2,2] %>%
                          str_remove("^.*:") %>%
                          str_trim(), "\n\n",
                      "You can visualize the results in the 'DGE Visualization' tab.")
    } else {
        msg <- "The specified differential gene expression (DGE) analysis is not valid.\n\nThe sample size in each group should be > 1."
    }
    return(msg)
}



cts_to_dds <- function(mtx, metadata, var = 1) {
    
    withProgress(message = "Loading Data..", value = 0.3, {
    
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = mtx,
                                  colData = metadata,
                                  design= as.formula(paste0("~", var)))
    
    incProgress(0.5, message = "Building DESeq2 Data Objects..")

    dds <- DESeq2::DESeq(dds)
    return(dds)
    })
}


assign_km_clu <- function(vsd, km_res) {
    vsd@colData$Kmeans <- LETTERS[km_res$cluster]
    return(vsd)
}


assign_km_clu_col <- function(coldata, km_res) {
    coldata$Kmeans <- LETTERS[km_res$cluster]
    return(coldata)
}


trubble <- function(cts) {
    tmp <- as.data.frame(
        rbind(
            cts[1:5, ],
            ... = rep("...", length(cts[1, ])),
            cts[(nrow(cts) - 4):(nrow(cts)), ]
        )
    )
    
    if (ncol(tmp) > 10) {
        tmp2 <- tmp[, 1:10]
    } else {
        tmp2 <- tmp
    }
    
    nr <- nrow(cts)
    nc <- ncol(cts)
    
    if (ncol(tmp) > 10) {
        output <- paste(
            "Your pre-processed data contains", nr, "genes and", nc, 
            "samples. Showing the first 10 samples:\n"
        )
    } else {
        output <- paste(
            "Your pre-processed data contains", nr, "genes and", 
            nc, "samples.\n"
        )
    }
    
    test <- as.matrix(tmp2)
    test <- rbind(colnames(tmp2), test)
    y <- sprintf(paste0("%",max(nchar(test)),"s"), test)
    y <- matrix(y, nrow = 12)
    
    gen <- c("", rownames(tmp2))
    gen <- gsub("\\s", " ", format(gen, width = max(nchar(gen))))
    
    if (ncol(tmp) > 10) {
        output2 <- paste("\n", ncol(tmp) - 10, "Samples not shown\n")
    } else {
        output2 <- NULL
    }
    
    cat(output, "\n")
    for(i in 1:nrow(y)) {
        cat(gen[i], y[i, ], "\n")
    }
    
    cat(output2)
}


# Custom HTSeq count loader that is tolerant to different column names
htseq_to_mtx <- function(files) {
    if (is.character(files)) {
        file_names <- files
        file_paths <- files
    } else {
        file_names <- files$name
        file_paths <- files$datapath
    }

    df_list <- list()
    for (i in seq_along(file_paths)) {
        df <- read.table(file_paths[i], header = TRUE, check.names = FALSE)
        # detect gene and count columns
        gene_candidates <- c('gene_id', 'Geneid', 'GeneID', 'Gene', 'gene', colnames(df)[1])
        count_candidates <- c('raw_count', 'count', 'Counts', 'htseq_count', colnames(df)[2])
        gene_col <- intersect(gene_candidates, colnames(df))[1]
        count_col <- intersect(count_candidates, colnames(df))[1]
        if (is.na(gene_col) || is.na(count_col)) {
            stop('Unrecognized HTSeq count file format')
        }
        df$symbol <- stringr::str_split(df[[gene_col]], pattern = '\\|') %>%
            purrr::map_chr(`[`, 1)
        df <- df %>%
            dplyr::filter(symbol != '?') %>%
            dplyr::select(symbol, !!count_col)
        names(df)[2] <- 'raw_count'
        df <- df[!duplicated(df$symbol),]
        df$sample <- basename(file_names)[i]
        df_list[[i]] <- df
    }

    a <- dplyr::bind_rows(df_list) %>%
        tidyr::pivot_wider(names_from = sample, values_from = raw_count)
    mtx <- as.matrix(a[,-1])
    rownames(mtx) <- a$symbol
    return(mtx)
}

