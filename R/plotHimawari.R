#' Create a himawari plot
#'
#' Create a himawari (sunflower) plot to simultaneously visualize log-fold changes and the percentage of expressing cells.
#' 
#' @param logfc A numeric matrix of statistics, usually a log-fold change for each gene (row) in each cluster (column)
#' compared to the average across all clusters.
#' @param percent A numeric matrix with the same dimensions as \code{logfc}, containing the percentage of expressing cells.
#' @param colors A logical scalar indicating whether a blue-white-red color scheme should be added automatically.
#' This can be \code{FALSE} to allow colors to be manually set if values other than log-FCs are used in \code{logfc}.
#' @param ... Arguments to pass to \code{\link{geom_himawari}}.
#' 
#' @details
#' Each matrix entry is represented by a sunflower in a grid.
#' \code{logfc} is used for coloring of the sunflower, while \code{percent} is used to determine the number of petals.
#' The motivation for this approach is discussed in more detail in \code{?\link{geom_himawari}}.
#' 
#' @return
#' A \link{ggplot} object containing the himawari plot.
#'
#' @author Aaron Lun
#' @examples
#' ############################
#' #### Setting up the data ###
#' ############################
#'
#' library(scRNAseq)
#' sce <- ZeiselBrainData()
#' 
#' library(scran)
#' library(scater)
#' sce <- logNormCounts(sce)
#' markers <- pairwiseTTests(logcounts(sce), sce$level1class)
#' output <- getTopMarkers(markers[[1]], markers[[2]], n=2)
#' 
#' features <- unlist(unlist(output))
#' object <- sce
#' group <- sce$level1class
#'
#' ##################################
#' #### Setting up the statistics ###
#' ##################################
#'
#' num <- numDetectedAcrossCells(object, ids = group, 
#'     subset_row = features, average = TRUE)
#' ave <- sumCountsAcrossCells(object, ids = group, subset_row = features,
#'     exprs_values="logcounts", average = TRUE)
#' 
#' ave <- ave - rowMeans(ave)
#' 
#' ########################
#' #### Making the plot ###
#' ########################
#' 
#' plotHimawari(ave, num) 
#'
#' @seealso
#' \code{\link{geom_himawari}}, for more manual control over the himawari parameters.
#' @export
#' @importFrom ggplot2 ggplot aes_string scale_color_gradient2 scale_fill_gradient2
#' theme element_blank
plotHimawari <- function(logfc, percent, colors=TRUE, ...) {
    if (!identical(dim(logfc), dim(percent))) {
        stop("two matrices should have the same dimensions")
    }

    rn <- factor(rownames(logfc))
    cn <- factor(colnames(logfc))
    evals_long <- data.frame(
        Row = rep(rn, ncol(logfc)),
        Col = rep(cn, each = nrow(logfc)), 
        LogFC = as.numeric(logfc),
        Percent = as.numeric(percent)
    )

    G <- ggplot(evals_long) + 
        geom_himawari(aes_string(x="Row", y="Col", fill="LogFC", colour="LogFC", percent="Percent"), ...)

    if (colors) {
        G <- G +
            scale_fill_gradient2(low="blue", high="red") +
            scale_color_gradient2(low="blue", high="red", mid="grey", guide=FALSE)
    }

    G
}
