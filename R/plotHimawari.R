#' Create a himawari plot
#'
#' Create a himawari (sunflower) plot to simultaneously visualize two matrices of statistics.
#' Each matrix entry is represented by a sunflower in a grid.
#' The primary matrix is used for coloring of the sunflower, while the secondary matrix is used to determine the number of petals.
#' 
#' @param stat1 A numeric matrix of statistics.
#' @param stat2 A numeric matrix with the same dimensions as \code{stat1}, but with values in [0, 1].
#' @param radius Numeric scalar specifying the size of the sunflower faces.
#' Should be less than 0.5.
#' @param npetals Integer scalar specifying the maximum number of petals.
#' @param petal.len Numeric scalar specifying the length of the petals.
#' Should be less than \code{0.5 - radius}.
#' @param petal.width Numeric scalar specifying the width of the petals.
#' 
#' @details
#' This plot aims to visualize two matrices in a single grid using sunflowers to represent each entry of the matrix.
#' The first matrix is visualized using color, and the second matrix is visualized based on the number of petals.
#' As such, values of the second matrix are expected to lie within [0, 1].
#'
#' The motivation is to avoid using the size to visualize \code{stat2}.
#' This complicates interpretation as the background color effectively becomes a new endpoint on the color scale,
#' requiring readers to consider two dimensions of color transitions.
#' At worst, the background color is the same as one of the existing endpoints and the interpretation is confounded.
#' 
#' By comparison, the himawari plot is simpler as the color scale only has one dimension for \code{stat1}.
#' At a glance, the plot can be easily interpreted in the same manner as a heatmap.
#' \code{stat2} is pushed into the background but is still available for a more detailed inspection of the plot.
#'
#' The \code{\link{sunflowerplot}} name was already taken, hence the unusual name for this function.
#' At least it sounds artistic.
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
#' # Adding a standard red-white-blue color scheme
#' G <- plotHimawariBase(ave, num) + 
#'     scale_fill_gradient2(low="blue", high="red") +
#'     scale_color_gradient2(low="blue", high="red", mid="grey", guide=FALSE)
#' @export
#' @importFrom ggforce geom_circle
#' @importFrom ggplot2 ggplot coord_fixed aes_string scale_x_continuous scale_y_continuous
#' theme element_blank
plotHimawariBase <- function(stat1, stat2, radius=0.3, npetals=10, petal.len=0.1, petal.width=2) {
    if (!identical(dim(stat1), dim(stat2))) {
        stop("two matrices should have the same dimensions")
    }

    rn <- factor(rownames(stat1))
    cn <- factor(colnames(stat1))

    evals_long <- data.frame(
        Row = rep(rn, ncol(stat1)),
        Col = rep(cn, each = nrow(stat1)), 
        Stat1 = as.numeric(stat1),
        Stat2 = as.numeric(stat2)
    )

    # Replacing text with coordinates for the time being.
    evals_long$Row <- as.integer(evals_long$Row)
    evals_long$Col <- as.integer(evals_long$Col)

    G <- ggplot(evals_long) + coord_fixed() 

    # Generating the sunflower petals.
    adjr <- radius + petal.len
    notches <- vector("list", nrow(evals_long))

    for (i in seq_len(nrow(evals_long))) {
        if (evals_long$Stat2[i] > 0) { 
            number <- ceiling(evals_long$Stat2[i] * npetals)
            angles <- 2 * pi * seq_len(number)/npetals
            notches[[i]] <- data.frame(
                x0=evals_long$Row[i], 
                y0=evals_long$Col[i], 
                x1=evals_long$Row[i] + adjr * sin(angles),
                y1=evals_long$Col[i] + adjr * cos(angles),
                Stat1=evals_long$Stat1[i])
        }
    }

    notches <- do.call(rbind, notches)
    if (!is.null(notches)) {
        G <- G + geom_segment(
            aes_string(x="x0", xend="x1", y="y0", yend="y1", color="Stat1"),
            size=petal.width, data=notches, lineend="round")
    }

    G + geom_circle(aes_string(x0="Row", y0="Col", r=radius, fill="Stat1")) +
        scale_y_continuous(labels=levels(cn), breaks=seq_len(nlevels(cn))) +
        scale_x_continuous(labels=levels(rn), breaks=seq_len(nlevels(rn))) + 
        theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
}
