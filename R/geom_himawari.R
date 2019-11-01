#' Geom objects for himawari plots
#'
#' This plot is designed for the situation where each datapoint is defined by a unique combination of two variables and is associated with two statistics, the second of which is bounded in [0, 1].
#' Each datapoint is visualized by a sunflower (hence, himawari) where the first statistic is used for the color and the second statistic is used to determine the number of petals.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @section Motivation:
#' Our aim is to avoid using the size to visualize the second statistic.
#' This complicates interpretation as the background color effectively becomes a new endpoint on the color scale,
#' requiring readers to consider two dimensions of color transitions.
#' At worst, the background color is the same as one of the existing endpoints and the interpretation is confounded.
#' 
#' By comparison, the himawari plot is simpler as the color scale only has one dimension that is reserved for the first statistic.
#' At a glance, the plot can be easily interpreted in the same manner as a heatmap.
#' The second statistic is deliberately less visible to simplify interpretation,
#' but is still available for a more detailed inspection of the plot.
#'
#' The \code{\link{sunflowerplot}} name was already taken, hence the unusual name for this function.
#' At least it sounds artistic.
#' 
#' @section Aesthetics:
#' \code{geom_himawari} requires the following aesthetics:
#' \itemize{
#' \item \code{x}, a categorical variable used to determine the x-axis position of the sunflower for each row in \code{data}.
#' \item \code{y}, a categorical variable used to determine the y-axis position of the sunflower for each row in \code{data}.
#' \item \code{fill}, a numeric variable used to determine the color of each sunflower for each row in \code{data}.
#' \item \code{percent}, a numeric variable containing values from [0, 1], 
#' used to determine the number of petals on the sunflower for each row in \code{data}.
#' }
#' It also understands the following optional aesthetics:
#' \itemize{
#' \item \code{color}, the color of the petals.
#' Usually set to the same value as \code{fill}.
#' \item \code{ratio}, the ratio of the radius of the sunflower's face to the petal length.
#' \item \code{size}, the width of the lines used to draw the petals.
#' \item \code{npetals}, the total number of petals on a sunflower with \code{percent=1}.
#' }
#' 
#' @examples
#' stuff <- data.frame(X=rep(LETTERS, 5), 
#'    Y=as.character(rep(1:5, each=26)), 
#'    LogFC=rnorm(26*5), 
#'    Prop=runif(26*5))
#' 
#' ggplot(stuff) + geom_himawari(aes(x=X, y=Y, fill=LogFC, percent=Prop))
#'
#' @export
#' @aliases GeomHimawari
#' @importFrom ggplot2 layer
geom_himawari <- function(mapping = NULL, data = NULL, stat = "identity",
    position = "identity", ..., na.rm = FALSE, show.legend = NA,
    inherit.aes = TRUE) 
{
    layer(
        geom = GeomHimawari , data = data, mapping = mapping, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

#' @export
#' @importFrom ggplot2 ggproto aes Geom draw_key_point
#' @importFrom grid polygonGrob gList gpar segmentsGrob
GeomHimawari <- ggproto("GeomHimawari", Geom,
    required_aes = c("x", "y", "fill", "percent"),

    default_aes = aes(color="grey50", ratio = 3, size = 2, npetals = 10),

    draw_key = draw_key_point,

    draw_panel = function(data, panel_params, coord) {
        coords <- coord$transform(data, panel_params)

        # Computing ellipse height and width.
        curpetals <- ceiling(coords$percent * coords$npetals)
        angles <- 2 * pi * sequence(curpetals)/rep(coords$npetals, curpetals)

        x0 <- rep(coords$x, curpetals)
        y0 <- rep(coords$y, curpetals)
        col <- rep(coords$colour, curpetals)

        # Computing the half-height and width for an ellipse.
        hwidth <- diff(head(panel_params$x.major, 2))/2
        hheight <- diff(head(panel_params$y.major, 2))/2
        SCALE <- 0.8
        x1 <- x0 + hwidth * sin(angles) * SCALE
        y1 <- y0 + hheight * cos(angles) * SCALE

        # Generating points for each ellipse (gridExtra::ellipseGrob has unpredictable 'size').
        N <- 50
        perim.angles <- seq_len(N)/N*2*pi
        FACE <- SCALE * rep(coords$ratio/(coords$ratio + 1), N)
        xperim <- rep(coords$x, each=N) + sin(perim.angles) * hwidth * FACE 
        yperim <- rep(coords$y, each=N) + cos(perim.angles) * hheight * FACE

        gList(
            segmentsGrob(x0=x0, y0=y0, x1=x1, y1=y1,
                gp = gpar(col = col, lwd = coords$size * .pt)
            ),
            polygonGrob(x=xperim, y=yperim,
                id.lengths=rep(N, nrow(coords)),
                gp = gpar(fill = coords$fill)
            )
        )
    }
)



