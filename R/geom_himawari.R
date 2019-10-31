library(ggplot2)
library(gridExtra)

geom_himawari <- function(mapping = NULL, data = NULL, stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA,
    inherit.aes = TRUE, ...) 
{
    layer(
        geom = GeomHimawari , data = data, mapping = mapping, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

GeomHimawari <- ggproto("GeomHimawari", Geom,
    required_aes = c("x", "y", "colour", "percent"),

    default_aes = aes(face=0.05, petal.ratio = 0.3, size = 2, npetals = 10),

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
        x1 <- x0 + hwidth * sin(angles) * 0.8
        y1 <- y0 + hheight * cos(angles) * 0.8

        # Generating points for each ellipse (gridExtra::ellipseGrob has unpredictable 'size').
        N <- 50
        perim.angles <- seq_len(N)/N*2*pi
        xperim <- rep(coords$x, each=N) + sin(perim.angles) * hwidth * 0.6
        yperim <- rep(coords$y, each=N) + cos(perim.angles) * hheight * 0.6

        grid::gList(
            grid::segmentsGrob(x0=x0, y0=y0, x1=x1, y1=y1,
                gp = grid::gpar(col = col,
                    lwd = coords$size * .pt)
            ),
            grid::polygonGrob(x=xperim, y=yperim,
                id.lengths=rep(N, nrow(coords)),
                gp = grid::gpar(fill = coords$colour)
            )
        )
    }
)


stuff <- data.frame(X=rep(LETTERS, 5), Y=as.character(rep(1:5, each=26)), LogFC=rnorm(26*5), NDetected=runif(26*5))
ggplot(stuff) + geom_himawari(aes(x=X, y=Y, color=LogFC, percent=NDetected))
