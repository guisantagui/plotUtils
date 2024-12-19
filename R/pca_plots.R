
#' Obtain the top contributing variables to two PCs
#'
#' Given a `prcomp` object and two PCs, gives the n top contributing variables
#' to those two PCs, based on the variance contributed to the PCs by each
#' variable.
#'
#' @param PC A `prcomp` object.
#' @param topN Integer. The number of top contributing variables to be returned.
#' @param x The first PC for obtaining top contributing variables.
#' @param y The second PC for obtaining top contributing variables.
#'
#' @return A character vector of the top n contributing variables.
#'
#' @export
getTopContrib <- function(PC, topN = 12, x = "PC1", y = "PC2"){
        if (class(PC) != "prcomp"){
                stop("PC needs to be a prcomp object", call. = F)
        }
        if (topN > nrow(PC$rotation)){
                stop("topN can't be greater than the number of variables in PC object.",
                     call. = F)
        }
        if ((!x %in% colnames(PC$x)) | (!y %in% colnames(PC$x))){
                stop(sprintf("Both %s and %s need to be PCs in PC object",
                             x,
                             y),
                     call. = F)
        }
        contrib <- facto_summarize(PC,
                                   "var",
                                   axes = c(as.numeric(gsub("PC", "", x)),
                                            as.numeric(gsub("PC", "", y))))
        contrib <- contrib[order(contrib$contrib, decreasing = T),
                           c("name", "contrib")]
        topContrib <- as.character(contrib$name[1:topN])
        return(topContrib)
}

#' Plot a PCA scoreplot or biplot.
#'
#' Given a `prcomp` object, plots either a scoreplot or a biplot, with several
#' options.
#'
#' @param PC A `prcomp` object.
#' @param x The principal component to be shown in the x axis. `"PCN"`.
#' @param y The principal component to be shown in the y axis. `"PCN"`.
#' @param samp_info A dataframe with metadata of the samples, for coloring.
#' Needs to have a column named `"sample"` containing sample names of the
#' samples in `prcomp` object. If not provided, no shaping or coloring of the
#' points will be possible.
#' @param col A variable in `samp_info` dataframe which will be used to color
#' the points.
#' #' @param shape A variable in `samp_info` dataframe which will be used to
#' shape the points.
#' @param labs Logical. If sample labels should be displayed in the plot.
#' @param topNFeats Integer. Number of top contributing components to be
#' displayed in the biplot. By default all features will be displayed.
#' @param fix_coord Logical. If fixed coordinate scale of units in both axes
#' should be forced.
#' @param point_size Integer. Size of the points for the score plot.
#'
#' @return A `ggplot` object of the scoreplot/biplot.
#'
#' @export
plotPCA <- function(PC,
                    x = "PC1",
                    y = "PC2",
                    samp_info = NULL,
                    col = NULL,
                    shape = NULL,
                    labs = F,
                    biplot = F,
                    topNFeats = NULL,
                    fix_coord = T,
                    point_size = 3){
        if (class(PC) != "prcomp"){
                stop("PC needs to be a prcomp object", call. = F)
        }
        dat <- data.frame(obsnames = row.names(PC$x), PC$x)
        if ((!x %in% colnames(dat)[colnames(dat) != "obsnames"]) |
            (!x %in% colnames(dat)[colnames(dat) != "obsnames"])){
                stop(sprintf("%s or %s are not proper principal components.",
                             x,
                             y),
                     call. = F)
        }

        x_sym <- rlang::sym(x)
        y_sym <- rlang::sym(y)
        labs_in <- "obsnames"
        aes_args <- list(x = x_sym, y = y_sym, label = ensym(labs_in))

        dat <- dat[, c("obsnames", x, y)]
        if (!is.null(samp_info)){
                if (class(samp_info) != "data.frame"){
                        stop("sample_info must be a data.frame.",
                             call. = F)
                }
                if (!"sample" %in% colnames(samp_info)){
                        stop("There is no 'sample' column in sample_info dataframe.",
                             call. = F)
                }
                dat <- cbind.data.frame(dat,
                                        samp_info[match(dat$obsnames,
                                                        samp_info$sample),
                                                  colnames(samp_info) != "sample"])
                if (!is.null(col)){
                        if (col %in% colnames(dat)){
                                aes_args$col <- sym(col)
                        }else{
                                stop(sprintf("%s not in sample_info", col),
                                     call. = F)
                        }
                }
                if (!is.null(shape)){
                        if (shape %in% colnames(dat)){
                                aes_args$shape <- sym(shape)
                        }else{
                                stop(sprintf("%s not in sample_info", shape),
                                     call. = F)
                        }
                }
        }
        propVar <- summary(PC)$importance[2, c(x, y)]
        propX <- round(propVar[names(propVar) == x]*100, digits = 2)
        propY <- round(propVar[names(propVar) == y]*100, digits = 2)

        pcaPlt <- ggplot(dat, do.call(aes, aes_args)) +
                geom_point(size = point_size) +
                xlab(sprintf("PC1 (%s %%)", propX)) +
                ylab(sprintf("PC1 (%s %%)", propY)) +
                theme(title = ggtext::element_markdown(),
                      axis.title.y = ggtext::element_markdown(),
                      panel.background = element_blank(),
                      panel.border = element_rect(colour = "black", fill=NA,
                                                  linewidth = 1),
                      panel.grid.major = element_line(colour = "#d4d4d4"),
                      legend.position = "right")
        if (fix_coord){
                pcaPlt <- pcaPlt +
                        coord_fixed()
        }
        if (labs){
                pcaPlt <- pcaPlt +
                        geom_text_repel()
        }
        if (!is.null(topNFeats) & !biplot){
                warning("topNFeats is not NULL but biplot is FALSE. To show top N contributing features in the plot set biplot to TRUE.")
        }
        if (biplot){
                datapc <- data.frame(varnames=rownames(PC$rotation),
                                     PC$rotation)
                mult <- min(
                        (max(dat[,y]) - min(dat[,y])/(max(datapc[,y])-min(datapc[,y]))),
                        (max(dat[,x]) - min(dat[,x])/(max(datapc[,x])-min(datapc[,x])))
                )
                datapc <- transform(datapc,
                                    v1 = .7 * mult * (get(x)),
                                    v2 = .7 * mult * (get(y))
                )
                datapc$x0 <- rep(0, nrow(datapc))
                datapc$y0 <- rep(0, nrow(datapc))
                if(!is.null(topNFeats)){
                        varPlotFilt <- getTopContrib(PC,
                                                     topN = topNFeats,
                                                     x = x,
                                                     y = y)
                        datapc <- datapc[datapc$varnames %in% varPlotFilt, ]
                }
                pcaPlt <- pcaPlt +
                        geom_text_repel(data=datapc,
                                        aes(x=v1, y=v2,
                                            label=varnames),
                                        color = "black",
                                        size = 3,
                                        max.overlaps = 100,
                                        inherit.aes = F) +
                        geom_segment(data = datapc, aes(x=x0,
                                                        y=y0,
                                                        xend=v1,
                                                        yend=v2),
                                     arrow = arrow(length=unit(0.2,"cm"),
                                                   type = "closed",
                                                   angle = 20),
                                     alpha=0.75,
                                     color="black",
                                     size = 0.5,
                                     inherit.aes = F)
        }
        return(pcaPlt)
}

#' Plot a PCA multi-scoreplot or biplot.
#'
#' Given a `prcomp` object, and the number of PCs to be plotted, plots either a
#' multi-scoreplot or a multi-biplot, with several options.
#'
#' @param PC A `prcomp` object.
#' @param nComps Integer. The number of components to be plotted.
#' @param samp_info A dataframe with metadata of the samples, for coloring.
#' Needs to have a column named `"sample"` containing sample names of the
#' samples in `prcomp` object. If not provided, no shaping or coloring of the
#' points will be possible.
#' @param col A variable in `samp_info` dataframe which will be used to color
#' the points.
#' #' @param shape A variable in `samp_info` dataframe which will be used to
#' shape the points.
#' @param labs Logical. If sample labels should be displayed in the plot.
#' @param topNFeats Integer. Number of top contributing components to be
#' displayed in the biplot. By default all features will be displayed.
#' @param point_size Integer. Size of the points for the score plot.
#'
#' @return A `ggplot` object of the scoreplot/biplot.
#'
#' @export
doPCAMultiPlot <- function(PC,
                           nComps,
                           samp_info = NULL,
                           col = NULL,
                           shape = NULL,
                           labs = F,
                           topNFeats = NULL,
                           biplot = F,
                           point_size = 3){
        if (class(PC)[1] != "prcomp"){
                stop("PC needs to be a prcomp object", call. = F)
        }
        plotList <- list()
        for(j in 2:(nComps)){
                #j <- 2
                for(i in 1:(nComps - 1)){
                        #i <- 1
                        if(j > i){
                                scPlot <- plotPCA(PC,
                                                  x = sprintf("PC%s", i),
                                                  y = sprintf("PC%s", j),
                                                  samp_info = samp_info,
                                                  col = col,
                                                  shape = shape,
                                                  labs = labs,
                                                  topNFeats = topNFeats,
                                                  biplot = biplot,
                                                  fix_coord = F,
                                                  point_size = point_size)
                                if(j < nComps){
                                        scPlot <- scPlot +
                                                theme(axis.title.x = element_blank(),
                                                      axis.text.x = element_blank())
                                }
                                if(i > 1){
                                        scPlot <- scPlot +
                                                theme(axis.title.y = element_blank(),
                                                      axis.text.y = element_blank())
                                }
                        }else{
                                scPlot <- NA
                        }
                        plotList[[sprintf("PC%s_PC%s", i, j)]] <- scPlot
                }
        }
        multPlot <- ggarrange(plotlist = plotList,
                              common.legend = T,
                              ncol = nComps - 1,
                              nrow = nComps - 1,
                              widths = c(1, rep(.8, nComps-2)),
                              heights = c(rep(.8, nComps-2), 1))
        return(multPlot)
}
