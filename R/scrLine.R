#' Screening: Lines
#'
#' Line plots for data screening
#'
#' Create line plots for data screening.
#'
#' @param cou a 3-digit country ISO code character string.
#' @param isic an integer to specify the version of STAN.
#' @param sources a list naming the sources to plot.
#' @param namevar a character string naming the variables to plot.
#' @param nameind a character string naming the industries to plot.
#' @param nameyear a numeric vector with years to plot.
#' @param display a logical to open the output file in a web browser.
#'
#' @author OECD STAN
#' @keywords plot
#' @seealso \code{\link{detrendScale}}
#' @export
#' @examples
#' data <- data.frame(sou=c(rep('UNSDSNA', 6)),
#' var=c(rep('EXPO', 6)),
#' ind=c(rep('CTOTAL', 6)),
#' year=c(1970:1975),
#' value=c(1.506e+11, 2.357e+11, 2.924e+11, 6.278e+11, 1.445e+12, 1.408e+12))
#' scrLine(data=data, isic=3, namevar='EXPO', nameind='CTOTAL', display=TRUE)

scrLine <- function(data=stop("'data' must be specified"),
                    isic=4,
                    sources=NULL,
                    by="var",
                    namevar=NULL,
                    nameind=NULL,
                    nameyear=NULL,
                    display=TRUE)
{
    require(ggplot2)
    require(gridExtra)
    ##
    if (isic==3)
    {
        path.cou <- PATH.COUi3
        stan.ind <- STANi3.INDALL
    }
    if (isic==4)
    {
        path.cou <- PATH.COUi4
        stan.ind <- STANi4.INDALL
    }

    data.plot <- data

    if (!is.null(sources))
    {
        list.sou <- sources[[cou]]
        list.sou <- sub("NSO", "NSONA", list.sou)
        list.sou <- sub("STD-SNA", "ANA", list.sou)
        list.sou <- c(list.sou, "UNSDSNA", "NAPATCH")
        data.plot <- data.plot[data.plot$sou%in%list.sou,]
    }

    if (!is.null(namevar)) data.plot <- data.plot[data.plot$var%in%namevar,]
    if (!is.null(nameind)) data.plot <- data.plot[data.plot$ind%in%nameind,]
    if (!is.null(nameyear)) data.plot <- data.plot[data.plot$year%in%nameyear,]

    data.plot$ind <- factor(data.plot$ind, levels = stan.ind[stan.ind%in%unique(data.plot$ind)])

    if (by=="var")
    {
        for (var in namevar) # currently only one value in namevar var = "PROD"
        {
            data.plot.var <- data.plot[data.plot$var==var,]
            ## data.plot.var <- data.plot.var[!is.na(data.plot.var$ind),]
            ## data.plot.var <- data.plot[data.plot$var==var & data.plot$ind%in%c("CTOTAL", "C01T05"),]

            if (nrow(data.plot.var) > 0)
            {

                ## p <- ggplot(data.plot.var[data.plot.var$sou=="NAPATCH", ], aes(x = year, y = value)) +
                p <- ggplot(data = data.plot.var, aes(x = year, y = value)) +
                    geom_line(alpha = 0) +
                        facet_wrap(~ ind, scales = "free_y", ncol = 1) +
                            expand_limits(y=0) +
                                xlim(min(nameyear), max(nameyear)) +
                                    ylab(label = NULL) +
                                        xlab(label = NULL) +
                                            theme_bw() +
                                                theme(legend.position = "top", legend.box = "horizontal") +
                                                    ggtitle(label = paste(cou, var))
                ##
                p1 <- p +
                    geom_line(data = data.plot.var[data.plot.var=="NAPATCH",], aes(x = year, y = value, colour = sou)) +
                        scale_color_manual(values = 1)
                ##
                p2 <- p +
                    geom_line(data = data.plot.var[data.plot.var$sou!="NAPATCH",], aes(x = year, y = value, colour = sou, linetype = sou))
                ##
                p3 <- arrangeGrob(p1, p2, ncol=2)
                print(p3)

            }

        }
    }
}
