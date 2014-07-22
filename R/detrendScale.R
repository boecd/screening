#' Detrend Scale
#'
#' Screening: detrend and scale
#'
#' Outlier detection using de-trending and scaling of the time series for all non-time dimension combination categories.
#'
#' @param x a dataframe with columns "variable" and "ISICcombination".
#' @param variable a character string specifying a member of "variable" to be checked.
#' @param name a character string for the new variable name.
#' @param vis logical to display plot.
#' @param codesISIC a vector of industry codes in "ISICcombination".
#'
#' @author OECD STAN
#' @keywords Screening
#' @seealso nothing
#' @export
#' @examples
#' x <- detrendScale(x, "VAEMP", "VAEMPdiff", nameind = unique(x$ind))

###################################################
### code chunk number 31: screening_PA2.Rnw:3126-3189
###################################################
detrendScale <- function(x,
                         var,
                         name="newVar",
                         vis=TRUE,
                         nameind,
                         plotcol=6)
{
    x[, name] <- x[, var]
    x[, paste0(name, "dflag")] <- x[, var]
    ##
    for(i in nameind)
    {
        x[x$ind==i, name] <- scale(c(0, diff(x[x$ind==i, var])))
    }
    if(vis)
    {
        par(mfrow=c(ceiling(length(nameind)/plotcol), plotcol), mar=c(0.1,0.1,0,0), xaxt="n", yaxt="n")
        ylims <- c(min(x[, name], na.rm=TRUE), max(x[, name], na.rm=TRUE))
        xmid <- as.numeric(max(x$year)) - (as.numeric(max(x$year)) - as.numeric(min(x$year))) / 2
        ##
        plotcount <- 0
        for(i in nameind)
        {
            ##
            if(any(x[x$ind==i, name] > 0, na.rm=TRUE) )
            {
                plotcount <- plotcount + 1
                VA <- x[x$ind==i, name]
                x[, paste0(name, "dflag")] <- ifelse(VA > 2.5 || VA < -2.5, TRUE, FALSE)
                n <- length(VA)
                plot(subset(x, x$ind == i, select=c("year", name)),
                     type="n",pch=18,cex=0.5, ylim=ylims)
                ppoints <- rep("black", n)
                ppoints[VA > 2.5 ] <- "red"
                ppoints[VA < -2.5 ] <- "red"
                ppch <- rep(20, n)
                ppch[VA > 2.5 ] <- 19
                ppch[VA < -2.5 ] <- 19
                lines(subset(x, x$ind==i, select=c("year", name)) ,
                      type="o",pch=ppch, col=ppoints,cex=1)
                text(x=xmid, y=1.0, i, cex=1.5)
            }
            abline(h = 2.5)
            abline(h = -2.5)
        }
        addplot <- (ceiling(length(nameind) / plotcol) - (length(nameind) / plotcol)) * plotcol + length(nameind) - plotcount
        for (i in c(1:addplot))
        {
            plot.new()
        }
    }
}
