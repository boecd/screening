##
library(robustbase)
## library(ggplot2)
library(mFilter)
##
path.data <- "/home/z930/Dropbox/MARKUP/Data/"
path.data.out <- "/home/z930/Dropbox/MARKUP_own/Data/"
path.plot <- "/home/z930/Dropbox/UNIDO/programs/plot/"
stan_ind_all<- read.csv(paste0(path.data.out,'stan_ind.csv'))
ind_label <- stan_ind_all[!is.na(stan_ind_all$Ind4),colnames(stan_ind_all)%in%c('Ind4','LbInd_en','STANi4')]
stan_ind <- stan_ind_all[stan_ind_all$STANi4<145, colnames(stan_ind_all)%in%c('Ind4','A10','A21','A38','A64','A88','STANi4')]
STAN.INDA10 <- as.character(stan_ind[!is.na(stan_ind$A10),1])
STAN.INDA21 <- as.character(stan_ind[!is.na(stan_ind$A21),1])
STAN.INDA38 <- as.character(stan_ind[!is.na(stan_ind$A38),1])
STAN.INDA64 <- as.character(stan_ind[!is.na(stan_ind$A64),1])
STAN.INDA88 <- as.character(stan_ind[!is.na(stan_ind$A88),1])
##
namecou <- substr(list.files(path = paste(path.data,"STAN_i4/COU/",sep='')), start = 1, stop = 3)
namecou <- namecou[14]
##
namevar <- 'VALU'
##
nameind <- STAN.INDA10
indlist <- 'A10'

for (cou in namecou) {
  pdf(file = paste0(path.plot,namevar,'_',cou,'_',indlist,'.pdf'), width = 18, height = 9)
  ## for (indc in STAN.INDA10) {
    data <- read.csv(paste0(path.data,'STAN_i4/COU/',cou,'.csv'), header = F)[,-6]
    names(data) <- c('cou','var','ind','year','value')
    data <- data[data$var%in%namevar & data$ind%in%nameind,]
    ##
    growth.all <- NULL
    for(ind in nameind) {
      data.ind <- data[data$ind==ind,]
      data.ind.ts <- ts(data.ind$value, start = min(data.ind$year), end = max(data.ind$year))
      growth.ind <- diff(data.ind.ts) / lag(data.ind.ts, -1)
      growth.ind <- cbind.data.frame(ind, as.numeric(time(growth.ind)), as.numeric(growth.ind))
      growth.all <- rbind(growth.all, growth.ind)
    }
    names(growth.all) <- c('ind','year','value')
  par(mfrow = c(1,2)) # one row, two columns
  for (ind in nameind) {
    data.plot <- merge(growth.all[growth.all$ind==nameind[1],], growth.all[growth.all$ind==ind,], by = 'year')
    ##  
    robreg <- lmrob(value.y ~ value.x, data = data.plot)
    reg <- lm(value.y ~ value.x, data = data.plot)
    ## p <- ggplot() + geom_point(data = data.plot, aes(x = value.x, y = value.y)) + xlab(ind_label$LbInd_en[ind_label$Ind4==nameind[1]]) + ylab(ind_label$LbInd_en[ind_label$Ind4==indc]) + ggtitle(cou) + geom_segment(aes(x = min(value.x), y = min(value.y) * robreg$coefficients[2], xend = max(value.x), yend = max(data.plot$value.x) * robreg$coefficients[2]))
    if (!nrow(data.plot)==0) {
      plot(data.plot$value.x, data.plot$value.y, pch = 19, main = paste(cou,ind))
      abline(robreg, col = 'red')
      abline(reg, lty = 2)
      with(data.plot, text(data.plot$value.x, data.plot$value.y, labels = data.plot$year, pos = 4))
      fit2 <- arima(data.plot$value.y, order = c(1,0,0))
      fit2smooth <- hpfilter(fit2$residuals, drift=F)
      ## row.names(fit2$residuals)
      ## str(fit2)
      ## abs(fit2$residuals / sd(fit2$residuals)]
      ## plot(data.plot$year, fit2$residuals / sd(fit2$residuals), type = 'l')
      ## sd(fit2$residuals)
      plot(data.plot$year, fit2$residuals, type = 'l')
      lines(data.plot$year, fit2smooth$trend, col = "green")
    }
  }
  dev.off()
}


  gdp.ADB.df.cou.hp.trend <- as.data.frame(cbind(seq(start(gdp.ADB.df.cou.hp$trend)[1], end(gdp.ADB.df.cou.hp$trend)[1], 1), gdp.ADB.df.cou.hp$trend))

?labels
?plot

## In R the function ccf() can be used for estimating these cross correlations (p.27)
## ccf(x, y, lag.max = NULL, type = c("correlation", "covariance"),
##     plot = TRUE, na.action = na.fail, ...)


## arima(x, order = c(0, 0, 0),
##       seasonal = list(order = c(0, 0, 0),
##       period = NA),
##       xreg = NULL, include.mean = TRUE,
##       transform.pars = TRUE,
##       fixed = NULL,
##       init = NULL,
##       method = c("CSS-ML", "ML", "CSS"),
##       n.cond,
##       optim.method = "BFGS",
##       optim.control = list(),
##       kappa = 1e6)
