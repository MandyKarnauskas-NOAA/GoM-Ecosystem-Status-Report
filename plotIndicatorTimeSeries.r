################################################################################
#                                                                              #
#   Standardized plotting of indicator time series for Eco Status Report       #
#   M. Karnauskas, Nov 23, 2016 -  updated Mar 17 2020                         #
#                                                                              # 
#   Typical formatting used by Alaska Report Card and CC IEA                   #
#   Plots time series, values above and below 1 S.D., and mean                 # 
#   Highlights last 5 years of data and shows pattern in mean and trend        #
#                                                                              #
#   Note!!!  Need to use standardized .csv formatting                          #
#   Column 1 is time values, columns 2+ are indicator data                     #
#   Row 1 is indicator name (main title of plot)                               #
#   Row 2 is units (y-axis label)                                              #
#   Row 3 is spatial extent or other specifying information                    #
#   Time can be in yearly or monthly time step                                 #
#      - If annual time series, years must be in YYYY format (e.g., 2011)      #
#      - Monthly time series can be in any combination of text abbreviation    #
#         and year formats (e.g, Jan1986, Jan-86, 1986jan)                     #
#         Note: must exclude days! (e.g, 1-Dec will be read as December 2001)  #     
#                                                                              #
################################################################################
#                                                                              #
#  INPUTS AS FOLLOWS:                                                          #
#  filename  = .csv file in standardized indicator reporting format (above)    # 
#  coltoplot = column number of indicator file to plot (only col 2 is default) # 
#  sublabel  = whether extent description should appear within main label      #
#  plotrownum and plotcolnum are for multi-panel plots (if coltoplot >1)       #
#   - specify layout (e.g., 4 panels could be plotrownum = 2, plotcolnum = 2   #
#  adjlabmain = manual adjustment of text size of main label                   #
#  adjlaby    = manual adjustment of text size of y-axis label                 #
#  yposadj    = manual adjustment of position of y-axis label                  #
#  widadj     = adjust total width of plot                                     #
#  trendAnalysis = whether to highlight trend in mean and SD over last 5 years #
#   ** default is T unless fewer than 4 data points available in last 5 years  # 
#  outname    = specify alternate output filename (default is same as input)   #
#  sameYscale = for multi-panel plots, if y-axis scale should be the same      #
#  anom = calculate and plot monthly anomalies                                 #
#               - for monthly anomalies: anom = "mon"                          #
#               - for standardized monthly anomalies: anom ="stmon"            # 
#                                                                              #
#   function examples:                                                         #
#  plotIndicatorTimeSeries("indicator.csv", coltoplot=2:4, plotrownum=3)       #
#  plotIndicatorTimeSeries("amo.csv", coltoplot=2, plotrownum=1, plotcolnum=1, adjlabmain=1, sublabel=F)
#  plotIndicatorTimeSeries("aveSocialConnectedness.csv", coltoplot=2, plotrownum=1, plotcolnum=1, sublabel=F)
#  plotIndicatorTimeSeries("menhaden_abundance_index.csv", coltoplot=2, plotrownum=1, plotcolnum=1, sublabel=F, yposadj=0.8, widadj=0.8)
#  plotIndicatorTimeSeries("seagrass_acreage.csv", coltoplot=2:7, plotrownum=3, plotcolnum=2, adjlabmain=1, sublabel=T, widadj=0.6, trendAnalysis=F)
#                                                                              #
#                                                                              #
################################################################################

plotIndicatorTimeSeries <-  function(filename, coltoplot=2, sublabel=F, plotrownum = 1, plotcolnum = 1, 
                                     yposadj=1, widadj=1, hgtadj=1, trendAnalysis=T, redgreen=T, outname=NA, sameYscale=F, 
                                     anom="none")  {

  d1 <- read.table(filename, header=F, sep=",", skip=0, quote="")                         # load data file
  d <- read.table(filename, header=F, sep=",", skip=3, quote="")                          # load data file labels
  d <- d[rowSums(d[2:ncol(d)], na.rm=T)!=0,]

  tim_all <- d$V1                                                                   # temporal data
  if ( class(tim_all)=="numeric" )  {  monthly <- F  }   else  {  monthly <- T  }   # decide whether monthly or yearly and adjust accordingly
  if (monthly==T)  {
    yrlis <- as.numeric(unlist(regmatches(as.character(tim_all), gregexpr("[[:digit:]]+", as.character(tim_all)))))
      yrlis[which(yrlis < 1500)] <- yrlis[which(yrlis < 1500)] + 2000
      yrlis[which(yrlis > 2050)] <- yrlis[which(yrlis > 2050)] - 100
    molis <- matches <- unlist(regmatches(as.character(tim_all), gregexpr("[[:alpha:]]+", as.character(tim_all))))
    tim_all <- yrlis + (match(tolower(molis), tolower(month.abb))-1)/12  }
  
  if (monthly==F) { wid <- length(tim_all) }  else  { wid <- length(tim_all)/12 }       # adjustment for width
  if (length(tim_all) <= 10 & length(tim_all) > 5) {  wid <- wid*2  }
  if (length(tim_all) <= 5)  {  wid <- wid*3  }
  wid <- wid * widadj     #  set adjusted width if specified  

 if (plotcolnum + plotrownum > 2)  { plotcolnum2 <- plotcolnum*0.65; plotrownum2 <- plotrownum*0.65 }  else { plotcolnum2 <- plotcolnum; plotrownum2 <- plotrownum }
                                                                                # set graphics specifications
if (is.na(outname))  {  filnam <- paste(c(unlist(strsplit(filename, ".csv"))), ".pdf", sep="") }   else   {  filnam <- outname  }
                                                                                # layout for single or multi-panel plots
  nf <- layout(matrix(c(1:(plotrownum*plotcolnum*2)), plotrownum, plotcolnum*2, byrow = TRUE), rep(c(wid/5, 1), plotcolnum), rep(4, plotrownum))
  layout.show(nf)  
  
  if (length(coltoplot)==1 & length(tim_all) <= 5 | trendAnalysis==F)  {        # layout for single plots with fewer than 5 data points or no trend analysis
  nf <- layout(matrix(c(1:(plotrownum*plotcolnum)), plotrownum, plotcolnum, byrow = TRUE), rep(c(wid/5), plotcolnum), rep(3, plotrownum))
  layout.show(nf)         } 

  ymin <- min(d[,coltoplot], na.rm=T) * 0.99                                     # get common y scale
  ymax <- max(d[,coltoplot], na.rm=T) * 1.01    
                            
for (i in coltoplot)  {                                                         # loop through indicator columns
  
  co_all <- d[,i]                                                               # data
  
  if (anom=="mon")  { 
    moref <- (match(tolower(molis), tolower(month.abb)))
    moav  <- tapply(co_all, moref, mean)
    for (m in 1:12) {
      co_all[which(moref==m)] <- co_all[which(moref==m)] - moav[m]
    }  }
  
  if (anom=="stmon")  { 
    moref <- (match(tolower(molis), tolower(month.abb)))
    moav  <- tapply(co_all, moref, mean)
    most  <- tapply(co_all, moref, sd)
    for (m in 1:12) {
      co_all[which(moref==m)] <- (co_all[which(moref==m)] - moav[m])/most[m]
    }  }
  
  if (sum(!is.na(co_all)) == 0) {  plot.new(); plot.new()  }  else {
  
  tim <- tim_all[!is.na(co_all)]                                                # for dealing with missing values 
  co <- co_all[!is.na(co_all)]
  
if (length(tim) > 5) {  
  
  if (trendAnalysis==T)  {  par(mar=c(2.5,5,3,0), xpd=F)  }  else  {  par(mar=c(2.5,5,3,1), xpd=F)  } 
  par(mgp=c(3*yposadj,1,0))
  if (sublabel==T) { mm <- paste(as.character(d1[1,i]), "\n", as.character(d1[3,i])) } else { mm <- d1[1,i] }
  yl <- d1[2,i]
    if (anom=="mon")   { yl <- paste(yl, "\n", "monthly anomaly") }
    if (anom=="stmon") { yl <- paste(yl, "\n", "standardized monthly anomaly") }
    
  if (sameYscale==T)  {   plot(tim_all, co_all, col=0, axes=F, xlab="", ylab=yl, main=mm, ylim=c(ymin, ymax))    }                   # plot time series
  if (sameYscale==F)  {   plot(tim_all, co_all, col=0, axes=F, xlab="", ylab=yl, main=mm)                        }                   # plot time series
    colind <- c("#FF000080", "#00FF0080")                                       # shading of anomalies +/- 1 S.D.  
if (length(tim) >= 5 & redgreen==T) {
    for (j in 2:length(tim))  {  polygon(c(tim[j-1], tim[j], tim[j], tim[j-1]), y=c(mean(co, na.rm=T), mean(co, na.rm=T), co[j], co[j-1]), col=colind[as.numeric(mean(co[(j-1):j], na.rm=T) > mean(co, na.rm=T))+1], border=F) }  
                      }
  polygon(c(min(tim_all, na.rm=T)-5, max(tim_all, na.rm=T)+5, max(tim_all, na.rm=T)+5, min(tim_all, na.rm=T)-5), 
      c(mean(co_all, na.rm=T)-sd(co_all, na.rm=T), mean(co_all, na.rm=T)-sd(co_all, na.rm=T), mean(co_all, na.rm=T)+sd(co_all, na.rm=T), mean(co_all, na.rm=T)+sd(co_all, na.rm=T)), col="white", border=T)
  if (trendAnalysis==T)  {   polygon(c(max(tim_all, na.rm=T)-4.5-as.numeric(monthly)/2.1, max(tim_all, na.rm=T)+0.5-as.numeric(monthly)/2.4, max(tim_all, na.rm=T)+0.5-as.numeric(monthly)/2.4, max(tim_all, na.rm=T)-4.5-as.numeric(monthly)/2.1), 
      c((mean(co_all, na.rm=T)-sd(co_all, na.rm=T)), (mean(co_all, na.rm=T)-sd(co_all, na.rm=T)), (mean(co_all, na.rm=T)+sd(co_all, na.rm=T)), (mean(co_all, na.rm=T)+sd(co_all, na.rm=T))), col="#0000FF20", border=F)       }
  if (mean(diff(tim_all)) <= 1.1 )  {  lines(tim_all, co_all, lwd=2); points(tim_all, co_all, pch=20, cex=0.75)   }                       # plot time series 
  if (mean(diff(tim_all)) > 1.1  )  {  points(tim_all, co_all, pch=20, cex=1.5)   }
  abline(h=mean(co, na.rm=T), lty=8); abline(h=mean(co, na.rm=T)+sd(co, na.rm=T), lty=1); abline(h=mean(co)-sd(co), lty=1)
    if (length(tim) > 10)  { axis(1, at=seq(1900, 2050, 5)) } else { axis(1, at=seq(1900, 2050, 2)) }    
  axis(1, at=seq(1900, 2050, 1), tck=-0.015, lab=rep("", 151))                  # add axes
  axis(2, las=2); box()
       
  if (trendAnalysis==T)  {        
  par(mar=c(2.5,0,3,0))                                                         #  second panel on mean and trend of last 5 years
  if (monthly == F)  {  last5 <- co_all[(nrow(d)-4):nrow(d)]  }  else  {  last5 <- co_all[(nrow(d)-59):nrow(d)]  }
  plot(1, xlim=c(0.94,1.06), ylim=c(0.6, 1.6), col=0, axes=F, xlab="", ylab="")
  points(1, 1.225, pch=20, cex=5)                                                 # analyze mean of last 5 years
  if (sum(is.na(last5)) < 2)  {
  if (mean(last5, na.rm=T) > (mean(co, na.rm=T)+sd(co, na.rm=T)))  { text(1, 1.2, col="white", "+", cex=2.6, font=2) }
  if (mean(last5, na.rm=T) < (mean(co, na.rm=T)-sd(co, na.rm=T)))  { text(1, 1.2, col="white", "-", cex=2.6, font=2) }
  if (monthly == F)  {  res   <- summary(lm(last5~tim[1:5]))   } else {    res   <- summary(lm(last5~tim[1:60]))   }
    slope <- coef(res)[2,1]                                                     # analyze trend in last 5 years
    slopelim <- (1.0/ (length(last5))) * sd(co, na.rm=T)
      if (slope >  slopelim)  { arrows(0.98, 0.89, x1 = 1.02, y1 = 1.01, length = 0.08, angle = 45, code = 2, lwd = 3)  }  
      if (slope <  -slopelim) { arrows(0.98, 1.01, x1 = 1.02, y1 = 0.89, length = 0.08, angle = 45, code = 2, lwd = 3) }  
      if (slope <= slopelim & slope >= -slopelim) { arrows(0.97, 0.95, x1 = 1.03, y1 = 0.95, length = 0.08, angle = 45, code = 3, lwd = 3) } 
            }    }     }                                                             # end looping through indicator columns

if (length(tim) <= 5) { 
  
  par(mar=c(2.5,5,3,1), xpd=F)
  par(mgp=c(3*yposadj,1,0))
  if (sublabel==T) { mm <- paste(as.character(d1[1,i]), "\n", as.character(d1[3,i])) } else { mm <- d1[1,i] }
  if (sameYscale==T)  {   plot(tim_all, co_all, col=0, axes=F, xlab="", ylab=d1[2,i], main=mm, ylim=c(ymin, ymax))    }                   # plot time series
  if (sameYscale==F)  {   plot(tim_all, co_all, col=0, axes=F, xlab="", ylab=d1[2,i], main=mm)                        }                   # plot time series
    colind <- c("#FF000080", "#00FF0080")                                       # shading of anomalies +/- 1 S.D.  
if (length(tim) >= 5 & redgreen==T) {    for (j in 2:length(tim))  {  polygon(c(tim[j-1], tim[j], tim[j], tim[j-1]), 
                                 y=c(mean(co, na.rm=T), mean(co, na.rm=T), co[j], co[j-1]), col=colind[as.numeric(mean(co[(j-1):j], na.rm=T) > mean(co, na.rm=T))+1], border=F) }     }
  polygon(c(min(tim_all)-5, max(tim_all)+5, max(tim_all)+5, min(tim_all)-5), 
          c(mean(co_all, na.rm=T)-sd(co_all, na.rm=T), mean(co_all, na.rm=T)-sd(co_all, na.rm=T), mean(co_all, na.rm=T)+sd(co_all, na.rm=T), mean(co_all, na.rm=T)+sd(co_all, na.rm=T)), col="white", border=T)
  if (mean(diff(tim_all)) <= 1 )  {  lines(tim_all, co_all, lwd=2); points(tim_all, co_all, pch=20)   }                       # plot time series 
  if (mean(diff(tim_all)) > 1  )  {  points(tim_all, co_all, pch=20, cex=1.5)   }
  abline(h=mean(co, na.rm=T), lty=8); abline(h=mean(co, na.rm=T)+sd(co, na.rm=T), lty=1); abline(h=mean(co, na.rm=T)-sd(co, na.rm=T), lty=1)
  axis(1, at=tim) 
  axis(1, at=seq(1900, 2050, 1), tck=-0.015, lab=rep("", 151))                                                 # add axes
  axis(2, las=2); box()
  if (length(coltoplot)>1 & trendAnalysis==T)  {
  par(mar=c(0,0,0,0), xpd=F)                    
  plot(1, col="white", axes=F, xlab="", ylab="")     }           
      }   }  
       }
  
# adjust plot size for extra long labels 
longlabs <- max(nchar(as.character(mm)), nchar(as.character(d1[2,i])))
if ( longlabs > 30  )  {   
  wid <- longlabs/30 * wid    
  hgtadj <- longlabs/30 * hgtadj   } 
# 
 
dev.copy(pdf, filnam, width=((wid+10)/7)*plotcolnum2/1.3, height=hgtadj*(3.5*plotrownum2)/1.3)  #, pointsize=12, res=72*4)
dev.off()                                                                       # close graphics device
}                                                                               # end of function

