
rm(list=ls())

setwd("C:/Users/mkarnauskas/Desktop/FINAL_TIME_SERIES_DATA")
source("plotIndicatorTimeSeries_vFeb17.r")

plotIndicatorTimeSeries("amo.csv", outname="fig4.1.pdf")
plotIndicatorTimeSeries("SST_3regions.csv", coltoplot=2:4, plotrownum=3, plotcolnum=1, sublabel=T, sameYscale=T, widadj=1.1, outname="fig4.2.pdf")
plotIndicatorTimeSeries("SLR_state.csv", coltoplot=2:5, plotrownum=4, plotcolnum=1, widadj=0.6, hgtadj=0.92, sublabel=T, sameYscale=T, outname="fig4.3.pdf")

plotIndicatorTimeSeries("Eutrophication.csv", coltoplot=2:16, plotrownum=5, plotcolnum=3, widadj=0.95, hgtadj=0.9, yposadj = 1.1, sublabel=T, outname="fig5.1.pdf")
plotIndicatorTimeSeries("bottomDO.csv", coltoplot=2:5, plotrownum=2, plotcolnum=2, widadj=1.1, sublabel=T, sameYscale=T, yposadj=0.8, outname="fig5.2.pdf") 
plotIndicatorTimeSeries("pH.csv", coltoplot=4, yposadj=1.3, widadj=2.2, outname="fig5.3.pdf")

plotIndicatorTimeSeries("seagrass_acreage.csv", coltoplot=2:7, plotrownum=3, plotcolnum=2, redgreen=F, hgtadj=0.92, sublabel=T, widadj=0.8, trendAnalysis=F, outname="fig6.1.pdf")
plotIndicatorTimeSeries("artificial_structuresFINAL.csv", coltoplot=3:4, plotrownum=2, plotcolnum=1, yposadj=1.3, widadj=0.7,  hgtadj=1.1, outname="fig6.2.pdf")
plotIndicatorTimeSeries("total_lulc_wetland.csv", widadj=1.5, outname="fig6.4.pdf")

plotIndicatorTimeSeries("NPP.csv", widadj=2.8, outname="fig7.1.pdf")
plotIndicatorTimeSeries("zooplankton.csv", coltoplot=2:3, plotrownum=2, plotcolnum=1, yposadj=1.1, sublabel=T, widadj=1.3, hgtadj=1.1, sameYscale=F, outname="fig7.2.pdf")
plotIndicatorTimeSeries("menhaden_abundance_index.csv", yposadj=0.8, widadj=0.8, outname="fig7.3.pdf")

plotIndicatorTimeSeries("biodiversity.csv",  coltoplot=c(8,11,2,5), plotrownum=2, plotcolnum=2, yposadj=0.8, sublabel=T, sameYscale=T, outname="fig8.1.pdf") 
plotIndicatorTimeSeries("biodiversity.csv", coltoplot=c(10,13,4,7), plotrownum=2, plotcolnum=2, yposadj=0.9, sublabel=T, sameYscale=T, outname="fig8.2.pdf")  
plotIndicatorTimeSeries("mean_trophic_level_catch.csv", coltoplot=2:5, plotrownum=4, plotcolnum=1, yposadj=1.0, sublabel=T, outname="fig8.3.pdf")
plotIndicatorTimeSeries("stock_status.csv", coltoplot=2:3, plotrownum=2, plotcolnum=1, sameYscale=T, widadj=2.85, hgtadj=1.2, outname="fig8.4.pdf")

plotIndicatorTimeSeries("economically_important_spp_abundance.csv", coltoplot=3:13, plotrownum=4, plotcolnum=3, yposadj=0.9, sublabel=F, outname="fig9.1.pdf")
plotIndicatorTimeSeries("economically_important_spp_abundance.csv", outname="fig9.2.pdf")
plotIndicatorTimeSeries("bird_standardized_abundancesFINAL.csv", coltoplot=3:7, plotrownum=5, plotcolnum=1, widadj=1.1, hgtadj=0.92, yposadj=1.1, sublabel=T, sameYscale=F, outname="fig9.3.pdf")
plotIndicatorTimeSeries("bird_standardized_abundancesFINAL.csv", sublabel=F, outname="fig9.4.pdf")


plotIndicatorTimeSeries("human_pop.csv", coltoplot=3:8, plotrownum=3, plotcolnum=2, yposadj=1, sublabel=T, widadj=0.4, outname="fig10.1.pdf")
plotIndicatorTimeSeries("avePopDensity.csv", widadj=1.5, redgreen=F, outname="fig10.2.pdf")
plotIndicatorTimeSeries("total_lulc_urban.csv", widadj=1.5, outname="fig10.4.pdf")

plotIndicatorTimeSeries("totalEmployment.csv", widadj=1.95, outname="fig10.6.pdf")
plotIndicatorTimeSeries("totalGDP.csv", coltoplot=3, widadj=1.95, outname="fig10.7.pdf")

plotIndicatorTimeSeries("commercial_landings_revenue.csv", coltoplot=2:3, plotrownum=2, plotcolnum=1, hgtadj=1.2, outname="fig10.8.pdf")
plotIndicatorTimeSeries("aveSocialConnectedness.csv", widadj=1.7, hgtadj=1.2, redgreen=F, outname="fig10.10.pdf")
plotIndicatorTimeSeries("recreational_effort.csv", coltoplot=2:3, plotrownum=2, plotcolnum=1, yposadj=1.0, widadj=1.2, hgtadj=1.1, outname="fig10.13.pdf")



########  example

plotIndicatorTimeSeries("example.csv", sublabel=F, widadj=0.9, yposadj=0.7)

par(mar=c(0,1,1,0), xpd=T)
plot(1:10, 1:10, col="white", ylim=c(1.5,8), axes=F, xlab="", ylab="")

points(c(1,1,1), c(2,3,4), pch=20, cex=6)                                                 # analyze mean of last 5 years
text(1,4, col="white", "+", cex=2.6, font=2) 
text(1,2, col="white", "-", cex=2.6, font=2)

arrows(6.55, 2.15, x1 = 6.85, y1 = 1.85, length = 0.1, angle = 45, code = 2, lwd = 3)    
arrows(6.5, 3, x1 = 6.9, y1 = 3, length = 0.1, angle = 45, code = 3, lwd = 3)   
arrows(6.55, 3.85, x1 = 6.85, y1 = 4.15, length = 0.1, angle = 45, code = 2, lwd = 3) 
        
text(1.25, 4, "greater than 1 S.D. from the mean", pos=4)
text(1.25, 3, "within 1 S.D. from the mean", pos=4)
text(1.25, 2, "less than 1 S.D. from the mean", pos=4)

text(7.0, 4, "slope greater than 1 S.D.", pos=4)
text(7.0, 3, "no trend", pos=4)
text(7.0, 2, "slope less than 1 S.D.", pos=4)      

text(5.5, 5.75, "recent trend analysis  ", font=2)
legend(1.55, 8.5, col=1, lty=c(8,1), lwd=2, c("mean indicator value        ", "+/- 1 S.D. from mean"), bty="n", horiz=T)
legend(3, 7.5, col=c("#FF000080", "#00FF0080"), pch=15, pt.cex=3, c("below 1 S.D.           ", "above 1 S.D."), bty="n", horiz=T)
legend(4.25, 5.25, col="#0000FF20", pch=15, pt.cex=4, c("   last 5 years"), bty="n", horiz=T)
abline(h=6.25)

dev.copy(pdf, "ex2.pdf", width=3, height=2) #, pointsize=12, res=72*4)
dev.off()   
