# Jeff Hemsley
# Synchronized volume over time with events for different sources
#

# used to adjust the alpha (transparency) of rgb colors below.
library(VGAM)

# set up some params for later use. I could have surfaced more plot 
# params but for this plot these are things I was fiddling with
plot.type <- "h"
text.adjust <- c(.5,.3) # left-right & up-down with .5 being center

# this is volume of data flow over hours, so how many hours. Normally
# you have this or will calculate it, but I am making random data
num.hours <- 300
my.local.x <- 1:num.hours

# points in time when events happened. These are entirely arbitrary.
event.x.vec <- c(45, 75, 275) 

# for this plot we are comparing three different information flow
# volumes: tweets, blog posts and news. Yes, blogs and news can be
# a fuzzy distinction, but this is about the R code.
# I use a random Pareto distribution because it is a reasonable
# approximation for some types of information flows/attention 
# (and if you want a reference supporting that let me know).
news.text <- "news"
tweets.counts <- rparetoI(num.hours, scale=10000, shape=3)
news.color <- rgb(128,30,128,alpha=255,maxColorValue=255) 

blogs.text <- "blogs"
blogs.counts <- rparetoI(num.hours, scale=100, shape=2)
blogs.color <- rgb(32,135,91,alpha=255,maxColorValue=255)

tweets.text <- "tweets"
news.counts <- rparetoI(num.hours, scale=1, shape=1.5)
tweets.color <- rgb(196,159,47,alpha=255,maxColorValue=255)

# probably not the best practice here, but I take a vector
# of x values for events at hour X and a vector of y min 
# and max. So for an x, draw a dotted line from y.min to
# y.max at x[i]
plot.events <- function(x.vec, y.min.max) {
  for (i in 1:length(x.vec)) {
    event.x <- x.vec[i]
    lines(c(event.x, event.x), y.min.max, col="black", lty=2)    
  }
}

# work horse function. Again, I could surface more plotting
# parameters, but I am focused on a specific kind of comparison
# I set a few defaults. X is expected to be a sequence, but it
# doesn't have to start at 0. I'm using hours, but it could be
# any units.
plot.vol <- function(x, y, event.x.vec, caption="", plot.type="h", col="black", adj=c(.5,.5)) {
  
  # after the basic plot we want to add the events and then the 
  # caption. Simple calcs to find where to put 'em:
  my.local.y.lim <- c(0, max(y)) 
  # I tried c(min(y), max(y)) but like the above better for
  # histogram type plots
  my.local.y.mid <- round(mean(my.local.y.lim), 0)
  my.local.x.mid <- round(median(x), 0)
  
  plot(x, y, ylim=my.local.y.lim, yaxt="n"
       , type=plot.type, lwd=2, col=col, bty="n"
       , main="", xlab="", ylab="")
  
  # I keep the captions simple and I put them in the
  # plot so that the data set plots are close enough together
  # for comparison
  plot.events(event.x.vec, my.local.y.lim)
  text(my.local.x.mid, my.local.y.mid, labels=caption, col=adjustcolor(col, alpha.f = .5), cex=5, adj=text.adjust)
  mtext("hour", side=1, line=1.5, cex=.8, adj=.1)
  mtext("volume", side=2, line=3.5, cex=.8) # #padj=-5,     ) #adj=c(0,1), cex=.8, outer=T)
  axis(2, at=axTicks(2), labels=format(axTicks(2), scientific=F, big.mark=","), las=2, tick=F, line=-2.2)
}
  
# now plot out each set. I set mfrow to get all three on the 
# same screen. The xpd param controls clipping for plot area.
# I only seem to need to set this for histogram type plots
#par(mar=c(5,4.5,2,2), mfrow=c(3,1), xpd=F) # c(bottom, left, top, right) c(5, 4, 4, 2) + 0.1.
par(mar=c(3.5, 5.5, 2, 2), mfrow=c(3,1), xpd=F) # c(bottom, left, top, right) c(5, 4, 4, 2) + 0.1.
plot.vol (my.local.x, tweets.counts, event.x.vec, tweets.text, plot.type, tweets.color, text.adjust)
plot.vol (my.local.x, blogs.counts, event.x.vec, blogs.text, plot.type, blogs.color, text.adjust)
plot.vol (my.local.x, news.counts, event.x.vec, news.text, plot.type, news.color, text.adjust)

# gimmy back my defaults.
par(mar=c(5.1,5.1,4.1,2.1), mfrow=c(1,1), xpd=NA) 

