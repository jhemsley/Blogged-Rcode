# Author: Jeff Hemsley jhemsley at uw dot edu
# Twitter: @JeffHemsley
# Created: Sometime in 2010
# 
# the location of the files I used are at:
# http://somelab.net/wp-content/uploads/2013/01/thread_data_v7.csv
# http://somelab.net/wp-content/uploads/2013/01/thread_data_v6.csv
# note that rows are dates and cols are threads: series in cols.
# Grab these files or make your own and fix the path info below
#
f.path <- "c:/r/RUserGrp/"
f.name <- "thread_data_v7.csv"
f.raw.thread.mat <- paste(f.path, f.name, sep="")
plot.file.name <- paste(f.path, "topic5.png", sep="")

raw.thread.mat <- as.data.frame(read.csv(file=f.raw.thread.mat
                                         , header=TRUE, row.names=1))
num.conversations <- dim(raw.thread.mat) [2]
num.posts <- sum(raw.thread.mat)

# and, I already checked when the weekend dates are, so this vector
# will highlight the weekends in the plot: 1: time off, 0: work
week.end <- c(1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1
              , 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0)

# for these examples, the intensity is random binomial distributed
# becuase it seems to loosly resemble actual data
conversasion.intensity <- rbinom(num.conversations, 10, 0.3)
# since we are going to color the threads based on this, lets
# normalize the values 0 - 1
conversasion.intensity <- conversasion.intensity/max(conversasion.intensity)

# I usually use rgb colors, but hsv allows me to work in monochrome 
# h=0: red, .1: gold/yellow, .3: green, .6: blue, .7: purpleish
# V:  1 is brightest
# s (saturation) conversasion.intensity. 0 to 1, with 0 being none
conversasion.intensity.color <- hsv(h=.1, s=conversasion.intensity, v=1, alpha=1)

# I sent this out to a png, but it looks pretty nifty sent to pdf or svg
png(filename=plot.file.name, width=1024, height=768)
plotstackedseries(raw.thread.mat, seriesborders=T, splineseries=T,
                  col.vec=conversasion.intensity.color,
                  col.fg=rgb(.5,.5,.5,.5), weekend=week.end, 
                  f.main="Talk threads by volume and intensity",
                  f.ylab="Posts", f.xlab="Days"
)

sub.title <- paste("1 topic, ", num.conversations, " threads, ", num.posts, " posts")
mtext(sub.title, side=3)
dev.off()

