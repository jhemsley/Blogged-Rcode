# Author: Jeff Hemsley jhemsley at uw dot edu
# Twitter: @JeffHemsley
# Created: Sometime in 2010
# 
# Function creates a stacked spline from a matrix of data. Can also use polygones
#   for straight lines instead of splines. The function is not mature in that it 
#   hasn't been tested (or even written) to deal a broad range of cases. I have
#   used it to plot a number of threads over time (days), but other applications
#   are certainly possible.
# 
#  mat.in: matrix where rows are x-axis and columns are y-axis. Uses rownames
#          for x-axis lables. The cell values indicate the y value for a given
#          column (thread) at a point in time (day).
#  seriesborders: (FALSE) to print borders around the spline or just the fill.
#          default is NO borders.
#  splineseries: (TRUE) if true then spline else polygone
#  col.vec: vector of colors. If null, random colors are assigned to the (columns) 
#          threads. 
#  col.fg: The color for the spline border. Default is grey.
#  weekend: a vector with the same length as the number of rows in mat.in. Function
#          expects a 0 (no shading) or 1 (vertical shaded block). If the i-th value 
#          in the vector is 1, then were x == 1 the function will shade. Used to 
#          note weekends. Alternate colors currently not supported.
#
plotstackedseries<-function(mat.in, seriesborders=FALSE, splineseries=TRUE
      , col.vec=NULL, col.fg=NULL, weekend=NULL
			, f.main="Stacked Spline Plot", f.ylab="Counts", f.xlab="Days"){ 
  	
	myfg <- par("fg")
	mybg <- par("bg")
	rows <- dim(mat.in)[1]
	cols <- dim(mat.in)[2]
	# Since we are plotting a shape instead of a line, we need an additonal
	# start and end point for each thread. This allows us to close the shape
	new.rows <- rows + 2
	a <- 2
	b <- rows + 1
	# we can adjust how curved the spline is relative to the original points. 
	# At 0 there is no cruve, at -1 it overshoots the points, I like .9.
	spline.shape <- c(0,rep(.9,rows),0)
	
    # new matrix with a first and last row of zeros to close the splines
    zip.vec <- rep(0, cols)
    m <- rbind(zip.vec, mat.in, zip.vec)

	# Now. The way to do this is to over plot each thread on top of the last.
    # So the thread at the top has to be shaped according to its own cell 
    # values as well as all the cells below it. In otherwords, we need to 
    # do a cummulative sum on the rows (we expect these to represent days).
	m <- t(apply(m, 1, cumsum))
  
	# now we plot an empty plot that we'll put splines on later
	y.max <- max(m)
	x <- seq(1:length(mat.in[,1]))
	# all of our splines are x y plots, so we need xs.
    x.vec <- c(0,x,max(x)) 

	plot(x, mat.in[,1], type="n", ylim=c(0,y.max)
		  , xaxt="n"
	    , yaxt="n"
			, main=f.main
			, ylab=f.ylab
			, xlab=f.xlab
			, bty="n"
	)
	
    # custom axis stuff.
    my.at <- axTicks(1)
	if (my.at[1] == 0) {
	  my.at[1] <- 1
	} 
    # bottom axis lables are the row names of the matrix
	axis(1, at=my.at, label=rownames(raw.thread.mat)[my.at], line=-.7
	     , las=0, tck=-.02, cex.axis=1.1, lwd=1) # las=2 for perpindicular
    # left had y axis 
	axis(2, at=axTicks(2), label=axTicks(2), line=-.7
	     , las=2, tck=0, cex.axis=1.1, lwd=0) 
  
    # if no color vector is passed in, use random heat colors
	if (is.null(col.vec)) {
	  col.vec <- sample(heat.colors(cols), size=cols, replace=F)
	}

    # deal with borders.
	if (seriesborders==FALSE) {
		col.fg <- col.vec
	} else if (is.null(col.fg)) { 
		col.fg <- myfg
	} 

	#print weekends?
	if (is.null(weekend)==FALSE) {
	  y.lable.max <- max(axTicks(2)) 
		for (i in 1:rows) {
			if (weekend[i]>0) {
				x.weekend<-c(i,i,i+1,i+1)
				y.weekend<-c(0,y.lable.max,y.lable.max,0)
				polygon(x.weekend, y.weekend, col=rgb(0,0,1,.1), border=NA)
			}
		}
	}

	# for the series, we work from back to frount. We plot the 
    # tallest filled in spline shape first, followed by the next
    # tallest and so on. So each spline series is plotted over the
    # last one. Nifty.
	for (i in cols:1) {
		
		if (length(col.fg)==1) {
			local.fg <- col.fg
		} else {
			local.fg <- col.fg[i]
		}

        # we can plot a spline or a polygon
		if (splineseries==TRUE) {
		  xspline(x.vec, m[,i], col=col.vec[i], shape=spline.shape, open=F
              , border=local.fg, lty=1, lwd=.5)
		} else {
			polygon(x.vec, m[,i], col=col.vec[i], border=local.fg)
		}
	}

	par(fg=myfg, bg=mybg)
}
