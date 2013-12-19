calculateZoomMarginVector <- function(g, zoom.node, start.zoom.in.margin) {
	l <- cbind(V(g)$x, V(g)$y)
	V(g)$norm.x <- layout.norm(l, -1, 1, -1, 1)[,1]
	V(g)$norm.y <- layout.norm(l, -1, 1, -1, 1)[,2]

	node.to.zoom.to.x <- V(g)[zoom.node]$norm.x # 0.2580286
	node.to.zoom.to.y <- V(g)[zoom.node]$norm.y # -0.08265183

	zoom.both.max <- start.zoom.in.margin
	original.scale <- 1

	zoom.top <- -((zoom.both.max/2) - (node.to.zoom.to.y/original.scale))
	zoom.bottom <- -((zoom.both.max/2) + (node.to.zoom.to.y/original.scale))
	zoom.left <- -((zoom.both.max/2) + (node.to.zoom.to.x/original.scale))
	zoom.right <- -((zoom.both.max/2) - (node.to.zoom.to.x/original.scale))
	# below, left, top, right
	zoom.margin <- c(zoom.bottom, zoom.left, zoom.top, zoom.right)
	zoom.margin
}

zoom.to.zoom <- function(start.zoom.margin, end.zoom.margin, steps.out) {
	zoom.bottom.vec <- seq(from=start.zoom.margin[1], to=end.zoom.margin[1], length=steps.out)
	zoom.left.vec <- seq(from=start.zoom.margin[2], to=end.zoom.margin[2], length=steps.out)
	zoom.top.vec <- seq(from=start.zoom.margin[3], to=end.zoom.margin[3], length=steps.out)
	zoom.right.vec <- seq(from=start.zoom.margin[4], to=end.zoom.margin[4], length=steps.out)

	zoom.margin.df <- data.frame(bottom=zoom.bottom.vec, left=zoom.left.vec, top=zoom.top.vec, right=zoom.right.vec)
	zoom.margin.df
}

loop.its <- max(V(g)$color.wave.iteration)

for (i in 4:loop.its) {

	wave.start <- i
	wave.end <- wave.start - 3
	
	if (wave.end < 3) {
		wave.end <- 3
	}

	cat(" wave ", i, "\n")
	tweet.wave.col.palette.count <- 1
	for (k in wave.start:wave.end) {
		
		wave.it.nodes <- which(V(g)$color.wave.iteration == k)
		tmp.color <- tweet.wave.col.palette[tweet.wave.col.palette.count]
		tweet.wave.col.palette.count <- tweet.wave.col.palette.count + 1
		cat(k, ": ", length(wave.it.nodes), " ", tmp.color, "\n", sep="")

		V(g)$color[wave.it.nodes] <- tmp.color
		if (k < wave.start) {
			V(g)$size[wave.it.nodes] <- tweeted.size 
		}
	}

	plot.f.name <- paste(dir.path.frames, "Peace_frame_", frame.number, "_", frame.set, ".png", sep="")
	frame.number <- frame.number + 1
	png(plot.f.name, width=frame.width, height=frame.height)
		  par(bg="black", xpd=NA, mar=c(0,0,0,0))
		  plot.igraph(g, edge.arrow.size=0.02, edge.arrow.width=0, margin=local.zoom.margin , main="", edge.lty=edge.lty)
	dev.off()
}

GetPathPoints <- function(start.x, end.x, start.y, end.y, space.between) {

	# get slope
	move.path.m <- ( (end.y - start.y) / (end.x - start.x) ) 
	# solve for b
	move.path.b <- start.y - move.path.m * start.x
	# distance between points
	move.path.d <- sqrt( (end.x - start.x)^2 + (end.y - start.y)^2 ) 
	# how many points along the path
	move.path.num.points <- floor(move.path.d / space.between)
	# make a vec of distances from starting point (x0,y0)
	move.path.d.vec <- seq(from=space.between, by=space.between, length=move.path.num.points)

	# when going from pos x to neg x, need move path to be neg
	if (end.x < start.x) {
		move.path.d.vec <- move.path.d.vec * -1
	}

	# x = x0 + distance/sqrt(1 + slope^2)
	move.path.x <- start.x + move.path.d.vec/sqrt(1 + move.path.m^2)
	# y = m * x + b
	move.path.y <- move.path.m * move.path.x + move.path.b

	data.frame(x = move.path.x, y = move.path.y)
}

