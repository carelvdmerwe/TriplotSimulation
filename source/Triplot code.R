# Sugnet Lubbe, Department of Statistical Sciences, UCT, November 2015
# ====================================================================

biplot.sample.control <- function (J, col = c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon"), pch = 3, cex = 1, label = F, label.cex = 0.75, label.side = "bottom", alpha = 1) 
{  while (length(col) < J) col <- c(col, col)
   col <- as.vector(col[1:J])
   while (length(pch) < J) pch <- c(pch, pch)
   pch <- as.vector(pch[1:J])
   while (length(cex) < J) cex <- c(cex, cex)
   cex <- as.vector(cex[1:J])
   while (length(label) < J) label <- c(label, label)
   label <- as.vector(label[1:J])
   while (length(label.cex) < J) label.cex <- c(label.cex, label.cex)
   label.cex <- as.vector(label.cex[1:J])
   while (length(label.side) < J) label.side <- c(label.side, label.side)
   label.side <- as.vector(label.side[1:J])
   while (length(alpha) < J) alpha <- c(alpha, alpha)
   alpha <- as.vector(alpha[1:J])
   list(col = col, pch = pch, cex = cex, label = label, label.cex = label.cex, label.side = label.side, alpha = alpha)
}

biplot.mean.control <- function (J, class.names, which = NULL, col = c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon"), pch = 16, cex = 1.5, label = T, label.cex = 1, label.side = "bottom", alpha = 1) 
{  if (!all(is.numeric(which))) which <- match(which, class.names, nomatch = 0)
   which <- which[which <= J]
   which <- which[which > 0]
   mean.num <- length(which)
   while (length(col) < mean.num) col <- c(col, col)
   col <- as.vector(col[1:mean.num])
   while (length(pch) < mean.num) pch <- c(pch, pch)
   pch <- as.vector(pch[1:mean.num])
   while (length(cex) < mean.num) cex <- c(cex, cex)
   cex <- as.vector(cex[1:mean.num])
   while (length(label) < mean.num) label <- c(label, label)
   label <- as.vector(label[1:mean.num])
   while (length(label.cex) < mean.num) label.cex <- c(label.cex, label.cex)
   label.cex <- as.vector(label.cex[1:mean.num])
   while (length(label.side) < mean.num) label.side <- c(label.side, label.side)
   label.side <- as.vector(label.side[1:mean.num])
   while (length(alpha) < mean.num) alpha <- c(alpha, alpha)
   alpha <- as.vector(alpha[1:mean.num])
   list(which = which, col = col, pch = pch, cex = cex, label = label, label.cex = label.cex, label.side = label.side, alpha = alpha)
}

biplot.new.sample.control <- function (n, col = "black", pch = 1, cex = 1, label = F, label.cex = 0.75, label.side = "bottom", alpha = 1,bg="white") 
{  
   while (length(col) < n) col <- c(col, col)
   col <- as.vector(col[1:n])
   while (length(col) < n) bg <- c(bg, bg)
   bg <- as.vector(bg[1:n])
   while (length(pch) < n) pch <- c(pch, pch)
   pch <- as.vector(pch[1:n])
   while (length(cex) < n) cex <- c(cex, cex)
   cex <- as.vector(cex[1:n])
   while (length(label) < n) label <- c(label, label)
   label <- as.vector(label[1:n])
   while (length(label.cex) < n) label.cex <- c(label.cex, label.cex)
   label.cex <- as.vector(label.cex[1:n])
   while (length(label.side) < n) label.side <- c(label.side, label.side)
   label.side <- as.vector(label.side[1:n])
   while (length(alpha) < n) alpha <- c(alpha, alpha)
   alpha <- as.vector(alpha[1:n])
   list(col = col, pch = pch, cex = cex, label = label, label.cex = label.cex, label.side = label.side, alpha = alpha,bg=bg)
}

biplot.ax.control <- function (p, X.names, which = 1:p, type = "prediction", col = "seagreen", lwd = 1, lty = 1, label = "Orthog", label.col = col, label.cex = 0.75, label.dist = 0, ticks = 5, tick.col = col, tick.size = 1, 
    tick.label = T, tick.label.col = tick.col, tick.label.cex = 0.6, tick.label.side = "left", tick.label.offset = 0.5, tick.label.pos = 1, predict.col = col, predict.lwd = lwd, predict.lty = lty, 
    ax.names = X.names, rotate = NULL, orthogx = 0, orthogy = 0, oblique = NULL) 
{  if (!all(is.numeric(which))) which <- match(which, X.names, nomatch = 0)
   which <- which[which <= p]
   which <- which[which > 0]
   ax.num <- length(which)
   if (type != "prediction" & type != "interpolation") stop("Incorrect type of biplot axes specified")
   while (length(col) < ax.num) col <- c(col, col)
   col <- as.vector(col[1:ax.num])
   while (length(lwd) < ax.num) lwd <- c(lwd, lwd)
   lwd <- as.vector(lwd[1:ax.num])
   while (length(lty) < ax.num) lty <- c(lty, lty)
   lty <- as.vector(lty[1:ax.num])
   if (label != "Orthog" & label != "Hor" & label != "Paral") stop("Incorrect specification of axis label direction")
   while (length(label.col) < ax.num) label.col <- c(label.col, label.col)
   label.col <- as.vector(label.col[1:ax.num])
   while (length(label.cex) < ax.num) label.cex <- c(label.cex, label.cex)
   label.cex <- as.vector(label.cex[1:ax.num])
   while (length(label.dist) < ax.num) label.dist <- c(label.dist, label.dist)
   label.dist <- as.vector(label.dist[1:ax.num])
   while (length(ticks) < ax.num) ticks <- c(ticks, ticks)
   ticks <- as.vector(ticks[1:ax.num])
   while (length(tick.col) < ax.num) tick.col <- c(tick.col, tick.col)
   tick.col <- as.vector(tick.col[1:ax.num])
   while (length(tick.size) < ax.num) tick.size <- c(tick.size, tick.size)
   tick.size <- as.vector(tick.size[1:ax.num])
   while (length(tick.label) < ax.num) tick.label <- c(tick.label, tick.label)
   tick.label <- as.vector(tick.label[1:ax.num])
   while (length(tick.label.col) < ax.num) tick.label.col <- c(tick.label.col, tick.label.col)
   tick.label.col <- as.vector(tick.label.col[1:ax.num])
   while (length(tick.label.cex) < ax.num) tick.label.cex <- c(tick.label.cex, tick.label.cex)
   tick.label.cex <- as.vector(tick.label.cex[1:ax.num])
   while (length(tick.label.side) < ax.num) tick.label.side <- c(tick.label.side, tick.label.side)
   tick.label.side <- as.vector(tick.label.side[1:ax.num])
   while (length(tick.label.offset) < ax.num) tick.label.offset <- c(tick.label.offset, tick.label.offset)
   tick.label.offset <- as.vector(tick.label.offset[1:ax.num])
   while (length(tick.label.pos) < ax.num) tick.label.pos <- c(tick.label.pos, tick.label.pos)
   tick.label.pos <- as.vector(tick.label.pos[1:ax.num])
   while (length(predict.col) < ax.num) predict.col <- c(predict.col, predict.col)
   predict.col <- as.vector(predict.col[1:ax.num])
   while (length(predict.lwd) < ax.num) predict.lwd <- c(predict.lwd, predict.lwd)
   predict.lwd <- as.vector(predict.lwd[1:ax.num])
   while (length(predict.lty) < ax.num) predict.lty <- c(predict.lty, predict.lty)
   predict.lty <- as.vector(predict.lty[1:ax.num])
   ax.names <- ax.names[which]
   while (length(ax.names) < p) ax.names <- c(ax.names, "")
   ax.names <- as.vector(ax.names[1:ax.num])
   if (!is.null(oblique)) if (length(oblique) != p) stop("For oblique translations values must be specified for each variable")
   while (length(orthogx) < p) orthogx <- c(orthogx, orthogx)
   orthogx <- as.vector(orthogx[1:p])
   while (length(orthogy) < p) orthogy <- c(orthogy, orthogy)
   orthogy <- as.vector(orthogy[1:p])
   list(which = which, type = type, col = col, lwd = lwd, lty = lty, label = label, label.col = label.col, label.cex = label.cex, label.dist = label.dist, ticks = ticks, tick.col = tick.col, tick.size = tick.size, 
        tick.label = tick.label, tick.label.col = tick.label.col, tick.label.cex = tick.label.cex, tick.label.side = tick.label.side, tick.label.offset = tick.label.offset, tick.label.pos = tick.label.pos, 
        predict.col = predict.col, predict.lty = predict.lty, predict.lwd = predict.lwd, names = ax.names, rotate = rotate, orthogx = orthogx, orthogy = orthogy, oblique = oblique)
}

biplot.trajectory.control <- function (p, X.names, which = 1:p, type = "prediction", col = NULL, lwd = 1, lty = 1, label.side = "right", label.col = col, label.cex = 0.75, label.dist = 0, ticks = 5, tick.col = col, tick.size = 1, 
                                       tick.label = T, tick.label.col = tick.col, tick.label.cex = 0.6, tick.label.side = "left", tick.label.offset = 0.5, tick.label.pos = 1, predict.col = col, predict.lwd = lwd, predict.lty = lty, 
                                       ax.names = X.names, rotate = NULL, orthogx = 0, orthogy = 0, oblique = NULL, num.points=100) 
{  if (!all(is.numeric(which))) which <- match(which, X.names, nomatch = 0)
   which <- which[which <= p]
   which <- which[which > 0]
   ax.num <- length(which)
   if (type != "prediction" & type != "interpolation" & type != "prediction.circle" & type != "prediction.normal" & type != "prediction.back") stop("Incorrect type of biplot axes specified")
   if (is.null(col)) { require (RColorBrewer)
                       col <- c(brewer.pal(12,"Set3")[-2],brewer.pal(8,"Accent"),brewer.pal(7,"Dark2"))    }           
   while (length(col) < ax.num) col <- c(col, col)
   col <- as.vector(col[1:ax.num])
   while (length(lwd) < ax.num) lwd <- c(lwd, lwd)
   lwd <- as.vector(lwd[1:ax.num])
   while (length(lty) < ax.num) lty <- c(lty, lty)
   lty <- as.vector(lty[1:ax.num])
   while (length(label.side) < ax.num) label.side <- c(label.side, label.side)
   label.side <- as.vector(label.side[1:ax.num])
   while (length(label.col) < ax.num) label.col <- c(label.col, label.col)
   label.col <- as.vector(label.col[1:ax.num])
   while (length(label.cex) < ax.num) label.cex <- c(label.cex, label.cex)
   label.cex <- as.vector(label.cex[1:ax.num])
   while (length(label.dist) < ax.num) label.dist <- c(label.dist, label.dist)
   label.dist <- as.vector(label.dist[1:ax.num])
   while (length(ticks) < ax.num) ticks <- c(ticks, ticks)
   ticks <- as.vector(ticks[1:ax.num])
   while (length(tick.col) < ax.num) tick.col <- c(tick.col, tick.col)
   tick.col <- as.vector(tick.col[1:ax.num])
   while (length(tick.size) < ax.num) tick.size <- c(tick.size, tick.size)
   tick.size <- as.vector(tick.size[1:ax.num])
   while (length(tick.label) < ax.num) tick.label <- c(tick.label, tick.label)
   tick.label <- as.vector(tick.label[1:ax.num])
   while (length(tick.label.col) < ax.num) tick.label.col <- c(tick.label.col, tick.label.col)
   tick.label.col <- as.vector(tick.label.col[1:ax.num])
   while (length(tick.label.cex) < ax.num) tick.label.cex <- c(tick.label.cex, tick.label.cex)
   tick.label.cex <- as.vector(tick.label.cex[1:ax.num])
   while (length(tick.label.side) < ax.num) tick.label.side <- c(tick.label.side, tick.label.side)
   tick.label.side <- as.vector(tick.label.side[1:ax.num])
   while (length(tick.label.offset) < ax.num) tick.label.offset <- c(tick.label.offset, tick.label.offset)
   tick.label.offset <- as.vector(tick.label.offset[1:ax.num])
   while (length(tick.label.pos) < ax.num) tick.label.pos <- c(tick.label.pos, tick.label.pos)
   tick.label.pos <- as.vector(tick.label.pos[1:ax.num])
   while (length(predict.col) < ax.num) predict.col <- c(predict.col, predict.col)
   predict.col <- as.vector(predict.col[1:ax.num])
   while (length(predict.lwd) < ax.num) predict.lwd <- c(predict.lwd, predict.lwd)
   predict.lwd <- as.vector(predict.lwd[1:ax.num])
   while (length(predict.lty) < ax.num) predict.lty <- c(predict.lty, predict.lty)
   predict.lty <- as.vector(predict.lty[1:ax.num])
   ax.names <- ax.names[which]
   while (length(ax.names) < p) ax.names <- c(ax.names, "")
   ax.names <- as.vector(ax.names[1:ax.num])
   if (!is.null(oblique)) if (length(oblique) != p) stop("For oblique translations values must be specified for each variable")
   while (length(orthogx) < p) orthogx <- c(orthogx, orthogx)
   orthogx <- as.vector(orthogx[1:p])
   while (length(orthogy) < p) orthogy <- c(orthogy, orthogy)
   orthogy <- as.vector(orthogy[1:p])
   list(which = which, type = type, col = col, lwd = lwd, lty = lty, label.side = label.side, label.col = label.col, label.cex = label.cex, 
        label.dist = label.dist, ticks = ticks, tick.col = tick.col, tick.size = tick.size, tick.label = tick.label, tick.label.col = tick.label.col, 
        tick.label.cex = tick.label.cex, tick.label.side = tick.label.side, tick.label.offset = tick.label.offset, tick.label.pos = tick.label.pos, 
        predict.col = predict.col, predict.lty = predict.lty, predict.lwd = predict.lwd, names = ax.names, rotate = rotate, orthogx = orthogx, orthogy = orthogy, oblique = oblique, num.points=num.points)
}

biplot.alpha.bag.control <- function (J, bag.names, which = NULL, alpha = 0.95, col = c("blue","green","gold","cyan","magenta","black","red","grey","purple","salmon"), lty = 1, lwd = 1, max = 2500, min = 10, Tukey.median = F, pch = 15, cex = 1) 
{  if (!all(is.numeric(which))) which <- match(which, bag.names, nomatch = 0)
   which <- which[which <= J]
   which <- which[which > 0]
   bag.num <- length(which)
   while (length(alpha) < bag.num) alpha <- c(alpha, alpha)
   alpha <- as.vector(alpha[1:bag.num])
   if (any(alpha < 0 | alpha > 0.99)) stop(message = "alpha not to be negative or larger than 0.99")
   alpha.entered <- alpha
   alpha <- round(alpha, digits = 2)
   if (any(abs(alpha.entered - alpha) > 0)) cat("alpha has been rounded to two desimal places\n")
   alpha <- 100 * alpha
   while (length(col) < bag.num) col <- c(col, col)
   col <- as.vector(col[1:bag.num])
   while (length(lty) < bag.num) lty <- c(lty, lty)
   lty <- as.vector(lty[1:bag.num])
   while (length(lwd) < bag.num) lwd <- c(lwd, lwd)
   lwd <- as.vector(lwd[1:bag.num])
   while (length(cex) < bag.num) cex <- c(cex, cex)
   cex <- as.vector(cex[1:bag.num])
   while (length(pch) < bag.num) pch <- c(pch, pch)
   pch <- as.vector(pch[1:bag.num])
   while (length(max) < bag.num) max <- c(max, max)
   max <- as.vector(max[1:bag.num])
   while (length(min) < bag.num) min <- c(min, min)
   min <- as.vector(min[1:bag.num])
   while (length(Tukey.median) < bag.num) Tukey.median <- c(Tukey.median, Tukey.median)
   Tukey.median <- as.vector(Tukey.median[1:bag.num])
   list(which = which, alpha = alpha, col = col, lty = lty, lwd = lwd, max = max, min = min, Tukey.median = Tukey.median, pch = pch, cex = cex)
}

biplot.kappa.ellipse.control <- function (J, ellipse.names, df=2, which = NULL, kappa = NULL, alpha = 0.95, col = c(1:8, 3:1), lty = 1, lwd = 1, alpha.transparency = 0.5) 
{  if (!all(is.numeric(which))) which <- match(which, ellipse.names, nomatch = 0)
   which <- which[which <= J]
   which <- which[which > 0]
   ellipse.num <- length(which)
   if (!is.null(alpha)) { while (length(alpha) < ellipse.num) alpha <- c(alpha, alpha)
                          alpha <- as.vector(alpha[1:ellipse.num])
                          if (any(alpha < 0 | alpha > 0.99)) stop(message = "alpha not to be negative or larger than 0.99")
                          alpha.entered <- alpha
                          if (is.null(kappa)) kappa <- sqrt(qchisq(alpha, df))                                               }
   while (length(kappa) < ellipse.num) kappa <- c(kappa, kappa)
   kappa <- as.vector(kappa[1:ellipse.num])
   while (length(col) < ellipse.num) col <- c(col, col)
   col <- as.vector(col[1:ellipse.num])
   while (length(lty) < ellipse.num) lty <- c(lty, lty)
   lty <- as.vector(lty[1:ellipse.num])
   while (length(lwd) < ellipse.num) lwd <- c(lwd, lwd)
   lwd <- as.vector(lwd[1:ellipse.num])
   while (length(alpha.transparency) < ellipse.num) alpha.transparency <- c(alpha.transparency, alpha.transparency)
   alpha.transparency <- as.vector(alpha.transparency[1:ellipse.num])
   list(which = which, kappa = kappa, col = col, lty = lty, lwd = lwd, alpha.transparency = alpha.transparency)
}

biplot.density.1D.control <- function (J, class.names, which = NULL, bw = "nrd0", kernel = "gaussian", col = c(1:8, 3:1), lty = 1, lwd = 1) 
{  if (!all(is.numeric(which))) which <- match(which, class.names, nomatch = 0)
   which <- which[which <= J]
   which <- which[which > 0]
   class.num <- length(which)
   while (length(bw) < class.num) bw <- c(bw, bw)
   bw <- as.vector(bw[1:class.num])
   while (length(kernel) < class.num) kernel <- c(kernel, kernel)
   kernel <- as.vector(kernel[1:class.num])
   while (length(col) < class.num) col <- c(col, col)
   col <- as.vector(col[1:class.num])
   while (length(lty) < class.num) lty <- c(lty, lty)
   lty <- as.vector(lty[1:class.num])
   while (length(lwd) < class.num) lwd <- c(lwd, lwd)
   lwd <- as.vector(lwd[1:class.num])
   list(which = which, bw = bw, kernel = kernel, col = col, lty = lty, lwd = lwd)
}

biplot.density.2D.control <- function (J, class.names, which = NULL, contours = F, h = NULL, n = 100, col = c("green", "yellow", "red"), contour.col = "black", cuts = 50, cex = 0.6, tcl = -0.2, mgp = c(0, -0.25, 0), 
                                       layout.heights = c(100, 10), legend.mar = c(2, 5, 0, 5)) 
{  if (!is.null(which)) if (which == "all") which <- 0
                        else if (!all(is.numeric(which))) which <- match(which, class.names, nomatch = 0)
   which <- which[which <= J]
   which <- which[which >= 0]
   if (!is.null(which)) which <- which[1]
   list(which = which, contours = contours, h = h, n = n, col = col, contour.col = contour.col, cuts = cuts, cex = cex, tcl = tcl, mgp = mgp, layout.heights = layout.heights, legend.mar = legend.mar)
}

biplot.class.region.control <- function (J, space.fill = 5, col = NULL, alpha = 1) 
{  if (is.null(col))
     col <- (colors()[round(seq(from=1,to=99,length=J+2))+261])[2:(J+1)]
   while (length(col) < J) col <- c(col, col)
   col <- as.vector(col[1:J])
   while (length(alpha) < J) alpha <- c(alpha, alpha)
   alpha <- as.vector(alpha[1:J])
   list(space.fill = space.fill[1], col = col, alpha = alpha)
}

biplot.legend.control <- function (columns = 1, columns.betw = -1, betw = c(1, -1, 0, 1), mar = c(3, 1, 3, 1), size = 2, text.width.multiplier = 1, label.cex = 0.7, label.col = "black", sample.cex = 1.2) 
{  list(columns = columns, columns.betw = columns.betw, betw = betw, mar = mar, size = size, text.width.multiplier = text.width.multiplier, label.cex = label.cex, label.col = label.col, sample.cex = sample.cex)
}

biplot.legend.type.control <- function (means = F, samples = F, bags = F) 
{  list(means = means, samples = samples, bags = bags)
}

# ----------------------------------------------------------------
# UTILITY FUNCTIONS
# ----------------------------------------------------------------

indmat  <- function (groep.vec) 
{  elements <- levels(factor(groep.vec))
   Y <- matrix(0, nrow = length(groep.vec), ncol = length(elements))
   dimnames(Y) <- list(NULL, paste(elements))
   for (i in 1:length(elements)) Y[groep.vec == elements[i], i] <- 1
   return(Y)
}

biplot.check.G <- function (G, n) 
{  if (is.null(G)) { G <- matrix(indmat(rep(1, n)), ncol = 1)
                     dimnames(G) <- list(1:n, "AllData")      }
   if (nrow(G) != n) stop("number of rows of X and G differ")
   if (is.null(dimnames(G))) dimnames(G) <- list(NULL, paste("class", 1:ncol(G), sep = ""))
   if (length(dimnames(G)[[2]]) == 0) dimnames(G)[[2]] <- paste("class", 1:ncol(G), sep = "")
   if (ncol(G) == 1) class.vec <- rep(dimnames(G)[[2]], n) else class.vec <- apply(t(apply(G, 1, function(x) x == max(x))), 1, function(s, G) dimnames(G)[[2]][s], G = G)
   G
}

biplot.check.X <- function (X, scaled.mat, centred.mat=TRUE) 
{  X <- as.matrix(X)
   unscaled.X <- X
   means <- apply(X, 2, mean)
   sd <- sqrt(apply(X, 2, var))
   if (!centred.mat) {  X <- X
                        means <- rep(0, ncol(X))
                        sd <- rep(1, ncol(X))     }      
   else { if (scaled.mat) X <- scale(X) else { X <- scale(X, scale = FALSE)
                                               sd <- rep(1, ncol(X))        }
        }  
    if (is.null(dimnames(X))) dimnames(X) <- list(paste(1:nrow(X)), paste("V", 1:ncol(X), sep = ""))
    if (length(dimnames(X)[[1]]) == 0) dimnames(X)[[1]] <- paste(1:nrow(X))
    if (length(dimnames(X)[[2]]) == 0) dimnames(X)[[2]] <- paste("V", 1:ncol(X), sep = "")
    list(X = X, unscaled.X = unscaled.X, means = means, sd = sd)
}

biplot.legend <- function (legend.type, legend.style, mean.list, sample.list, bag.list, class.names, quality.print = FALSE, quality = NA) 
{  
key.R <- function (x, y, ..., title = "", align = TRUE, background = 0, border = 0, between = 2, corner = c(missing(x), 1), divide = 3, transparent = FALSE, cex = par("cex"), cex.title = 1.5 * max(cex), col = par("col"), lty = par("lty"), lwd = par("lwd"), 
    font = par("font"), pch = par("pch"), adj = 0, type = "l", size = 5, columns = 1, between.columns = 3, angle = 0, density = -1, space = NULL, text.width.multiplier = 1) 
{  check.types <- function(x, type, types) {
                    n <- length(types)
                    match.type <- pmatch(type, types, duplicates.ok = TRUE)
                    if (any(is.na(match.type))) stop(paste(x, " must be \"", paste(types[-n], collapse = "\", \""), ", or \"", types[n], "\"", sep = ""))
                    types[match.type]
   }
   x <- x
   y <- y
   oldxpd <- par("xpd")
   on.exit(par(xpd = oldxpd), TRUE)
   rest <- list(...)
   colnames <- names(rest)
   for (i in (1:length(colnames))[colnames == "text"]) { if (!is.list(rest[[i]])) rest[[i]] <- list(rest[[i]])  }
   actions <- c("points", "lines", "rectangles", "text")
   colnames <- check.types("key components", colnames, actions)
   nrows <- max(sapply(unlist(rest, recursive = FALSE), length))
   ncols <- length(colnames)
   if (!missing(cex)) {  oldcex <- par("cex")
                         on.exit(par(cex = oldcex), TRUE)
                         par(cex = mean(cex))                 }
   cx <- par("cxy")[1]
   cy <- par("cxy")[2]
   cx1 <- cx/par("cex")
   cy1 <- cy/par("cex")
   replen <- function(a, b, n) rep(if (is.null(a)) b else a, length = n)
   for (j in seq(ncols)) {  this <- rest[[j]]
                            this$cex <- replen(this$cex, cex, nrows)
                            this$size <- replen(this$size, size, nrows)
                            this$type <- replen(this$type, type, nrows)
                            this$density <- replen(this$density, density, nrows)
                            this$angle <- replen(this$angle, angle, nrows)
                            this$col <- replen(this$col, col, nrows)
                            this$lty <- replen(this$lty, lty, nrows)
                            this$lwd <- replen(this$lwd, lwd, nrows)
                            this$adj <- replen(this$adj, adj, nrows)
                            this$font <- replen(this$font, font, nrows)
                            this$pch <- replen(this$pch, pch, nrows)
                            rest[[j]] <- this                                            }
   text.adj <- width <- height <- matrix(0, nrows, ncols)
   between <- rep(between, length = ncols)
   for (j in seq(ncols)) {  this <- rest[[j]]
                            for (i in seq(nrows)) { switch(colnames[j], points = { sx <- sy <- this$cex[i]         }, 
                                                                        lines = { sx <- this$size[i]
                                                                                  sy <- this$cex[i]                }, 
                                                                        rectangles = { sx <- this$size[i]
                                                                                       sy <- this$cex[i]           }, 
                                                                        text = { sx <- nchar(this[[1]][i]) * this$cex[i] * text.width.multiplier
                                                                                 sy <- this$cex[i]
                                                                                 text.adj[i, j] <- this$adj[i]     })
                                                    width[i, j] <- sx * cx1 + between[j] * cx
                                                    height[i, j] <- sy * cy1
                                                  }
                         }
   if (columns != 1) { slice <- function(x, p) {
                                  m <- nrow(x)
                                  n <- ncol(x)
                                  if (m%%p != 0) x <- rbind(x, matrix(0, p - m%%p, n))
                                  q <- nrow(x)/p
                                  dim(x) <- c(q, p, n)
                                  x <- aperm(x, c(1, 3, 2))
                                  dim(x) <- c(q, n * p)
                                  x
                               }
                       width[, ncols] <- width[, ncols] + cx * between.columns
                       width <- slice(width, columns)
                       nc <- ncol(width)
                       width[, nc] <- width[, nc] - cx * between.columns
                       height <- slice(height, columns)
                       text.adj <- slice(text.adj, columns)
                     }
   nc <- ncol(width)
   nr <- nrow(width)
   if (align)  for (j in seq(nc)) width[, j] <- max(width[, j]) 
   xpos <- ypos <- matrix(0, nr, nc)
   for (j in seq(length = nc - 1)) xpos[, j + 1] <- xpos[, j] + width[, j]
   xmax <- x + max(xpos + width)
   Between <- rep(between, each = nrow(xpos))
   xpos <- xpos + x + cx * Between * 0.5
   i <- text.adj != 0
   if (any(i)) xpos[i] <- (xpos + text.adj * (width - Between * cx))[i]
   for (i in seq(nr)) height[i, ] <- max(height[i, ])
   ypos[, ] <- y - cumsum(height[, 1])
   if (nchar(title)) ypos <- ypos - cy1 * cex.title
   ymin <- min(ypos) 
   title.excess <- x + cex.title * nchar(title) * cx1 * text.width.multiplier - xmax
   if (title.excess > 0) xmax <- xmax + title.excess
   x.offset <- (x - xmax) * corner[1]
   xpos <- xpos + x.offset + max(0, title.excess/2)
   y.offset <- (y - ymin) * (1 - corner[2])
   ypos <- ypos + y.offset + 0.5 * height
   if (background == 0) border <- as.numeric(border)
   if (!transparent) polygon(c(x, xmax, xmax, x) + x.offset, c(y, y, ymin, ymin) + y.offset + 0, col = background, border = border)
   if (nchar(title)) text((x + xmax)/2 + x.offset, y + y.offset - cex.title/2 * cy1, title, adj = 0.5, cex = cex.title)
   if (columns != 1) {  restack <- function(x, p) {  n <- ncol(x)/p
                                                     q <- nrow(x)
                                                     dim(x) <- c(q, n, p)
                                                     x <- aperm(x, c(1, 3, 2))
                                                     dim(x) <- c(p * q, n)
                                                     x
                                                  }
                        xpos <- restack(xpos, columns)[1:nrows, , drop = FALSE]
                        ypos <- restack(ypos, columns)[1:nrows, , drop = FALSE]
                     }
   for (j in seq(ncols)) {  this <- rest[[j]]
                            for (i in seq(nrows)) { switch(colnames[j], points = { points(xpos[i, j], ypos[i, j], cex = this$cex[i], col = this$col[i], font = this$font[i], pch = this$pch[[i]])            }, 
                                                                        lines = { if (this$type[i] != "p") { lines(xpos[i, j] + seq(0, 1, length = divide) * cx1 * this$size[i], rep(ypos[i, j], divide), cex = this$cex[i], lwd = this$lwd[i], type = this$type[i], lty = this$lty[i], pch = this$pch[[i]], font = this$font[i], col = this$col[i])
                                                                                                              if (this$type[i] == "b" || this$type[i] == "o") points(xpos[i, j] + seq(0, 1, length = divide) * cx1 * this$size[i], rep(ypos[i, j], divide), cex = this$cex[i], lwd = this$lwd[i], type = "p", lty = 1, pch = this$pch[[i]], font = this$font[i], col = this$col[i])
                                                                                                           } 
                                                                                  else points(xpos[i, j] + 0.5 * cx1 * this$size[i], ypos[i, j], cex = this$cex[i], lwd = this$lwd[i], type = this$type[i], lty = this$lty[i], pch = this$pch[[i]], font = this$font[i], col = this$col[i])
                                                                                }, 
                                                                        rectangles = { polygon(xpos[i, j] + c(0, rep(this$size[i] * cx1, 2), 0), ypos[i, j] + cy1 * this$cex[i] * c(-0.5, -0.5, 0.5, 0.5), col = this$col[i], angle = this$angle[i], density = this$density[i], border = FALSE)
                                                                                     }, 
                                                                        text = { text(xpos[i, j], ypos[i, j], this[[1]][i], adj = this$adj[i], cex = this$cex[i], col = this$col[i], font = this$font[i])            })
                                                  }
                         }
   }

   if (all(legend.type == FALSE)) return(cat("Change legend.type to obtain a legend\n"))
   par(pty = "m", mar = legend.style$mar)
   plot(x = c(0, 10), y = c(0, 10), type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
   usr <- par("usr")
   x <- usr[1]
   y <- usr[4]
   if (quality.print) mtext(text = paste("Quality of display", paste(round(quality * 100, 2), "%", sep = "")), adj = 0, cex = 0.8, at = 0, side = 3, line = 0.5, las = 0)
   if (legend.type$bags) 
     {  if (legend.type$means & legend.type$samples) key.R(x = x, y = y, corner = c(0, 1), between = legend.style$betw, border = T, columns = legend.style$columns, between.columns = legend.style$columns.betw, 
                                                           size = legend.style$size, text.width.multiplier = legend.style$text.width.multiplier, points = list(pch = mean.list$pch, col = mean.list$col, 
                                                           cex = legend.style$sample.cex), points = list(pch = sample.list$pch, col = sample.list$col, cex = legend.style$sample.cex), lines = list(lty = bag.list$lty, col = bag.list$col, 
                                                           lwd = bag.list$lwd), text = list(class.names, cex = legend.style$label.cex, col = legend.style$label.col))
        if (legend.type$means & !legend.type$samples) key.R(x = x, y = y, corner = c(0, 1), between = legend.style$betw, border = T, columns = legend.style$columns, between.columns = legend.style$columns.betw, 
                                                            size = legend.style$size, text.width.multiplier = legend.style$text.width.multiplier, points = list(pch = mean.list$pch, col = mean.list$col, 
                                                            cex = legend.style$sample.cex), lines = list(lty = bag.list$lty, col = bag.list$col, lwd = bag.list$lwd), text = list(class.names, cex = legend.style$label.cex, 
                                                            col = legend.style$label.col))
        if (!legend.type$means & legend.type$samples) key.R(x = x, y = y, corner = c(0, 1), between = legend.style$betw, border = T, columns = legend.style$columns, between.columns = legend.style$columns.betw, 
                                                            size = legend.style$size, text.width.multiplier = legend.style$text.width.multiplier, points = list(pch = sample.list$pch, col = sample.list$col, 
                                                            cex = legend.style$sample.cex), lines = list(lty = bag.list$lty, col = bag.list$col, lwd = bag.list$lwd), text = list(class.names, cex = legend.style$label.cex, 
                                                            col = legend.style$label.col))
        if (!legend.type$means & !legend.type$samples) key.R(x = x, y = y, corner = c(0, 1), between = legend.style$betw, border = T, columns = legend.style$columns, between.columns = legend.style$columns.betw, 
                                                             size = legend.style$size, text.width.multiplier = legend.style$text.width.multiplier, lines = list(lty = bag.list$lty, col = bag.list$col, 
                                                             lwd = bag.list$lwd), text = list(class.names, cex = legend.style$label.cex, col = legend.style$label.col))
     }
   else 
     {  if (legend.type$means & legend.type$samples) key.R(x = x, y = y, corner = c(0, 1), between = legend.style$betw, border = T, columns = legend.style$columns, between.columns = legend.style$columns.betw, 
                                                           size = legend.style$size, text.width.multiplier = legend.style$text.width.multiplier, points = list(pch = mean.list$pch, col = mean.list$col, 
                                                           cex = legend.style$sample.cex), points = list(pch = sample.list$pch, col = sample.list$col, cex = pch.samples.size[1]), text = list(class.names, cex = legend.style$label.cex, 
                                                           col = legend.style$label.col))
         if (legend.type$means & !legend.type$samples) key.R(x = x, y = y, corner = c(0, 1), between = legend.style$betw, border = T, columns = legend.style$columns, between.columns = legend.style$columns.betw, 
                                                             size = legend.style$size, text.width.multiplier = legend.style$text.width.multiplier, points = list(pch = mean.list$pch, col = mean.list$col, 
                                                             cex = legend.style$sample.cex), text = list(class.names, cex = legend.style$label.cex, col = legend.style$label.col))
         if (!legend.type$means & legend.type$samples) key.R(x = x, y = y, corner = c(0, 1), between = legend.style$betw, border = T, columns = legend.style$columns, between.columns = legend.style$columns.betw, 
                                                             size = legend.style$size, text.width.multiplier = legend.style$text.width.multiplier, points = list(pch = sample.list$pch, col = sample.list$col, 
                                                             cex = legend.style$sample.cex), text = list(class.names, cex = legend.style$label.cex, col = legend.style$label.col))
    }
}

Eigen.twosided <- function (A, B, eps = 1e-08) 
{  if (!(all((A - t(A)) <= 1e-06))) stop("A not symmetric")
   if (!(all((B - t(B)) <= 1e-06))) stop("B not symmetric")
   if (any(eigen(B)$values < eps)) stop("B not positive definite")
   svd.B.out <- svd(B)
   B.sqrt <- svd.B.out$u %*% diag(sqrt(svd.B.out$d)) %*% t(svd.B.out$u)
   svd.2.out <- svd(solve(B.sqrt) %*% A %*% solve(B.sqrt))
   W <- solve(B.sqrt) %*% svd.2.out$u
   Lambda.mat <- diag(svd.2.out$d)
   list(A.mat = A, B.mat = B, W.mat = W, Lambda.mat = Lambda.mat)
}

calc.alpha.bags <- function (X, alpha = 0.95, max.num = 2500, Tukey.median = TRUE, c.hull.n = 10) 
{  if (nrow(X) > max.num) X <- X[sample(1:nrow(X), max.num), ]
   x <- X[, 1]
   y <- X[, 2]
   if (is.vector(x) || (is.matrix(x) && !is.data.frame(x))) { if (!is.numeric(x)) 
                                                              stop(message = "x is not a numeric dataframe or vector.")    }
   if ((!is.matrix(x) && !is.vector(x)) || is.data.frame(x)) { if ((!is.data.frame(x) && !is.numeric(x)) || (!all(sapply(x, data.class) == "numeric"))) stop(message = "x is not a numeric dataframe or vector.")    }
   x <- as.matrix(x)
   if (dim(x)[2] != 1) stop(message = "x is not a vector.")
   if (is.vector(y) || (is.matrix(y) && !is.data.frame(y))) { if (!is.numeric(y)) stop(message = "y is not a numeric dataframe or vector.")    }
   if ((!is.matrix(y) && !is.vector(y)) || is.data.frame(y)) { if ((!is.data.frame(y) && !is.numeric(y)) || (!all(sapply(y, data.class) == "numeric"))) stop(message = "y is not a numeric dataframe or vector.")    }
   y <- as.matrix(y)
   if (dim(y)[2] != 1) stop(message = "y is not a vector.")
   if (nrow(x) != nrow(y)) stop(message = "x and y should have the same length!")
   na.x <- !is.finite(x)
   na.y <- !is.finite(y)
   ok <- !(na.x | na.y)
   x <- x[ok, , drop = FALSE]
   y <- y[ok, , drop = FALSE]
   n <- nrow(x)
   if (length(x) == 0) stop(message = "All observations have missing values")
   if (n == 1) stop(message = "The valid sample size should be at least two!")
   dimny <- dimnames(y)[[1]]
   if (length(dimny) == 0) dimny <- 1:n
   if (n < c.hull.n) { warning(paste("Samples too few for alpha bag for this class. Convex hull constructed with all sample points shown. "))
                       chull(x, y)                                                                                                                }
   else { storage.mode(x) <- "double"
          storage.mode(y) <- "double"
          interpx <- rep(0, 2 * n)
          storage.mode(interpx) <- "double"
          interpy <- rep(0, 2 * n)
          storage.mode(interpy) <- "double"
          datatyp <- matrix(0, n, 3)
          storage.mode(datatyp) <- "double"
          datatyp2 <- matrix(0, n, 2)
          storage.mode(datatyp2) <- "double"
          pxpy <- matrix(0, n, 3)
          storage.mode(pxpy) <- "double"
          whisk <- 2
          abagplot.uit <- .Fortran("abagplot", as.integer(n), as.integer(alpha), x, y, as.integer(whisk), tukm = double(2), interpx = interpx, interpy = interpy, num = as.integer(0), datatyp = datatyp, 
                                   indoutl = integer(n), datatyp2 = datatyp2, pxpy = pxpy, boxpl = as.integer(0), nointer = as.integer(0), PACKAGE = "UBbipl")
          tukmedian <- abagplot.uit$tukm
          x.vec <- abagplot.uit$interpx
          y.vec <- abagplot.uit$interpy
          if (all(x.vec == 0) & all(y.vec == 0)) stop(message = " x and y both null vectors")
          nie.nul <- !((x.vec == 0) & (y.vec == 0))
          list(x.vec[nie.nul], y.vec[nie.nul], tukmedian)                                                                                                                                                     }
}

calc.concentration.ellipse <- function (X, kappa = 2, covmat = NULL) 
{  means <- matrix(apply(X, 2, mean), nrow = 2)
   if (is.null(covmat)) covmat <- cov(X)
   range.vec <- apply(X, 2, range)
   mid.vec <- apply(range.vec, 2, function(x) (x[2] + x[1])/2)
   dif <- max(range.vec[2, ] - range.vec[1, ])/2
   xlim <- c(mid.vec[1] - dif, mid.vec[1] + dif)
   ylim <- c(mid.vec[2] - dif, mid.vec[2] + dif)
   svd.covmat <- svd(covmat)
   a <- (0:6283)/1000
   Y <- cbind(cos(a), sin(a))
   Y <- Y %*% diag(sqrt(svd.covmat$d)) %*% t(svd.covmat$v) * kappa
   Y + matrix(rep(1, 6284), ncol = 1) %*% t(means)
}

calibrate.axis <- function (j, unscaled.X, means, sd, axes.rows, ax.which, ax.tickvec, ax.orthogxvec, ax.orthogyvec, ax.oblique) 
{  ax.num <- ax.which[j]
   tick <- ax.tickvec[j]
   ax.direction <- axes.rows[ax.num, ]
   r <- ncol(axes.rows)
   ax.orthog <- rbind(ax.orthogxvec, ax.orthogyvec)
   if (nrow(ax.orthog) < r) ax.orthog <- rbind(ax.orthog, 0)
   if (nrow(axes.rows) > 1) phi.vec <- diag(1/diag(axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, j] else phi.vec <- (1/(axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, j]
   number.points <- 100
   std.ax.tick.label <- pretty(unscaled.X[, ax.num], n = tick)
   std.range <- c(min(std.ax.tick.label), max(std.ax.tick.label))
   std.ax.tick.label.min <- std.ax.tick.label - (std.range[2] - std.range[1])
   std.ax.tick.label.max <- std.ax.tick.label + (std.range[2] - std.range[1])
   std.ax.tick.label <- c(std.ax.tick.label, std.ax.tick.label.min, std.ax.tick.label.max)
   interval <- (std.ax.tick.label - means[ax.num])/sd[ax.num]
   axis.vals <- seq(from = min(interval), to = max(interval), length = number.points)
   axis.vals <- sort(unique(c(axis.vals, interval)))
   number.points <- length(axis.vals)
   axis.points <- matrix(0, nrow = number.points, ncol = r)
   for (i in 1:r) axis.points[, i] <- ax.orthog[i, ax.num] + (axis.vals - phi.vec[ax.num]) * ax.direction[i]
   if (!is.null(ax.oblique)) for (i in 1:r) axis.points[, i] <- axis.vals * ax.direction[i] - ((ax.oblique[ax.num] - means[ax.num])/sd[ax.num]) * ax.direction[i] + (((ax.oblique - means)/sd) %*% axes.rows[, i])/p
   axis.points <- cbind(axis.points, axis.vals * sd[ax.num] + means[ax.num], 0)
   for (i in 1:number.points) if (any(zapsmall(axis.points[i, r + 1] - std.ax.tick.label) == 0)) axis.points[i, r + 2] <- 1
   axis.points
}

calibrate.cat.axis <- function (j, axes.rows, ax.which, ax.orthogxvec, ax.orthogyvec, ax.oblique, markers, labels) 
{  ax.num <- ax.which[j]
   ax.direction <- axes.rows[ax.num, ]
   r <- ncol(axes.rows)
   ax.orthog <- rbind(ax.orthogxvec, ax.orthogyvec)
   if (nrow(ax.orthog) < r) ax.orthog <- rbind(ax.orthog, 0)
   if (nrow(axes.rows) > 1) phi.vec <- diag(1/diag(axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, j] else phi.vec <- (1/(axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, j]
   std.range <- range(markers)
   tick.label.min <- min(markers) - (std.range[2] - std.range[1])
   tick.label.max <- max(markers) + (std.range[2] - std.range[1])
   axis.vals <- c(tick.label.min, markers, tick.label.max)
   number.points <- length(axis.vals)
   axis.points <- as.data.frame(matrix(0, nrow = number.points, ncol = r))
   axis.points[,3] <- c(0, labels, 0)
   axis.points[,4] <- 1
   axis.points[1,4] <- axis.points[number.points,4] <- 0
   for (i in 1:r) axis.points[, i] <- ax.orthog[i, ax.num] + (axis.vals - phi.vec[ax.num]) * ax.direction[i]
   if (!is.null(ax.oblique)) for (i in 1:r) axis.points[, i] <- axis.vals * ax.direction[i] - ax.oblique[ax.num] * ax.direction[i] + (ax.oblique %*% axes.rows[, i])/p
   axis.points
}


biplot.create.regions2 <- function (Z,region.style, plot.range=NULL, col=NULL, add=TRUE, show.points=FALSE, plot=TRUE, plot.coords=NULL, border=c("black",NA), eps=0.00001, ...) 
{
  usr <- par("usr")
 #aaa <<- Z
 closest.mid <- function(x)
 {
   order(as.matrix(dist(rbind(x,Z)))[1,-1])[1]
 } 

#second part 
J <- nrow(Z)
plot.range <- 1*c(min(usr[c(1,3)]),max(usr[c(2,4)]))
x.vec <- y.vec <- seq(from=plot.range[1], to=plot.range[2], length=region.style$space.fill*100)
#frame.points <- rbind(cbind(plot.range[1],seq(from=plot.range[1],to=plot.range[2],length=50)),cbind(plot.range[2],seq(from=plot.range[1],to=plot.range[2],length=50)),
#                      cbind(seq(from=plot.range[1],to=plot.range[2],length=50),plot.range[1]),cbind(seq(from=plot.range[1],to=plot.range[2],length=50),plot.range[2]))
#colnames(frame.points) <- c("Var1","Var2")
my.grid <- expand.grid(x.vec, y.vec)
#for (h in 1:region.style$space.fill)
#{
#  out <- apply (my.grid, 1, closest.mid)
#  polys <- vector("list", J)
#  for (j in 1:J) polys[[j]] <- my.grid[out==j,][chull(my.grid[out==j,]),]
#  new.grid <- NULL
#  for (i in 1:(J-1))
#    for (k in (i+1):J)
#      for (i1 in 1:nrow(polys[[i]]))
#        for (k1 in 1:nrow(polys[[k]]))                    
#        {
#          if (nrow(polys[[i]]) > 1 & nrow(polys[[k]]) > 1) add.points <- cbind(rnorm(3,(polys[[i]][i1,1] + polys[[k]][k1,1])/2,(plot.range[2]-plot.range[1])/100),rnorm(3,(polys[[i]][i1,2] + polys[[k]][k1,2])/2,(plot.range[2]-plot.range[1])/100))
#          else add.points <- cbind(c(rnorm(3,Z[i,1],(plot.range[2]-plot.range[1])/100),rnorm(3,Z[k,1],(plot.range[2]-plot.range[1])/100)),c(rnorm(3,Z[i,2],(plot.range[2]-plot.range[1])/100),rnorm(3,Z[k,2],(plot.range[2]-plot.range[1])/100)))
#          new.grid <- rbind (new.grid, add.points)
#        }      
#  my.grid <- rbind(new.grid, frame.points)
#}
out <- apply (my.grid, 1,  closest.mid)
polys <- vector("list", J)
for (j in 1:J) polys[[j]] <- my.grid[out==j,][chull(my.grid[out==j,]),]
return(polys)
}


#biplot.create.regions <- function (region.style, plot.range, region.mid, rotate.mat=diag(2), reflect.mat=diag(2), ...) 
#{
#      
#      J <- nrow(region.mid)
#       x.vec <- y.vec <- seq(from=plot.range[1], to=plot.range[2], length=100)
#       frame.points <- rbind(cbind(plot.range[1],seq(from=plot.range[1],to=plot.range[2],length=50)),cbind(plot.range[2],seq(from=plot.range[1],to=plot.range[2],length=50)),
#                                 cbind(seq(from=plot.range[1],to=plot.range[2],length=50),plot.range[1]),cbind(seq(from=plot.range[1],to=plot.range[2],length=50),plot.range[2]))
#       colnames(frame.points) <- c("Var1","Var2")
#       my.grid <- expand.grid(x.vec, y.vec)
#           my.grid <- as.matrix(my.grid) %*% reflect.mat %*% t(rotate.mat)
#           mid.trans <- region.mid %*% reflect.mat %*% t(rotate.mat)
#       a<<- my.grid
#       b <<- region.mid
#       out <- knn.class.func(vec = my.grid,class.means= region.mid)
#       cc <<- out
#       for (h in 1:region.style$space.fill)
#         {
#          hh <<-h
#           Z.region <- vector("list", J)
#           for (j in 1:J) 
#             {
#             jj  <<- j
#             Z.region[[j]] <- my.grid[out==levels(out)[j],][chull(my.grid[out==levels(out)[j],]),]
#             }
#           #ee <<- "after"
#           new.grid <- NULL
#           for (i in 1:(J-1))
#             for (k in (i+1):J)
#                for (i1 in 1:nrow(Z.region[[i]]))
#                   for (k1 in 1:nrow(Z.region[[k]]))                    
#                                     {
#                                            if (nrow(Z.region[[i]]) > 1 & nrow(Z.region[[k]]) > 1) add.points <- cbind(rnorm(region.style$space.fill,(Z.region[[i]][i1,1] + Z.region[[k]][k1,1])/2,(plot.range[2]-plot.range[1])/100),rnorm(region.style$space.fill,(Z.region[[i]][i1,2] + Z.region[[k]][k1,2])/2,(plot.range[2]-plot.range[1])/100))
#                                                else add.points <- cbind(c(rnorm(region.style$space.fill,mid.trans[i,1],(plot.range[2]-plot.range[1])/100),rnorm(region.style$space.fill,mid.trans[k,1],(plot.range[2]-plot.range[1])/100)),c(rnorm(region.style$space.fill,mid.trans[i,2],(plot.range[2]-plot.range[1])/100),rnorm(region.style$space.fill,mid.trans[k,2],(plot.range[2]-plot.range[1])/100)))
#                        new.grid <- rbind (new.grid, add.points)
#                                      kk1 <- k1   
#                                      }      
#          my.grid <- rbind(new.grid, frame.points)
#        }

       #out <- apply (my.grid, 1, function(x){biplot.QDA.class.func(vec = x, J, phi.means = region.mid)})
       #Z.region <- vector("list", J)
       #for (j in 1:J) Z.region[[j]] <- my.grid[out==levels(out)[j],][chull(my.grid[out==levels(out)[j],]),]
      #     Z.region <- lapply (Z.region, function(X) X %*% rotate.mat %*% reflect.mat)
#}

#knn.class.func <- function(vec, class.means)
#  {newa <- data.frame(V1=vec[,1],V2=vec[,2],stringsAsFactors = TRUE)
#  newa[,1] <- as.numeric(as.character(newa[,1]))
#  newa[,2] <- as.numeric(as.character(newa[,2]))
#    newb <- data.frame(cbind(Class = dimnames(class.means)[[1]],V1=class.means[,1],V2=class.means[,2]),stringsAsFactors = TRUE)
#    newb[,2] <- as.numeric(as.character(newb[,2]))
#    newb[,3] <- as.numeric(as.character(newb[,3]))
#    x <- sknn(Class ~ ., kn=1,newb)
#    predict(x,newa)$class
#  }

#biplot.LDA.class.func <- function (vec, class.means, n, Mrr, class.dim=2, prior.prob=rep(1/J,J)) 
#{
#   J <- nrow(class.means)
#   while (length(vec) < class.dim) vec <- c(vec,0)
#   mat <- (rbind (vec, class.means))
#   rev(order(log(prior.prob)-as.matrix(dist(mat)^2)[1,-1]*(n-J)/2))[1]
#}

#biplot.QDA.class.func <- function (vec, J, phi.means, phi.sd=rep(1,length(phi.means)), Vmat=diag(2), prior.prob=rep(1/J,J)) 
#{
#   phi.vec <- vec %*% t(Vmat[,1:2])*phi.sd + phi.means
#   rev(order(log(prior.prob) - 0.5*phi.vec))[1]
#}

# ----------------------------------------------------------------
# DRAWING FUNCTIONS
# ----------------------------------------------------------------

draw.biplot <- function (Z, G = matrix(1, nrow = nrow(Z), ncol = 1), classes = 1, Z.means = NULL, z.axes = NULL, z.trajectories = NULL, z.bags = NULL, z.ellipse = NULL, Z.new = NULL, Z.density = NULL, Z.regions = NULL, sample.style, mean.style = NULL, 
                         ax.style = NULL, bag.style = NULL, ellipse.style = NULL, new.sample.style = NULL, density.style = NULL, region.style = NULL,
                                                 predict.samples = NULL, predict.means = NULL, Title = NULL, exp.factor = 1.2, plot.coords = NULL, ...) 
{
.samples.plot <- function(Z, G, classes, sample.style) {
                   x.vals <- Z[, 1]
                   y.vals <- Z[, 2]
                   invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
                   Z <- Z[invals, ]
                   G <- G[invals, ,drop=F]
                   for (j in 1:length(classes)) { class.num <- classes[j]
                                                  Z.class <- Z[G[, class.num] == 1, , drop = FALSE]
                                                  text.pos <- match(sample.style$label.side[j], c("bottom", "left", "top", "right"))
                                                  if (sample.style$label[j]) text(Z.class[, 1], Z.class[, 2], labels = dimnames(Z.class)[[1]], cex = sample.style$label.cex[j], col = sample.style$col[j], pos = text.pos)
                                                  if(nrow(Z.class)>0) for (i in 1:nrow(Z.class)) points(x = Z.class[i, 1], y = Z.class[i, 2], pch = sample.style$pch[j], col = sample.style$col[j], cex = sample.style$cex[j])                  }
}
.predict.func <- function(p.point, coef, col, lty, lwd) {
                   if (is.na(coef[2])) lines(c(p.point[1], coef[1]), rep(p.point[2], 2), col = col, lwd = lwd, lty = lty)
                   else if (coef[2] == 0) lines(rep(p.point[1], 2), p.point[2:3], col = col, lwd = lwd, lty = lty)
                        else { intercept.projection <- p.point[2] + p.point[1]/coef[2]
                               project.on.x <- (intercept.projection - coef[1])/(coef[2] + 1/coef[2])
                               project.on.y <- coef[1] + coef[2] * project.on.x
                               lines(c(p.point[1], project.on.x), c(p.point[2], project.on.y), col = col, lwd = lwd, lty = lty)     }
}
.marker.label.cm <- function(x, y, grad, marker.val, expand = 1, col, label.on.off, side, pos, offset, label.col, cex) {
                      uin <- par("pin")/c(usr[2] - usr[1], usr[4] - usr[3])
                      mm <- 1/(uin[1] * 25.4)
                      d <- expand * mm
                      if (grad == "v") { lines(rep(x, 2), c(y - d, y + d), col = col)
                                         if (label.on.off == 1) text(x, y - d, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)        }
                      if (grad == "h") { lines(c(x - d, x + d), rep(y, 2), col = col)
                                         if (label.on.off == 1) if (side == "right") text(x + d, y, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
                                                                else text(x - d, y, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)                     }
                      if (is.numeric(grad)) {  b <- d * sqrt(1/(1 + grad * grad))
                                               a <- b * grad
                                               lines(c(x - b, x + b), c(y - a, y + a), col = col)
                                               if (label.on.off == 1) if (side == "right") text(x + b, y + a, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)
                                                                      else text(x - b, y - a, label = marker.val, pos = pos, offset = offset, col = label.col, cex = cex)                  }
}
.marker.func <- function(vec, coef, col, tick.size, side, pos, offset, label.col, cex) {
                  x <- as.numeric(vec[1])
                  y <- as.numeric(vec[2])
                  marker.val <- vec[3]
                  label.on.off <- as.numeric(vec[4])
                  if (is.na(coef[2])) .marker.label.cm(x, y, grad = "h", marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
                  else if (coef[2] == 0) .marker.label.cm(x, y, grad = "v", marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
                       else .marker.label.cm(x, y, grad = -1/coef[2], marker.val, expand = tick.size, col = col, label.on.off = label.on.off, side = side, pos = pos, offset = offset, label.col = label.col, cex = cex)
}
.lin.axes.plot <- function(z.axes, ax.style, predict.mat) {
                    for (i in 1:length(ax.style$which)) 
                      {  ax.num <- ax.style$which[i]
                         marker.mat <- z.axes[[i]][z.axes[[i]][, 4] == 1, 1:3]
                         marker.mat <- marker.mat[rev(order(marker.mat[, 3])), ]
                         x.vals <- marker.mat[, 1]
                         y.vals <- marker.mat[, 2]
                         lin.coef <- coefficients(lm(y.vals ~ x.vals))
                         if (is.na(lin.coef[2])) abline(v = x.vals, col = ax.style$col[i], lwd = ax.style$lwd[i], lty = ax.style$lty[i])
                         else abline(coef = lin.coef, col = ax.style$col[i], lwd = ax.style$lwd[i], lty = ax.style$lty[i])
                         if (ax.style$label == "Hor") {  par(las = 1)
                                                         adjust <- c(0.5, 1, 0.5, 0)       }
                         if (ax.style$label == "Orthog") { par(las = 2)
                                                           adjust <- c(1, 1, 0, 0)         }
                         if (ax.style$label == "Paral") {  par(las = 0)
                                                           adjust <- c(0.5, 0.5, 0.5, 0.5) }
                         h <- nrow(marker.mat)
                         if (is.na(lin.coef[2])) 
                           { if (y.vals[1] < y.vals[h]) mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                             else mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                           }
                         else 
                           { y1.ster <- lin.coef[2] * usr[1] + lin.coef[1]
                             y2.ster <- lin.coef[2] * usr[2] + lin.coef[1]
                             x1.ster <- (usr[3] - lin.coef[1])/lin.coef[2]
                             x2.ster <- (usr[4] - lin.coef[1])/lin.coef[2]
                                 if (lin.coef[2] == 0) { if (x.vals[1] < x.vals[h]) mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                                                         else mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y.vals[1], col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                                                       }
                             if (lin.coef[2] > 0) 
                               {  if (x.vals[1] < x.vals[h]) if (y1.ster <= usr[4] & y1.ster >= usr[3]) mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                                                             else mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                                  else if (y2.ster <= usr[4] & y2.ster >= usr[3]) mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                                       else mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                               }
                             if (lin.coef[2] < 0) 
                               {  if (x.vals[1] < x.vals[h]) if (y1.ster <= usr[4] & y1.ster >= usr[3]) mtext(text = ax.style$names[i], side = 2, line = ax.style$label.dist[i], adj = adjust[2], at = y1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                                                             else mtext(text = ax.style$names[i], side = 3, line = ax.style$label.dist[i], adj = adjust[3], at = x2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                                  else if (y2.ster <= usr[4] & y2.ster >= usr[3]) mtext(text = ax.style$names[i], side = 4, line = ax.style$label.dist[i], adj = adjust[4], at = y2.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                                       else mtext(text = ax.style$names[i], side = 1, line = ax.style$label.dist[i], adj = adjust[1], at = x1.ster, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                               }
                            }
                                                invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
                        std.markers <- marker.mat[invals, 3]
                        if (is.numeric(std.markers)) std.markers <- zapsmall(std.markers)
                        x.vals <- x.vals[invals]
                        y.vals <- y.vals[invals]
                        if (ax.style$tick.label[i]) label.on.off <- rep(1, sum(invals)) else rep(0, sum(invals))
                        if (!ax.style$tick.label[i]) label.on.off[c(1, length(label.on.off))] <- 1
                        apply(cbind(x.vals, y.vals, std.markers, label.on.off), 1, .marker.func, coef = lin.coef, col = ax.style$tick.col[i], tick.size = ax.style$tick.size[i], side = ax.style$tick.label.side[i], 
                                                                                                 pos = ax.style$tick.label.pos[i], offset = ax.style$tick.label.offset[i], label.col = ax.style$tick.label.col[i], 
                                                                                                 cex = ax.style$tick.label.cex[i])
                        if (!is.null(predict.mat)) apply(cbind(predict.mat, y.vals[1]), 1, .predict.func, coef = lin.coef, col = ax.style$predict.col[i], lty = ax.style$predict.lty[i], lwd = ax.style$predict.lwd[i])
                      }
}

.nonlin.axes.plot <- function(z.axes, ax.style, predict.mat) {
                       for (i in 1:length(ax.style$which)) 
                         {  ax.num <- ax.style$which[i]
                            axis.mat <- z.axes[[i]]
                            axis.mat <- axis.mat[rev(order(axis.mat[, 3])), ]
                            x.vals <- axis.mat[, 1]
                            y.vals <- axis.mat[, 2]
                            xy.before <- rbind(axis.mat[-1,1:2],axis.mat[nrow(axis.mat),1:2])
                            xy.after <- rbind(axis.mat[1,1:2],axis.mat[-nrow(axis.mat),1:2])
                            coef.mat <- matrix(NA,nrow=nrow(axis.mat),ncol=2)
                            for (j in 1:nrow(axis.mat)) coef.mat[j,] <- coefficients(lm(c(xy.after[j,2],xy.before[j,2]) ~ c(xy.after[j,1],xy.before[j,1])))
                            invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
                            axis.mat <- axis.mat[invals,,drop=F]
                            coef.mat <- coef.mat[invals,,drop=F]
                            lines(axis.mat[,1], axis.mat[,2], col=ax.style$col[i], lwd=ax.style$lwd[i], lty=ax.style$lty[i])
                            if (ax.style$label.side[i]=="below") label.pos <- 1
                            if (ax.style$label.side[i]=="left") label.pos <- 2
                            if (ax.style$label.side[i]=="above") label.pos <- 3
                            if (ax.style$label.side[i]=="right") label.pos <- 4                     
                            if (nrow(axis.mat)>0) text(x=axis.mat[1,1], y=axis.mat[1,2], label = ax.style$names[i], offset = ax.style$label.dist[i], pos = label.pos, col = ax.style$label.col[i], cex = ax.style$label.cex[i])
                            marker.mat <- axis.mat[axis.mat[,4]==1,1:3, drop=F]
                            if (nrow(marker.mat)>0) { marker.mat[,3] <- zapsmall(marker.mat[,3])
                                                      coef.mat <- coef.mat[axis.mat[,4]==1,,drop=F]
                                                      if (ax.style$tick.label[i]) label.on.off <- rep(1, nrow(marker.mat)) else rep(0, nrow(marker.mat))
                                                      if (!ax.style$tick.label[i]) label.on.off[c(1, length(label.on.off))] <- 1
                                                      for (j in 1:nrow(marker.mat))                           
                                                        .marker.func (c(marker.mat[j,],label.on.off[j]), coef=coef.mat[j,], col = ax.style$tick.col[i], tick.size = ax.style$tick.size[i], side = ax.style$tick.label.side[i], 
                                                                      pos = ax.style$tick.label.pos[i], offset = ax.style$tick.label.offset[i], label.col = ax.style$tick.label.col[i], cex = ax.style$tick.label.cex[i])
                                                    }             
                        }                 
}

.bags.plot <- function(z.bags, bag.style) {
                for (i in 1:length(z.bags)) {  mat <- cbind(unlist(z.bags[[i]][1]), unlist(z.bags[[i]][2]))
                                               mat <- rbind(mat, mat[1, ])
                                               lines(mat, col = bag.style$col[i], lty = bag.style$lty[i], lwd = bag.style$lwd[i])
                                               if (bag.style$Tukey.median[i]) points(unlist(z.bags[[i]][3]), col = bag.style$col[i], pch = bag.style$pch, cex = bag.style$cex)
                                            }
}

.ellipse.plot <- function(z.ellipse, ellipse.style) {
                   for (i in 1:length(z.ellipse)) { lines(z.ellipse[[i]], col = ellipse.style$col[i], lty = ellipse.style$lty[i], lwd = ellipse.style$lwd[i])  }
}

.new.samples.plot <- function(Z.new, new.sample.style) {

                      points(Z.new[, 1], Z.new[, 2], pch = new.sample.style$pch, col = new.sample.style$col, cex = new.sample.style$cex,bg = new.sample.style$bg )
                       pos.vec <- rep(0, nrow(Z.new))
                       pos.vec <- match(new.sample.style$label.side, c("bottom", "left", "top", "right"))
                       if (any(new.sample.style$label)) text(Z.new[new.sample.style$label, 1], Z.new[new.sample.style$label, 2], labels = dimnames(Z.new)[[1]][new.sample.style$label], 
                                                             cex = new.sample.style$label.cex[new.sample.style$label], pos = pos.vec[new.sample.style$label])
}

.class.means.plot <- function(Z.means, mean.style) {
                       points(Z.means[, 1], Z.means[, 2], pch = mean.style$pch, col = mean.style$col, cex = mean.style$cex)
                       pos.vec <- rep(0, nrow(Z.means))
                       pos.vec <- match(mean.style$label.side, c("bottom", "left", "top", "right"))
                       if (any(mean.style$label)) text(Z.means[mean.style$label, 1], Z.means[mean.style$label, 2], labels = dimnames(Z.means)[[1]][mean.style$label], cex = mean.style$label.cex[mean.style$label], 
                                                       pos = pos.vec[mean.style$label])
}

.density.plot <- function(Z.density, density.style) {
                   levels.rect <- pretty(range(Z.density$z), n = density.style$cuts)
                   col.use <- colorRampPalette(density.style$col)
                   col.use <- col.use(length(levels.rect) - 1)
                   image(Z.density, breaks = levels.rect, col = col.use, add = TRUE)
                   if (density.style$contours) contour(Z.density, levels = levels.rect, col = density.style$contour.col, add = TRUE)
                   list(levels.rect, col.use)
}

.density.legend <- function(levels.rect, col.use) {
                     par(pty = "m", mar = density.style$legend.mar)
                     plot(range(levels.rect), y = 1:2, ylim = c(10, 100), xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", frame.plot = FALSE)
                     rect(xleft = levels.rect[-length(levels.rect)], ybottom = 10, xright = levels.rect[-1], ytop = 50, col = col.use, border = FALSE)
                     axis(side = 1, at = pretty(levels.rect, n = 8), labels = pretty(levels.rect, n = 8), line = 0, cex.axis = density.style$cex, mgp = density.style$mgp, tcl = density.style$tcl, las = 0)
}

.regions.plot  <- function(Z.regions, region.style) {
                     for (j in 1:length(Z.regions))
                                           polygon (Z.regions[[j]], col=region.style$col[j], border=NA)
}

   par(pty = "s", ...)
   if (!is.null(Z.density)) layout(mat = matrix(1:2, ncol = 1), heights = density.style$layout.heights)
   if (is.null(plot.coords))
     plot(Z[, 1] * exp.factor, Z[, 2] * exp.factor, xlim = range(Z[, 1] * exp.factor), ylim = range(Z[, 2] * exp.factor), xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", asp = 1)
   else
     plot(plot.coords$x, plot.coords$y, xlim = range(plot.coords$x), ylim = range(plot.coords$y), xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", asp = 1)  
   usr <- par("usr")

   if (!is.null(predict.samples)) predict.mat <- Z[predict.samples, , drop = F] else predict.mat <- NULL
   if (!is.null(predict.means)) predict.mat <- rbind(predict.mat, Z.means[predict.means, , drop = F])
   if (!is.null(Z.density)) density.out <- .density.plot(Z.density, density.style) else density.out <- NULL
   if (!is.null(Z.regions)) .regions.plot (Z.regions, region.style)
   if (!is.null(z.axes)) .lin.axes.plot(z.axes, ax.style, predict.mat)
   if (!is.null(z.trajectories)) .nonlin.axes.plot(z.trajectories, ax.style, predict.mat)
   if (length(classes) > 0) .samples.plot(Z, G, classes, sample.style)
   if (length(mean.style$which) > 0) .class.means.plot(Z.means, mean.style)
   if (!is.null(Z.new)) .new.samples.plot(Z.new, new.sample.style)
   if (length(z.bags) > 0) .bags.plot(z.bags, bag.style)
   if (length(z.ellipse) > 0) .ellipse.plot(z.ellipse, ellipse.style)
   if (!is.null(Title)) title(main=Title)
   if (!is.null(density.out)) .density.legend(density.out[[1]], density.out[[2]])
}

# =========================================================================================================================
# Main functions
# =========================================================================================================================

Postprob.triplot <- function (post.prob, X, G = NULL, scaled.mat = FALSE, dim.biplot = c(2, 1, 3), e.vects = 1:2, correlation.biplot = FALSE, classes = 1:ncol(G), 
                               samples = list(...), ax = list(...), ax.new = NULL, newprob=NULL, new.samples=list(...),
                               class.means = list(...), predict.means = NULL, predict.samples = NULL, alpha.bags = list(...), kappa.ellipse = list(...), density.style = list(...), class.regions = list(...),
                                                           colour.scheme = NULL, Title = NULL, exp.factor = 1.2, plot.coords=NULL, dim3.plane.col = "lightgrey", dim3.xdiameter = 2, dim3.ydiameter = 2, reflect = c(FALSE, "x", "y"), rotate = 0, select.origin = FALSE, legend.type = list(...), 
                               legend.format = list(...), adequacies.print = FALSE, output = 1:10, predictivity.print = FALSE, quality.print = FALSE, adjust.3d = c(0.5, 0.5), bag.alpha.3d = 0.7, aspect.3d = "iso", ax.list.3d = "black", 
                               cex.3d = 0.6, col.text.3d = "black", font.3d = 2, predictions.3D = TRUE, size.ax.3d = 0.5, size.means.3d = 10, size.points.3d = 5, xTitles.3d = c("", "", "Dim 1", "Dim 2", "Dim 3"), ID.labs = FALSE, 
                               ID.3d = 1:nrow(X), large.scale = FALSE, ort.lty = 1, prior.p = "sample.size", class.dim=2, spline.control = list(...), nonlin.ax=FALSE, eps=1e-5, ...) 
{  dim.biplot <- dim.biplot[1]
   if (dim.biplot != 1 & dim.biplot != 2) stop("Only 1D and 2D biplots")
   reflect <- reflect[1]
   J <- ncol(post.prob)
   n <- nrow(post.prob)
   if (!is.null(X))
   {
   X.info <- biplot.check.X(X, scaled.mat)
   X <- X.info$X
   unscaled.X <- X.info$unscaled.X
   means <- X.info$means
   sd <- X.info$sd
   G <- biplot.check.G(G, nrow(X))
   Nmat <- t(G) %*% G
   p <- ncol(X)
   }
   else G <- matrix (1, nrow=nrow(post.prob),ncol=J)
   if (!all(is.numeric(classes))) classes <- match(classes, dimnames(G)[[2]], nomatch = 0)
   classes <- classes[classes <= J]
   classes <- classes[classes > 0]
   
   one <- matrix (1, nrow=n, ncol=1)
   post.prob[post.prob<eps] <- eps
   if (!is.null(newprob)) { post.prob.new <- newprob
                            post.prob.new[post.prob.new<eps] <- eps
                          }     
  
   Lmat <- log(post.prob)   
   oneJ <- matrix (1, nrow=J, ncol=1)
   S <- (diag(n)-1/n*one%*%t(one)) %*% Lmat %*% (diag(J)-1/J*oneJ%*%t(oneJ))
   S.svd <- svd(S)
   
   Z <- S.svd$u[,e.vects[1:dim.biplot]] %*% diag(S.svd$d[e.vects[1:dim.biplot]])
   if (!is.null(X))
   {
   Bmat <- solve(t(Z) %*% Z) %*% t(Z) %*% X
   Br <- t(Bmat)
   quality <- NULL
   adequacy <- NULL
   axis.predictivity <- NULL
   sample.predictivity <- NULL
   if (adequacies.print & predictivity.print) stop("adequacies.print and predictivity.print cannot both be set to True")
   if (adequacies.print) dimnames(X)[[2]] <- paste(dimnames(X)[[2]], " (", fit.adequacy, ")", sep = "")
   if (predictivity.print) dimnames(X)[[2]] <- paste(dimnames(X)[[2]], " (", round(fit.predictivity, digits = 2), ")", sep = "")
   reflect.mat <- diag(dim.biplot)
   if (reflect == "x" & dim.biplot < 3) reflect.mat[1, 1] <- -1
   if (reflect == "y" & dim.biplot == 2) reflect.mat[2, 2] <- -1
   if (reflect == "xy" & dim.biplot == 2) reflect.mat[1:2, 1:2] <- diag(-1, 2)
   rotate.mat <- diag(dim.biplot)
   if (dim.biplot == 2) { if (!is.null(ax$rotate)) { if (is.numeric(ax$rotate)) { radns <- pi * rotate/180
                                                                                  rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)            }
                                                     else { if (ax$rotate == "maxpred") {  ax$rotate <- (names(fit.predictivity))[fit.predictivity == max(fit.predictivity)]
                                                                                           ax$rotate <- match(ax$rotate, dimnames(X)[[2]])                                      }
                                                            else ax$rotate <- match(ax$rotate, dimnames(X)[[2]])
                                                            radns <- -atan2(V.mat[ax$rotate, e.vects[2]], V.mat[ax$rotate, e.vects[1]])
                                                            rotate.mat <- matrix(c(cos(radns), -sin(radns), sin(radns), cos(radns)), ncol = 2)
                                                          }
                                                   }
                        }

             var.names <- dimnames(X)[[2]]
         num.vars <- p
         z.axes <- NULL
         z.trajectories <- NULL
        if (nonlin.ax)
              {
            ax <- do.call("biplot.trajectory.control", c(num.vars, list(var.names), ax))
            if ((length(ax$which) > 0) & (ax$type == "prediction")) z.trajectories <- lapply(1:length(ax$which), biplot.spline.axis, Z, unscaled.X, means=means, sd=sd, n.int=ax$ticks, spline.control=spline.control)
            for (j in 1:length(z.trajectories))
              z.trajectories[[j]][,1:2] <- z.trajectories[[j]][,1:2] %*% rotate.mat %*% reflect.mat
          }
        Z <- Z %*% rotate.mat %*% reflect.mat
        Br <- Br %*% rotate.mat %*% reflect.mat
        class.means.mat <- as.matrix(solve(t(G) %*% G) %*% t(G) %*% unscaled.X, ncol = ncol(unscaled.X))
        Z.means.mat <- NULL
        if (!is.null(class.means)) Z.means.mat <- as.matrix(solve(t(G) %*% G) %*% t(G) %*% Z, ncol = ncol(Z))
        dimnames(Z) <- list(dimnames(X)[[1]], NULL)
     }
   else { z.axes <- NULL
          z.trajectories <- NULL
                } 
   Z.new <- NULL
   if (!is.null(newprob)) {  Snew <- log(post.prob.new) - 1/n*(matrix(1,ncol=1,nrow=nrow(newprob))%*%t(one)%*%Lmat) - (log(post.prob.new)%*%oneJ%*%t(oneJ))+ mean(Lmat)*matrix(1,ncol=1,nrow=nrow(newprob))%*%t(oneJ)/J   
                             Z.new <- Snew %*% S.svd$v[,e.vects[1:dim.biplot]]
                                                         if (is.null(dimnames(newprob)[[1]])) dimnames(Z.new) <- list(paste("N", 1:nrow(Z.new), sep = ""), NULL) else dimnames(Z.new) <- list(dimnames(newprob)[[1]], NULL)
                        }

   if (!nonlin.ax & !is.null(X)) 
     {
                ax <- do.call("biplot.ax.control", c(num.vars, list(var.names), ax))
        if (ax$type == "prediction") if (nrow(Br) > 1) axes.direction <- solve(diag(diag(Br %*% t(Br)))) %*% Br  else axes.direction <- (1/(Br %*% t(Br))) %*% Br
                else axes.direction <- Br
            if (length(ax$which) == 0) z.axes <- NULL else z.axes <- lapply(1:length(ax$which), calibrate.axis, unscaled.X, means, sd, axes.direction, ax$which, ax$ticks, ax$orthogx, ax$orthogy, ax$oblique)
          }     
   alpha.bags <- do.call("biplot.alpha.bag.control", c(J, list(dimnames(G)[[2]]), alpha.bags))
   z.bags <- vector("list", length(alpha.bags$which))
   if (length(alpha.bags$which) > 0) for (j in 1:length(alpha.bags$which)) { class.num <- alpha.bags$which[j]
                                                                             mat <- Z[G[, class.num] == 1, ]
                                                                             flush.console()
                                                                             cat(paste("alpha bag for class ", dimnames(G)[[2]][class.num], " with ", nrow(mat), " samples", sep = ""), "\n")
                                                                             if (dim.biplot == 2) z.bags[[j]] <- calc.alpha.bags(mat, alpha.bags$alpha[j], alpha.bags$max[j], alpha.bags$Tukey.median[j], alpha.bags$min[j])
                                                                             if (dim.biplot == 1) z.bags[[j]] <- quantile(mat, c((100 - alpha.bags$alpha[j])/200, 1 - (100 - alpha.bags$alpha[j])/200, 0.5))                    }
   kappa.ellipse <- do.call("biplot.kappa.ellipse.control", c(J, list(dimnames(G)[[2]]), dim.biplot-1, kappa.ellipse))
   z.ellipse <- vector("list", length(kappa.ellipse$which))
   if (length(kappa.ellipse$which) > 0) for (j in 1:length(kappa.ellipse$which)) { class.num <- kappa.ellipse$which[j]
                                                                                   mat <- Z[G[, class.num] == 1, ]                     
                                                                                   if (dim.biplot == 2) z.ellipse[[j]] <- calc.concentration.ellipse(mat, kappa.ellipse$kappa[j])
                                                                                   if (dim.biplot == 1) z.ellipse[[j]] <- qnorm(c(1 - pnorm(kappa.ellipse$kappa[j]), pnorm(kappa.ellipse$kappa[j])), mean(mat), sqrt(var(mat)))       }
   if (dim.biplot == 1) { density.style <- do.call("biplot.density.1D.control", c(J, list(dimnames(G)[[2]]), density.style))
                          z.density <- vector("list", length(density.style$which))
                          if (length(density.style$which) > 0) for (j in 1:length(density.style$which)) { class.num <- density.style$which[j]
                                                                                                          mat <- Z[G[, class.num] == 1, ]
                                                                                                          z.density[[j]] <- density(mat, bw = density.style$bw[j], kernel = density.style$kernel[j])            }
                        }
   if (dim.biplot == 2) { density.style <- do.call("biplot.density.2D.control", c(J, list(dimnames(G)[[2]]), density.style))
                          if (!is.null(density.style$which)) { if (density.style$which == 0) mat <- Z else mat <- Z[G[, density.style$which] == 1, ]
                                                               x.range <- range(Z[, 1])
                                                               y.range <- range(Z[, 2])
                                                               width <- max(x.range[2] - x.range[1], y.range[2] - y.range[1])
                                                               xlim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
                                                               ylim <- mean(Z[, 1]) + c(-1, 1) * 0.75 * width
                                                               if (is.null(density.style$h)) z.density <- kde2d(mat[, 1], mat[, 2], n = density.style$n, lims = c(xlim, ylim))
                                                               else z.density <- kde2d(mat[, 1], mat[, 2], h = density.style$h, n = density.style$n, lims = c(xlim, ylim))        }
                          else z.density <- NULL
                        }

   if (!is.null(colour.scheme)) { my.sample.col <- colorRampPalette(colour.scheme)
                                  samples$col <- my.sample.col(samples$col)         }
   samples <- do.call("biplot.sample.control", c(J, samples))
   new.samples <- do.call("biplot.new.sample.control", c(max(1, nrow(newprob)), new.samples))
   if (is.null(X)) ax <- NULL
   class.means <- do.call("biplot.mean.control", c(J, list(dimnames(G)[[2]]), class.means))
   legend.format <- do.call("biplot.legend.control", legend.format)
   legend.type <- do.call("biplot.legend.type.control", legend.type)

   Z.region <- NULL
   if (dim.biplot == 2)
     {
           class.regions <- do.call("biplot.class.region.control", c(J, class.regions))
           if (class.dim > J) { warning ("classification in 2D")
                                class.dim <- 2
                                                  }
           plot.range <- range(Z * exp.factor)                                                
           #a<<- Z.means.mat
           #Z.region <- neighbour.region2 (S.svd$v[,e.vects[1:dim.biplot]], plot.range=plot.range, plot=F, plot.coords=plot.coords)                                
           Z.region <- biplot.create.regions2(S.svd$v[,e.vects[1:dim.biplot]], plot.range=plot.range, plot=F, plot.coords=plot.coords,region.style=class.regions,region.mid = Z.means.mat)                                
           
           ####Z.region moet hier verander####
           #biplot.create.regions <- function (region.style, plot.range, region.mid, rotate.mat=diag(2), reflect.mat=diag(2), region.func, ...)
           #neighbour.region2 <- function (Z, plot.range=NULL, col=NULL, add=TRUE, show.points=aFALSE, plot=TRUE, plot.coords=NULL, border=c("black",NA), eps=0.00001, ...) 
           #Z.region <- biplot.create.regions(region.style = class.regions, plot.range = plot.range, region.mid = Z.means.mat)
           }
   if (dim.biplot == 2) draw.biplot(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, z.trajectories = z.trajectories, z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, Z.density = z.density, 
                                    Z.regions = Z.region, sample.style = samples, mean.style = class.means, ax.style = ax, bag.style = alpha.bags, ellipse.style = kappa.ellipse, new.sample.style = new.samples, 
                                                                        density.style = density.style, region.style = class.regions, predict.samples = predict.samples, predict.means = predict.means, Title = Title, exp.factor = exp.factor, 
                                                                        plot.coords = plot.coords, ...)

                                                                                                                                                
   if (dim.biplot == 1) draw.biplot.1D(Z = Z, G = G, classes = classes, Z.means = Z.means.mat, z.axes = z.axes, z.bags = z.bags, z.ellipse = z.ellipse, Z.new = Z.new, Z.density = z.density, sample.style = samples, 
                                       mean.style = class.means, ax.style = ax, bag.style = alpha.bags, ellipse.style = kappa.ellipse, new.sample.style = new.samples, density.style = density.style, predict.samples = predict.samples, 
                                       predict.means = predict.means, Title = Title, exp.factor = exp.factor, ...)
   if (!is.null(predict.samples)) predict.mat <- Z[predict.samples, , drop = F] %*% t(Vr) + matrix(1, nrow = length(predict.samples)) %*% means else predict.mat <- NULL
   if (!is.null(predict.means)) predict.mat <- rbind(predict.mat, Z[predict.means, , drop = F] %*% t(Vr) + matrix(1, nrow = length(predict.means)) %*%  means)
   if (!is.null(predict.mat)) dimnames(predict.mat) <- list(c(dimnames(X)[[1]][predict.samples], dimnames(G)[[2]][predict.means]), dimnames(X)[[2]])
   if (any(unlist(legend.type))) { dev.new()
                                   sample.list <- list(pch = samples$pch, col = samples$col)
                                   mean.list = list(pch = rep(NA, J), col = rep(NA, J))
                                   mean.list$pch[class.means$which] <- class.means$pch
                                   mean.list$col[class.means$which] <- class.means$col
                                   bag.list = list(lty = rep(1, J), col = rep(NA, J), lwd = rep(NA, J))
                                   bag.list$lty[alpha.bags$which] <- alpha.bags$lty
                                   bag.list$col[alpha.bags$which] <- alpha.bags$col
                                   bag.list$lwd[alpha.bags$which] <- alpha.bags$lwd
                                   if (length(alpha.bags$which) == 0 & length(kappa.ellipse$which) > 0) { bag.list$lty[kappa.ellipse$which] <- kappa.ellipse$lty
                                                                                                          bag.list$col[kappa.ellipse$which] <- kappa.ellipse$col
                                                                                                          bag.list$lwd[kappa.ellipse$which] <- kappa.ellipse$lwd        }
                                   biplot.legend(legend.type, legend.format, mean.list = mean.list, sample.list = sample.list, bag.list = bag.list, class.names = dimnames(G)[[2]], quality.print = quality.print, quality = quality)
                                }
invisible (NULL)

Z.newoutput<<- Z.new
Z.regionoutput<<- Z.region
Z.midoutput <<- S.svd$v[,e.vects[1:dim.biplot]]

}


closest.midsim <- function(Z.mid,test.mat)
{
  #as.matrix(dist(rbind(x,Z.mid)))[1,-1])[1]
  apply(test.mat,1,function(x){order(as.matrix(dist(rbind(x,Z.mid)))[1,-1])[1]})
} 
