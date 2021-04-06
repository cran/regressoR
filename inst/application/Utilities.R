
# Edit > Folding > Collapse All (is of much help to visualize in an orderly way the code).

# WRAPPERS ----------------------------------------------------------------------------------------------------------------

# In order to avoid changing the server code and since these functions always use the same parameters, 
# the following wrappers were made.

# Wrapper of regressoR::exe to set the environment
# exe <- function(...){
#   regressoR::exe(..., envir = parent.frame())
# }

# Wrapper of regressoR::comparative_table to set the list of values and the language
comparative_table <- function(sel){
  regressoR::comparative_table(sel, IndicesM)
}

# GLOBAL VARIABLES --------------------------------------------------------------------------------------------------------

# -- Data
datos             <- NULL
datos.originales  <- NULL
datos.prueba      <- NULL
datos.aprendizaje <- NULL
variable.predecir <- NULL
real.val          <- NULL
contador          <- 0
semilla           <- FALSE
nombres.modelos   <- c()
# -- Basic Statistics
correlacion   <- NULL
cod.poder.cat <- NULL
cod.poder.num <- NULL

cod.cor       <- NULL
cod.disp      <- NULL
cod.dya.cat   <- NULL
cod.dya.num   <- NULL
cod.normal    <- NULL
# -- Models
IndicesM  <- list()
# -- RL
cod.rl.modelo <- NULL
cod.rl.pred   <- NULL
cod.rl.ind    <- NULL
# -- RLR
cod.rlr.modelo   <- NULL
cod.rlr.pred     <- NULL
cod.rlr.ind      <- NULL
cod.select.landa <- NULL
# -- KNN
cod.knn.modelo <- NULL
cod.knn.pred   <- NULL
cod.knn.ind    <- NULL
knn.stop.excu  <- FALSE
# -- SVM
cod.svm.modelo <- NULL
cod.svm.pred   <- NULL
cod.svm.ind    <- NULL
# --- RD
cod.rd.ind    <- NULL
cod.rd.modelo <- NULL
cod.rd.pred   <- NULL
cv.glm.lasso  <- NULL
cv.glm.ridge  <- NULL
n.comp.rd     <- NULL
# -- DT
cod.dt.modelo <- NULL
cod.dt.pred   <- NULL
cod.dt.ind    <- NULL
# -- RF
cod.rf.modelo <- NULL
cod.rf.pred   <- NULL
cod.rf.ind    <- NULL
rf.stop.excu  <- FALSE
# -- BOOSTING
cod.b.modelo <- NULL
cod.b.pred   <- NULL
cod.b.ind    <- NULL
# -- NN
cod.nn.modelo <- NULL
cod.nn.pred   <- NULL
cod.nn.ind    <- NULL
NN_EXECUTION  <- TRUE
mean.nn       <- NULL
sd.nn         <- NULL
mean.nn.np    <- NULL
sd.nn.np      <- NULL
# -- Prediction of New Individuals
datos.originales.completos  <- NULL
datos.aprendizaje.completos <- NULL
datos.prueba.completos      <- NULL
variable.predecir.pn        <- NULL
modelo.seleccionado.pn      <- NULL
contadorPN                  <- 0
code.trans.pn               <- ""
modelo.nuevos               <- NULL
predic.nuevos               <- NULL
# --  Reports
salida.code  <- NULL


# crear.traslation <- function() {
#   library(plyr)
#   archivo <- read.table("diccionario.csv", header = TRUE, sep = ";", as.is = TRUE)
#   translation <- dlply(archivo , .(key), function(s) key = as.list(s))
# 
#   save(translation, file = "translation.bin")
# }



# FUNCTIONS ---------------------------------------------------------------------------------------------------------------

# To help reduce the huge list of imports:

# Functions taken from the PSYCH package
pairs.panels <- function (x, smooth = TRUE, scale = FALSE, density = TRUE, ellipses = TRUE,
                           digits = 2, method = "pearson", pch = 20, lm = FALSE, cor = TRUE,
                           jiggle = FALSE, factor = 2, hist.col = "cyan", show.points = TRUE,
                           rug = TRUE, breaks = "Sturges", cex.cor = 1, wt = NULL, smoother = FALSE,
                           stars = FALSE, ci = FALSE, alpha = 0.05, ...){
  "panel.hist.density" <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1], usr[2], 0, 1.5))
    tax <- table(x)
    if (length(tax) < 11) {
      breaks <- as.numeric(names(tax))
      y <- tax/max(tax)
      interbreak <- min(diff(breaks)) * (length(tax) -
                                           1)/41
      rect(breaks - interbreak, 0, breaks + interbreak,
           y, col = hist.col)
    }
    else {
      h <- hist(x, breaks = breaks, plot = FALSE)
      breaks <- h$breaks
      nB <- length(breaks)
      y <- h$counts
      y <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = hist.col)
    }
    if (density) {
      tryd <- try(d <- density(x, na.rm = TRUE, bw = "nrd",
                               adjust = 1.2), silent = TRUE)
      if (class(tryd) != "try-error") {
        d$y <- d$y/max(d$y)
        lines(d)
      }
    }
    if (rug)
      rug(x)
  }
  "panel.cor" <- function(x, y, prefix = "", ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    if (is.null(wt)) {
      r <- cor(x, y, use = "pairwise", method = method)
    }
    else {
      r <- cor.wt(data.frame(x, y), w = wt[, c(1:2)])$r[1,
                                                        2]
    }
    txt <- format(c(round(r, digits), 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (stars) {
      pval <- r.test(sum(!is.na(x * y)), r)$p
      symp <- symnum(pval, corr = FALSE, cutpoints = c(0,
                                                       0.001, 0.01, 0.05, 1), symbols = c("***", "**",
                                                                                          "*", " "), legend = FALSE)
      txt <- paste0(txt, symp)
    }
    cex <- cex.cor * 0.8/(max(strwidth("0.12***"), strwidth(txt)))
    if (scale) {
      cex1 <- cex * abs(r)
      if (cex1 < 0.25)
        cex1 <- 0.25
      text(0.5, 0.5, txt, cex = cex1)
    }
    else {
      text(0.5, 0.5, txt, cex = cex)
    }
  }
  "panel.smoother" <- function(x, y, pch = par("pch"), col.smooth = "red",
                               span = 2/3, iter = 3, ...) {
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points)
        points(x, y, pch = pch, ...)
    }
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      if (smooth & ci) {
        lml <- loess(y ~ x, degree = 1, family = "symmetric")
        tempx <- data.frame(x = seq(min(x, na.rm = TRUE),
                                    max(x, na.rm = TRUE), length.out = 47))
        pred <- predict(lml, newdata = tempx, se = TRUE)
        if (ci) {
          upperci <- pred$fit + confid * pred$se.fit
          lowerci <- pred$fit - confid * pred$se.fit
          polygon(c(tempx$x, rev(tempx$x)), c(lowerci,
                                              rev(upperci)), col = adjustcolor("light grey",
                                                                               alpha.f = 0.8), border = NA)
        }
        lines(tempx$x, pred$fit, col = col.smooth, ...)
      }
      else {
        if (smooth)
          lines(stats::lowess(x[ok], y[ok], f = span,
                              iter = iter), col = col.smooth)
      }
    }
    if (ellipses)
      draw.ellipse(xm, ym, xs, ys, r, col.smooth = col.smooth,
                   ...)
  }
  "panel.lm" <- function(x, y, pch = par("pch"), col.lm = "red",
                         ...) {
    ymin <- min(y)
    ymax <- max(y)
    xmin <- min(x)
    xmax <- max(x)
    ylim <- c(min(ymin, xmin), max(ymax, xmax))
    xlim <- ylim
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points) {
        points(x, y, pch = pch, ylim = ylim, xlim = xlim,
               ...)
      }
    }
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      lml <- lm(y ~ x)
      if (ci) {
        tempx <- data.frame(x = seq(min(x, na.rm = TRUE),
                                    max(x, na.rm = TRUE), length.out = 47))
        pred <- predict.lm(lml, newdata = tempx, se.fit = TRUE)
        upperci <- pred$fit + confid * pred$se.fit
        lowerci <- pred$fit - confid * pred$se.fit
        polygon(c(tempx$x, rev(tempx$x)), c(lowerci,
                                            rev(upperci)), col = adjustcolor("light grey",
                                                                             alpha.f = 0.8), border = NA)
      }
      if (ellipses) {
        xm <- mean(x, na.rm = TRUE)
        ym <- mean(y, na.rm = TRUE)
        xs <- sd(x, na.rm = TRUE)
        ys <- sd(y, na.rm = TRUE)
        r = cor(x, y, use = "pairwise", method = method)
        draw.ellipse(xm, ym, xs, ys, r, col.smooth = col.lm,
                     ...)
      }
      abline(lml, col = col.lm, ...)
    }
  }
  "draw.ellipse" <- function(x = 0, y = 0, xs = 1, ys = 1,
                             r = 0, col.smooth, add = TRUE, segments = 51, ...) {
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    if (!is.na(r)) {
      if (abs(r) > 0)
        theta <- sign(r)/sqrt(2)
      else theta = 1/sqrt(2)
      shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta,
                                                              theta, -theta, theta), ncol = 2, byrow = TRUE)
      ellipse <- unit.circle %*% shape
      ellipse[, 1] <- ellipse[, 1] * xs + x
      ellipse[, 2] <- ellipse[, 2] * ys + y
      if (show.points)
        points(x, y, pch = 19, col = col.smooth, cex = 1.5)
      lines(ellipse, ...)
    }
  }
  "panel.ellipse" <- function(x, y, pch = par("pch"), col.smooth = "red",
                              ...) {
    segments = 51
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1] - abs(0.05 * usr[1]), usr[2] + abs(0.05 *
                                                            usr[2]), 0, 1.5))
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points) {
        points(x, y, pch = pch, ...)
      }
    }
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    if (!is.na(r)) {
      if (abs(r) > 0)
        theta <- sign(r)/sqrt(2)
      else theta = 1/sqrt(2)
      shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta,
                                                              theta, -theta, theta), ncol = 2, byrow = TRUE)
      ellipse <- unit.circle %*% shape
      ellipse[, 1] <- ellipse[, 1] * xs + xm
      ellipse[, 2] <- ellipse[, 2] * ys + ym
      points(xm, ym, pch = 19, col = col.smooth, cex = 1.5)
      if (ellipses)
        lines(ellipse, ...)
    }
  }
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  if (missing(cex.cor))
    cex.cor <- 1
  for (i in 1:ncol(x)) {
    if (is.character(x[[i]])) {
      x[[i]] <- as.numeric(as.factor(x[[i]]))
      colnames(x)[i] <- paste(colnames(x)[i], "*", sep = "")
    }
  }
  n.obs <- nrow(x)
  confid <- qt(1 - alpha/2, n.obs - 2)
  if (!lm) {
    if (cor) {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor,
            lower.panel = panel.smoother, pch = pch, ...)
    }
    else {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.smoother,
            lower.panel = panel.smoother, pch = pch, ...)
    }
  }
  else {
    if (!cor) {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.lm,
            lower.panel = panel.lm, pch = pch, ...)
    }
    else {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor,
            lower.panel = panel.lm, pch = pch, ...)
    }
  }
}

# Functions taken from the DUMMIES package
dummy <- function (x, data = NULL, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE) {
  if (is.null(data)) {
    name <- as.character(sys.call(1))[2]
    name <- sub("^(.*\\$)", "", name)
    name <- sub("\\[.*\\]$", "", name)
  }
  else {
    if (length(x) > 1)
      stop("More than one variable provided to produce dummy variable.")
    name <- x
    x <- data[, name]
  }
  if (drop == FALSE && class(x) == "factor") {
    x <- factor(x, levels = levels(x), exclude = NULL)
  }
  else {
    x <- factor(x, exclude = NULL)
  }
  if (length(levels(x)) < 2) {
    if (verbose)
      warning(name, " has only 1 level. Producing dummy variable anyway.")
    return(matrix(rep(1, length(x)), ncol = 1, dimnames = list(rownames(x),
                                                               c(paste(name, sep, x[[1]], sep = "")))))
  }
  mm <- model.matrix(~x - 1, model.frame(~x - 1))
  colnames.mm <- colnames(mm)
  if (verbose)
    cat(" ", name, ":", ncol(mm), "dummy varibles created\n")
  mm <- matrix(fun(mm), nrow = nrow(mm), ncol = ncol(mm), dimnames = list(NULL,
                                                                          colnames.mm))
  colnames(mm) <- sub("^x", paste(name, sep, sep = ""), colnames(mm))
  if (!is.null(row.names(data)))
    rownames(mm) <- rownames(data)
  return(mm)
}

dummy.data.frame <- function (data, names = NULL, omit.constants = TRUE, dummy.classes = c("factor" ,"character"), all = TRUE, ...) {
  df <- data.frame(row.names = row.names(data))
  new.attr <- list()
  for (nm in names(data)) {
    old.attr <- attr(df, "dummies")
    if (nm %in% names || (is.null(names) && (dummy.classes == "ALL" || class(data[, nm]) %in% dummy.classes))) {
      dummies <- dummy(nm, data, ...)
      if (ncol(dummies) == 1 & omit.constants) {
        dummies <- matrix(nrow = nrow(data), ncol = 0)
      }
      if (ncol(dummies) > 0)
        new.attr[[nm]] <- (ncol(df) + 1):(ncol(df) + ncol(dummies))
    }
    else {
      if (!all)
        (next)()
      dummies <- data[, nm, drop = FALSE]
    }
    df <- cbind(df, dummies)
  }
  attr(df, "dummies") <- new.attr
  return(df)
}

# Functions taken from the scatterplot3d package
scatterplot3d <- function (x, y = NULL, z = NULL, color = par("col"), pch = par("pch"),
                           main = NULL, sub = NULL, xlim = NULL, ylim = NULL, zlim = NULL,
                           xlab = NULL, ylab = NULL, zlab = NULL, scale.y = 1, angle = 40,
                           axis = TRUE, tick.marks = TRUE, label.tick.marks = TRUE,
                           x.ticklabs = NULL, y.ticklabs = NULL, z.ticklabs = NULL,
                           y.margin.add = 0, grid = TRUE, box = TRUE, lab = par("lab"),
                           lab.z = mean(lab[1:2]), type = "p", highlight.3d = FALSE,
                           mar = c(5, 3, 4, 3) + 0.1, bg = par("bg"), col.axis = par("col.axis"),
                           col.grid = "grey", col.lab = par("col.lab"), cex.symbols = par("cex"),
                           cex.axis = 0.8 * par("cex.axis"), cex.lab = par("cex.lab"),
                           font.axis = par("font.axis"), font.lab = par("font.lab"),
                           lty.axis = par("lty"), lty.grid = par("lty"), lty.hide = NULL,
                           lty.hplot = par("lty"), log = "", asp = NA, ...)
{
  mem.par <- par(mar = mar)
  x.scal <- y.scal <- z.scal <- 1
  xlabel <- if (!missing(x))deparse(substitute(x))
  ylabel <- if (!missing(y))deparse(substitute(y))
  zlabel <- if (!missing(z))deparse(substitute(z))
  if (highlight.3d && !missing(color))
    warning("color is ignored when highlight.3d = TRUE")
  if (!is.null(d <- dim(x)) && (length(d) == 2) && (d[2] >=
                                                    4))
    color <- x[, 4]
  else if (is.list(x) && !is.null(x$color))
    color <- x$color
  xyz <- xyz.coords(x = x, y = y, z = z, xlab = xlabel, ylab = ylabel,
                    zlab = zlabel, log = log)
  if (is.null(xlab)) {
    xlab <- xyz$xlab
    if (is.null(xlab))
      xlab <- ""
  }
  if (is.null(ylab)) {
    ylab <- xyz$ylab
    if (is.null(ylab))
      ylab <- ""
  }
  if (is.null(zlab)) {
    zlab <- xyz$zlab
    if (is.null(zlab))
      zlab <- ""
  }
  if (length(color) == 1)
    color <- rep(color, length(xyz$x))
  else if (length(color) != length(xyz$x))
    stop("length(color) ", "must be equal length(x) or 1")
  if (length(pch) == 1)
    pch <- rep(pch, length(xyz$x))
  else if (length(pch) != length(xyz$x))
    stop("length(pch) ", "must be equal length(x) or 1")
  if (length(bg) == 1)
    bg <- rep(bg, length(xyz$x))
  else if (length(bg) != length(xyz$x))
    stop("length(bg) ", "must be equal length(x) or 1")
  angle <- (angle%%360)/90
  yz.f <- scale.y * abs(if (angle < 1) angle else if (angle >
                                                      3) angle - 4 else 2 - angle)
  yx.f <- scale.y * (if (angle < 2)
    1 - angle
    else angle - 3)
  if (angle > 2) {
    temp <- xyz$x
    xyz$x <- xyz$y
    xyz$y <- temp
    temp <- xlab
    xlab <- ylab
    ylab <- temp
    temp <- xlim
    xlim <- ylim
    ylim <- temp
  }
  angle.1 <- (1 < angle && angle <= 2) || angle > 3
  angle.2 <- 1 < angle && angle <= 3
  dat <- data.frame(xyz[c("x", "y", "z")], col = color, pch = pch,
                    bg = bg, stringsAsFactors = FALSE)
  if (!is.null(xlim)) {
    xlim <- range(xlim)
    dat <- dat[xlim[1] <= dat$x & dat$x <= xlim[2], , drop = FALSE]
  }
  if (!is.null(ylim)) {
    ylim <- range(ylim)
    dat <- dat[ylim[1] <= dat$y & dat$y <= ylim[2], , drop = FALSE]
  }
  if (!is.null(zlim)) {
    zlim <- range(zlim)
    dat <- dat[zlim[1] <= dat$z & dat$z <= zlim[2], , drop = FALSE]
  }
  n <- nrow(dat)
  if (n < 1)
    stop("no data left within (x|y|z)lim")
  y.range <- range(dat$y[is.finite(dat$y)])
  if (type == "p" || type == "h") {
    y.ord <- rev(order(dat$y))
    dat <- dat[y.ord, ]
    if (length(cex.symbols) > 1)
      if (length(cex.symbols) != length(y.ord))
        stop("length(cex.symbols) ", "must be equal length(x) or 1")
    else cex.symbols <- cex.symbols[y.ord]
    daty <- dat$y
    daty[!is.finite(daty)] <- mean(daty[is.finite(daty)])
    if (highlight.3d && !(all(diff(daty) == 0)))
      dat$col <- rgb(red = seq(0, 1, length = n) * (y.range[2] -
                                                      daty)/diff(y.range), green = 0, blue = 0)
  }
  p.lab <- par("lab")
  y.range <- y.range.fix <- range(dat$y[is.finite(dat$y)],
                                  ylim)
  y.prty <- pretty(y.range, n = lab[2], min.n = max(1, min(0.5 *
                                                             lab[2], p.lab[2])))
  y.scal <- round(diff(y.prty[1:2]), digits = 12)
  y.add <- min(y.prty)
  dat$y <- (dat$y - y.add)/y.scal
  y.max <- (max(y.prty) - y.add)/y.scal
  if (!is.null(ylim))
    y.max <- max(y.max, ceiling((ylim[2] - y.add)/y.scal))
  x.range <- x.range.fix <- range(dat$x[is.finite(dat$x)],
                                  xlim)
  x.prty <- pretty(x.range, n = lab[1], min.n = max(1, min(0.5 *
                                                             lab[1], p.lab[1])))
  x.scal <- round(diff(x.prty[1:2]), digits = 12)
  dat$x <- dat$x/x.scal
  x.range <- range(x.prty)/x.scal
  x.max <- ceiling(x.range[2])
  x.min <- floor(x.range[1])
  if (!is.null(xlim)) {
    x.max <- max(x.max, ceiling(xlim[2]/x.scal))
    x.min <- min(x.min, floor(xlim[1]/x.scal))
  }
  x.range <- range(x.min, x.max)
  z.range <- range(dat$z[is.finite(dat$z)], zlim)
  z.prty <- pretty(z.range, n = lab.z, min.n = max(1, min(0.5 *
                                                            lab.z, p.lab[2])))
  z.scal <- round(diff(z.prty[1:2]), digits = 12)
  dat$z <- dat$z/z.scal
  z.range <- range(z.prty)/z.scal
  z.max <- ceiling(z.range[2])
  z.min <- floor(z.range[1])
  if (!is.null(zlim)) {
    z.max <- max(z.max, ceiling(zlim[2]/z.scal))
    z.min <- min(z.min, floor(zlim[1]/z.scal))
  }
  z.range <- range(z.min, z.max)
  if (!is.na(asp)) {
    x.i <- x.min:x.max
    z.i <- z.min:z.max
    range.x <- abs(diff(range(x.i * x.scal)))
    range.z <- abs(diff(range(z.i * z.scal)))
    asp <- asp * (range.z/(length(z.i) - 1))/(range.x/(length(x.i) -
                                                         1))
  }
  plot.new()
  if (angle.2) {
    x1 <- x.min + yx.f * y.max
    x2 <- x.max
  }
  else {
    x1 <- x.min
    x2 <- x.max + yx.f * y.max
  }
  plot.window(c(x1, x2), c(z.min, z.max + yz.f * y.max), asp = asp)
  temp <- strwidth(format(rev(y.prty))[1], cex = cex.axis/par("cex"))
  lheight <- (strheight("\n") - strheight("M")) * asp
  lheight2 <- (strheight("\n") - strheight("M"))
  if (angle.2)
    x1 <- x1 - temp - y.margin.add
  else x2 <- x2 + temp + y.margin.add
  plot.window(c(x1, x2), c(z.min, z.max + yz.f * y.max), asp = asp)
  if (angle > 2)
    par(usr = par("usr")[c(2, 1, 3:4)])
  usr <- par("usr")
  title(main, sub, ...)
  if (grid) {
    i <- x.min:x.max
    segments(i, z.min, i + (yx.f * y.max), yz.f * y.max +
               z.min, col = col.grid, lty = lty.grid)
    i <- 0:y.max
    segments(x.min + (i * yx.f), i * yz.f + z.min, x.max +
               (i * yx.f), i * yz.f + z.min, col = col.grid, lty = lty.grid)
  }
  if (axis) {
    xx <- if (angle.2)
      c(x.min, x.max)
    else c(x.max, x.min)
    if (tick.marks) {
      xtl <- (z.max - z.min) * (tcl <- -par("tcl"))/50
      ztl <- (x.max - x.min) * tcl/50
      mysegs <- function(x0, y0, x1, y1) segments(x0, y0,
                                                  x1, y1, col = col.axis, lty = lty.axis)
      i.y <- 0:y.max
      mysegs(yx.f * i.y - ztl + xx[1], yz.f * i.y + z.min,
             yx.f * i.y + ztl + xx[1], yz.f * i.y + z.min)
      i.x <- x.min:x.max
      mysegs(i.x, -xtl + z.min, i.x, xtl + z.min)
      i.z <- z.min:z.max
      mysegs(-ztl + xx[2], i.z, ztl + xx[2], i.z)
      if (label.tick.marks) {
        las <- par("las")
        mytext <- function(labels, side, at, line = -0.5,
                           ...) mtext(text = labels, side = side, at = at,
                                      line = line, col = col.lab, cex = cex.axis,
                                      font = font.lab, ...)
        if (is.null(x.ticklabs))
          x.ticklabs <- format(i.x * x.scal)
        if (!is.na(asp)) {
          linepad <- (usr[3] - z.min)/lheight2 + 0.5
          mytext(x.ticklabs, side = 1, at = i.x, line = linepad)
        }
        else {
          mytext(x.ticklabs, side = 1, at = i.x)
        }
        if (is.null(z.ticklabs))
          z.ticklabs <- format(i.z * z.scal)
        if (!is.na(asp)) {
          if (angle.1) {
            if (angle > 2) {
              linepad <- (x2 - usr[1])/lheight + 0.5
            }
            else {
              linepad <- (x2 - usr[2])/lheight + 0.5
            }
          }
          else {
            if (angle > 2) {
              linepad <- (usr[2] - x1)/lheight + 0.5
            }
            else {
              linepad <- (usr[1] - x1)/lheight + 0.5
            }
          }
        }
        else {
          linepad = -0.5
        }
        mytext(z.ticklabs, side = if (angle.1)
          4
          else 2, at = i.z, adj = if (0 < las && las <
                                      3)
            1
          else NA, line = linepad)
        temp <- if (angle > 2)
          rev(i.y)
        else i.y
        if (is.null(y.ticklabs))
          y.ticklabs <- format(y.prty)
        else if (angle > 2)
          y.ticklabs <- rev(y.ticklabs)
        text(i.y * yx.f + xx[1], i.y * yz.f + z.min,
             y.ticklabs, pos = if (angle.1)
               2
             else 4, offset = 1, col = col.lab, cex = cex.axis/par("cex"),
             font = font.lab)
      }
    }
    if (!is.na(asp)) {
      if (angle.1) {
        if (angle > 2) {
          linepad <- (x2 - usr[1])/lheight + 0.5
        }
        else {
          linepad <- (x2 - usr[2])/lheight + 0.5
        }
      }
      else {
        if (angle > 2) {
          linepad <- (usr[2] - x1)/lheight + 0.5
        }
        else {
          linepad <- (usr[1] - x1)/lheight + 0.5
        }
      }
    }
    else {
      linepad = -0.5
    }
    mytext2 <- function(lab, side, line, at) mtext(lab, side = side,
                                                   line = line, at = at, col = col.lab, cex = cex.lab,
                                                   font = font.axis, las = 0)
    lines(c(x.min, x.max), c(z.min, z.min), col = col.axis,
          lty = lty.axis)
    if (!is.na(asp)) {
      mytext2(xlab, 1, line = (usr[3] - z.min)/lheight2 +
                1.5, at = mean(x.range))
    }
    else {
      mytext2(xlab, 1, line = 1.5, at = mean(x.range))
    }
    lines(xx[1] + c(0, y.max * yx.f), c(z.min, y.max * yz.f +
                                          z.min), col = col.axis, lty = lty.axis)
    mytext2(ylab, if (angle.1)
      2
      else 4, line = linepad + 1, at = z.min + y.max * yz.f)
    lines(xx[c(2, 2)], c(z.min, z.max), col = col.axis, lty = lty.axis)
    mytext2(zlab, if (angle.1)
      4
      else 2, line = linepad + 2, at = mean(z.range))
    if (box) {
      if (is.null(lty.hide))
        lty.hide <- lty.axis
      temp <- yx.f * y.max
      temp1 <- yz.f * y.max
      lines(c(x.min + temp, x.max + temp), c(z.min + temp1,
                                             z.min + temp1), col = col.axis, lty = lty.hide)
      lines(c(x.min + temp, x.max + temp), c(temp1 + z.max,
                                             temp1 + z.max), col = col.axis, lty = lty.axis)
      temp <- c(0, y.max * yx.f)
      temp1 <- c(0, y.max * yz.f)
      lines(temp + xx[2], temp1 + z.min, col = col.axis,
            lty = lty.hide)
      lines(temp + x.min, temp1 + z.max, col = col.axis,
            lty = lty.axis)
      temp <- yx.f * y.max
      temp1 <- yz.f * y.max
      lines(c(temp + x.min, temp + x.min), c(z.min + temp1,
                                             z.max + temp1), col = col.axis, lty = if (!angle.2)
                                               lty.hide
            else lty.axis)
      lines(c(x.max + temp, x.max + temp), c(z.min + temp1,
                                             z.max + temp1), col = col.axis, lty = if (angle.2)
                                               lty.hide
            else lty.axis)
    }
  }
  x <- dat$x + (dat$y * yx.f)
  z <- dat$z + (dat$y * yz.f)
  col <- as.character(dat$col)
  if (type == "h") {
    z2 <- dat$y * yz.f + z.min
    segments(x, z, x, z2, col = col, cex = cex.symbols, lty = lty.hplot,
             ...)
    points(x, z, type = "p", col = col, pch = dat$pch, bg = dat$bg,
           cex = cex.symbols, ...)
  }
  else points(x, z, type = type, col = col, pch = dat$pch,
              bg = dat$bg, cex = cex.symbols, ...)
  if (axis && box) {
    lines(c(x.min, x.max), c(z.max, z.max), col = col.axis,
          lty = lty.axis)
    lines(c(0, y.max * yx.f) + x.max, c(0, y.max * yz.f) +
            z.max, col = col.axis, lty = lty.axis)
    lines(xx[c(1, 1)], c(z.min, z.max), col = col.axis, lty = lty.axis)
  }
  ob <- ls()
  rm(list = ob[!ob %in% c("angle", "mar", "usr", "x.scal",
                          "y.scal", "z.scal", "yx.f", "yz.f", "y.add", "z.min",
                          "z.max", "x.min", "x.max", "y.max", "x.range.fix", "y.range.fix",
                          "xlabel", "ylabel", "zlabel", "x.prty", "y.prty", "z.prty",
                          "mem.par")])
  rm(ob)
  invisible(list(xyz.convert = function(x, y = NULL, z = NULL) {
    xyz <- xyz.coords(x, y, z)
    if (angle > 2) {
      temp <- xyz$x
      xyz$x <- xyz$y
      xyz$y <- temp
    }
    y <- (xyz$y - y.add)/y.scal
    return(list(x = xyz$x/x.scal + yx.f * y, y = xyz$z/z.scal +
                  yz.f * y))
  }, points3d = function(x, y = NULL, z = NULL, type = "p",
                         ...) {
    xyz <- xyz.coords(x, y, z)
    if (angle > 2) {
      temp <- xyz$x
      xyz$x <- xyz$y
      xyz$y <- temp
    }
    y2 <- (xyz$y - y.add)/y.scal
    x <- xyz$x/x.scal + yx.f * y2
    y <- xyz$z/z.scal + yz.f * y2
    mem.par <- par(mar = mar, usr = usr)
    if (type == "h") {
      y2 <- z.min + yz.f * y2
      segments(x, y, x, y2, ...)
      points(x, y, type = "p", ...)
    } else points(x, y, type = type, ...)
  }, plane3d = function(Intercept, x.coef = NULL, y.coef = NULL,
                        lty = "dashed", lty.box = NULL, draw_lines = TRUE, draw_polygon = FALSE,
                        polygon_args = list(border = NA, col = rgb(0, 0, 0, 0.2)),
                        ...) {
    if (!is.atomic(Intercept) && !is.null(coef(Intercept))) {
      Intercept <- coef(Intercept)
      if (!("(Intercept)" %in% names(Intercept))) Intercept <- c(0,
                                                                 Intercept)
    }
    if (is.null(lty.box)) lty.box <- lty
    if (is.null(x.coef) && length(Intercept) == 3) {
      x.coef <- Intercept[if (angle > 2) 3 else 2]
      y.coef <- Intercept[if (angle > 2) 2 else 3]
      Intercept <- Intercept[1]
    }
    mem.par <- par(mar = mar, usr = usr)
    x <- x.min:x.max
    y <- 0:y.max
    ltya <- c(lty.box, rep(lty, length(x) - 2), lty.box)
    x.coef <- x.coef * x.scal
    z1 <- (Intercept + x * x.coef + y.add * y.coef)/z.scal
    z2 <- (Intercept + x * x.coef + (y.max * y.scal + y.add) *
             y.coef)/z.scal
    if (draw_polygon) do.call("polygon", c(list(c(x.min,
                                                  x.min + y.max * yx.f, x.max + y.max * yx.f, x.max),
                                                c(z1[1], z2[1] + yz.f * y.max, z2[length(z2)] + yz.f *
                                                    y.max, z1[length(z1)])), polygon_args))
    if (draw_lines) segments(x, z1, x + y.max * yx.f, z2 +
                               yz.f * y.max, lty = ltya, ...)
    ltya <- c(lty.box, rep(lty, length(y) - 2), lty.box)
    y.coef <- (y * y.scal + y.add) * y.coef
    z1 <- (Intercept + x.min * x.coef + y.coef)/z.scal
    z2 <- (Intercept + x.max * x.coef + y.coef)/z.scal
    if (draw_lines) segments(x.min + y * yx.f, z1 + y * yz.f,
                             x.max + y * yx.f, z2 + y * yz.f, lty = ltya, ...)
  }, box3d = function(...) {
    mem.par <- par(mar = mar, usr = usr)
    lines(c(x.min, x.max), c(z.max, z.max), ...)
    lines(c(0, y.max * yx.f) + x.max, c(0, y.max * yz.f) +
            z.max, ...)
    lines(c(0, y.max * yx.f) + x.min, c(0, y.max * yz.f) +
            z.max, ...)
    lines(c(x.max, x.max), c(z.min, z.max), ...)
    lines(c(x.min, x.min), c(z.min, z.max), ...)
    lines(c(x.min, x.max), c(z.min, z.min), ...)
  }, contour3d = function(f, x.count = 10, y.count = 10, type = "l",
                          lty = "24", x.resolution = 50, y.resolution = 50, ...) {
    if (class(f) == "lm") {
      vars <- all.vars(formula(f))
    } else vars <- c("z", "x", "y")
    for (x1 in seq(x.range.fix[1], x.range.fix[2], length = x.count)) {
      d <- data.frame(x1, seq(y.range.fix[1], y.range.fix[2],
                              length = y.resolution))
      names(d) <- vars[-1]
      if (class(f) == "lm") {
        d[vars[1]] <- predict(f, newdata = d)
      } else d[vars[1]] <- f(d[[1]], d[[2]])
      xyz <- xyz.coords(d)
      if (angle > 2) {
        temp <- xyz$x
        xyz$x <- xyz$y
        xyz$y <- temp
      }
      y2 <- (xyz$y - y.add)/y.scal
      x <- xyz$x/x.scal + yx.f * y2
      y <- xyz$z/z.scal + yz.f * y2
      mem.par <- par(mar = mar, usr = usr)
      if (type == "h") {
        y2 <- z.min + yz.f * y2
        segments(x, y, x, y2, ...)
        points(x, y, type = "p", ...)
      } else points(x, y, type = type, lty = lty, ...)
    }
    for (x2 in seq(y.range.fix[1], y.range.fix[2], length = y.count)) {
      d <- data.frame(seq(x.range.fix[1], x.range.fix[2],
                          length = x.resolution), x2)
      names(d) <- vars[-1]
      if (class(f) == "lm") {
        d[vars[1]] <- predict(f, newdata = d)
      } else d[vars[1]] <- f(d[[1]], d[[2]])
      xyz <- xyz.coords(d)
      if (angle > 2) {
        temp <- xyz$x
        xyz$x <- xyz$y
        xyz$y <- temp
      }
      y2 <- (xyz$y - y.add)/y.scal
      x <- xyz$x/x.scal + yx.f * y2
      y <- xyz$z/z.scal + yz.f * y2
      mem.par <- par(mar = mar, usr = usr)
      if (type == "h") {
        y2 <- z.min + yz.f * y2
        segments(x, y, x, y2, ...)
        points(x, y, type = "p", ...)
      } else points(x, y, type = type, lty = lty, ...)
    }
  }, par.mar = mem.par))
}

# Functions taken from the rattle package

printRandomForests <- function (model, models = NULL, include.class = NULL, format = "") {
  if (is.null(models) || models == 0) 
    models <- 1:model$ntree
  for (i in models) printRandomForest(model, i, include.class, 
                                      format)
}

printRandomForest <- function (model, n = 1, include.class = NULL, format = "", comment = "") {
  if (!inherits(model, "randomForest")) 
    stop(Rtxt("the model is not of the 'randomForest' class"))
  if (format == "VB") 
    comment = "'"
  tr <- randomForest::getTree(model, n)
  tr.paths <- rattle:::getRFPathNodesTraverse(tr)
  tr.vars <- attr(model$terms, "dataClasses")[-1]
  cat(sprintf("%sRandom Forest Model %d", comment, n), "\n\n")
  cat(paste(comment, "-------------------------------------------------------------\n", 
            sep = ""))
  if (format == "VB") 
    cat("IF FALSE THEN\n' This is a No Op to simplify the code\n\n")
  nrules <- 0
  for (i in seq_along(tr.paths)) {
    tr.path <- tr.paths[[i]]
    nodenum <- as.integer(names(tr.paths[i]))
    target <- levels(model$y)[tr[nodenum, "prediction"]]
    if (!is.null(include.class) && target %notin% include.class) 
      (next)()
    cat(sprintf("%sTree %d Rule %d Node %d %s\n \n", comment, 
                n, i, nodenum, ifelse(is.null(target), "Regression (to do - extract predicted value)", 
                                      paste("Decision", target))))
    if (format == "VB") 
      cat("ELSE IF TRUE\n")
    nrules <- nrules + 1
    var.index <- tr[, 3][abs(tr.path)]
    var.names <- names(tr.vars)[var.index]
    var.values <- tr[, 4][abs(tr.path)]
    for (j in 1:(length(tr.path) - 1)) {
      var.class <- tr.vars[var.index[j]]
      if (var.class == "character" | var.class == "factor" | 
          var.class == "ordered") {
        node.op <- "IN"
        var.levels <- levels(exe(model$call$data)[[var.names[j]]])
        bins <- rattle:::sdecimal2binary(var.values[j])
        bins <- c(bins, rep(0, length(var.levels) - length(bins)))
        if (tr.path[j] > 0) 
          node.value <- var.levels[bins == 1]
        else node.value <- var.levels[bins == 0]
        node.value <- sprintf("(\"%s\")", paste(node.value, 
                                                collapse = "\", \""))
      }
      else if (var.class == "integer" | var.class == "numeric") {
        if (tr.path[j] > 0) 
          node.op <- "<="
        else node.op <- ">"
        node.value <- var.values[j]
      }
      else stop(sprintf("Rattle E234: getRFRuleSet: class %s not supported.", 
                        var.class))
      if (format == "VB") 
        cat(sprintf("AND\n%s %s %s\n", var.names[j], 
                    node.op, node.value))
      else cat(sprintf("%d: %s %s %s\n", j, var.names[j], 
                       node.op, node.value))
    }
    if (format == "VB") 
      cat("THEN Count = Count + 1\n")
    cat("-----------------------------------------------------------------\n")
  }
  if (format == "VB") 
    cat("END IF\n\n")
  cat(sprintf("%sNumber of rules in Tree %d: %d\n\n", comment, 
              n, nrules))
}

# To do
recover_cat <- function(){
  unlockBinding("cat", .BaseNamespaceEnv)
  
  .BaseNamespaceEnv$cat <- function (..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE){
    if (is.character(file))
      if (file == "")
        file <- stdout()
      else if (substring(file, 1L, 1L) == "|") {
        file <- pipe(substring(file, 2L), "w")
        on.exit(close(file))
      }
      else {
        file <- file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
      }
      .Internal(cat(list(...), file, sep, fill, labels, append))
  }
  
  lockBinding("cat", .BaseNamespaceEnv)
}

overwrite_cat <- function(){
  unlockBinding("cat", .BaseNamespaceEnv)
  
  .BaseNamespaceEnv$cat <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE){
    file <- stderr()
    sep <- ""
    
    msg <- .makeMessage(..., domain = NULL, appendLF = TRUE)
    call <- sys.call()
    cond <- simpleMessage(msg, call)
    
    if (is.character(file))
      if (file == "")
        file <- stdout()
    else if (substring(file, 1L, 1L) == "|") {
      file <- pipe(substring(file, 2L), "w")
      on.exit(close(file))
    }
    else {
      file <- file(file, ifelse(append, "a", "w"))
      on.exit(close(file))
    }
    defaultHandler <- function(c) {
      .Internal(cat(as.list(conditionMessage(c)), file, sep, fill, labels, append))
    }
    withRestarts({
      signalCondition(cond)
      defaultHandler(cond)
    }, muffleMessage = function() NULL)
    invisible()
  }
  
  lockBinding("cat",.BaseNamespaceEnv)
}