
# Edit > Folding > Collapse All (is of much help to visualize in an orderly way the code).

# FUNCTIONS ---------------------------------------------------------------------------------------------------------------

# To help reduce the huge list of imports:


# Functions taken from the dplyr package. near()
near <- function (x, y, tol = .Machine$double.eps^0.5) 
{
  abs(x - y) < tol
}


# -----------------------FOR pairs.panels-----------------------------
# Functions taken from the psych package. cor.wt()
cor.wt <- function (data, vars = NULL, w = NULL, sds = NULL, cor = TRUE) 
{
  cl <- match.call()
  if (is.list(data) && !is.data.frame(data)) {
    w <- data$n
    sds <- data$sd
    x <- data$mean
  }
  else {
    x <- data
  }
  if (!is.null(vars)) {
    x <- x[, vars]
    w <- w[, vars]
    sds <- sds[, vars]
  }
  if (is.null(w)) 
    w <- matrix(rep(rep(1/nrow(x), nrow(x)), ncol(x)), nrow = nrow(x), 
                ncol = ncol(x))
  if (is.null(ncol(w))) {
    wt <- w/sum(w)
  }
  else {
    wt <- t(t(w)/colSums(w))
  }
  cnames <- colnames(x)
  for (i in 1:ncol(x)) {
    if (is.factor(x[, i]) || is.logical(x[, i])) {
      x[, i] <- as.numeric(x[, i])
      colnames(x)[i] <- paste(cnames[i], "*", sep = "")
    }
  }
  means <- colSums(x * wt, na.rm = TRUE)
  xc <- scale(x, center = means, scale = FALSE)
  if (is.null(sds)) {
    xs <- xc/sqrt(w)
  }
  else {
    xs <- xc * sds/sqrt(w)
  }
  xwt <- sqrt(wt) * xc
  if (any(is.na(xwt))) {
    cov <- apply(xwt, 2, function(x) colSums(xwt * x, na.rm = TRUE))
  }
  else {
    cov <- crossprod(xwt)
  }
  if (cor) {
    r <- cov2cor(cov)
  }
  else {
    r <- cov
  }
  xw <- wt * xc
  result <- list(r = r, xwt = xwt, wt = wt, mean = means, xc = xc, 
                 xs = xs)
  result$Call <- cl
  class(result) <- c("psych", "cor.wt")
  return(result)
}

# Functions taken from the dplyr package. fisherz()
fisherz <- function (rho) 
{
  0.5 * log((1 + rho)/(1 - rho))
}

# Functions taken from the dplyr package. fisherz2r()
fisherz2r <- function (z) 
{
  (exp(2 * z) - 1)/(1 + exp(2 * z))
}

# Functions taken from the dplyr package. r.con()
r.con <- function (rho, n, p = 0.95, twotailed = TRUE) 
{
  z <- fisherz(rho)
  if (n < 4) {
    stop("number of subjects must be greater than 3")
  }
  se <- 1/sqrt(n - 3)
  p <- 1 - p
  if (twotailed) 
    p <- p/2
  dif <- qnorm(p)
  zlow <- z + dif * se
  zhigh <- z - dif * se
  ci <- c(zlow, zhigh)
  ci <- fisherz2r(ci)
  return(ci)
}


# Functions taken from the psych package. r.test()
r.test <- function (n, r12, r34 = NULL, r23 = NULL, r13 = NULL, r14 = NULL, 
          r24 = NULL, n2 = NULL, pooled = TRUE, twotailed = TRUE) 
{
  cl <- match.call()
  if (is.null(r34) & is.null(r13) & is.null(r23)) {
    t <- r12 * sqrt(n - 2)/sqrt(1 - r12^2)
    p <- 1 - pt(abs(t), n - 2)
    if (twotailed) 
      p <- 2 * p
    ci <- r.con(r12, n)
    result <- list(Call = cl, Test = "Test of significance of a  correlation", 
                   t = t, p = p, ci = ci)
  }
  else {
    if (is.null(r23)) {
      if (is.null(r34)) {
        stop("You seem to be testing two dependent correlations, but have not specified the other correlation(s)  correctly.")
      }
      if (!is.null(r13)) {
        stop("You seem to be testing two dependent correlations, but have not specified the correlation(s)  correctly.")
      }
      xy.z <- 0.5 * log((1 + r12)/(1 - r12))
      xz.z <- 0.5 * log((1 + r34)/(1 - r34))
      if (is.null(n2)) 
        n2 <- n
      se.diff.r <- sqrt(1/(n - 3) + 1/(n2 - 3))
      diff <- xy.z - xz.z
      z <- abs(diff/se.diff.r)
      p <- (1 - pnorm(z))
      if (twotailed) 
        p <- 2 * p
      result <- list(Call = cl, Test = "Test of difference between two independent correlations", 
                     z = z, p = p)
    }
    else {
      if (is.null(r14)) {
        if (!is.null(r34)) {
          if (is.null(r13)) {
            r13 <- r34
          }
        }
        if (is.null(r13)) {
          stop("You seem to be trying to test two dependent correlations, but have not specified the other correlation(s)")
        }
        diff <- r12 - r13
        determin = 1 - r12 * r12 - r23 * r23 - r13 * 
          r13 + 2 * r12 * r23 * r13
        av = (r12 + r13)/2
        cube = (1 - r23) * (1 - r23) * (1 - r23)
        t2 = diff * sqrt((n - 1) * (1 + r23)/(((2 * (n - 
                                                       1)/(n - 3)) * determin + av * av * cube)))
        p <- pt(abs(t2), n - 3, lower.tail = FALSE)
        if (twotailed) 
          p <- 2 * p
        cl <- paste("r.test(n = ", n, ",  r12 = ", r12, 
                    ",  r23 = ", r23, ",  r13 = ", r13, ")")
        result <- list(Call = cl, Test = "Test of difference between two correlated  correlations", 
                       t = t2, p = p)
      }
      else {
        z12 <- fisherz(r12)
        z34 <- fisherz(r34)
        pooledr <- (r12 + r34)/2
        if (pooled) {
          r1234 = 1/2 * ((r13 - pooledr * r23) * (r24 - 
                                                    r23 * pooledr) + (r14 - r13 * pooledr) * 
                           (r23 - pooledr * r13) + (r13 - r14 * pooledr) * 
                           (r24 - pooledr * r14) + (r14 - pooledr * 
                                                      r24) * (r23 - r24 * pooledr))
          z1234 <- r1234/((1 - pooledr^2) * (1 - pooledr^2))
        }
        else {
          r1234 = 1/2 * ((r13 - r12 * r23) * (r24 - r23 * 
                                                r34) + (r14 - r13 * r34) * (r23 - r12 * r13) + 
                           (r13 - r14 * r34) * (r24 - r12 * r14) + (r14 - 
                                                                      r12 * r24) * (r23 - r24 * r34))
          z1234 <- r1234/((1 - r12^2) * (1 - r34^2))
        }
        ztest <- (z12 - z34) * sqrt(n - 3)/sqrt(2 * (1 - 
                                                       z1234))
        z <- ztest
        p <- (1 - pnorm(abs(z)))
        if (twotailed) 
          p <- 2 * p
        result <- list(Call = cl, Test = "Test of difference between two dependent correlations", 
                       z = z, p = p)
      }
    }
  }
  class(result) <- c("psych", "r.test")
  return(result)
}


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
#--------------------------------------------------------------------

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



#----------------------htmlwidgets------------------------
# Funciones tomadas del paquete htmlwidgets

#' Eval character vectors to JS code
#'
#' @param ... character vectors to evaluate
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @export e_JS
#' @examples
#' e_JS('5 * 3')
#' 
e_JS <- function (...) 
{
  x <- c(...)
  if (is.null(x)) 
    return()
  if (!is.character(x)) 
    stop("The arguments for JS() must be a character vector")
  x <- paste(x, collapse = "\n")
  structure(x, class = unique(c("JS_EVAL", oldClass(x))))
}


#----------------------colourpicker------------------------
colourInput <- function (inputId, label, value = "white", showColour = c("both", 
                                                          "text", "background"), palette = c("square", "limited"), 
          allowedCols = NULL, allowTransparent = FALSE, returnName = FALSE, 
          closeOnClick = FALSE) 
{
  showColour <- match.arg(showColour)
  palette <- match.arg(palette)
  value <- restoreInput(id = inputId, default = value)
  shiny::addResourcePath("colourpicker-binding", system.file("srcjs", 
                                                             package = "colourpicker"))
  shiny::addResourcePath("colourpicker-lib", system.file("www", 
                                                         "shared", "colourpicker", package = "colourpicker"))
  deps <- list(htmltools::htmlDependency("colourpicker-binding", 
                                         "0.1.0", c(href = "colourpicker-binding"), script = "input_binding_colour.js"), 
               htmltools::htmlDependency("colourpicker-lib", "0.1.0", 
                                         c(href = "colourpicker-lib"), script = "js/colourpicker.min.js", 
                                         stylesheet = "css/colourpicker.min.css"))
  inputTag <- shiny::tags$input(id = inputId, type = "text", 
                                class = "form-control shiny-colour-input", `data-init-value` = value, 
                                `data-show-colour` = showColour, `data-palette` = palette)
  if (!is.null(allowedCols)) {
    allowedCols <- toJSON(allowedCols)
    inputTag <- shiny::tagAppendAttributes(inputTag, `data-allowed-cols` = allowedCols)
  }
  if (returnName) {
    inputTag <- shiny::tagAppendAttributes(inputTag, `data-return-name` = "true")
  }
  if (allowTransparent) {
    inputTag <- shiny::tagAppendAttributes(inputTag, `data-allow-alpha` = "true")
  }
  if (closeOnClick) {
    inputTag <- shiny::tagAppendAttributes(inputTag, `data-close-on-click` = "true")
  }
  inputTag <- shiny::div(class = "form-group shiny-input-container", 
                         `data-shiny-input-type` = "colour", label, inputTag)
  htmltools::attachDependencies(inputTag, deps)
}

#---------------------------jsonlite---------------------------
asJSON <- function (x, ...) 
{
  standardGeneric("asJSON")
}

toJSON <- function (x, dataframe = c("rows", "columns", "values"), matrix = c("rowmajor", 
                                                                    "columnmajor"), Date = c("ISO8601", "epoch"), POSIXt = c("string", 
                                                                                                                             "ISO8601", "epoch", "mongo"), factor = c("string", "integer"), 
          complex = c("string", "list"), raw = c("base64", "hex", "mongo", 
                                                 "int", "js"), null = c("list", "null"), na = c("null", 
                                                                                                "string"), auto_unbox = FALSE, digits = 4, pretty = FALSE, 
          force = FALSE, ...) 
{
  dataframe <- match.arg(dataframe)
  matrix <- match.arg(matrix)
  Date <- match.arg(Date)
  POSIXt <- match.arg(POSIXt)
  factor <- match.arg(factor)
  complex <- match.arg(complex)
  raw <- match.arg(raw)
  null <- match.arg(null)
  x <- force(x)
  if (!missing(na)) {
    na <- match.arg(na)
  }
  else {
    na <- NULL
  }
  indent <- if (isTRUE(pretty)) 
    0L
  else NA_integer_
  ans <- asJSON(x, dataframe = dataframe, Date = Date, POSIXt = POSIXt, 
                factor = factor, complex = complex, raw = raw, matrix = matrix, 
                auto_unbox = auto_unbox, digits = digits, na = na, null = null, 
                force = force, indent = indent, ...)
  if (is.numeric(pretty)) {
    #prettify(ans, pretty)
  }
  else {
    class(ans) <- "json"
    return(ans)
  }
}
#----------------------colourpicker------------------------