## ---------------------------------------------------------------------
##  h y p e r d u a l s . R  Hyper-dual Numbers as S4 classes
## ---------------------------------------------------------------------


#-- Class definition and validity check---------------------------------

setClass("MetaHyper",
    representation = "VIRTUAL"
)

setClass("Hyperdual",
    representation = representation(f0 = "numeric", f1 = "numeric",
                                    f2 = "numeric", f12 = "numeric"),
    prototype = list(f0 = numeric(), f1 = numeric(),
                     f2 = numeric(), f12 = numeric()),
    contains = "MetaHyper"
)

.hyperdual.valid <- function(object) {
    len <- length(object@f0)
    if (len != length(object@f1) || len != length(object@f2) ||
        len != length(object@f12)) {
        return(FALSE)
    } else { 
        return(TRUE)
    }
}

setValidity("Hyperdual", .hyperdual.valid)


#-- Creating and testing hyper-dual numbers ----------------------------

hyperdual <- function(x, eps1=0.0, eps2=0.0, eps1eps2=0.0) {
    nx <- length(x)
    if (length(eps1eps2) == 1) eps1eps2 <- rep(eps1eps2, nx)
    if (length(eps2) == 1) eps2 <- rep(eps2, nx)
    if (length(eps1) == 1) eps1 <- rep(eps1, nx)
    if (length(eps1) != nx || length(eps2) != nx || length(eps1eps2) != nx)
        stop("Length of 'x' and all 'eps' must be the same, or 1.")
    new("Hyperdual", f0=as.numeric(x), f1=eps1, f2=eps2, f12=eps1eps2)
}

is.hyperdual <- function(x) { is(x, "Hyperdual") }


#-- Coercing hyper-dual numbers ----------------------------------------

as.hyperdual <- function(x) {
    if (is.hyperdual(x)) {
    	return(x)
    } else if (is.numeric(x)) {
    	return(hyperdual(x))
    } else {
        stop("For conversion, argument 'x' must be numeric or hyperdual.")
    }
}

setAs("Hyperdual", "numeric", 
      function(from) { return(from@f0) }
)

setMethod("as.numeric", signature(x = "Hyperdual"),
          function(x) { as(x,"numeric") }
)


#-- Printing hyper-dual numbers ----------------------------------------

.hyperdual.print <- function(x, digits = 5) {
    fmt <- paste("%.", digits, "f", sep="")
    s <- sprintf(fmt, c(x@f0, x@f1, x@f2, x@f12))
    noquote( paste(s[1], "+", s[2], "eps1+", s[3], "eps2+", s[4], "eps1eps2", sep = "") )
}

print.Hyperdual <- function(x, ...) {
    xpr <- .hyperdual.print(x, ...)
    print(xpr)
    return(invisible(xpr))
}

setMethod("show", "Hyperdual", function(object) { print.Hyperdual(object) })


#-- Length and accessor functions --------------------------------------

setGeneric("eps0", function(x) { standardGeneric("eps0") })
setMethod("eps0", "Hyperdual", function(x) { x@f0 })

setGeneric("eps1", function(x) { standardGeneric("eps1") })
setMethod("eps1", "Hyperdual", function(x) { x@f1 })

setGeneric("eps2", function(x) { standardGeneric("eps2") })
setMethod("eps2", "Hyperdual", function(x) { x@f2 })

setGeneric("eps1eps2", function(x) { standardGeneric("eps1eps2") })
setMethod("eps1eps2", "Hyperdual", function(x) { x@f12 })

setMethod("length", "Hyperdual", function(x) { length(x@f0) })


#-- Extraction and replacement ---------------------------------------

setMethod("[", "Hyperdual",
          function(x, i, j, drop) {
              if (!missing(j))
                  warning("Second argument to extractor function ignored.")
              hyperdual(x@f0[i], x@f1[i], x@f2[i], x@f12[i])
          }
)

setReplaceMethod("[", signature(x = "Hyperdual"),
                 function(x, i, j, value) {
                     if (!missing(j))
                         warning("Second argument to extractor function ignored.")
                     ij.f0 <- x@f0
                     ij.f1 <- x@f1
                     ij.f2 <- x@f2
                     ij.f12 <- x@f12
                     if (is.hyperdual(value)) {
                         ij.f0[i] <- value@f0
                         ij.f1[i] <- value@f1
                         ij.f2[i] <- value@f2
                         ij.f12[i] <- value@f12
                         return(hyperdual(ij.f0, ij.f1, ij.f2, ij.f12))
                     } else {
                         x[i] <- as.hyperdual(value)
                         return(x)
                     }
                 }
)

#-- Concatenation of hyper-dual numbers --------------------------------

.hyperdual.cPair <- function(x, y) {
    # x <- as.hyperdual(x)
    # y <- as.hyperdual(y)
    hyperdual(c(x@f0, y@f0), eps1=c(x@f1, y@f1),
              eps2=c(x@f2, y@f2), eps1eps2=c(x@f12, y@f12))
}

setGeneric(".cPair", function(x,y) { standardGeneric(".cPair") })

setMethod(".cPair", c("Hyperdual", "Hyperdual"), function(x, y) { .hyperdual.cPair(x, y) })
setMethod(".cPair", c("Hyperdual", "ANY"), function(x, y){ .hyperdual.cPair(x, as.hyperdual(y)) })
setMethod(".cPair", c("ANY", "Hyperdual"), function(x, y){ .hyperdual.cPair(as.hyperdual(x),y) })
setMethod(".cPair", c("ANY", "ANY"), function(x,y) { c(x, y) })

chyperdual <- function(x, ...) {
    if (nargs() < 3)
        .cPair(x, ...)
    else
        .cPair(x, Recall(...))
}


#-- Arithmetic operations on hyper-dual numbers ------------------------

.hyperdual.neg <- function(e1) {
    hyperdual(-e1@f0, -e1@f1,  -e1@f2,  -e1@f12)
}

.hyperdual.add <- function(e1, e2) {
    e1 <- as.hyperdual(e1)
    e2 <- as.hyperdual(e2)
    hyperdual(e1@f0 + e2@f0, e1@f1 + e2@f1, e1@f2 + e2@f2, e1@f12 + e2@f12)
}

.hyperdual.sub <- function(e1, e2) {
    e1 <- as.hyperdual(e1)
    e2 <- as.hyperdual(e2)
    hyperdual(e1@f0 - e2@f0, e1@f1 - e2@f1, e1@f2 - e2@f2, e1@f12 - e2@f12)
}


.hyperdual.mul <- function(e1, e2) {
    e1 <- as.hyperdual(e1)
    e2 <- as.hyperdual(e2)
    hyperdual(e1@f0 * e2@f0,
              e1@f0*e2@f1 + e1@f1*e2@f0,
              e1@f0*e2@f2 + e1@f2*e2@f0,
              e1@f0*e2@f12 + e1@f1*e2@f2 + e1@f2*e2@f1 + e1@f12*e2@f0)
}

.hyperdual.pow <- function(e1, e2) {
    e1 <- as.hyperdual(e1)
    e2 <- as.numeric(e2)
    if (is.hyperdual(e2))
        stop("Exponent cannot be a hyperdual number.")
    w <- e2
    d <- e2 * e1@f0^(w-1)
    hyperdual(e1@f0^w, e1@f1 * d, e1@f2 * d,
              e1@f12*d + e2*(e2-1) * e1@f1 * e1@f2 * e1@f0^(w-2))
}

.hyperdual.div <- function(e1, e2) {
    e2 <- as.hyperdual(e2)
    if (is.hyperdual(e1)) {
        a <- 1/e2@f0; b <- -a*a
        w <- e1 * hyperdual(a, e2@f1 * b, e2@f2 * b,
                            e2@f12*b - 2*e2@f1*e2@f2*b*a)
    } else {
        a <- 1/e2@f0; b <- -e1 * a*a
        w <- hyperdual(e1 * a, e2@f1 * b, e2@f2 * b,
                       e2@f12*b - 2*e2@f1*e2@f2*b*a)
    }
    w
}

setMethod("Arith", signature("Hyperdual", "missing"),
          function(e1, e2) {
              switch(.Generic,
                     "+" = e1,
                     "-" = .hyperdual.neg(e1),
                     stop(paste("Unary operator", .Generic,
                                "not allowed on hyper-dual numbers."))
              )
          } 
)

.hyperdual.arith <- function(e1, e2) {
    e1 <- as.hyperdual(e1)
    e2 <- as.hyperdual(e2)
    switch(.Generic,
           "+" = .hyperdual.add(e1, e2),
           "-" = .hyperdual.sub(e1, e2),
           "*" = .hyperdual.mul(e1, e2),
           "/" = .hyperdual.div(e1, e2),
           "^" = .hyperdual.pow(e1, e2),
           stop(paste("Binary operator \"", .Generic,
                      "\" not defined for hyper-dual numbers."))
    )
}

setMethod("Arith", signature(e1 = "Hyperdual", e2="ANY"), .hyperdual.arith)
setMethod("Arith", signature(e1 = "ANY", e2="Hyperdual"), .hyperdual.arith)
setMethod("Arith", signature(e1 = "Hyperdual", e2="Hyperdual"), .hyperdual.arith)


#-- Logical operations on hyper-dual numbers ---------------------------

.hyperdual.logic <- function(e1, e2) {
    stop("No logic currently implemented for hyper-dual numbers.")
}

setMethod("Logic", signature(e1 = "Hyperdual", e2 = "ANY"), .hyperdual.logic)
setMethod("Logic", signature(e1 = "ANY", e2 = "Hyperdual"), .hyperdual.logic)
setMethod("Logic", signature(e1 = "Hyperdual", e2 = "Hyperdual"), .hyperdual.logic)


#-- Comparisons of hyper-dual numbers ----------------------------------

.hyperdual.equal <- function(e1, e2) {
    (e1@f0 == e2@f0) & (e1@f1 == e2@f1) & (e1@f2 == e2@f2) & (e1@f3 == e2@f3)
}

.hyperdual.greater <- function(e1, e2) {
    greater <- e1@f0 > e2@f0
    return(greater)
}

".hyperdual.compare" <- function(e1,e2) {
    e1 <- as.hyperdual(e1)
    e2 <- as.hyperdual(e2)
    switch(.Generic,
           "==" =  .hyperdual.equal(e1,e2),
           "!=" = !.hyperdual.equal(e1,e2),
           ">"  =  .hyperdual.greater(e1,e2),
           "<"  = !.hyperdual.greater(e1,e2) & !.hyperdual.equal(e1,e2),
           ">=" =  .hyperdual.greater(e1,e2) |  .hyperdual.equal(e1,e2),
           "<=" = !.hyperdual.greater(e1,e2) |  .hyperdual.equal(e1,e2),
           stop(paste(.Generic, "not supported for hyper-dual numbers."))
    )
}

setMethod("Compare", signature(e1="Hyperdual", e2="ANY" ), .hyperdual.compare)
setMethod("Compare", signature(e1="ANY" , e2="Hyperdual"), .hyperdual.compare)
setMethod("Compare", signature(e1="Hyperdual", e2="Hyperdual"), .hyperdual.compare)


#-- Some summary functions ---------------------------------------------

if (!isGeneric("sum")) {
setGeneric("sum",
           function(x, ..., na.rm = FALSE) {
               standardGeneric("sum")
           },
           useAsDefault = function(x, ..., na.rm = FALSE) {
               base::sum(x, ..., na.rm = na.rm)
           },
           group = "Summary"
)
}

.hyperdual.sum <- function(x) {
    return(hyperdual(sum(x@f0), sum(x@f1), sum(x@f2), sum(x@f12)))
}

setMethod("Summary", "Hyperdual",
          function(x, ..., na.rm = FALSE) {
              switch(.Generic,
                     # max =  .hyperdual.max( x, ..., na.rm=na.rm),
                     # min = -.hyperdual.max(-x, ..., na.rm=na.rm),
                     # range = chyperdual(min(x, na.rm=na.rm), max(x,na.rm=na.rm)),
                     # prod = .hyperdual.prod(x),
                     sum = .hyperdual.sum(x),
                     stop(paste(.Generic, "not allowed on hyper-dual numbers."))
              )
          }
)


#-- Mathematical functions on hyperdual numbers ------------------------

setMethod("abs", "Hyperdual",
          function(x) if (x@f0 < 0) -x else x)

setMethod("sqrt", "Hyperdual",
          function(x) x^0.5
)

setMethod("exp", "Hyperdual",
          function(x) {
              d <- exp(x@f0)
              hyperdual(d, d * x@f1, d * x@f2,
                        d*(x@f12 + x@f1*x@f2))
          }
)

setMethod("log", "Hyperdual",
          function(x) {
              d1 <- x@f1/x@f0; d2 <- x@f2/x@f0; 
              hyperdual(log(x@f0), d1, d2,
                        x@f12/x@f0 - d1*d2)
          }
)

setMethod("sin", "Hyperdual",
          function(x) {
              f <- sin(x@f0); d <- cos(x@f0)
              hyperdual(f, d * x@f1, d * x@f2,
                        d*x@f12 - f*x@f1*x@f2)
          }
)

setMethod("cos", "Hyperdual",
          function(x) {
              f <- cos(x@f0); d <- -sin(x@f0)
              hyperdual(f, d * x@f1, d * x@f2,
                        d*x@f12 - f*x@f1*x@f2)
          }
)

setMethod("tan", "Hyperdual",
          function(x) {
              f <- tan(x@f0); d <- f*f + 1
              hyperdual(f, d * x@f1, d * x@f2,
                        d*x@f12 + 2*f*d*x@f1*x@f2)
          }
)


#-- Hessian matrix using hyper-dual numbers ----------------------------



#-- EoF ----------------------------------------------------------------
