## ---------------------------------------------------------------------
##  d u a l n u m b e r s . R  Dual Numbers as S4 classes
## ---------------------------------------------------------------------


#-- Class definition and validity check---------------------------------

setClass("MetaDual",
    representation = "VIRTUAL"
)

setClass("DualNumber",
    representation = representation(real = "numeric", eps = "numeric"),
    prototype = list(real = numeric(), eps = numeric()),
    contains = "MetaDual"
)

.dual.valid <- function(object) {
    len <- length(object@real)
    if (len != length(object@eps)) {
        FALSE
    } else { 
        return(TRUE)
    }
}

setValidity("DualNumber", .dual.valid)


#-- Creating and testing dual numbers ----------------------------------
"dual" <- function(x, eps) {
    nx <- length(x)
    if (missing(eps))  eps <- rep(0, nx)
    if (length(eps) == 1) eps <- rep(eps, nx)
    new("DualNumber", real = as.numeric(x), eps = eps)
}

is.dual <- function(x) { is(x, "DualNumber") }


#-- Coercing dual numbers ----------------------------------------------

as.dual <- function(x) {
    if (is.dual(x)) {
        return(x)
    } else if (is.numeric(x)) {
        return(dual(x, 0))
    } else {
        stop("For conversion argument 'x' must be numeric or dual.")
    }
}

setAs("DualNumber", "numeric", 
      function(from) { return(from@real) }
)

setMethod("as.numeric", signature(x = "DualNumber"),
          function(x) { as(x,"numeric") }
)

        
#-- Printing dual numbers ----------------------------------------------

.dual.print <- function(x, digits = 5) {
    fmt <- paste("%.", digits, "f", sep="")
    s0 <- sprintf(fmt, x@real)
    s1 <- sprintf(fmt, x@eps)
    noquote( paste(s0, " + ", s1, "eps", sep = "") )
}

print.DualNumber <- function(x, ...) {
    xpr <- .dual.print(x, ...)
    print(xpr)
    return(invisible(xpr))
}

setMethod("show", "DualNumber", function(object) { print.DualNumber(object) })


#-- Length and element extraction --------------------------------------

setGeneric("real", function(x) { standardGeneric("real") })
setMethod("real", "DualNumber", function(x) { x@real })

setGeneric("eps", function(x) { standardGeneric("eps") })
setMethod("eps", "DualNumber", function(x) { x@eps })

setMethod("length", "DualNumber", function(x) { length(x@real) })


#-- Extraction and replacement ---------------------------------------

setMethod("[", "DualNumber",
          function(x, i, j, drop) {
              if (!missing(j))
                  warning("Second argument to extractor function ignored.")
              dual(x@real[i], x@eps[i])
          }
)

setReplaceMethod("[", signature(x = "DualNumber"),
                 function(x, i, j, value) {
                     if (!missing(j))
                         warning("Second argument to extractor function ignored.")
                     ij.real <- x@real
                     ij.eps <- x@eps
                     if (is.dual(value)) {
                         ij.real[i] <- value@real
                         ij.eps[i] <- value@eps
                         return(dual(x = ij.real, eps = ij.eps))
                     } else {
                         x[i] <- as.dual(value)
                         return(x)
                     }
                 }
)


#-- Concatenation of dual numbers --------------------------------------

.dual.cPair <- function(x, y) {
    x <- as.dual(x)
    y <- as.dual(y)
    dual(c(x@real, y@real), c(x@eps, y@eps))
}

setGeneric(".cPair", function(x,y) { standardGeneric(".cPair") })

setMethod(".cPair", c("DualNumber", "DualNumber"), function(x, y) { .dual.cPair(x, y) })
setMethod(".cPair", c("DualNumber", "ANY"), function(x, y){ .dual.cPair(x, as.dual(y)) })
setMethod(".cPair", c("ANY", "DualNumber"), function(x, y){ .dual.cPair(as.dual(x),y) })
setMethod(".cPair", c("ANY", "ANY"), function(x,y) {c(x, y)})

cdual <- function(x, ...) {
    if (nargs() < 3)
        .cPair(x, ...)
    else
        .cPair(x, Recall(...))
}


#-- Arithmetic operations on dual numbers ------------------------------

.dual.neg <- function(d1) {
    dual(-d1@real, -d1@eps)
}

.dual.add <- function(d1, d2) {
    d1 <- as.dual(d1)
    d2 <- as.dual(d2)
    dual(d1@real + d2@real, d1@eps + d2@eps)
}

.dual.sub <- function(d1, d2) {
    d1 <- as.dual(d1)
    d2 <- as.dual(d2)
    dual(d1@real - d2@real, d1@eps - d2@eps)
}


.dual.mul <- function(d1, d2) {
    d1 <- as.dual(d1)
    d2 <- as.dual(d2)
    dual(d1@real*d2@real, d1@real*d2@eps + d1@eps*d2@real)
}

.dual.pow <- function(d1, d2) {
    d1 <- as.dual(d1)
    d2 <- as.numeric(d2)
    #d2 <- as.dual(d2)
    #dual(d1@real^d2@real,
    #     d1@eps*d2@real*d1@real^(d2@real-1) + d2@eps*d1@real^d2*log(d1@real))
    dual(d1@real^d2,
         d1@eps*d2*d1@real^(d2-1))
}

.dual.div <- function(d1, d2) {
    d1 <- as.dual(d1)
    d2 <- as.dual(d2)
    dual(d1@real/d2@real, (d1@eps*d2@real - d1@real*d2@eps)/(d2@real*d2@real))
}

setMethod("Arith", signature("DualNumber", "missing"),
          function(e1, e2) {
              switch(.Generic,
                     "+" = e1,
                     "-" = .dual.neg(e1),
                     stop(paste("Unary operator", .Generic,
                                "not allowed on dual numbers."))
              )
          } 
)

.dual.arith <- function(e1, e2) {
    e1 <- as.dual(e1)
    e2 <- as.dual(e2)
    switch(.Generic,
           "+" = .dual.add(e1, e2),
           "-" = .dual.sub(e1, e2),
           "*" = .dual.mul(e1, e2),
           "/" = .dual.div(e1, e2),
           "^" = .dual.pow(e1, e2),
           stop(paste("Binary operator \"", .Generic,
                      "\" not defined for dual numbers."))
    )
}

setMethod("Arith", signature(e1 = "DualNumber", e2="ANY"), .dual.arith)
setMethod("Arith", signature(e1 = "ANY", e2="DualNumber"), .dual.arith)
setMethod("Arith", signature(e1 = "DualNumber", e2="DualNumber"), .dual.arith)


#-- Logical operations on dual numbers

.dual.logic <- function(e1, e2) {
    stop("No logic currently implemented for dual numbers.")
}

setMethod("Logic", signature(e1 = "DualNumber", e2 = "ANY"), .dual.logic)
setMethod("Logic", signature(e1 = "ANY", e2 = "DualNumber"), .dual.logic)
setMethod("Logic", signature(e1 = "DualNumber", e2 = "DualNumber"), .dual.logic)


#-- Comparisons of dual numbers ----------------------------------------

.dual.equal <- function(e1, e2) {
    (e1@real == e2@real) & (e1@eps == e2@eps)
}

.dual.greater <- function(e1, e2) {
    greater <- e1@real > e2@real | (e1@real == e2@real & e1@eps > e2@eps)
    return(greater)
}

".dual.compare" <- function(e1,e2) {
    e1 <- as.dual(e1)
    e2 <- as.dual(e2)
    switch(.Generic,
           "==" =  .dual.equal(e1,e2),
           "!=" = !.dual.equal(e1,e2),
           ">"  =  .dual.greater(e1,e2),
           "<"  = !.dual.greater(e1,e2) & !.dual.equal(e1,e2),
           ">=" =  .dual.greater(e1,e2) |  .dual.equal(e1,e2),
           "<=" = !.dual.greater(e1,e2) |  .dual.equal(e1,e2),
           stop(paste(.Generic, "not supported for dual numbers."))
    )
}

setMethod("Compare", signature(e1="DualNumber", e2="ANY" ), .dual.compare)
setMethod("Compare", signature(e1="ANY" , e2="DualNumber"), .dual.compare)
setMethod("Compare", signature(e1="DualNumber", e2="DualNumber"), .dual.compare)


#-- Some summary functions ---------------------------------------------

setGeneric("max",
           function(x, ..., na.rm = FALSE) {
               standardGeneric("max")
           },
           useAsDefault = function(x, ..., na.rm = FALSE) {
               base::max(x, ..., na.rm = na.rm)
           },
           group = "Summary"
)

setGeneric("min",
           function(x, ..., na.rm = FALSE) {
               standardGeneric("min")
           },
           useAsDefault = function(x, ..., na.rm = FALSE) {
               base::min(x, ..., na.rm = na.rm)
           },
           group = "Summary"
)

setGeneric("range",
           function(x, ..., na.rm = FALSE) {
               standardGeneric("range")
           },
           useAsDefault = function(x, ..., na.rm = FALSE) {
               base::range(x, ..., na.rm = na.rm)
           },
           group = "Summary"
)

setGeneric("prod",
           function(x, ..., na.rm = FALSE) {
               standardGeneric("prod")
           },
           useAsDefault = function(x, ..., na.rm = FALSE) {
               base::prod(x, ..., na.rm = na.rm)
           },
           group = "Summary"
)

setGeneric("sum",
           function(x, ..., na.rm = FALSE) {
               standardGeneric("sum")
           },
           useAsDefault = function(x, ..., na.rm = FALSE) {
               base::sum(x, ..., na.rm = na.rm)
           },
           group = "Summary"
)

.dual.max <- function(x, ..., na.rm=FALSE) {
    imax <- which.max(x@real)
    return(x[i])
}

.dual.prod <- function(x) {
    n <- length(x)
    res <- x[1]
    if (n == 1) return(res)
    for (i in 2:n) {
        res <- res * x[i]
    }
    return(res)
}

.dual.sum <- function(x) {
    return(dual(sum(x@real), sum(x@eps)))
}

setMethod("Summary", "DualNumber",
          function(x, ..., na.rm = FALSE) {
              switch(.Generic,
                     max =  .dual.max( x, ..., na.rm=na.rm),
                     min = -.dual.max(-x, ..., na.rm=na.rm),
                     range = cdual(min(x, na.rm=na.rm), max(x,na.rm=na.rm)),
                     prod = .dual.prod(x),
                     sum = .dual.sum(x),
                     stop(paste(.Generic, "not allowed on dual numbers."))
              )
          }
)


#-- Mathematical functions on dual numbers -----------------------------

setMethod("sin", "DualNumber",
          function(x) dual(sin(x@real), x@eps*cos(x@real))
)

setMethod("cos", "DualNumber",
          function(x) dual(cos(x@real), -x@eps*sin(x@real))
)

setMethod("exp", "DualNumber",
          function(x) dual(exp(x@real), x@eps*exp(x@real))
)

setMethod("log", "DualNumber",
          function(x) dual(log(x@real), x@eps/x@real)
)

setMethod("abs", "DualNumber",
          function(x) if (x@real < 0) -x else x)

setMethod("Math", "DualNumber",
          function(x) {
              switch(.Generic,
                     sqrt = dual(sqrt(x@real), 0.5*x@eps/sqrt(x@real)),
#                      abs = ,
#                      log = ,
#                      exp =,
#                      cosh =,
#                      sinh =,
#                      acos =,
#                      acosh =,
#                      asin =,
#                      asinh =,
#                      atan =,
#                      atanh =,
#                      cos =,
#                      sin =,
#                      tan =,
#                      tanh =,
#                      lgamma =,
#                      cumsum =,
#                      gamma =,
#                      ceiling =,
#                      floor = ,
                     stop(paste(.Generic, "not allowed on dual numbers."))
              )
          }
)


#-- Gradient function using dual numbers -------------------------------

graddual <- function(f, x) {
    n <- length(x)
    g <- numeric(n)
    xdual <- as.dual(x)
    for (i in 1:n) {
        xdual[i] <- dual(x[i], 1.0)
        g[i] <- eps(f(xdual))
        xdual[i] <- as.dual(x[i])
    }
    return(g)
}


# ----------------------------------------------------------------------
