##  --------------------------------------------------------
##          d u a l n u m b e r s . R  Dual Numbers 
##  --------------------------------------------------------
#
#   Literature:
#     A. Griewank and A. Walther. Evaluating Derivatives:
#     Principles and Techniques of Algorithmic Differentiation.
#     SIAM Society for Industrial and Applied Mathematics, 2008.
#
#   TODO
#     Define 'dualnum' as S3 subclass of numeric.
#     How to handle the c() Function? [Is it a blow-up?]
#

#-- constructor and print function -------------------------
is.dualnum <- function(x) {
    class(x) == "dualnum"
}

as.dualnum <- function(x) {
    if (is.dualnum(x)) {
        xd <- x
    } else {
        xd <- c(x, 1.0)
        class(xd) <- "dualnum"
    }
    xd
}

print.dualnum <- function(dn)
    cat("Dual number: [", real(dn), ",", epsilon(dn), "]\n")

#-- real and epsilon part ----------------------------------
real <- function(dn) UseMethod("real")
real.dualnum <- function(dn) dn[1]
epsilon <- function(dn) UseMethod("epsilon")
epsilon.dualnum <- function(dn) dn[2]

#-- binary operators ---------------------------------------
add.dualnum <- `+.dualnum` <- function(dn1, dn2) {
    if (!is.dualnum(dn1)) {
        dn = c(dn1 + dn2[1], dn2[2])
    } else if (!is.dualnum(dn2)) {
        dn = c(dn1[1] + dn2, dn1[2])
    } else {
        dn <- c(dn1[1] + dn2[1], dn1[2] + dn2[2])
    }
    class(dn) <- "dualnum"; dn
}

sub.dualnum <- `-.dualnum` <- function(dn1, dn2=NULL) {
    if (is.null(dn2)) {
        dn = c(-dn1[1], -dn1[2])
    } else if (!is.dualnum(dn1)) {
        dn = c(dn1 - dn2[1], -dn2[2])
    } else if (!is.dualnum(dn2)) {
        dn = c(dn1[1] - dn2, dn1[2])
    } else {
        dn <- c(dn1[1] - dn2[1], dn1[2] - dn2[2])
    }
    class(dn) <- "dualnum"; dn
}

mul.dualnum <- `*.dualnum` <- function(dn1, dn2) {
    if (!is.dualnum(dn1)) {
        dn <- c(dn1 * dn2[1], dn1 * dn2[2])
    } else if (!is.dualnum(dn2)) {
        dn <- c(dn1[1] * dn2, dn1[2] * dn2)
    } else {
        dn <- c(dn1[1] * dn2[1],
                dn1[1] * dn2[2] + dn1[2] * dn2[1])
    }
    class(dn) <- "dualnum"; dn
}

div.dualnum <- `/.dualnum` <- function(dn1, dn2) {
    if (!is.dualnum(dn1)) {
        dn <- c(dn1/dn2[1], -dn1*dn2[2]/dn2[1]^2)
    } else if (!is.dualnum(dn2)) {
        dn <- c(dn1[1]/dn2, dn1[2]/dn2)
    } else {
        dn <- c(dn1[1]/dn2[1],
                (dn1[2]*dn2[1] - dn1[1]*dn2[2])/(dn2[1]*dn2[1]))
    }
    class(dn) <- "dualnum"; dn
}

pow.dualnum <- `^.dualnum` <- function(dn1, r) {
    if (is.dualnum(r)) {
        stop("Exponentiation with dual numbers forbidden.")
    } else {
        dn <- c(dn1[1]^r, dn1[2]*r*dn1[1]^(r-1))
    }
    class(dn) <- "dualnum"; dn
}


#-- mathematical functions ---------------------------------
abs.dualnum <- function(dn1) {
    if (dn1[1] >= 0) dn = dn1
    else dn = -dn1
}

sqrt.dualnum <- function(dn1) {
    dn <- c(sqrt(dn1[1]), 0.5*dn1[2]/sqrt(dn1[1]))
    class(dn) <- "dualnum"; dn
}

exp.dualnum <- function(dn1) {
    dn <- c(exp(dn1[1]), dn1[2]*exp(dn1[1]))
    class(dn) <- "dualnum"; dn
}

sin.dualnum <- function(dn1) {
    dn <- c(sin(dn1[1]), dn1[2]*cos(dn1[1]))
    class(dn) <- "dualnum"; dn
}

cos.dualnum <- function(dn1) {
    dn <- c(cos(dn1[1]), -dn1[2]*sin(dn1[1]))
    class(dn) <- "dualnum"; dn
}

#-- calculate gradients ------------------------------------

graddual <- function(fun, x) {
    n <- length(x)
    if (n > 1)
        stop("The multivariate case is not yet implemented.")
    fx <- fun(as.dualnum(x))
    if (!is.dualnum(fx))
        stop("Function 'fun' does not work with dual numbers.")
    epsilon(fx)
}

#-- EoF ----------------------------------------------------
