set.seed(0)
x = sample(1e3, 1e7, replace = TRUE)
xdbl = sample(seq(1, 5e2, by = .5), 1e7, replace = TRUE)

check_one <- function(vec, numnas = 513,  numinfs = 0, nalast = TRUE, fromlast = FALSE, show.timings = TRUE, incl_sorttime = FALSE,
                      enforce_class = TRUE) {
    if(length(vec) > 0 && numnas > 0) {
        navals <- if(is(vec, "integer")) NA_integer_ else c(NA_real_, NaN)
        vec[1:numnas] = rep(navals, length.out = numnas)
    }
    if(!is(vec, "integer") && length(vec) > numnas + numinfs && numinfs > 0) {
        vec[(numnas + 1):(numnas + numinfs)] = rep(c(Inf, -Inf), length.out = numinfs)
    }
    cat("vector [len", length(vec), "cl:", class(vec), "nalast:", nalast, "fromlast: ", fromlast,  "numNAs:", sum(is.na(vec)),  "numInfs:", sum(!is.na(vec) & !is.finite(vec)), "]\n")
    sorttime <- system.time({y <- sort(vec, na.last = nalast)})[["elapsed"]]
    if(enforce_class)
        class(y) <- class(vec)
    z <- y
    if(length(y) > 0)
        z[1] <- z[1] ## unset sortedness
    if(enforce_class)
        stopifnot(identical(class(vec), class(z)))

    ytimed <- system.time({yd <- duplicated(y, fromLast = fromlast)})[["elapsed"]]
    ztimed <- system.time({zd <- duplicated(z, fromLast = fromlast)})[["elapsed"]]
    ytimeu <- system.time({yu <- unique(y, fromLast = fromlast)})[["elapsed"]]
    ztimeu <- system.time({zu <- unique(z, fromLast = fromlast)})[["elapsed"]]
    if(show.timings) {
        cat(
            "duplicated speedup: ", ztimed/(ytimed + if(incl_sorttime) sorttime else 0),"\n",
            "unique speedup: ", ztimeu/(ytimeu + if(incl_sorttime) sorttime else 0), "\n")
    }
    if(!identical(yd, zd)) {
        print(which(yd != zd))
        stop("yd and zd not equal")
    }

    if(!identical(yu, zu)) {
        stop("yu and zu not equal")
    }
    TRUE
}


multicheck <- function(vec, nalast, fromlast, show.timings = TRUE) {
    check_one(vec, 0, 0, nalast = nalast, fromlast = fromlast, show.timings)
    if(length(vec) >4) {
        check_one(vec, 1, 0, nalast = nalast, fromlast = fromlast, show.timings)
        if(!is(vec, "integer"))
            check_one(vec, 2, 2, nalast = nalast, fromlast = fromlast, show.timings)
    }
    if(length(vec) > 1100) {
        check_one(vec, 513, 0, nalast = nalast, fromlast = fromlast, show.timings)
        check_one(vec, length(vec) - 1)
        if(!is(vec, "integer")) {
            check_one(vec, 513, 515, nalast = nalast, fromlast = fromlast, show.timings)
            half <- floor(length(vec)/2)
            check_one(vec, half, length(vec) - half -1, nalast = nalast, fromlast = fromlast, show.timings)
        }
    }
    TRUE
}
full_check = function(vec, show.timings = TRUE) {
    multicheck(vec, TRUE, TRUE, show.timings)
    multicheck(vec, TRUE, FALSE, show.timings)
    multicheck(vec, FALSE, TRUE, show.timings)
    multicheck(vec, FALSE, FALSE, show.timings)
    TRUE
}


## long(ish) vectors normal case
full_check(x)
full_check(xdbl)

## doesn't break on length 0
full_check(integer(0))
full_check(numeric(0))

## does something reasonable on length 1
full_check(x[1])
full_check(xdbl[1])

## what happens with short vectors
full_check(x[1:5])
full_check(xdbl[1:5])

## what happens with all NAs
full_check(rep(NA_real_, 1200))

## what happens with all non-finite
full_check(c(Inf, -Inf, -Inf))

z <- x
class(z) <- "hahaha_no"

## the output of these are (very) wrong on purpose so we can tell they
## are hit instead of the altrep code
duplicated.hahaha_no <- function(x, incomparables = FALSE, ...) {
    rep(c(TRUE, FALSE), length.out = length(x))
}

unique.hahaha_no <- function(x, incomparables = FALSE, ...) {
    x[c(1, 5, length(x))]
}

full_check(z)

zdbl <- xdbl
class(zdbl) <- "hahaha_no"

full_check(zdbl)
