set.seed(0)
nnas = 513
x = sample(1e3, 1e7, replace = TRUE)
xdbl = sample(seq(1, 5e2, by = .5), 1e7, replace = TRUE)

check_one <- function(vec, numnas = 513,  numinfs = 0, nalast = TRUE, fromlast = FALSE, show.timings = TRUE, incl_sorttime = FALSE) {
    if(length(vec) > 0 && numnas > 0) {
        navals <- if(is(vec, "integer")) NA_integer_ else c(NA_real_, NaN)
        vec[1:numnas] = rep(navals, length.out = numnas)
    }
    if(!is(vec, "integer") && length(vec) > numnas + numinfs && numinfs > 0) {
        vec[(numnas + 1):(numnas + numinfs)] = rep(c(Inf, -Inf), length.out = numinfs)
    }
    cat("vector [len", length(vec), "cl:", class(vec), "nalast:", nalast, "fromlast: ", fromlast,  "numNAs:", sum(is.na(vec)),  "numInfs:", sum(!is.na(vec) & !is.finite(vec)), "]\n")
    sorttime <- system.time({y <- sort(vec, na.last = nalast)})[["elapsed"]]
    z <- y
    if(length(y) > 0)
        z[1] <- z[1] ## unset sortedness
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
        if(!is(vec, "integer"))
            check_one(vec, 513, 515, nalast = nalast, fromlast = fromlast, show.timings)
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


full_check(x)
full_check(xdbl)


full_check(integer(0))
full_check(numeric(0))


full_check(x[1])
full_check(xdbl[1])

full_check(x[1:5])
full_check(xdbl[1:5])

full_check(rep(NA_real_, 1200))

full_check(c(Inf, -Inf, -Inf))
