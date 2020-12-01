set.seed(0)
x = sample(1e3, 1e7, replace = TRUE)
xdbl = sample(seq(1, 5e2, by = .5), 1e7, replace = TRUE)

check_one <- function(vec, numnas = 513, numnans = 0, numinfs = 0, nalast = TRUE, fromlast = FALSE, show.timings = TRUE, incl_sorttime = FALSE,
                      enforce_class = TRUE) {
    if(length(vec) > 0 && numnas > 0) {
        if(is(vec, "integer"))
            vec[1:numnas] = NA_integer_
        else {
            vec[1:numnas] = NA_real_
            if(numnans > 0)
                vec[numnas + (1:numnans)] = NaN
        }
    }
    if(!is(vec, "integer") && length(vec) > numnas + numinfs && numinfs > 0) {
        vec[(numnas + 1):(numnas + numinfs)] = rep(c(Inf, -Inf), length.out = numinfs)
    }
    cat("vector [len", length(vec), "cl:", class(vec), "nalast:", nalast, "fromlast: ", fromlast,  "numNA/NANs:", sum(is.na(vec)),  "numInfs:", sum(!is.na(vec) & !is.finite(vec)), "]\n")
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
    ytimea <- system.time({ya <- anyDuplicated(y, fromLast = fromlast)})[["elapsed"]]
    ztimea <- system.time({za <- anyDuplicated(z, fromLast = fromlast)})[["elapsed"]]
    if(show.timings) {
        cat(
            "Duplicate entries: ", sum(zd), "\n",
            "duplicated speedup: ", ztimed/(ytimed + if(incl_sorttime) sorttime else 0),"\n",
            "unique speedup: ", ztimeu/(ytimeu + if(incl_sorttime) sorttime else 0), "\n",
            "anyDuplicated speedup: ", ztimea/(ytimea + if(incl_sorttime) sorttime else 0), "\n")
    }
    if(!identical(yd, zd)) {
        print(which(yd != zd))
        stop("duplicated didn't give perfectly identical results")
    }

    if(!identical(yu, zu)) {
        stop("unique didn't give perfectly identical results")
    }

    if(!identical(ya, za)) {
        print(c(ya, za))
        stop("anyDuplicated did not give perfectly identical results")
    }
    data.frame(dup_entries = sum(zd), mode = mode(x), numnans = sum(is.na(x)),
               dup_time= ztimed, sorted_dup_time = ytimed,
               unq_time = ztimeu, sorted_unq_time = ytimeu,
               anydup_time = ztimea, sorted_anydup_time = ytimea, stringsAsFactors = FALSE)
}

addtolist <- function(lst, new) c(lst, list(new))

multicheck <- function(vec, nalast, fromlast, show.timings = TRUE, manychecks = FALSE) {
    ret <- list(check_one(vec, 0, 0, 0, nalast = nalast, fromlast = fromlast, show.timings = show.timings))
    if(manychecks) {
        if(length(vec) >4) {
            ret <- addtolist(ret, check_one(vec, 1, 0, 0, nalast = nalast, fromlast = fromlast, show.timings))
            ret <- addtolist(ret, check_one(vec, 2, 0, 0, nalast = nalast, fromlast = fromlast, show.timings))
            if(!is(vec, "integer")) {
                ret <- addtolist(ret, check_one(vec, 2, 0, 2, nalast = nalast, fromlast = fromlast, show.timings))
                ret <- addtolist(ret, check_one(vec, 2, 1, 0, nalast = nalast, fromlast = fromlast, show.timings))
                ret <- addtolist(ret, check_one(vec, 2, 2, 0, nalast = nalast, fromlast = fromlast, show.timings))
            }
        }
        if(length(vec) > 1100) {
            ret <- addtolist(ret, check_one(vec, 513, 0, 0, nalast = nalast, fromlast = fromlast, show.timings))
            ret <- addtolist(ret, check_one(vec, length(vec) - 1))
            if(!is(vec, "integer")) {
                ret <- addtolist(ret, check_one(vec, 513, 515, nalast = nalast, fromlast = fromlast, show.timings))
            }
        }
    }
    if(manychecks) {
        do.call(rbind, ret)
    } else {
        ret[[1]]
    }
}

full_check = function(vec, show.timings = TRUE, manychecks = FALSE) {
    rbind(multicheck(vec, TRUE, TRUE, show.timings, manychecks),
          multicheck(vec, TRUE, FALSE, show.timings, manychecks),
          multicheck(vec, FALSE, TRUE, show.timings, manychecks),
          multicheck(vec, FALSE, FALSE, show.timings, manychecks))
}


## long(ish) vectors normal case

##what happens at different levels of duplication density
w <- runif(1e7)

w2 <- round(w, 0)
w3 <- round(w, 1)
w4 <- round(w, 2)
w5 <- round(w, 4)
w6 <- round(w, 6)
w7 <- round(w, 8)
w8 <- round(w, 10)

timings <- rbind(full_check(x, manychecks = TRUE),
                 full_check(xdbl, manychecks = TRUE),
                 full_check(w),
                 full_check(w2),
                 full_check(w3),
                 full_check(w4),
                 full_check(w5),
                 full_check(w6),
                 full_check(w7),
                 full_check(w8))


## additional sanity checks

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
full_check(rep(NaN, 1200))

## what happens with all non-finite
full_check(c(Inf, -Inf, -Inf))





## ## what happens for mostly non-duplicated

## w <- as.numeric(1:1e7)
## w[1] <- w[1] + 0
## w2 <- w
## w2[2] <- w[1]

## w3 <- w
## w3[1e7 - 1] <- w3[1e7]


## full_check(w)
## full_check(w2)
## full_check(w3)


z <- sort(x)
class(z) <- "hahaha_no"

## the output of these methods are (very) wrong on purpose so we can tell they
## are hit instead of the altrep code
duplicated.hahaha_no <- function(x, incomparables = FALSE, ...) {
    rep(c(TRUE, FALSE), length.out = length(x))
}

unique.hahaha_no <- function(x, incomparables = FALSE, ...) {
    x[c(1, 5, length(x))]
}

z2 <- z
z2[1] = z2[1]
stopifnot(identical(duplicated(z), duplicated(z2)))
stopifnot(identical(unique(z), unique(z2)))


## full_check(z)

## zdbl <- xdbl
## class(zdbl) <- "hahaha_no"

## full_check(zdbl)


print(timings)
