set.seed(0)

x = sample(1e3, 1e7, replace = TRUE)
nas = sample(1:1e7, 513, replace = TRUE)
x[nas] = NA_integer_
o = order(x, na.last = TRUE)
z = sort(x, na.last = TRUE)

print(system.time({xdup <- duplicated(x)}))
print(system.time({zdup <- duplicated(z)}))
stopifnot(identical(xdup[o], zdup))


xdbl = sample(seq(1, 5e2, by = .5), 1e7, replace = TRUE)
xdbl[nas] = NA_real_
odbl = order(xdbl, na.last = TRUE)
zdbl = sort(xdbl, na.last = TRUE)
zdbl_noalt = zdbl
zdbl_noalt[1] = zdbl_noalt[1]
print("duplicated with doubles with only NAs")
print(system.time({xdbldup <- duplicated(xdbl)}))
print(system.time({zdbldup <- duplicated(zdbl)}))
print(system.time({zdbldup_noalt <- duplicated(zdbl_noalt)}))


print("unique with doubles with NAs")
print(system.time({xdblunq <- unique(xdbl)}))
print(system.time({zdblunq <- unique(zdbl)}))
print(system.time({zdblunq_noalt <- unique(zdbl_noalt)}))

stopifnot(identical(sort(xdblunq,na.last = TRUE), zdblunq),
          identical(zdblunq, zdblunq_noalt))

## how much do we buy with the no NAs at all pass:
gc()
zdnonas = sort(xdbl)
zd1na = sort(c(zdnonas, NA_real_), na.last = TRUE)
zdnonas = sort(xdbl)
print("No NAs at all vs 1 NA")
print(system.time(duplicated(zdnonas)))
print(system.time(duplicated(zd1na)))
