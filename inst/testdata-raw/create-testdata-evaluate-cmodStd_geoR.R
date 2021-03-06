# save output from gstat and geoR package
set.seed(87)
n = 10
coords = matrix(runif(n * 2), nrow = n)
d = as.matrix(dist(coords))

# check geoR
mod = c("exponential", "matern", "spherical", "gaussian")
evar = c(0, .5, 0, .5)
fvar = c(0, 0, .5, .5)
r = runif(4)
par3 = runif(4, 0, 2)
psill = rgamma(4, 2, 2)
geoR_cov_params = list(mod = mod,
                         evar = evar,
                         fvar = fvar,
                         r = r,
                         par3 = par3,
                         psill = psill)

count = 0
geoR_cov = vector("list", length(evar) * length(mod))
for (i in seq_along(mod)) {
        for (j in seq_along(evar)) {
                count = count + 1
                geoR_cov[[count]] = 
                        geoR::varcov.spatial(coords, cov.model = mod[i], 
                                             kappa = par3[j],
                                             nugget = (evar[j] + fvar[j]), 
                                             cov.pars = c(psill[j], r[j]))$varcov
        }
}

# comparison with spam
mod = c("exponential", "spherical", "wendland1", "wendland2", "wu1", "wu2", "wu3")
evar = c(0, .5, 0, .5)
fvar = c(0, 0, .5, .5)
r = runif(4)
par3 = runif(4, 0, 2)
psill = rgamma(4, 2, 2)
spam_cov_params = list(mod = mod,
                         evar = evar,
                         fvar = fvar,
                         r = r,
                         par3 = par3,
                         psill = psill)
count = 0
spam_cov = vector("list", length(evar) * length(mod))
count = 0
for (i in 1:length(mod)) {
        for (j in 1:length(evar)) {
                count = count + 1
                spam_cov[[count]] = 
                        spam::covmat(d, 
                                     theta = c(r[j], psill[j], (evar[j] + fvar[j])),
                                     type = mod[i])
        }
}

# comparison with spam (matern)
mod = c("matern")
evar = c(0, .5, 0, .5)
fvar = c(0, 0, .5, .5)
r = runif(4)
par3 = runif(4, 0, 2)
psill = rgamma(4, 2, 2)
spam_matern_params = list(mod = mod,
                         evar = evar,
                         fvar = fvar,
                         r = r,
                         par3 = par3,
                         psill = psill)
spam_matern = vector("list", length(evar) * length(mod))
count = 0
for (i in 1:length(mod)) {
        for (j in 1:length(evar)) {
                count = count + 1
                spam_matern[[count]] = 
                        spam::covmat(d,
                                     theta = c(r[j], psill[j], par3[j], (evar[j] + fvar[j])), 
                                     type = mod[i])
        }
}

# save output
fpath = system.file("testdata",  package = "gear")
fname = paste(fpath, "/cmod_std_data.rda", sep = "")
save(n, coords, d, geoR_cov, spam_cov,
     spam_matern,
     geoR_cov_params,
     spam_cov_params,
     spam_matern_params,
     compress = "bzip2",
     file = fname,
     version = 2)
