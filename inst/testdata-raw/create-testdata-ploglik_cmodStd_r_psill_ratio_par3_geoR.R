set.seed(6)
rpap_data = geoR::grf(50, cov.pars = c(1, 1), nugget = 0.1,
              messages = FALSE)
y = rpap_data$data
x = matrix(1, nrow = length(y))
coords = rpap_data$coords
d = ganiso_d(rpap_data$coords, coords2 = rpap_data$coords, 
             invert = TRUE)
geoR_ml = geoR::likfit(rpap_data, ini = c(1, 1), 
                       cov.model = "matern",
                       nugget = 0.1, kappa = 1,
                       psiA = pi/8, psiR = 1.5, 
                       fix.nug = TRUE, 
                       fix.kappa = FALSE,
                       fix.psiA = TRUE,
                       fix.psiR = FALSE,
                       messages = FALSE)
geoR_reml = geoR::likfit(rpap_data, ini = c(1, 1), 
                         cov.model = "matern",
                         nugget = 0.1, kappa = 1,
                         psiA = pi/8, psiR = 1.5, 
                         fix.nug = TRUE, 
                         fix.kappa = FALSE, 
                         fix.psiA = TRUE,
                         fix.psiR = FALSE, 
                         messages = FALSE, lik.method = "REML")
# # geoR_ml$info.minimisation.function$par
# # geoR_reml$info.minimisation.function$par

aniso_coords = geoR::coords.aniso(coords, aniso.pars = geoR_reml$aniso.pars)
rpap_data_noaniso = rpap_data
rpap_data_noaniso$coords = aniso_coords
geoR_reml_noaniso = geoR::likfit(rpap_data_noaniso, ini = c(1, 1),
                                 cov.model = "matern", kappa = 1,
                                 nugget = 0.1, fix.nug = TRUE,
                                 fix.kappa = FALSE,
                                 messages = FALSE,
                                 lik.method = "REML")

geoR_ml_loglik = geoR::loglik.GRF(rpap_data, obj.model = geoR_ml)
geoR_reml_loglik = geoR::loglik.GRF(rpap_data, obj.model = geoR_reml)
geoR_reml_noaniso_loglik = geoR::loglik.GRF(rpap_data_noaniso, obj.model = geoR_reml_noaniso)

# save output
fpath = system.file("testdata",  package = "gear")
fname = paste(fpath, "/ploglik_cmodStd_r_psill_ratio_par3.rda", sep = "")
save(x, y, d, coords, geoR_ml, geoR_reml,
     geoR_ml_loglik, geoR_reml_loglik, geoR_reml_noaniso_loglik,
     compress = "bzip2",
     file = fname,
     version = 2)
