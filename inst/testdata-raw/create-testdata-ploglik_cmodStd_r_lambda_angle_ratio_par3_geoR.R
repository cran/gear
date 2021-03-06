set.seed(120)
rlarp_data = geoR::grf(50, cov.pars = c(1, 1), nugget = 0.1,
              messages = FALSE)
y = rlarp_data$data
x = matrix(1, nrow = length(y))
coords = rlarp_data$coords
d = ganiso_d(rlarp_data$coords, coords2 = rlarp_data$coords, 
             invert = TRUE)
geoR_ml = geoR::likfit(rlarp_data, ini = c(1, 1), 
                       cov.model = "matern",
                       nugget = 0.1, kappa = 1,
                       psiA = pi/8, psiR = 1.5, 
                       fix.nug = FALSE, 
                       fix.psiA = FALSE,
                       fix.psiR = FALSE,
                       fix.kappa = FALSE,
                       messages = FALSE)
geoR_reml = geoR::likfit(rlarp_data, ini = c(1, 1), 
                         cov.model = "matern",
                         nugget = 0.1, kappa = 1,
                         psiA = pi/8, psiR = 1.5, 
                         fix.nug = FALSE, 
                         fix.psiA = FALSE, 
                         fix.psiR = FALSE,
                         fix.kappa = FALSE, 
                         messages = FALSE,
                         lik.method = "REML")
# geoR_ml$info.minimisation.function$par
# geoR_reml$info.minimisation.function$par

aniso_coords = geoR::coords.aniso(coords, aniso.pars = geoR_reml$aniso.pars)
rlarp_data_noaniso = rlarp_data
rlarp_data_noaniso$coords = aniso_coords
geoR_reml_noaniso = geoR::likfit(rlarp_data_noaniso, ini = c(1, 1),
                                 cov.model = "matern", kappa = 1,
                                 nugget = 0.1, fix.nug = TRUE,
                                 fix.kappa = FALSE,
                                 messages = FALSE,
                                 lik.method = "REML")

geoR_ml_loglik = geoR::loglik.GRF(rlarp_data, obj.model = geoR_ml)
geoR_reml_loglik = geoR::loglik.GRF(rlarp_data, obj.model = geoR_reml)
geoR_reml_noaniso_loglik = geoR::loglik.GRF(rlarp_data_noaniso, obj.model = geoR_reml_noaniso)

kc_ml = geoR::krige.control(obj.model = geoR_ml)
geoR_out = geoR::output.control(messages = FALSE)
geoR_ml_beta = geoR::krige.conv(rlarp_data, krige = kc_ml,
                                locations = cbind(1, 1),
                                output = geoR_out)$beta.est
kc_reml = geoR::krige.control(obj.model = geoR_reml)
geoR_reml_beta = geoR::krige.conv(rlarp_data, krige = kc_reml,
                                  locations = cbind(1, 1),
                                  output = geoR_out)$beta.est


# save output
fpath = system.file("testdata",  package = "gear")
fname = paste(fpath, "/ploglik_cmodStd_r_lambda_angle_ratio_par3.rda", sep = "")
save(x, y, d, coords, geoR_ml, geoR_reml,
     geoR_ml_loglik, geoR_reml_loglik, geoR_reml_noaniso_loglik,
     geoR_ml_beta, geoR_reml_beta,
     compress = "bzip2",
     file = fname,
     version = 2)
