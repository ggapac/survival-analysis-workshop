bootstrap <- function(x, b=1000) {
    set.seed(0)
    boot_samples <- replicate(b, sample(x, replace = T))
    boot_distr <- apply(boot_samples, 2, mean)
    
    boot_distr
}

boot_perc_ci <- function(boot_distr, alpha=0.025) {
    boot_distr <- sort(boot_distr)
    b <- length(boot_distr)
    
    list("CIlow" = boot_distr[b * alpha],
         "CIupp" = boot_distr[b * (1 - alpha)])
}