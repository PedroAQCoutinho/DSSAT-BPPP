#function
soil_hydraulics <- function(sand, clay, soc, DF = 1, gravel = 0, digits = 4, 
                            PAW = TRUE) {
  if((sand > 1) | (clay > 1)) {
    stop("Sand & clay must be fractions, soc a percentage", call. = FALSE)
  }
  if((clay > 0.6) | soc > 8) {
    warning(paste("Validity of results questionable for sand fractions > 0.8", 
                  "or SOC percentage > 8"))
  }
  
  # pedotransfer functions
  wp <- wilt_point(sand, clay, soc) # Wilting point
  fc <- field_cap(sand, clay, soc) # Field capacity, no density effects
  fcdf <- field_cap_df(sand, clay, soc, DF) # Field capacity, w/density 
  thetas <- theta_s(sand, clay, soc) # Satured moisture content, no density
  thetasdf <- theta_sdf(sand, clay, soc, DF) # Satured moisture content, density 
  bd <- bdens(thetas, DF, gravel) # Bulk density
  ks <- ksat(sand, clay, soc, DF, gravel) # KSat, w/density and gravel
  
  # output
  out <- c("fc" = fcdf, "wp" = wp, "sat" = thetasdf, "bd" = bd, "ksat" = ks)
  if(PAW == TRUE) { 
    rodf <- ro_df(thetas, DF)
    gravel_pctv <- ((rodf / 2.65 ) * gravel) / (1 - gravel * ( 1 - rodf / 2.65))
    PAW <- (fcdf - wp) * (1 - gravel_pctv)
    out <- c(out, "PAW" = PAW)
  } 
  return(round(out, digits))
} 

