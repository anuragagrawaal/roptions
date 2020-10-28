put.rho = function(s, k, t, sd, r, d = 0){
  d1 = (log(s/k) + (r - d + (sd^2)/2) * t) / (sd * sqrt(t))
  d2 = d1 - (sd * sqrt(t))
  nd1 = pnorm(d1)
  nd2 = pnorm(d2)
  Rho = -1 * (k * t * exp(-1 * r * t) * pnorm(-d2))
  data.frame(put.rho = Rho)
}
