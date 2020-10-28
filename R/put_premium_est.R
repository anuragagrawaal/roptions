put.premium.est = function(s, k, t, sd, r, d = 0){
  d1 = (log(s/k) + (r - d + (sd^2)/2) * t) / (sd * sqrt(t))
  d2 = d1 - (sd * sqrt(t))
  p = (k * exp((-r * t)) * pnorm(-d2)) - (s * exp(-d * t) * pnorm(-d1))
  data.frame(put.premium.est = p)
}
