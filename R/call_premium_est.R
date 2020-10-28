call.premium.est = function(s, k, t, sd, r, d = 0){
  d1 = (log(s/k) + (r - d + (sd^2)/2) * t) / (sd * sqrt(t))
  d2 = d1 - (sd * sqrt(t))
  c = (s * exp(-d * t) * pnorm(d1)) - (k * exp((-r * t)) * pnorm(d2))
  data.frame(call.premium.est = c)
}
