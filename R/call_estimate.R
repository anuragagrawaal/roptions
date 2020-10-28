call.estimate = function(s, k, t, sd, r, d = 0){
  d1 = (log(s/k) + (r - d + (sd^2)/2) * t) / (sd * sqrt(t))
  d2 = d1 - (sd * sqrt(t))
  nd1 = pnorm(d1)
  nd2 = pnorm(d2)
  c = (s *exp(-d * t)* pnorm(d1)) - (k * exp((-r * t)) * pnorm(d2))
  Delta = nd1
  Gamma = ((1/sqrt(2 * pi)) * exp(-1 * (((d1 ^ 2)/2))))/ (s * sd * sqrt(t))
  Theta = ((-1 * s * (1/(sqrt(2 * pi))) * exp(-1 * (d1^2/2)) * sd) / (2 * sqrt(t))) - (r * k * exp(-r * t) * nd2)
  Vega = (s * sqrt(t) * (1/sqrt(2 * pi)) * exp(-1 * ((d1 ^ 2)/2)))
  Rho = (k * t * exp(-1 * r * t) * nd2)
  intrinsic.value = max(s - k,0)
  speculative.premium = c - intrinsic.value
  df2 = data.frame(estimate = c('premium.est', 'd1', 'd2', 'n(d1)', 'n(d2)', 'intrinsic.value', 'speculative.premium', 'Delta', 'Gamma', 'Theta', 'Vega','Rho'), est.call.option = c(c, d1, d2, nd1, nd2,  intrinsic.value, speculative.premium, Delta, Gamma, Theta, Vega, Rho))
  print(df2)
}
