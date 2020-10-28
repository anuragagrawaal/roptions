call.gamma = function(s, k, t, sd, r, d = 0){
  d1 = (log(s/k) + (r - d + (sd^2)/2) * t) / (sd * sqrt(t))
  d2 = d1 - (sd * sqrt(t))
  nd1 = pnorm(d1)
  nd2 = pnorm(d2)
  Gamma = ((1/sqrt(2 * pi)) * exp(-1 * (((d1 ^ 2)/2))))/ (s * sd * sqrt(t))
  data.frame(Gamma = Gamma)
}
