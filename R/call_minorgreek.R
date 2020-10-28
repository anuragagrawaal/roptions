call.minorgreek = function(minorgreek = c('lambda', 'vomma'), s, k, t, sd, r, d = 0){
  d1 = (log(s/k) + (r - d + (sd^2)/2) * t) / (sd * sqrt(t))
  d2 = d1 - (sd * sqrt(t))
  lambda = pnorm(d1) * (s / ((s * exp(-d * t) * pnorm(d1)) - (k * exp((-r * t)) * pnorm(d2))))
  vomma = s * sqrt(t) * (exp(-d1^2/2)/ (sqrt(2 * pi)))
  if(!is.character(minorgreek)){
    print('minor greek is not a character string')
  }
  else if(minorgreek == 'lambda'){
    data.frame(lambda.call = lambda)
  }
  else if (minorgreek == 'vomma'){
    data.frame(vomma.option = vomma)
  }
}
