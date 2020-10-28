put.greek = function(greek = c('delta', 'gamma', 'theta', 'vega', 'rho'), s, k, t, sd, r, d = 0){
  if(!is.character(greek)){
    print('unidentified greek. greek argument must be characher strings')
  }
  else if(greek == 'delta'){
    put.delta(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
  else if(greek == 'gamma'){
    put.gamma(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
  else if(greek == 'theta'){
    put.theta(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
  else if(greek == 'vega'){
    put.vega(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
  else if(greek == 'rho'){
    put.rho(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
}
