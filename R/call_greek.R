call.greek = function(greek = c('delta', 'gamma', 'theta', 'vega', 'rho'), s, k, t, sd, r, d = 0){
  if(!is.character(greek)){
    print('unidentified greek. greek argument must be characher strings')
  }
  else if(greek == 'delta'){
    call.delta(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
  else if(greek == 'gamma'){
    call.gamma(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
  else if(greek == 'theta'){
    call.theta(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
  else if(greek == 'vega'){
    call.vega(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
  else if(greek == 'rho'){
    call.rho(s = s, k = k, t = t, sd = sd, r = r, d = d)
  }
}
