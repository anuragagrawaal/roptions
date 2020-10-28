call.spread = function(k1, k2, c1, c2, llimit = 20, ulimit = 20){
  if(k1<k2){
    print('This is a Bull Call Spread because excercise price of long call (k1) is less than excercise price of short call (k2)')
  }else{'This is a Bear Call Spread because excercise price of long call (k1) is greater than excercise price of short call (k2)'}
  stock_price_at_expiration = round((k1 - llimit)):round((ulimit + k1))
  long_call = (map_dbl(round((k1 - llimit)):round((ulimit + k1)), .f = ~max(.x - k1,0))) - c1
  short_call = (-1* map_dbl(round((k1 - llimit)):round((ulimit + k1)), .f = ~max(.x - k2,0))) + c2
  profit_loss = long_call + short_call
  df = data.frame(stock_price_at_expiration, long_call, short_call, profit_loss)
  p1 = ggplot(data = df) +
    geom_line(aes(x = stock_price_at_expiration, y = long_call, colour = 'long_call')) +
    geom_line(aes(x = stock_price_at_expiration, y = short_call, colour = 'short_call')) +
    geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
    labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Bull/Bear Call Spread Plot', color = 'Option contract') +
    scale_colour_manual('', breaks = c('long_call', 'short_call', 'profit_loss'), values = c('blue', 'red', 'black'))
  print(df)
  print(ggplotly(p1))
}
