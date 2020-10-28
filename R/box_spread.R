box.spread = function(k_long_call, k_short_call, k_long_put, k_short_put, c1, c2, p1, p2, llimit = 20, ulimit = 20){
  stock_price_at_expiration = round((k_long_call - llimit)):round((ulimit + k_long_call))
  long_call = (map_dbl(stock_price_at_expiration , .f = ~max(.x - k_long_call,0))) - c1
  short_call =  (-1* map_dbl(stock_price_at_expiration, .f = ~max(.x - k_short_call,0))) + c2
  long_put = (map_dbl(stock_price_at_expiration, .f = ~max(k_long_put - .x,0))) - p1
  short_put = put_option = ( -1 * map_dbl(stock_price_at_expiration, .f = ~max(k_short_put - .x,0))) + p2
  profit_loss = long_call + short_call + long_put + short_put
  df = data.frame(stock_price_at_expiration, long_call, short_call, long_put, short_put, profit_loss)
  p1 = ggplot(data = df) +
    geom_line(aes(x = stock_price_at_expiration, y = long_call, colour = 'long_call')) +
    geom_line(aes(x = stock_price_at_expiration, y = short_call, colour = 'short_call')) +
    geom_line(aes(x = stock_price_at_expiration, y = long_put, colour = 'long_put')) +
    geom_line(aes(x = stock_price_at_expiration, y = short_put, colour = 'short_put')) +
    labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Box Spread Plot', color = 'Option contract') +
    scale_colour_manual('', breaks = c('long_call', 'short_call', 'profit_loss', 'long_put', 'short_put'), values = c('blue', 'red', 'black', 'green', 'yellow'))
  print(df)
  print(ggplotly(p1))
}
