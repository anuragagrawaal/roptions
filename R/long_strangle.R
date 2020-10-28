strangle.long = function(c, p, k_call, k_put, ulimit = 10, llimit = 10){
  stock_price_at_expiration = round((k_call - llimit)):round((ulimit + k_call))
  call_option = (map_dbl(round((k_call - llimit)):round((ulimit + k_call)), .f = ~max(.x - k_call,0))) - c
  put_option = (map_dbl(round((k_call  - llimit)):round((ulimit + k_call)), .f = ~max(k_put - .x,0))) - p
  profit_loss = call_option + put_option
  df = data.frame(stock_price_at_expiration, call_option, put_option, profit_loss)
  p1 = ggplot(data = df) +
    geom_line(aes(x = stock_price_at_expiration, y = call_option, colour = 'call_option')) +
    geom_line(aes(x = stock_price_at_expiration, y = put_option, colour = 'put_option')) +
    geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
    labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Long Straddle Plot', color = 'Option contract') +
    scale_colour_manual('', breaks = c('call_option', 'put_option', 'profit_loss'), values = c('blue', 'red', 'black'))
  print(ggplotly(p1))
  print(df)
}
