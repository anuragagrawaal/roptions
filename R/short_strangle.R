strangle.short = function(c, p, k_call, k_put = k1, ulimit = 10, llimit = 10){
  stock_price_at_expiration = round((k_call - llimit)):round((ulimit + k_call))
  call_option = (-1* map_dbl(round((k_call - llimit)):round((ulimit + k_call)), .f = ~max(.x - k_call,0))) + c
  put_option = ( -1 * map_dbl(round((k_call  - llimit)):round((ulimit + k_call)), .f = ~max(k_put - .x,0))) + p
  profit_loss = call_option + put_option
  df = data.frame(stock_price_at_expiration, call_option, put_option, profit_loss)
  p2 = ggplot(data = df) +
    geom_line(aes(x = stock_price_at_expiration, y = call_option, colour = 'call_option')) +
    geom_line(aes(x = stock_price_at_expiration, y = put_option, colour = 'put_option')) +
    geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
    labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Short Straddle Plot', color = 'Option contract') +
    scale_colour_manual('', breaks = c('call_option', 'put_option', 'profit_loss'), values = c('blue', 'red', 'black'))
  print(ggplotly(p2))
  print(df)
}
