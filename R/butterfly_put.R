butterfly.put = function(k1, k2, k3, p1, p2, p3, spread = c('long', 'short'), llimit = 20, ulimit = 20){

  if(!is.character(spread)){
    print('spread argument must be a character string')
  } else if(spread == 'long'){

  stock_price_at_expiration = round((k1 - llimit)):round((ulimit + k1))
  long_put1 = (map_dbl(stock_price_at_expiration , .f = ~max(k1 - .x,0))) - p1
  short_put1 = ( -1 * map_dbl(stock_price_at_expiration, .f = ~max(k2 - .x,0))) + p2
  short_put2 = short_put1
  long_put2 = (map_dbl(stock_price_at_expiration, .f = ~max(k3 - .x,0))) - p3
  profit_loss = long_put1 + short_put1 + short_put2 + long_put2
  df = data.frame(stock_price_at_expiration, long_put1, short_put1, short_put2, long_put2,  profit_loss)
  p1 = ggplot(data = df) +
    geom_line(aes(x = stock_price_at_expiration, y = long_put1, colour = 'long_put1')) +
    geom_line(aes(x = stock_price_at_expiration, y = short_put1, colour = 'short_put1')) +
    geom_line(aes(x = stock_price_at_expiration, y = short_put2, colour = 'short_put2')) +
    geom_line(aes(x = stock_price_at_expiration, y = long_put2, colour = 'long_put2')) +
    geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
    labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Long Butterfly Put Spread Plot', color = 'Option contract') +
    scale_colour_manual('', breaks = c('long_put1', 'short_put1','short_put2', 'long_put2', 'profit_loss'), values = c('blue', 'red', 'yellow', 'purple', 'black'))
  print(df)
  print(ggplotly(p1))

  } else if(spread == 'short'){



    stock_price_at_expiration = round((k1 - llimit)):round((ulimit + k1))
    long_put1 = (map_dbl(stock_price_at_expiration , .f = ~max(k2 - .x,0))) - p2
    short_put1 = ( -1 * map_dbl(stock_price_at_expiration, .f = ~max(k1 - .x,0))) + p1
    short_put2 = ( -1 * map_dbl(stock_price_at_expiration, .f = ~max(k3 - .x,0))) + p3
    long_put2 = long_put1
    profit_loss = long_put1 + short_put1 + short_put2 + long_put2
    df = data.frame(stock_price_at_expiration,  short_put1,long_put1, long_put2, short_put2,  profit_loss)
    p1 = ggplot(data = df) +
      geom_line(aes(x = stock_price_at_expiration, y = long_put1, colour = 'long_put1')) +
      geom_line(aes(x = stock_price_at_expiration, y = short_put1, colour = 'short_put1')) +
      geom_line(aes(x = stock_price_at_expiration, y = short_put2, colour = 'short_put2')) +
      geom_line(aes(x = stock_price_at_expiration, y = long_put2, colour = 'long_put2')) +
      geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
      labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Short Butterfly Put Spread Plot', color = 'Option contract') +
      scale_colour_manual('', breaks = c('long_put1', 'short_put1','short_put2', 'long_put2', 'profit_loss'), values = c('blue', 'red', 'yellow', 'purple', 'black'))
    print(df)
    print(ggplotly(p1))
  }

}
