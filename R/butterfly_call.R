function(k1, k2, k3, c1, c2, c3, spread = c('long', 'short'), llimit = 20, ulimit = 20){

  if(!is.character(spread)){
    print('spread argument must be a character string')
  } else if(spread == 'long'){

    stock_price_at_expiration = round((k1 - llimit)):round((ulimit + k1))
    long_call1 = (map_dbl(stock_price_at_expiration, .f = ~max(.x - k1,0))) - c1
    short_call1 = (-1* map_dbl(stock_price_at_expiration, .f = ~max(.x - k2,0))) + c2
    short_call2 = short_call1
    long_call2 = (map_dbl(stock_price_at_expiration, .f = ~max(.x - k3,0))) - c3
    profit_loss = long_call2 + long_call1 + short_call1 + short_call2
    df = data.frame(stock_price_at_expiration, long_call1, short_call1,  short_call2, long_call2, profit_loss)
    p1 = ggplot(data = df) +
      geom_line(aes(x = stock_price_at_expiration, y = long_call1, colour = 'long_call1')) +
      geom_line(aes(x = stock_price_at_expiration, y = short_call1, colour = 'short_call1')) +
      geom_line(aes(x = stock_price_at_expiration, y = short_call2, colour = 'short_call2')) +
      geom_line(aes(x = stock_price_at_expiration, y = long_call2, colour = 'long_call2')) +
      geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
      labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Long Butterfly Plot', color = 'Option contract') +
      scale_colour_manual('', breaks = c('long_call1', 'short_call1','short_call2', 'long_call2', 'profit_loss'), values = c('blue', 'red', 'yellow', 'purple', 'black'))
    print(df)
    print(p1)

  } else if(spread == 'short'){



    stock_price_at_expiration = round((k1 - llimit)):round((ulimit + k1))
    long_call1 = (map_dbl(stock_price_at_expiration, .f = ~max(.x - k2,0))) - c2
    short_call1 = (-1* map_dbl(stock_price_at_expiration, .f = ~max(.x - k1,0))) + c1
    short_call2 = (-1* map_dbl(stock_price_at_expiration, .f = ~max(.x - k3,0))) + c3
    long_call2 = long_call1
    profit_loss = long_call2 + long_call1 + short_call1 + short_call2
    df2 = data.frame(stock_price_at_expiration, short_call1, long_call1, long_call2,  short_call2, profit_loss)
    p2 = ggplot(data = df2) +
      geom_line(aes(x = stock_price_at_expiration, y = long_call1, colour = 'long_call1')) +
      geom_line(aes(x = stock_price_at_expiration, y = short_call1, colour = 'short_call1')) +
      geom_line(aes(x = stock_price_at_expiration, y = short_call2, colour = 'short_call2')) +
      geom_line(aes(x = stock_price_at_expiration, y = long_call2, colour = 'long_call2')) +
      geom_line(aes(x = stock_price_at_expiration, y = profit_loss, colour = 'profit_loss')) +
      labs(x = 'stock price at expiration', y = 'profit/loss', title = 'Long Butterfly Plot', color = 'Option contract') +
      scale_colour_manual('', breaks = c('long_call1', 'short_call1','short_call2', 'long_call2', 'profit_loss'), values = c('blue', 'red', 'yellow', 'purple', 'black'))
    print(df2)
    print(ggplotly(p2))

  }

}
