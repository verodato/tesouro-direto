#! /usr/bin/Rscript

# read last updated tesouro_last.json file and collect rates, prices

#
# securities codes and names dict
#

updatedData <- function(){
  
  secl <- list()
  for( i in 1:length(secnames)){
    name <- secnames[ i ]
    code <- seccodes[ i ]
    secl[[ name ]] <- code
  }
  
  #
  # read last updated file
  #
  
  r <- fromJSON(file = paste0(tesouro_last_dir, "tesouro_last.json"))
  
  # loop securities names / expiry dates and collect rates, prices
  
  ptab <- data.table()
  rtab <- data.table()
  for( name in names(r) ){
    if( name == 'lastUpdated' ){next}
    e <- r[[ name ]]
    for( venc in sort(names(e)) ){
      v <- paste( secl[[ name ]], substr(venc,9,10),substr(venc,6,7),substr(venc,3,4), sep='' )
      print(v)
      f <- e[[venc]]
      rate <- f[[1]][[2]]
      side <- 'sell'
      valor_minimo <- f[[1]][[3]]
      if( is.null(valor_minimo) ){
        side <- 'buy'
      }
      price <- f[[1]][[4]]
      # combine prices
      s <- data.table(price)
      names(s) <- v
      if ( length(ptab) > 0 ) {
        ptab <- cbind(s,ptab)
      }else{
        ptab <- s
      }
      # combine rates
      s <- data.table(rate)
      names(s) <- v
      if ( length(rtab) > 0 ) {
        rtab <- cbind(s,rtab)
      }else{
        rtab <- s
      }
    }
  }
  l <- list()
  l[[1]] <- rtab
  l[[2]] <- ptab
  return(l)
}

rtab <- updatedData()[[1]]
ptab <- updatedData()[[2]]

#
# prepare data to bind to long series and plot
#

prepare <- function( kw, tb ){
  # tb <- rtab
  # select securities columns by regexp
  temp <- tb[, .SD, .SDcols = names(tb) %like% kw]
  # retun null data.table if security not found
  if ( length( temp ) == 0 ) { return( temp ) }
  # get the expiry year to order columns
  temp1 <- data.table(c1=names(temp),c2=substr(names(temp),10,12) )
  myorder <- temp1[order(c2)][,c1]
  # use myorder to order temp
  setcolorder(temp, myorder) 
  
  r <- fromJSON( file = paste(tesouro_last_dir, "tesouro_last.json") )
  
  # put date into rtab
  dt <- r[['lastUpdated']]
  dia <- substr( dt, 1, 2 ) 
  mes <- substr( dt, 3, 4 ) 
  ano <- substr( dt, 5, 8 ) 
  dt <- as.Date( paste( ano, mes, dia, sep="-") )
  temp1 <- data.table(date=dt,temp)
  return( temp1 )
}
