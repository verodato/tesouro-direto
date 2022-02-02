#! /usr/bin/Rscript

library(XLConnect)
library(data.table)
library(bizdays)
library("RColorBrewer")
library("rjson")

# load directory settings
source('/home/sander/dados/tesouro_direto/settings.R')

path <- paste0(data_dir, 'png/')
pathp <- data_dir

# list of securities with buy offers?
e <- c(
  'NTN-B150830', 'NTN-B150840', 'NTN-B150555',
  'NTN-BPrinc150526','NTN-BPrinc150535','NTN-BPrinc150545',
  'NTN-F010131',
  'LTN010123','LTN010126',
  'NTN-C010125'
)

tobuy <- list()
for( i in 1:length(e)){
  asset <- e[ i ]
  tobuy[[ asset ]] <- 0
}

# colors
#
#display.brewer.pal(n = 5, name = 'Spectral')
# Hexadecimal color specification 
col <- rev(brewer.pal(n = 3, name = "Spectral"))
#col2 <- rev(brewer.pal(n = 5, name = "Spectral"))[c(1,5)]
#col3 <- rev(brewer.pal(n = 5, name = "Spectral"))[c(1,2,5)]

#
# marker each 1 months
#

makelabels1m <- function( aux ){
  memo <- aux[1,date]
  dts <- aux[1,date]
  for(i in 2:length(aux[,date])){
    if( month( aux[i,date] ) == month( memo ) ){
      e <- NA
    }else{
      e <- aux[i,date]
      memo <- e
    } 
    dts <- c(dts,e)
  }
  dts <- format(dts, format="%Y-%m")
  return(dts)
}

#
# marker each 3 months
#

makelabels3m <- function( aux ){
  memo <- aux[1,date]
  dts <- aux[1,date]
  for(i in 2:length(aux[,date])){
    trigdt <- month( memo ) + 2
    if( month( aux[i,date] ) <= trigdt ){
      e <- NA
    }else{
      e <- aux[i,date]
      memo <- e
    } 
    dts <- c(dts,e)
  }
  dts <- format(dts, format="%Y-%m")
  return(dts)
}

#
# long term chart
#

mychart <- function( e, title, ylim, ylab ){
  lb <- makelabels( e, title )
  e[,date := NULL]
  col <- rev(brewer.pal(n = length( e ), name = "Spectral"))
  # save to local file
  png(paste(path,title,".png",sep=""),width=1000,height=600)
  par(xpd=T,mar=c(10,4,3,4))
  matplot( 
    e, type = 'l', bty='n', lty = 1, main='', xaxt='n',
    ylab=ylab, col=col,ylim=ylim
  )
  # x axis
  lines( c(-200, length(lb)), c( ylim[1], ylim[1] ) )
  # tick marks on x axis
  tickhi <- (ylim[2]-ylim[1])/100
  xlabp <- ylim[1] - ((ylim[2]-ylim[1])/20)
  for( i in 1:length(lb)){
    l <- lb[ i ]
    if( !is.na( l ) ){
      lines( c( i, i ), c(ylim[1],ylim[1]+tickhi) )
      text( i, xlabp, l )
    }
  }
  dev.off()
}

#
# standard legend
#

starleg <- function(x,y,e,col,lbs){
  r <- legend(
    x,y, box.col='white', box.lwd = 10, lbs,
    cex=1, bg='white', col=col, #lty=1,,pch=16
    y.intersp = 1.3, x.intersp = 1 #pt.cex =1 # text.width = 1 #fil=rev(col)
  )
  # draw lines and points inside legend box
  x1 <- r$rect$left
  dx <- r$rect$w/6
  x2 <- x1 + dx
  xc <- x1 + (dx/2)
  for( i in 1:length(r$text$x)){
    py <- r$text$y[ i ]
    lines( c( x1, x2 ), c( py, py ), col = col[ i ], lwd=1 )
    pch=1
    if( lbs[ i ] %in% names(tobuy) ){
      pch=16
    }
    points( xc, py, pch=pch, col=col[ i ])
  }
}


starlegPrice1 <- function(x,y,e,col){
  legend(
    x,y, box.col='white', box.lwd = 10, names(e),
    cex=1,lty=1, bg='white',pch=16, col=col, 
    y.intersp = 1.3, x.intersp = 1 #pt.cex =1 # text.width = 1 #fil=rev(col)
  )
}

#  
# x axis with special labels
#

xaxisCP <- function( e, lb, ylim ){
  p <- ylim[1]
  lines(c(0,nrow(e)),c( p, p ))
  # ticks and labels positions
  d <- ylim[2]-ylim[1]
  offset <- d/100
  offset1 <- d/15
  lbs <- c()
  # loop labels vector
  for( i in 1:length( lb )){
    # draw tick marks if not NA
    if( !is.na( lb[ i ] )){
      lines( c( i, i ), c( p-offset, p+offset ) )
      lbs <- c( lbs, lb[ i ] )
    }
  }
  # colect a vector of all labels not NA and loop it
  n <- which( !is.na( lb ) )
  for( i in 1:length(n) ){
    # first element + half the difference between consecutive elements
    x <- n[ i ] + ((n[ i+1 ] - n[ i ])/2)
    # place labels in the middle of each month
    text( x, p-offset1, lbs[ i ], srt=-90 )
  }
  #for( i in 2:length(n) ){
    # first element + half the difference between consecutive elements
    #x <- n[ i-1 ] + ((n[ i ] - n[ i-1 ])/2)
    # place labels in the middle of each month
    #text( x, p-offset1, lbs[i-1] )
  #}
}

#
# function: chart CP curto prazo
#
# e <- ntnb

myCPchart <- function( e, title, ylim, ylab, xleg, yleg, toFile ){
  lb <- makelabels1m( e )
  s <- data.table(as.data.frame(e)[,c(2:ncol(e))])
  # colors
  if( ncol( s ) == 2 ){
    col <- rev(brewer.pal(n = 5, name = "Spectral"))[c(2,4)]
  }else if( ncol( s ) == 3 ){
    col <- rev(brewer.pal(n = 5, name = "Spectral"))[c(1,2,5)]
  }else{
    col <- rev(brewer.pal(n = ncol( s ), name = "Spectral"))
  }
  # save chart to file
  if(toFile){
    png(paste(paste0(tesouro_last_dir,'png/'),title,".png",sep=""),width=600,height=600)
  }
  par(xpd=T,mar=c(5,4,4,3))
  matplot( 
    s, type = 'n', pch=16, bty='n', lty = 1, xaxt='n', yaxt='n',
    ylab=ylab, col=col, ylim=ylim
  )
  # draw plot lines
  for( j in 1:ncol(s)){
    security_name <- names(s)[ j ]
    if( security_name %in% names(tobuy) ){
      lines( as.data.frame(s)[, j ], col = col[ j ], pch=16,type='o' )
    }else{
      lines( as.data.frame(s)[, j ], col = col[ j ], pch=1, type='o' )
    }
  }
  # shadows
  #NonNAindex <- which(!is.na(lb))
  #x1 <- NonNAindex[2]
  #x2 <- NonNAindex[3]
  #rect(x1,ylim[1],x2,ylim[2],col = rgb(0.5,0.5,0.5,0.1), border=NA)
  #x1 <- NonNAindex[5]
  #x2 <- NonNAindex[6]
  #rect(x1,ylim[1],x2,ylim[2],col = rgb(0.5,0.5,0.5,0.1), border=NA)
  # y axis
  axis(
    side=2, las=1, cex.axis=1, col = NA, col.ticks = NA
  )
  # x axis
  xaxisCP( s, lb, ylim )
  # legend
  if( grepl( 'LTN_cp_prices|NTN-BPrinc_cp_prices|NTN-C_prices', title ) ){
    lbs <- names(s)
    cores <- col
  }else{
    lbs <- rev(names(s))
    cores <- rev(col)
  }
  starleg(xleg, yleg, s, cores, lbs)
  #
  if(toFile){
    dev.off()
  }
  return(col)
}

# make chart labels beautiful

makelabels <- function( aux, title ){
  # only first day of each year is not NA
  memo <- aux[1,date]
  dts <- aux[1,date]
  for(i in 2:length(aux[,date])){
    if( year( aux[i,date] ) == year( memo ) ){
      e <- NA
    }else{
      e <- aux[i,date]
      memo <- e
    } 
    dts <- c(dts,e)
  }
  # format date to show only year
  dts <- format(dts, format="%Y")
  
  # index the labels vector
  s <- data.table( 
    id=seq(1,by=1,length=length(dts)),
    dts=dts
  )
  # select only no NA labels
  years <- s[!is.na(dts)]
  print('years')
  print(years)
  # reduce the number of labels by taking down some of them
  k <- nrow(years)/2
  if( grepl( 'NTN-F|NTN-BPrinc', title ) ){
    cr <-  rep(c(1,0,0),k) # criteria to turn elements to NA 
  }else{
    cr <-  c( 0, rep(c(1,0,0),k) ) # criteria to turn elements to NA  
  }
  years <- cbind(years, cr = cr[0:nrow(years)])
  years[,dts1 := ifelse(cr == 0, NA, dts)]
  # get the result back to the s data.table
  setkey(s,id)
  setkey(years,id)
  s <- years[s]
  return(s[,dts1])
}


# remove rows with repeated date

nodaterep <- function( e ){
  s <- data.table()
  memo <- list()
  dt <- as.character(e[ 1, date])
  memo[[ dt ]] <- 0
  for( i in 2:nrow(e) ){
    dt <- as.character(e[ i, date ])
    # only add row to s if date is new
    if(!( dt %in% names(memo) )){
      memo[[ dt ]] <- 0
      rol <- e[ i, ]
      s <- rbind( s, rol )
    }
  }
  return(s)
}

# rates: organize all durations in the same data table to make chart
#n <- "LTN011003"
#n <- "LTN070104"
#n <- "LTN010404"

alldur <- function( kw ){
  # loop instrument list ordered by expiration date
  i <- 1
  for( n in dl[,names] ){
    # if instrument is matched against a keyword
    if( grepl( kw, n )){
      # load instrument data into 'e'
      e <- l[[ n ]]
      # check if null
      if( is.null( e ) ){
        print(paste(n, 'is NULL...'))
      }else{
        #print(n)
        #print(l[[ n ]])
        # remove duplicated dates
        e <- nodaterep(e)
        # key by date and merge instrument
        setkey(e,date)
        if( i == 1 ){
          aux <- e
        }else{
          aux <- merge( aux, e, all=T )
        }
        setkey(aux,date)
        i <- i + 1
      }
    }
  }
  # save to file
  write.table( aux, file=paste(path, kw,'_rates.tsv',sep=''), sep='\t', row.names = F )
  return(aux)
}

# prices: organize all durations in the same data table to make chart

allpri <- function( kw ){
  # loop each instrument & duration
  i <- 1
  for( n in dlp[, names ] ){
    # search for a keyword
    if( grepl( kw, n )){
      e <- lp[[ n ]]
      # remove duplicated dates
      e <- nodaterep(e)
      setkey(e,date)
      if( i == 1 ){
        aux <- e
      }else{
        aux <- merge( aux, e, all=T )
      }
      #print(aux)
      i <- i + 1
    }
  }
  # save to file
  write.table( aux, file=paste(path, kw,'_prices.tsv',sep=''), sep='\t', row.names = F )
  
  return(aux)
}


# select only columns with last observation not NA

colnotNA <- function( dt ){
  # dt <- alldur('LTN')[date > refDate,]
  # lets work with data frame
  e <- as.data.frame(dt)
  # loop columns and select by criteria not NA
  s <- data.frame(e[,1])
  n <- c('date')
  for(j in 2:ncol(e)){
    if( !is.na( e[nrow(e),j] ) ){
      s <- cbind(s, e[,j])
      n <- c(n,names(e)[j])
    }
  }
  # back to data table
  s1 <- as.data.table(s)
  names(s1) <- n
  return(s1)
}

