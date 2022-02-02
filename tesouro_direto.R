#! /usr/bin/Rscript

# Lê planilhas xls e carrega informações em listas. Organize data by year and type of bond
# Roda chartUtils.R e update.R

library(XLConnect)
library(data.table)
library(bizdays)
library("RColorBrewer")
library("rjson")

# load directory settings
source('/home/sander/dados/tesouro_direto/settings.R')

path <- paste0(data_dir, 'png/')
pathp <- data_dir
pathTsv <- paste0(data_dir, 'tsv/')

secnames <- c(
  "Tesouro IPCA+ com Juros Semestrais", 
  "Tesouro IPCA+",
  "Tesouro Selic",
  "Tesouro Prefixado",
  "Tesouro Prefixado com Juros Semestrais",
  "Tesouro IGPM+ com Juros Semestrais"
)

seccodes <- c(
  "NTN-B",
  "NTN-BPrinc",
  "LFT",
  "LTN",
  "NTN-F",
  "NTN-C"
)

source(paste( pathp, 'chartUtils.R', sep='' ))
source(paste( pathp, 'update.R', sep='' ))

#
# dict of securities: list
#

secdict <- list()
for( i in 1:length(secnames)){
  name <- secnames[ i ]
  code <- seccodes[ i ]
  secdict[[ code ]] <- name
}
  
#
# dict of securities: data.table
#

secdt <- data.table()
for( i in 1:length(secnames)){
  name <- secnames[ i ]
  code <- seccodes[ i ]
  linha <- data.table( code = code, name = name )
  secdt <- rbind( secdt, linha )
}
#setkey(secdt, code)


# clear empty and NA rows 

clearrows <- function( df ){
  clean <- data.frame()
  for(i in 1:nrow( df )){
    trig <- 0
    for(j in 2:ncol( df )){
      if( is.na(df[i,j]) | df[i,j] == '' ){
        next
      }else{
        trig <- 1
      }
    }
    if( trig == 1 ){
      clean <- rbind(clean, df[i,])
    }
  }
  return(clean)
}

# read all sheets and organize data by year and type of bond
# 'name': security code + expiry date

tesouro_direto_a <- function(arq,tp){
  
  ano_base <- as.numeric( gsub('^.*_|.xls','',arq) )
  #print(ano_base)
  l <- list()
  
  if( file.exists(arq) ){
    wb <- loadWorkbook(arq)
  }else{
    return(l)
  }
  
  sheets <- getSheets(wb)
  # loop of sheets
  for(s in sheets){
    name <- gsub(' ','',s)
    df <- readWorksheet(wb, sheet = s, header = F, startRow = 3)
    # handle exception
    if( ano_base == 2008 ){
      df <- readWorksheet(wb, sheet = s, header = F, startRow = 2)
      df <- df[2:nrow(df),]
    }
    # clear empty and NA rows
    df <- clearrows( df )
    dt <- data.table(df[,c(1:5)])
    names(dt) <- c('date','bid','ask','puc','puv')
    #
    # organize dates
    #
    if( ano_base == 2016 ){
      dt[,date := ifelse( date == '2016-08-23 00:00:00', '23/08/2016', date )]
    }
    if( ano_base == 2008 ){
      dt[,date := as.Date(date)]
    }
    if( class( dt[1,date] ) == 'character' ){
        dt[,date := as.Date(date,"%d/%m/%Y")]
    }else{
        dt[,date := as.Date(date)]
    }
    # taxa de compra e venda
    dt[,bid := as.numeric(gsub('%','',bid))]
    dt[,ask := as.numeric(gsub('%','',ask))]
    # preco unitario de compra e venda
    dt[,puc := as.numeric(puc)]
    dt[,puv := as.numeric(puv)]
    # sometimes percent data comes from excel divided by 100 
    if( dt[1,bid < 1] ){
      dt[,bid := 100*bid]
    }
    if( dt[1,ask < 1] ){
      dt[,ask := 100*ask]
    }
    # taxa: buy & sell average
    dt[,txm := (bid + ask)/2]
    # preco unitario: medias
    dt[,pum := (puc + puv)/2]
    #
    # create data structure for rates ou prices
    #
    if( tp == 't' ){
      tso <- dt[,list(date,txm)]
    }else if( tp == 'p' ){
      tso <- dt[,list(date,pum)]
    }else if( tp == 'all' ){
      tso <- dt
    }
    # harmonize sheet names
    if( grepl( 'NTNBP[0-9]', name ) ){
      name <- gsub( 'BP', '-BPrincipal', name )
    }
    if( grepl( 'NTNB[0-9]', name ) ){
      name <- gsub( 'B', '-B', name )
    }
    if( grepl( 'NTNC[0-9]', name ) ){
      name <- gsub( 'C', '-C', name )
    }
    if( grepl( 'NTNF[0-9]', name ) ){
      name <- gsub( 'F', '-F', name )
    }
    # name sheet and name list object if not 'all'
    if( tp != 'all' ){
      names(tso)[2] <- name 
    }
    # load info into a list
    l[[ name ]] <- tso
  }
  return(l)
}

# order lists by date of instrument expiration

orderByExpDate <- function(l){
  e <- names(l)
  # remove all but last 2 digits
  s <- gsub('.*(?=[0-9]{2})', '', e, perl = T)
  d <- data.table(id=s,names=e)[order(id)]
  return(d)
}

#
# construct list with selected info about bonds' interest and price
#
# l: list of security ticker & date of expiration (key) and the rates time series (value) data.table
# lp: same with prices instead of rates
# la: prices and rates

#mainFunc <- function(){
#periodo <- c(2002:2020)
current_year <- format(Sys.Date(), "%Y")
periodo <- c(2018:current_year)
l <- list()
lp <- list()
la <- list()
for(ano_base in periodo){
  # list of securities
  titulos <- c('LTN','NTN-B_Principal','NTN-B','NTN-C','NTN-F','LFT')
  for(titulo in titulos){
    # some bonds started later
    if( ano_base <= 2004 & titulo == 'NTN-B_Principal' ){next}
    if( ano_base <= 2003 & titulo == 'NTN-F' ){next}
    if( ano_base == 2002 & titulo == 'NTN-B' ){next}
    # construct file name
    if( ano_base >= 2012 ){
      xls <- paste(pathp,'xls/',titulo,'_',ano_base,'.xls',sep='')
    }else{
      xls <- paste(pathp,'xls/historico',gsub('_|-','',titulo),'_',ano_base,'.xls',sep='')
    }
    #
    # read sheet and load lists l and lp (keys: security code + expiry date, values: rate or price time series data.table)
    # choose 't' to get 'taxas' and 'p' to get 'prices'
    #
    # rates
    #
    e <- tesouro_direto_a(xls,'t')
    # stack tables
    for(n in names(e)){
      # organize
      if( n %in% names(l) ){
        l[[ n ]] <- rbind( l[[ n ]], e[[ n ]] )
      }else{
        l[[ n ]] <- e[[ n ]]
      }
    }
    #
    # prices 
    #
    e <- tesouro_direto_a(xls,'p')
    for(n in names(e)){
      # organize
      if( n %in% names(l) ){
        lp[[ n ]] <- rbind( lp[[ n ]], e[[ n ]] )
      }else{
        lp[[ n ]] <- e[[ n ]]
      }
    }
    #
    # all
    #
    e <- tesouro_direto_a(xls,'all')
    for(n in names(e)){
      # organize
      if( n %in% names(la) ){
        la[[ n ]] <- rbind( la[[ n ]], e[[ n ]] )
      }else{
        la[[ n ]] <- e[[ n ]]
      }
    }
  }
}

# order l and lp keys (security ticker & date of expiration) by expiration date. 
# that's the order we want to loop it. 

dl <- orderByExpDate(l)
dlp <- orderByExpDate(lp)

