#! /usr/bin/Rscript

#options(java.home="/lib/jvm/java-1.11.0-openjdk-amd64/bin/java/")
library(XLConnect)
library(data.table)
library(bizdays)
library("RColorBrewer")

# load directory settings
source('/home/sander/dados/tesouro_direto/settings.R')

# roda tesouro_direto.R
source(paste0(data_dir, 'tesouro_direto.R'))

# save last day prices and rates
source(paste0(data_dir, 'lastDay.R'))

# print charts to file?
#defaul <- FALSE
defaul <- TRUE

# start date
#refDate <- as.Date('2019-01-01')
#refDate <- as.Date('2020-01-01')
refDate <- as.Date('2020-01-01')

#
# NTN-BPrinc: IPCA (sem juros)
#

kw <- 'NTN-BPrinc'
alldur( kw )

ntnbp <- colnotNA( alldur( kw )[date > refDate,] )
ntnbp[,'NTN-BPrinc150545' := NULL]
# updated rates
res <- prepare( kw, rtab )
#res[,'NTN-BPrinc150545' := NULL]
#ntnbp_up <- rbind( ntnbp, res )

# price
ntnbpp <- colnotNA( allpri( kw )[date > refDate,] )
ntnbpp[,'NTN-BPrinc150545' := NULL]
# updated prices
res <- prepare( kw, ptab )
#res[,'NTN-BPrinc150545' := NULL]
#ntnbpp_up <- rbind( ntnbpp, res )

# charts

myCPchart( ntnbp, 'NTN-BPrinc_cp_rates', c(0,6), '%', 35, 1.3, toFile=defaul )
myCPchart( ntnbpp, 'NTN-BPrinc_cp_prices', c(1000,3500), 'BRL', 120, 3500, toFile=defaul )
# remove bad behaved series from chart
#ntnbpp[,'NTN-BPrinc150545' := NULL]

#
# NTN-B: IPCA com juros semestrais
#

kw <- 'NTN-B[0-1]'

ntnb <- colnotNA( alldur( kw )[date > refDate,] )
ntnb <- ntnb[,1:7]
# updated rates
res <- prepare( kw, rtab )
#ntnb_up <- res[,1:7]
#ntnb_up <- rbind( ntnb, res[,1:7] )
  
ntnbpr <- colnotNA( allpri( kw )[date > refDate,] )
ntnbpr <- ntnbpr[,1:7]
# updated prices
res <- prepare( kw, ptab )
#ntnbup_up <- rbind( ntnbpr, res[,1:7] )

# charts
myCPchart( ntnb, 'NTN-B_cp_rates', c(0,6), '%', 49, 1.8, toFile=defaul )
myCPchart( ntnbpr, 'NTN-B_cp_prices', c(3500,5000), 'BRL', 60, 5000, toFile=defaul )

#
# LTN: fixed rate
#

kw <- 'LTN'

ltn_rate <- colnotNA( alldur( kw )[date > refDate,] )
# updated rates
res <- prepare( kw, rtab )
#ltn_rate_up <- rbind( ltn_rate, res )

ltn_price <- colnotNA( allpri( kw )[date > refDate,] )
# updated prices
res <- prepare( kw, ptab )
#ltn_price_up <- rbind( ltn_price, res )

# charts
myCPchart( ltn_rate, 'LTN_cp_rates', c(1,12), '%', 25, 3.5, toFile=defaul )
myCPchart( ltn_price, 'LTN_cp_prices', c(500,1000), 'BRL', 120, 650, toFile=defaul )
#ltn_price <- ltn_price[,list(date,LTN010125)]

#
# NTN-F: Tesouro Prefixado com Juros Semestrais
#

ntnf <- colnotNA( alldur('NTN-F')[date > refDate,] )
myCPchart( ntnf, 'NTN-F_cp_rates', c(1,12), '%', 10, 3.5, toFile=defaul )

# price

ntnfp <- colnotNA( allpri('NTN-F')[date > refDate,] )
myCPchart( ntnfp, 'NTN-F_cp_prices', c(900,1450), 'BRL', 50, 1450, toFile=defaul )

#
# LFT: Selic
#

# rates
lft <- colnotNA( alldur('LFT')[date > refDate,] )
myCPchart( lft, 'LFT_cp_rates', c(0,0.4), '%', 3, 0.3, toFile=defaul )

# prices

lftp <- colnotNA( allpri('LFT')[date > refDate,] )
myCPchart( lftp, 'LFT_cp_prices', c(10400,11000), 'BRL', 5, 10800, toFile=defaul )

#
# NTN-C: IGPM
#

ntnc <- colnotNA( alldur('NTN-C')[date > refDate,] )
myCPchart( ntnc, 'NTN-C_cp_rates', c(-2,5), '%', 3, 5, toFile=defaul )

# price

ntncp <- colnotNA( allpri('NTN-C')[date > refDate,] )
#ntncp[,"NTN-C010311" := NULL]
#ntncp[,"NTN-C010408" := NULL]
myCPchart( ntncp, 'NTN-C_cp_prices', c(4000,10000), 'BRL', 5, 6500, toFile=defaul )

#
# compare NTN-B with BTN-BPrinc
#

setkey(ntnb,date)
setkey(ntnbp,date)
e <- merge(ntnb,ntnbp)
#names(e)
s <- cbind(
  date=e[,date],
  e[,'NTN-B150535'],
  e[,'NTN-BPrinc150535']
)
myCPchart( s, 'NTN-B35_comp_rates', c(3,5), '%', 2, 5, toFile=defaul )

s <- cbind( 
  date=e[,date],
  e[,'NTN-B150826'],
  e[,'NTN-BPrinc150826']
)
myCPchart( s, 'NTN-B26_comp_rates', c(2,5), '%', 2, 5, toFile=defaul )

s <- cbind( 
  date=e[,date],
  e[,'NTN-B150824'],
  e[,'NTN-BPrinc150824']
)
myCPchart( s, 'NTN-B24_comp_rates', c(1,5), '%', 2, 5, toFile=defaul )

#
# compare SELIC and IPCA
#

setkey(ntnc,date)
e <- merge(ntnc,ntnbp)
names(e)
s <- cbind( 
  date=e[,date],
  e[,'NTN-C010131'],
  e[,'NTN-BPrinc150535']
)
myCPchart( s, 'NTNB_NTNC_comp_rates', c(2,5), '%', 2, 5, toFile=defaul )
#matplot(s, type = 'o', pch=16, bty='n',lty = 1,main='', xaxt='n',col=col2) #ylim=c(4000,7500)

#
# run bash script to upload charts png to server
#

print('Run bash script to upload charts png to server.')
system(paste0("cd ",data_dir,"; ./update_png_server.sh"))

