#! /usr/bin/Rscript
#
# 1. take list 'la' (key: security code + expiration date, value: time series data.table
# 2. select from 'value' only the last line (last date) for each key
# 3. select only keys with expiration dates in the future
# 4. change data structure from 
#   key: `NTN-F010131`, value: date  bid  ask  puc puv  txm     pum
# to
#   key1: 'NTN-F' key2: 010131 value: same

#
# find date of last time info was updated in xls file
#

getLastDate <- function(){
  # initialize data in the past
  lastDate <- as.Date('2000-01-01')
  for( name in names(la) ) {
    e <- la[[ name ]]
    # check last date
    dt <- e[nrow(e),date]
    if( dt > lastDate ) {
      lastDate <- dt
    }
  }
  return( lastDate )
}

lastDate <- getLastDate()

# select only instruments that were not expired on the last date
ld1 <- list()
for( name in names(la) ) {
  e <- la[[ name ]]
  dt <- e[nrow(e),date]
  if( dt == lastDate ) {
    ld1[[ name ]] <- e[nrow(e),]
  }
}

# change data structure from 
#   key: `NTN-F010131`, value: date  bid  ask  puc puv  txm     pum
# to
#   key1: 'NTN-F' key2: 010131 value: same
ld2 <- list()
for( code_venc in names(ld1) ) {
  code <- gsub( '[0-9]', '', code_venc )
  venc <- gsub( '[^0-9]', '', code_venc )
  name <- secdict[[ code ]]
  value <- ld1[[ code_venc ]]
  value[,date := as.character(date)] 
  # build new list ld2
  venc <- as.Date(venc,"%d%m%y")
  name_venc <- paste( name, venc, sep='\t' )
  ld2[[ name_venc ]] <- value
}

#
# xlsLastUpdated
#
#print(lastDate)
ld2[[ 'xlsLastUpdated' ]] <- as.character(format(lastDate,'%d-%m-%Y'))

#
# save json to local file
#

jsonData <- toJSON(ld2)
write(jsonData, paste( pathp, "tsv/lastDay.json", sep='') ) 
write(jsonData, paste0(lastDay_dir, "lastDay.json" ))
