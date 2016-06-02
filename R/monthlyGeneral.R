getMonthly =
    #
    # getMonthly(u = "http://cdec.water.ca.gov/cgi-progs/queryMonthly?MRC")
    # getMonthly(u = "http://cdec.water.ca.gov/cgi-progs/queryF?SHA")
    # o = getMonthly(u = "http://cdec.water.ca.gov/cgi-progs/stages/FNFSUM", colClasses = c("character", rep("FormattedInteger", 14)))
    # o = getMonthly(u = "http://cdec.water.ca.gov/cgi-progs/stages/FNFSUM")  # don't need the colClasses
function(station = "MRC", u = sprintf("http://cdec.water.ca.gov/cgi-progs/queryMonthly?%s", station),
          doc = htmlParse(u, encoding = "UTF8"), ...)
{
    tt = readHTMLTable(doc, stringsAsFactors = FALSE, ...)
    if(length(tt) == 0)
       return(list())

    tt = tt[[1]]
    tt = tt[ names(tt) != "" ]
    names(tt) = gsub("&nbsp", "", names(tt))


    if(all(grepl("^[()A-Za-z ]*$", XML:::trim(tt[1,]))))
       tt = tt[ -1 , ]
    
    convertCols(tt)
}


convertCols =
function(df)
{
  df[ ] = lapply(df, convertCol)
  df
}

convertCol =
function(col)
{
  miss = grepl("-{2,}", col)
  if(all(miss))
     return(as.integer(rep(NA, length(col))))
  col[miss] = NA


  if(all(is.na(col) | grepl("^[0-9,]+$", col)))
     return(as(col, "FormattedInteger"))  
  
  if(all(is.na(col) | grepl("^[0-9]+$", col)))
     return(as.integer(col))

  if(all(is.na(col) | grepl("^[0-9,.]+$", col)))
     return(as(col, "FormattedNumber"))
  
  if(all(is.na(col) | grepl("^[0-9.]+$", col)))
     return(as.numeric(col))

  if(all(is.na(col) | grepl("^[0-9]{2}/[0-9]{4}$", col)))
     return(as.Date(sprintf("1/%s", col), "%d/%m/%Y"))

  if(all(is.na(col) | grepl("^[0-9]{2}/[0-9]{4}$", col)))
     return(as.Date(sprintf("1/%s", col), "%d/%m/%Y"))

  if(all(is.na(col) | grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4} [0-9]{2}:[0-9]{2}$", col)))  
     return(as.POSIXct(strptime(col, "%m/%d/%Y %H:%M")))

  col
}
