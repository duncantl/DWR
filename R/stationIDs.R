library(RCurl)
library(XML)

getIDsByLetter =
function(letter = "A",
         u = "http://cdec.water.ca.gov/cgi-progs/staMeta")
{
   txt = getForm(u, station_id = letter)
   doc = htmlParse(txt, encoding = "UTF8")
   a = getNodeSet(doc, "//pre/a")
   structure(XML:::trim(sapply(a, function(x) xmlValue(getSibling(x)))),  names = sapply(a, xmlValue))
}

getStationIDs =
function(letters = LETTERS)
{
  unlist(lapply(letters, getIDsByLetter))
}

