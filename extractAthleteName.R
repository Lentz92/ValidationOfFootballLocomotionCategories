#Extracting the name for the gps data after exporting it from openfield
extractAthleteName <- function(name){
  removeBeforeName <- sub('.*NIU2 ', '', name)
  removeAfterName <- sub(' .*', '', removeBeforeName)
  
  return(removeAfterName)
}