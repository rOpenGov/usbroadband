bb <-
function(baseurl = 'http://www.broadbandmap.gov/broadbandmap/',
         api, args = NULL, ...) {
    if(is.null(args))
        args <- ''
    else
        args <- paste('?',paste(names(args), args, sep='=', collapse='&'),sep='')
    h <- basicTextGatherer()
    curlPerform(url = paste(baseurl, api, args, sep=''),
                followlocation = 1L, ssl.verifypeer = 1L, ssl.verifyhost = 2L, 
                cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
                writefunction=h$update, ...)
    response <- h$value()
    out <- fromJSON(response)
    return(response)
}

.dataVersions <- c('jun2011','dec2011','jun2012','dec2012') # earlier versions no longer available

.geographyTypes <- c('censusplace', 'county', 'msa', 'usf', 'statesenate', 'statehouse', 'congdistrict', 'tribalnation')
