census <- function(geographyType=NULL, geographyId=NULL, geographyName=NULL, lat=NULL, long=NULL, version=NULL, all=TRUE, ...){
    if(!version %in% .dataVersions)
        stop("'version' not allowed")
    if(!geographyType %in% c('nation',.geographyTypes))
        stop("Unrecognized 'geographyType'")
    if(!geographyType=='nation' & (is.null(geographyName) & is.null(geographyId)))
        stop("Must specify 'geographyName' or 'geographyId'")
    if(!is.null(lat) & !is.null(lat))
        out <- bb(api=paste('census', geographyType, sep='/'),
                  args=c(format='json', latitude=lat, longitude=long, showall=all), ...)
    else if(!is.null(geographyId))
        out <- bb(api=paste('census', geographyType, 'fips', geographyId, sep='/'),
                  args=c(format='json'), ...)
    else if(!is.null(geographyName))
        out <- bb(api=paste('census', geographyType, geographyName, sep='/'),
                  args=c(format='json', all=all), ...)
    else
        stop("Must specify 'geographyIds' or 'geograpyNames', or 'lat' and 'long'")
    return(out)
}
