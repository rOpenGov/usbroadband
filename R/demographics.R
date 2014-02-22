demographics <- function(geographyType=NULL, geographyId=NULL, geographyName=NULL, lat=NULL, long=NULL, version=NULL, all=FALSE, ...){
    if(!version %in% .dataVersions)
        stop("'version' not allowed")
    if(!geographyType=='nation' & (is.null(geographyName) & is.null(geographyId)))
        stop("Must specify 'geographyName' or 'geographyId'")
    if(!is.null(lat) & !is.null(lat))
        out <- bb(api=paste('demographic', version, 'coordinates', sep='/'),
                  args=c(format='json', latitude=lat, longitude=long))
    else if(geographyType=='nation')
        out <- bb(api=paste('demographic', version, 'nation', sep='/'), args=c(format='json'))
    else if(geographyType %in% .geographyTypes) {
        if(!is.null(geographyId))
            out <- bb(api=paste('demographic', version, geographyType, 'ids', paste(head(geographyId,10),collapse=','), sep='/'),
                      args=c(format='json'))
        if(!is.null(geographyName))
            out <- bb(api=paste('demographic', version, geographyType, 'names', paste(head(geographyName,10),collapse=','), sep='/'),
                      args=c(format='json'))
        else
            stop("Must specify 'geographyIds' or 'geograpyNames'")
    }
    else
        stop("Unrecognized 'geographyType'")
    return(out)
}
