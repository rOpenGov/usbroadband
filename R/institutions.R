institutions <-
function(geographyType=NULL,
         geographyId=NULL,
         geographyName=NULL, 
         lat=NULL, long=NULL, 
         version=NULL, ...){
    if(!version %in% .dataVersions)
        stop("'version' not allowed")
    if(!geographyType=='nation' & (is.null(geographyName) & is.null(geographyId)))
        stop("Must specify 'geographyName' or 'geographyId'")
    if(!is.null(lat) & !is.null(lat))
        out <- bb(api='cai/closest', args=c(format='json', latitude=lat, longitude=long, maxresults))
    else if(geographyType=='nation')
        out <- bb(api=paste('cai', version, 'nation', sep='/'), args=c(format='json'))
    else if(geographyType %in% .geographyTypes) {
        if(!is.null(geographyId))
            out <- bb(api=paste('cai', version, geographyType, 'ids', paste(head(geographyId,10),collapse=','), sep='/'),
                      args=c(format='json'))
        if(!is.null(geographyName))
            out <- bb(api=paste('cai', version, geographyType, 'names', paste(head(geographyName,10),collapse=','), sep='/'),
                      args=c(format='json'))
        else
            stop("Must specify 'geographyIds' or 'geograpyNames'")
    }
    else
        stop("Unrecognized 'geographyType'")
    return(out)
}
