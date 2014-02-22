geography <- function(geographyType, geographyId=NULL, geographyName=NULL, fips=NULL, all=NULL, ...){
    if(is.null(fips) & is.null(geographyName) & is.null(geographyId))
        stop("Must specify 'geographyType', and 'fips', 'geographyName', or 'geographyId'")
    if(!is.null(fips)) {
        if(!is.null(geographyName))
            out <- bb(api=paste('geography/state',fips,geographyType,'names',geographyName,sep='/'),
                      args=c(format='json', all=all), ...)
        else
            out <- bb(api=paste('geography/state',fips,geographyType,sep='/'),
                      args=c(format='json', all=all), ...)
    } else if(geographyType %in% .geographyTypes) {
        if(!is.null(geographyId))
            out <- bb(api=paste('geography',geographyType,'id',geographyId,sep='/'),
                      args=c(format='json', all=all), ...)
        if(!is.null(geographyName))
            out <- bb(api=paste('geography',geographyType,'name',geographyName,sep='/'),
                      args=c(format='json', all=all), ...)
        else
            out <- bb(api=paste('geography',geographyType,sep='/'),
                      args=c(format='json', all=all), ...)
    }
    else
        stop("Unrecognized 'geographyType'")
    return(out)
}
