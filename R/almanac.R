almanac_parameters <- function(...){
    out <- bb(api='almanac/parameters', args=c(format='json'), ...)
    return(out)
}

almanac <- function(state, metric, ranking, geographyType, geographyId=NULL,
                    properties=NULL, sort='asc', version=NULL, ...){
    if(is.null(version))
        version <- 'dec2012'
    if(!version %in% .dataVersions)
        stop("'version' not allowed")
    if(!geographyType %in% .geographyTypes)
        stop("Unrecognized 'geographyType'")
    if(!metric %in% c('population','household'))
        stop("'metric' must be 'population' or 'household'")
    if(!is.null(properties))
        properties <- paste(properties, sep=',')
    if(state=='nation') {
        if(is.null(geographyId)){
            out <- bb(api=paste('almanac', version, 'rankby/nation', metric, ranking, 
                      geographyType, sep='/'),
                      args=c(format='json', order=sort, properties=properties), ...)
        } else {
            out <- bb(api=paste('almanac', version, 'rankby/nation', metric, ranking, 
                      geographyType, 'id', geographyId, sep='/'),
                      args=c(format='json', order=sort, properties=properties), ...)
        }
    } else if(is.na(as.numeric(state))) {
        stop("'state' must be a numeric FIPS code, see `geography`")
    } else {
        if(is.null(geographyId)){
            out <- bb(api=paste('almanac', version, 'rankby/state', state, metric, ranking,
                                geographyType, sep='/'),
                      args=c(format='json', order=sort, properties=properties), ...)
        } else {
            out <- bb(api=paste('almanac', version, 'rankby/state', state, metric, ranking,
                                geographyType, 'id', geographyId, sep='/'),
                      args=c(format='json', order=sort, properties=properties), ...)
        }
    }
    return(out)
}
