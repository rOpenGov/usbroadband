bbsummary <- function(geographyType, geographyId=NULL, metric=NULL, version=NULL, ...){
    if(!version %in% .dataVersions)
        stop("'version' not allowed")
    if(!geographyType %in% c('nation',.geographyTypes))
        stop("Unrecognized 'geographyType'")
    if(!metric %in% c('population','household'))
        stop("'metric' must be 'population' or 'household'")
    if(geographyType=='nation')
        out <- bb(api=paste('analyze', version, 'summary', metric, 'nation', sep='/'),
                  args=c(format='json'))
    else
        out <- bb(api=paste('analyze', version, 'summary', metric, geographyType, 'ids', paste(head(geographyId,10),collapse=','), sep='/'),
                  args=c(format='json'))
    return(out)
}
