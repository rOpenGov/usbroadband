speedtest <- function(geographyType, geographyId=NULL, geographyName=NULL, type=NULL, ...){
    if(!geographyType=='nation' & (is.null(geographyName) & is.null(geographyId)))
        stop("Must specify 'geographyName' or 'geographyId'")
    
    if(!is.null(type) && !type %in% c('any','ookla','mlab'))
        stop("'testType' must be in 'any', 'ookla', or 'mlab'")
    if(geographyType=='nation')
        out <- bb(api='speedtest/nation', args=c(format='json', speedTestType=type), ...)
    else if(geographyType %in% .geographyTypes) {
        if(!is.null(geographyId))
            out <- bb(api=paste('speedtest',geographyType,'ids',paste(head(geographyId,10),collapse=','),sep='/'),
                      args=c(format='json', speedTestType=type), ...)
        if(!is.null(geographyName))
            out <- bb(api=paste('speedtest',geographyType,'names',paste(head(geographyName,10),collapse=','),sep='/'),
                      args=c(format='json', speedTestType=type), ...)
        else
            stop("Must specify 'geographyIds' or 'geograpyNames'")
    }
    else
        stop("Unrecognized 'geographyType'")
    return(out)
}
