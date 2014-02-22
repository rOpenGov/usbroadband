availability <- function(state=NULL, county=NULL, version=NULL, ...){
    if(is.null(version))
        version <- 'dec2012'
    if(!version %in% .dataVersions)
        stop("'version' not allowed")
    if(is.null(state) & is.null(county))
        out <- bb(api=paste('county-availability',version,'nation',sep='/'),
                  args=c(format='json'), ...)
    else if(!is.null(state)){
        if(!is.na(as.numeric(state)))
            out <- bb(api=paste('county-availability',version,'stateid',state,sep='/'),
                      args=c(format='json'), ...)
        else
            out <- bb(api=paste('county-availability',version,'state',state,sep='/'),
                      args=c(format='json'), ...)
    } else if(!is.null(county)){
        if(!is.na(as.numeric(county)))
            out <- bb(api=paste('county-availability',version,'countyid',paste(county,sep=','),sep='/'),
                      args=c(format='json'), ...)
        else
            out <- bb(api=paste('county-availability',version,'county',paste(county,sep=','),sep='/'),
                      args=c(format='json'), ...)
    }
    else
        stop("Cannot execute request")
    return(out)
}
