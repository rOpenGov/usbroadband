county_availability <- function(state=NULL, county=NULL, version=NULL, ...){
    if(!version %in% .dataVersions)
        stop("'version' not allowed")
    if(is.null(state) & is.null(county))
        out <- bb(api=paste('county-availability',version,'nation',sep='/'),
                  args=c(format='json'), ...)
    else if(!is.null(state)){
        if(!is.na(as.numeric(state)))
            out <- bb(api=paste('county-availability',version,'stateids',state,sep='/'),
                      args=c(format='json'), ...)
        else
            out <- bb(api=paste('county-availability',version,'states',state,sep='/'),
                      args=c(format='json'), ...)
    } else if(!is.null(county)){
        if(!is.na(as.numeric(county)))
            out <- bb(api=paste('county-availability',version,'countyids',county,sep='/'),
                      args=c(format='json'), ...)
        else
            out <- bb(api=paste('county-availability',version,'county',county,sep='/'),
                      args=c(format='json'), ...)
    }
    else
        stop("Cannot execute request")
    return(out)
}
