funding <- function(states, type='bip', ...){
    if(!type %in% c('bip','btop'))
        stop("'type' must be 'bip' or 'btop'")
    if(states=='nation')
        out <- bb(api=paste(type,'nation',sep='/'), args=c(format='json'), ...)
    else if(!is.na(as.numeric(states)))
        out <- bb(api=paste(type,'stateids',paste(head(states, 10),collapse=','),sep='/'),
                  args=c(format='json'), ...)
    else
        out <- bb(api=paste(type,'states',paste(head(states, 10),collapse=','),sep='/'),
                  args=c(format='json'), ...)
    return(out)
}
