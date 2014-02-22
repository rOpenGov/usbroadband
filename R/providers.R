provider_parameters <- function(...){
    out <- bb(api='provider/parameters', args=c(format='json'), ...)
    return(out)
}

providers <- function(name=NULL, geographyType=NULL, geographyId=NULL, version=NULL, all=TRUE, ...){
    if(is.null(version))
        version <- 'dec2012'
    if(!version %in% .dataVersions)
        stop("'version' not allowed")
    if(!geographyType=='nation' & is.null(geographyId))
        stop("Must specify 'geographyName' or 'geographyId'")
    if(is.null(name))
        out <- bb(api='provider', args=c(format='json'), ...)
    else {
        if(is.null(geographyType) | is.null(geographyId))
            out <- bb(api=paste('provider/name', version, name, sep='/'),
                      args=c(format='json', all=as.character(TRUE)), ...)
        if(geographyType=='nation')
            out <- bb(api=paste('provider/name', version, 'nation', name, sep='/'),
                      args=c(format='json', all=as.character(TRUE)), ...)
        else if(geographyType %in% .geographyTypes)
            out <- bb(api=paste('provider/name', version, geographyType, geographyId, name, sep='/'),
                      args=c(format='json', all=as.character(TRUE)), ...)
        else
            stop("Unrecognized 'geographyType'")
    }
    return(out)
}

# {API Base}/provider/{dataVersion}/stats/state/{stateId}/{censusMetric}/{geographyType}/{geographyId}/{holdingCompanyNumber}/similar/{searchCriteria}
# http://www.broadbandmap.gov/developer/api/provider-statistics-api-similar-providers-within-a-geography
# {API Base}/provider/{dataVersion}/stats/state/{stateId}/{censusMetric}/{holdingCompanyNumber}/similar/{searchCriteria}
# http://www.broadbandmap.gov/developer/api/provider-statistics-api-similar-providers-within-a-state
# {API Base}/provider/{dataVersion}/stats/nation/{censusMetric}/{holdingCompanyNumber}/similar/{searchCriteria}
# http://www.broadbandmap.gov/developer/api/provider-statistics-api-similar-providers-within-nation


# # wireless providers by lat/long
# {API Base}/broadband/{dataVersion}/wireless?latitude={latitude}&longitude=-{longitude}
# http://www.broadbandmap.gov/developer/api/wireless-broadband-api
# {API Base}/broadband/{dataVersion}/wireline?latitude={latitude}&longitude={longitude}
# http://www.broadbandmap.gov/developer/api/wireline-broadband-api

# {API Base}/provider/{dataVersion}/providers/state/{stateId}/{censusMetric}/{geographyType}/{geographyId}
# http://www.broadbandmap.gov/developer/api/provider-statistics-api-all-providers-within-a-geography
# {API Base}/provider/{dataVersion}/providers/state/{stateId}/{censusMetric}
# http://www.broadbandmap.gov/developer/api/provider-statistics-api-all-providers-within-a-state
# {API Base}/provider/{dataVersion}/providers/nation/{censusMetric}
# http://www.broadbandmap.gov/developer/api/provider-statistics-api-all-providers-within-nation

# {API Base}/provider/{dataVersion}/stats/state/{stateId}/{censusMetric}/{geographyType}/{geographyId}/{holdingCompanyNumber}
# http://www.broadbandmap.gov/developer/api/provider-statistics-api-provider-within-a-geography
# {API Base}/provider/{dataVersion}/stats/state/{stateId}/{censusMetric}/{holdingCompanyNumber}
# http://www.broadbandmap.gov/developer/api/provider-statistics-api-provider-within-a-state
# {API Base}/provider/{dataVersion}/stats/nation/{censusMetric}/{holdingCompanyNumber}
# http://www.broadbandmap.gov/developer/api/provider-statistics-api-provider-within-nation
