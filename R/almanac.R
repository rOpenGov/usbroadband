almanac_parameters <- function(...){
    out <- bb(api='almanac/parameters', args=c(format='json'), ...)
    return(out)
}

almanac <- function(...){
    out <- bb(api='almanac', args=c(format='json'), ...)
    return(out)
}


# almanac rank by geography id within state
# {API Base}/almanac/parameters?{dataVersion}/rankby/state/{stateId}/{censusMetric}/{rankingMetric}/{geographyType}/id/{geographyId}}?properties={properties}&order={sortOrder}
# http://www.broadbandmap.gov/developer/api/almanac-api-ranking-by-geography-id-within-a-state

# almanac rank by geography id within nation
# {API Base}/almanac/{dataVersion}/rankby/nation/{censusMetric}/{rankingMetric}/{geographyType}/id/{geographyId}?properties={properties}&order={sortOrder}

# almanac rank by geography type only (within state or nation)
# {API Base}/almanac/{dataVersion}/rankby/state/{stateId}/{censusMetric}/{rankingMetric}/{geographyType}?properties={properties}&order={sortOrder}
# http://www.broadbandmap.gov/developer/api/almanac-api-ranking-by-geography-type-within-a-state
# http://www.broadbandmap.gov/developer/api/almanac-api-ranking-by-geography-type-within-the-nation
