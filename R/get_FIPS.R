#'Get the FIPS code for the selected data.
#'@name get_FIPS
#'@description The function will fetch and return the FIPS for the counties of interest.
#'    As some counties will change due to various causes, it is easier to track the counties
#'    by FIPS code for a long-term analysis.  Description of FIPS codes is available via
#'    \code{[here](https://en.wikipedia.org/wiki/FIPS_county_code)}
#'
#'@param counties counties of interest, defalut: all avaible counties.
#'@param states states of interest, defalt: all avaialble states.
#'@param overlap_state_county Logic. If true, the function will overlaping
#'       the input of states and counties. If false, the function will return
#'       results either in the states or in the counties.
#'@param combine_state_county Logic. If true, the county will be changed into
#'       county, state, e.g. Wake, NC; If false, no changes.
#'@return A tibble with tidy data.
#'@export
#'@import dplyr
#'@seealso \code{link(get_data)}
#'@examples
#'    get_FIPS(counties = "Wake", states = "NC")
#'    get_FIPS(states = "NC")
#'
get_FIPS <- function(counties = NULL,
                     states = NULL,
                     overlap_state_county = TRUE,
                     combine_state_county = TRUE
){
  output = get_data(counties = counties,
           states = states,
           overlap_state_county = overlap_state_county,
           combine_state_county = combine_state_county
           )
  # only selece three rows.
  output = select(output, c(FIPS, County, Year))
  # fetch duplicated counties.
  output %>%
    group_by(FIPS)%>%
    filter(n()>=1) %>%
    summarise(County = County[1])
  # return format
  # FIPS County
  # 01049 DeKalb, AL
}
