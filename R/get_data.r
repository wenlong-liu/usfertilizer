#' get_data function
#' County-level estimates of fertiizer application quantity from 1987 to 2012.
#'
#'
#' @description This function will return the full dataset (or a subset based on input parameters) of county-level estimates
#'  of fertilizer application quantity.  The fertilizer application type includes farm and non-farms.
#'  The data were estimated based on the sales of commecial fertilizer for each county originally from
#'  the Association of American Plant Food Control Officials (AAPFCO) commercial fertilizer sales data.
#'  Further details are available via: https://www.sciencebase.gov/catalog/item/5851b2d1e4b0f99207c4f238
#'
#'@param fert_type fertilizer type of interest, default: both Nitrogen and Phosphorus.
#'@param start_year start year to show data, default: 1987.
#'@param end_year last year to show data, default: 2012.
#'@param counties counties of interest, defalut: all avaible counties.
#'@param states states of interest, defalt: all avaialble states.
#'@param lat_max the maximum latitude of area of interest, default: .
#'@param lat_min the minmum latitude of area of interest, default:
#'@param long_max the maximum longitude of area of interest, default:
#'@param long_min the minimum longitude of area of interest, default:
#'@param FIPS FIPS numbers of interest, defalut: all the counties.
#'
#'@reference https://www.sciencebase.gov/catalog/item/5851b2d1e4b0f99207c4f238
#'@keywrods datasets
#'@import dplyr
#'@seealso
#'@example
#'
#'
#'
#'
#'
load("./data/usfertilizer_county.rda")
clean_data = as.data.frame(clean_data)
get_data <- function( fert_type = NULL,
                        years = NULL,
                        counties = NULL,
                        states = NULL,
                        lat_max = NULL,
                        lat_min = NULL,
                        long_max = NULL,
                        long_min = NULL,
                        FIPSs = NULL
                        )
{ if (is.null(fert_type)){
    fert_type = c("N","P")
}
  else{
    lapply(tolower(fert_type),check_list,check_list_data = tolower(clean_data$Fertilizer),
           warning_content= "Fertilizer")
  }
  output = filter(clean_data,Fertilizer %in% fert_type )
  # check years in the list or not.
  if(is.null(years)) {
    years = seq(1978,2012)
  }
  else
    {
    lapply(tolower(years),check_list,check_list_data = clean_data$Year,
           warning_content= "Years")
  }
  output = filter(output, Year %in% years)
  # The FINO and counties can only exist once.

  if(!is.null(FIPSs) & !is.null(counties)){
    stop("Users can only use one of Counties and FIPSs parameters.",call. = FALSE)
  }

  # check and return unavaialbe counties.
  # only check the lowercase, in case some users input lower case of county names.
  if(is.null(counties)){
    counties = clean_data$County
  }
  else
    {
    lapply(tolower(counties),check_list,check_list_data = tolower(clean_data$County),
           warning_content= "Counties")
  }

  # check and return unavaialbe counties.
  if(is.null(states)){
    states = clean_data$State
  }

    else {
    lapply(tolower(states),check_list,check_list_data = tolower(clean_data$State),
           warning_content= "States")
  }

  output = filter(output, County %in% counties | State %in% states )

  if(!is.null(lat_max) & !is.null(lat_min)
     & !is.null(long_max) & !is.null(long_min)){
         check_boundary(lat_max,max(clean_data$INTPTLAT,na.rm = T), F, "Max lat" )
         check_boundary(lat_min,min(clean_data$INTPTLAT,na.rm = T), T, "Min lat" )
         check_boundary(long_max,max(clean_data$INTPTLONG,na.rm = T), F, "Max long" )
         check_boundary(long_min,min(clean_data$INTPTLONG,na.rm = T), T, "Min long" )

  output = filter(output,
                  (INTPTLAT > lat_min) & (INTPTLAT < lat_min) &
                  (INTPTLONG > long_min) & (INTPTLONG < long_min)
                  )
  }

  # check FINO in the list or not.
  if(is.null(FIPSs)){
    FIPSs = clean_data$FIPS
  }

  else {
    lapply(FIPSs,check_list,check_list_data = clean_data$FIPS,
           warning_content= "FIPS")
  }

   output = filter(output, FIPS %in% FIPSs)
   return(output)
}

#' function to check the lat and long are within limits.
#'@param input the input lat or long value.
#'@param check_value the max or min value of lat or long.
#'@param expected_greater the input is expected to be greater or not.  Defalut:TRUE.

check_boundary <- function(input, check_value, expected_greater = TRUE, warning_content) {
  if(expected_greater){
    if(input < check_value){
      stop( paste(warning_content,input, "is out of bounde!"), call. = FALSE)}
  }
  else {
    if(input > check_value){
      stop( paste(warning_content,input, "is out of bounde!"), call. = FALSE)}
  }
}

#'check the list in the dataset or not.
#'@param input the list to check.
#'@param check_list_data the list in the datasets.
#'@param warning_content the content to be raised as an error.
#'

check_list <- function(input, check_list_data, warning_content){
  oldw = getOption("warn")
  options(warn = -1)

  if(!is.element(input,check_list_data)){
    stop(paste(warning_content, input[!(input %in% check_list_data)], "are not available in the dataset.\n
               Please deleted them and try againt.\n"),call. = FALSE)
  }
  options(warn = oldw)
}

