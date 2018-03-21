#' Data overview for usfertilizer
#'
#' Provides a summary of filtered data.
#'
#' @param fertilizer_data the dataset were selected based on the filters.
#' @keywords fertilizer summary
#' @export
#'
#' @examples
#' wake_fertilizer = get_data(counties = "wake")
#' data_overview(wake_fertilizer)
#'
#'
data_overview <- function(fertilizer_data){
    year_min = as.numeric(min(fertilizer_data$Year))
    year_max = as.numeric(max(fertilizer_data$Year))
    years_num = year_max - year_min + 1
    cat("\n The selected dataset includes", years_num, "years of data, ranging from",
        year_min, "to", year_max, ";")

    county_list = fertilizer_data$County[!duplicated(fertilizer_data$County)]
    state_list = fertilizer_data$State[!duplicated(fertilizer_data$State)]

    if (length(county_list) <= 5){
    cat("\n The counties include ", county_list, ";")
    }
    else {
      cat("\n The counties include ", county_list[1:5], ";")
    }

    if (length(state_list) <=5){
    cat(" in", length(state_list), "states, e.g.", state_list, ";")
    }
    else(
      cat(" in", length(state_list), "states, e.g.", state_list[1:5], ";")
    )

    Fert_max = as.numeric(max(fertilizer_data$Quantity))
    Fert_min = as.numeric(min(fertilizer_data$Quantity))
    cat("\n The range of fertilization quantity is ", Fert_min, "to", Fert_max, ";")

    Fert_Type = fertilizer_data$Fertilizer[!duplicated(fertilizer_data$Fertilizer)]
    Farm_Type = fertilizer_data$Farm.Type[!duplicated(fertilizer_data$Farm.Type)]
    cat("\n The fertilizer is applied as", Fert_Type, "and at the locations of", Farm_Type, ".")
}
