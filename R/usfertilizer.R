#' @keywords internal
"_PACKAGE"

# quiet the undefined global functions or variables.
if(getRversion() >= "2.15.1") utils::globalVariables(
  c("FIPS", "County", "Year", "Fertilizer","Farm.Type", "State",
    "INTPTLAT","INTPTLONG")
)
