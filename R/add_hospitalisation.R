#' add_hospitalisation
#'
#' @param variants_aggregated
#' @param hospitalisation_formatted
#'
#' @return
#' @export
#'
#' @examples
add_hospitalisation <- function(variants_aggregated,hospitalisation_formatted)
{


  variants_aggregated <- variants_aggregated %>% mutate(hospitalization = FALSE)


  hospitalisation_formatted <- hospitalisation_formatted %>% rowwise() %>%mutate(timehospi = as.Date(time)-7)


  varianttest <- variants_aggregated %>% split(.$time)
  hospitest <- hospitalisation_formatted %>% split(.$timehospi)


  common_geo <- intersect(names(varianttest), names(hospitest))

  varianttest <- varianttest[is.element(names(varianttest), common_geo)]
  hospitest <- hospitest[is.element(names(hospitest), common_geo)]


  hospi_sample <- function(variantdata, hospidata)
  {
    variantdata_desagregate <- expandRows(variantdata, count = "new_cases")
    variantdata_desagregate$hospitalization[sample(length(variantdata_desagregate$hospitalization),as.integer(hospidata$new_cases))] = T
    variantdata <- variantdata_desagregate %>% group_by_all()%>% summarise(new_cases = n())
    return(variantdata)
  }

  variants_aggregated <-  map2_df(.x = varianttest, .y = hospitest , .f = function(.x, .y) hospi_sample(variantdata = .x, hospidata = .y))
  return(variants_aggregated)
}
