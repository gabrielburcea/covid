#' map_main_spec
#'
#' @param data
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' 
#'
#' @examples
map_main_spec <- function(data){

  spell_data_specialty_mapped <- dplyr::left_join(data, specialty_map,
                                                  by = c("main_speciality_desc" = "spec_name")) %>%
    dplyr::select(-spec_code)

}
