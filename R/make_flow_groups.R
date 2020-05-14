#' make_flow_groups
#'
#' @param spell_data Hospital flow data with columns for
#'  ed non-admission (boolean);
#'  directorate (e.g. "Medical", "Surgical")
#'
#' @return spell_data with additional column for flow group
#' @export
#'
#' @examples
make_flow_groups <- function(spell_data){

  spell_data_mapped <- dplyr::left_join(spell_data, hrg_mapping, by = c("hrg_ae_code"))

  spell_data_specialty_mapped <- dplyr::left_join(spell_data_mapped, specialty_mapping,
                                                  by = c("main_specialty_start" = "spec_name")) %>%
    dplyr::select(-spec_code)

  spell_table <- spell_data_specialty_mapped %>%
    dplyr::mutate(flow_groups = dplyr::case_when(
      starts_with_ed == TRUE & ed_non_adm == TRUE & !(disposal_code == "Died in Department" | source_referral_ae == "Emergency Services") & hrg_mapp_code == 1 ~ "Flow 1" ,
      starts_with_ed == TRUE & ed_non_adm == TRUE & (disposal_code == "Died in Department" | source_referral_ae == "Emergency Services") & hrg_mapp_code == 2  ~ "Flow 2" ,
      starts_with_ed == TRUE & ed_non_adm == TRUE & !(disposal_code == "Died in Department" | source_referral_ae == "Emergency Services") & hrg_mapp_code == 2  ~ "Flow 2" ,
      directorate == "Medical" & starts_with_ed == TRUE & ed_admission == TRUE ~ "Flow 3",
      directorate == "Surgical" & starts_with_ed == TRUE & ed_admission == TRUE ~ "Flow 4"))

}

map_main_spec <- function(data){

  spell_data_specialty_mapped <- dplyr::left_join(data, specialty_map,
                                                  by = c("main_speciality_desc" = "spec_name")) %>%
    dplyr::select(-spec_code)

}
