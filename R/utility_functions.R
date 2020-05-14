#' cfg_write
#'
#' @param provided categorical levels of the variable that are provided in the original dataset
#' @param standard levels recoded to new categories
#' @param table the name of the table to which new table should be saved
#' @param path the path of the folder to which the table should be saved
#'
#' @return
#' @export
#'
#' @examples
cfg_write <- function(provided = c(1:2),
                      standard = c(1:2),
                      table = name_of_a_table,
                      path = "path/to/save") {
  table <- tibble::tibble(provided = provided, standard = standard)
  table

  saveRDS(table, file = path)

}

#' apply_cl_cm
#'
#' @param data a dataframe or a tibble
#' @param vars_ordered selecting the variables in the order you want. it is similiar to dplyr::select('var1', 'var2').
#' @param data_selected_order a bolean TRUE or FALSE. If TRUE then dataset obtained in vars_ordered will be saved into a folder of your choice.
#' @param column_mapping a bolean TRUE OR FALSE. If TRUE, then a column mapping tibble with 2 variables - 'provided' and 'standard'.
#' provided variable stores all the variables of data passed into this function, stored as rows; standard variable contains the same variables as in provided column but
#' all variables are transformed from CAPITAL letters variables to lower letters. e.g. PSEUDO_ID -> pseudo_id
#' @param config_folder_path the path of your choice where the column_mapping tibble (and other tibbles that form the process of recoding levels of variables) are stored
#' @param save_ordered_data the path folder where the data is saved
#'
#' @return
#' @export
#'
#' @examples
apply_cl_cm <- function(data,
                        vars_bolean = TRUE,
                        vars_ordered = c(1:50),
                        data_selected_order = TRUE,
                        save_ordered_data = "path/to/save/data.csv",
                        column_mapping = TRUE,
                        config_folder_path = "/config_folder_path/column_mapping.rds") {

  if(vars_bolean == TRUE) {

    vars_ordered <- data %>%
      dplyr::select(vars_ordered)

  }else{

    if(vars_bolean == FALSE & vars_ordered == FALSE) {
      vars_ordered <- data %>%
        dplyr::select_all()

    } else {
      print("Previous parameters, had vars_bolean == FALSE and vars_ordered is null.
              a similar data has been saved into your folder passed at saved_ordered_data")
    }

  }

  if (column_mapping == TRUE) {
    column_mapping <- tibble::tibble(names(vars_ordered)) %>%
      dplyr::rename(provided = "names(vars_ordered)") %>%
      dplyr::mutate(standard = tolower(provided))

    saveRDS(column_mapping, file = config_folder_path)

  }

  if (data_selected_order == TRUE) {
    write.csv(vars_ordered, file = save_ordered_data, row.names = FALSE)

  } else{
    NULL

  }


}



empty_as_na <- function(x) {
  if ("factor" %in% class(x))
    x <- as.character(x)
  ifelse(as.character(x) != "", x, NA)
}

data <- data %>% mutate_each(funs(empty_as_na))


data <- data %>%
  naniar::replace_with_na_if(.predicate = is.character,
                             condition = ~ .x %in% ("Not stated"))

data <- data %>% replace_na(list(ethnicity = "Missing"))








