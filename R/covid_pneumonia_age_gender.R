#' covid_infected_pneumonia
#'
#' @param data
#' @param start_date
#' @param end_date
#' @param plot_chart
#' @param hospital_name
#'
#' @return
#' @export
#'
#' @examples
covid_infected_pneumonia <- function(data, start_date, end_date, plot_chart = TRUE, hospital_name){


  covid_detect_dt <- data %>%
    dplyr::select(
      person_id,
      age_band,
      ethnicity,
      gender,
      covid_snomed_code1_desc_confirmed,
      event_result_status
    ) %>%
    dplyr::filter(covid_snomed_code1_desc_confirmed == "Corona virus detected") %>%
    dplyr::group_by(gender, age_band) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(freq = count/sum(count)) %>%
    dplyr::mutate(
      group = dplyr::case_when(
        gender == "Female" ~ "Female covid detected",
        gender == "Male" ~ "Male covid detected"
      )
    )


  pneumonia_covid_dt <- data %>%
    dplyr::select(
      person_id,
      age_band,
      ethnicity,
      gender,
      covid_snomed_code1_desc_confirmed,
      event_result_status
    ) %>%
    dplyr::filter(covid_snomed_code1_desc_confirmed == "Pneumonia - Corona virus") %>%
    dplyr::group_by(gender, age_band) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(freq = count/sum(count)) %>%
    dplyr::mutate(
      group = dplyr::case_when(
        gender == "Female" ~ "Female Pneumonia(Covid)",
        gender == "Male" ~ "Male Pneumonia(Covid)"
      )
    )

  df_numbers_only <-
    dplyr::full_join(covid_detect_dt,
                     pneumonia_covid_dt,
                     by = c("gender", "age_band", "count", "freq","group"))


  #Set title

  title_stub <-
    ": Covid detected and Pneumonia Covid acquired by Age and Gender,\n"
  hospital_name <- "Test"
  start_date_title <-
    format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <-
    paste(hospital_name,
          title_stub,
          start_date_title,
          " to ",
          end_date_title)

  plot_test <-
    ggplot2::ggplot(df_numbers_only, ggplot2::aes(age_band, count, fill = group)) +
    ggplot2::geom_col(
      data = dplyr::filter(
        df_numbers_only,
        group %in% c("Male covid detected", "Female covid detected")
      ),
      position = ggplot2::position_dodge()
    ) +
    ggplot2::geom_col(
      data = dplyr::filter(
        df_numbers_only,
        group %in% c("Male Pneumonia(Covid)", "Female Pneumonia(Covid)")
      ),
      position = ggplot2::position_dodge(0.9),
      width = 0.5
    ) +
    ggplot2::scale_fill_manual(
      name = "",
      breaks = c(
        "Male covid detected",
        "Male Pneumonia(Covid)" ,
        "Female covid detected",
        "Female Pneumonia(Covid)"
      ),
      labels = c(
        "Male Pneumonia(Covid)",
        "Male covid detected",
        "Female covid detected",
        "Female Pneumomia(Covid)"
      ),
      values = c("coral3", "lightcoral", "steelblue4", "lightblue2")
    ) +
    xlim(
      "0 yrs",
      "1-4 yrs",
      "5-9 yrs",
      '10-14 yrs',
      '15-19 yrs',
      '20-24 yrs',
      '25-29 yrs',
      '30-34 yrs',
      '35-39 yrs',
      '40-44 yrs',
      '45-49 yrs',
      '50-54 yrs',
      '55-59 yrs',
      '60-64 yrs',
      '65-69 yrs',
      '70-74 yrs',
      '75-79 yrs',
      '80-84 yrs',
      '85-89 yrs',
      '90-94 yrs',
      '95+'
    ) +
    ggplot2::xlab("Age Group") +
    ggplot2::ylab("Covid Detected and Pneumonia covid aquired") +
    ggplot2::labs(
      title = chart_title,
      subtitle = "Covid detection and Pneumonia acquired due to covid",
      y = "Covid detected and Pneumonia(Covid), n",
      x = "Age Group",
      caption = "Source: GDHU, Imperial College"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(
        t = 0,
        r = 21,
        b = 0,
        l = 0
      )),
      axis.text.x =  ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    ggplot2::scale_x_discrete(drop = FALSE)

  plot_test

  if(plot_chart == TRUE){

    plot_test

  }else{

   covid_detected_pneumonia_stats <-  plot_test %>% purrr::pluck('data') %>% dplyr::select(gender, age_band, count, freq, group)
   covid_detected_pneumonia_stats
  }

}

