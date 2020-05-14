#' covid_tested_infected_ethnicity
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
covid_tst_dtct_adm_died_ethn <-
  function(data,
           start_date,
           end_date,
           plot_chart = TRUE,
           hospital_name) {

    selected_vars <- data %>%
      dplyr::select(
        person_id,
        result_status,
        arrival_dt_tm,
        admission_dt_tm,
        event_result_txt,
        ethnicity,
        gender,
        orderable_type_code_desc_txt,
        covid_snomed_code1_desc_confirmed
      ) %>%
      dplyr::arrange(arrival_dt_tm) %>%
      dplyr::filter(arrival_dt_tm >= start_date) %>%
      tidyr::replace_na(list(ethnicity = "Missing"))

    ethnicity_tested <- selected_vars %>%
      dplyr::select(
        person_id,
        result_status,
        arrival_dt_tm,
        admission_dt_tm,
        event_result_txt,
        ethnicity,
        gender,
        orderable_type_code_desc_txt,
        covid_snomed_code1_desc_confirmed
      ) %>%
      dplyr::filter(arrival_dt_tm >= start_date) %>%
      tidyr::replace_na(list(ethnicity = "Missing")) %>%
      dplyr::filter(
        result_status == "suspected" |
          result_status == "negative" | result_status == "positive"
      ) %>%
      dplyr::group_by(ethnicity) %>%
      dplyr::summarise(tested = n()) %>%
      dplyr::mutate(freq_tested = tested / sum(tested))



    ethnicity_detected <- selected_vars %>%
      dplyr::select(
        person_id,
        result_status,
        arrival_dt_tm,
        disch_dt_tm,
        event_result_txt,
        ethnicity,
        gender,
        orderable_type_code_desc_txt,
        covid_snomed_code1_desc_confirmed
      ) %>%
      dplyr::filter(arrival_dt_tm >= end_date) %>%
      tidyr::replace_na(list(ethnicity = "Missing")) %>%
      dplyr::filter(
        event_result_txt == "DETECTED" |
          covid_snomed_code1_desc_confirmed == "Corona virus detected"
      ) %>%
      dplyr::group_by(ethnicity) %>%
      dplyr::summarise(detected = n()) %>%
      dplyr::mutate(freq_detected = detected / sum(detected))



    ethnicity_admitted <- selected_vars %>%
      dplyr::select(
        person_id,
        result_status,
        arrival_dt_tm,
        admission_dt_tm,
        disch_dt_tm,
        event_result_txt,
        ethnicity,
        gender,
        orderable_type_code_desc_txt,
        covid_snomed_code1_desc_confirmed
      ) %>%
      dplyr::filter(arrival_dt_tm >= end_date |
                      admission_dt_tm <= start_date) %>%
      tidyr::replace_na(list(ethnicity = "Missing")) %>%
      dplyr::filter(
        event_result_txt == "DETECTED" |
          covid_snomed_code1_desc_confirmed == "Corona virus detected"
      ) %>%
      dplyr::mutate(arrival_admission_time = difftime(admission_dt_tm, arrival_dt_tm,  units = "hours")) %>%
      dplyr::mutate(ae_admitted = dplyr::if_else(arrival_admission_time <= 48, TRUE, FALSE)) %>%
      dplyr::filter(ae_admitted == TRUE) %>%
      dplyr::group_by(ethnicity) %>%
      dplyr::summarise(admitted = n()) %>%
      dplyr::mutate(freq_admitted = admitted / sum(admitted))

    ethnicity_died <- selected_vars %>%
      dplyr::select(
        person_id,
        result_status,
        arrival_dt_tm,
        admission_dt_tm,
        disch_dt_tm,
        event_result_txt,
        ethnicity,
        gender,
        orderable_type_code_desc_txt,
        covid_snomed_code1_desc_confirmed,
        discharge_method_desc
      ) %>%
      tidyr::replace_na(list(ethnicity = "Missing")) %>%
      dplyr::filter(
        event_result_txt == "DETECTED" |
          covid_snomed_code1_desc_confirmed == "Corona virus detected"
      ) %>%
      dplyr::filter(arrival_dt_tm >= end_date |
                      admission_dt_tm <= start_date) %>%
      dplyr::mutate(arrival_admission_time = difftime(admission_dt_tm, arrival_dt_tm,  units = "hours")) %>%
      dplyr::mutate(ae_admitted = dplyr::if_else(arrival_admission_time <= 48, TRUE, FALSE)) %>%
      dplyr::filter(ae_admitted == TRUE &
                      discharge_method_desc == "Patient Died") %>%
      dplyr::group_by(ethnicity) %>%
      dplyr::summarise(admitted_died = n()) %>%
      dplyr::mutate(freq_admitted_died = admitted_died / sum(admitted_died))


    covid_tested_detected <-
      dplyr::full_join(ethnicity_tested, ethnicity_detected)

    covid_test_detect_admitted <-
      dplyr::full_join(covid_tested_detected, ethnicity_admitted)

    covid_test_detect_adm_died <-
      dplyr::full_join(covid_test_detect_admitted, ethnicity_died) %>%
      dplyr::select(
        ethnicity,
        tested,
        freq_tested,
        detected,
        freq_detected,
        admitted,
        freq_detected,
        admitted_died,
        freq_admitted_died
      )

    melted_cvid_dt_tst <- covid_test_detect_adm_died %>%
      dplyr::select(ethnicity, tested, detected, admitted, admitted_died) %>%
      tidyr::gather(key = "Event",
                    value = "Value",
                    tested,
                    detected,
                    admitted,
                    admitted_died) %>%
      tidyr::drop_na()

    melted_cvid_dt_tst$Event <-
      factor(
        melted_cvid_dt_tst$Event,
        levels = c("tested", "detected", "admitted", "admitted_died"),
        labels = c("Tested", "Detected", "Admitted", "Died")
      )

    title_stub <-
      ": Number of patients covid tested, detected and admitted across ethnicity groups,"
    hospital_name <- "ICHT"
    start_date_title <-
      format(as.Date(start_date), format = "%d %B %Y")
    end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
    chart_title <-
      paste(hospital_name,
            title_stub,
            start_date_title,
            " to ",
            end_date_title)



    plot <-
      ggplot2::ggplot(melted_cvid_dt_tst,
                      ggplot2::aes(ethnicity, y = Value, group = Event)) +
      ggplot2::geom_line(aes(color = Event)) +
      ggplot2::geom_point(aes(color = Event)) +
      ggplot2::scale_color_manual(values = c('orange', 'blue', 'red', 'black')) +
      ggplot2::scale_y_continuous(limits = c(0, NA),
                                  breaks = seq(0, round(max(
                                    melted_cvid_dt_tst$Value
                                  )), by = 30)) +
      #scale_color_brewer(palette = "Paired") +
      theme_minimal() +
      xlab("Ethnicity") +
      ggplot2::labs(
        title = chart_title,
        subtitle = "Number of patients of covid AE tested, detected and admitted across ethnicity groups\nNote: (i)results are intended for management information only; (ii) Counts include only patients that were addmited through AE",
        y = "Covid tested, detected, admitted and died n",
        x = "Ethnnicity",
        caption = "Source: GDHU, Imperial College"
      ) +
      theme(axis.text.x = element_text(angle = 90)) +
      ylab('Counts of AE tested, detected, admitted, died') +
      theme(legend.position = "bottom")

    plot

    if (plot_chart == TRUE) {
      plot

    } else{
      covid_tested_detect_adm <-
        plot %>% purrr::pluck('data') %>% dplyr::select(ethnicity, Event, Value)
      covid_tested_detect_adm
    }


  }


