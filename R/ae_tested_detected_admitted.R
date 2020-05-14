#' ae_covid_tested_detected_admitted
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
ae_covid_tested_detected_admittted <- function(data, start_date, end_date, plot_chart = TRUE, hospital_name){


  covid_selected_vars <- data %>%
    dplyr::select(
      person_id,
      arrival_dt_tm,
      admission_dt_tm,
      covid_snomed_code1_desc_confirmed,
      event_result_status,
      result_status
    ) %>%
    #dplyr::filter(arrival_dt_tm <= end_date & admission_dt_tm >= start_date) %>%
    dplyr::arrange(arrival_dt_tm)

  covid_tested_ae_weekly <- covid_selected_vars %>%
    dplyr::select(person_id,
                  arrival_dt_tm,
                  result_status,
                  covid_snomed_code1_desc_confirmed) %>%
    dplyr::mutate(Weekdays = lubridate::wday(arrival_dt_tm, label = TRUE, abbr = FALSE)) %>%
    dplyr::filter(result_status == "suspected" | result_status == "negative" | result_status == "positive") %>%
    dplyr::group_by(Weekdays) %>%
    dplyr::summarise(Counts_tested = n()) %>%
    dplyr::mutate(Freq_tested = Counts_tested / sum(Counts_tested)) %>%
    tidyr::drop_na()


  covid_detected_ae_weekly <- covid_selected_vars %>%
    dplyr::select(person_id,
                  arrival_dt_tm,
                  result_status,
                  covid_snomed_code1_desc_confirmed) %>%
    dplyr::mutate(Weekdays = lubridate::wday(arrival_dt_tm, label = TRUE, abbr = FALSE)) %>%
    dplyr::filter(result_status == "positive") %>%
    dplyr::group_by(Weekdays) %>%
    dplyr::summarise(Counts_detected = n()) %>%
    dplyr::mutate(Freq_detected = Counts_detected / sum(Counts_detected)) %>%
    tidyr::drop_na()

  covid_detected_admitted_weekly <- covid_selected_vars %>%
    dplyr::select(person_id,
                  arrival_dt_tm,
                  admission_dt_tm,
                  result_status,
                  covid_snomed_code1_desc_confirmed) %>%
    dplyr::mutate(Weekdays = lubridate::wday(arrival_dt_tm, label = TRUE, abbr = FALSE)) %>%
    dplyr::mutate(ae_admissions = difftime(admission_dt_tm, arrival_dt_tm, units = "hours")) %>%
    dplyr::mutate(ae_adm_bol = dplyr::if_else(ae_admissions <= 48, TRUE, FALSE)) %>%
    dplyr::filter(result_status == "positive" & ae_adm_bol == TRUE) %>%
    dplyr::group_by(Weekdays) %>%
    dplyr::summarise(Counts_detected_admitted = n()) %>%
    dplyr::mutate(Freq_counts_detected_admitted = Counts_detected_admitted / sum(Counts_detected_admitted)) %>%
    tidyr::drop_na()

  numbers_only_test_detect <- left_join(covid_tested_ae_weekly, covid_detected_ae_weekly) %>%
    dplyr::select(Weekdays, Counts_tested, Freq_tested,Counts_detected, Freq_detected)

  numbers_only <- left_join(numbers_only_test_detect, covid_detected_admitted_weekly, by = c('Weekdays')) %>%
    dplyr::select(Weekdays, Counts_tested, Freq_tested,Counts_detected,
                  Freq_detected,Counts_detected_admitted,Freq_counts_detected_admitted)


  melt_for_plt <- tidyr::gather(numbers_only, key = "Event", value = Value, Counts_tested, Counts_detected, Counts_detected_admitted) %>%
    dplyr::select(Weekdays, Event, Value)



  #Set title


  title_stub <-
    ": Covid tested, detected and admitted,\n"
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


  plot <- ggplot2::ggplot(melt_for_plt, ggplot2::aes(Weekdays, Value, group = Event)) +
    #ggplot2::geom_bar(stat = "identity", alpha = 0.4, width = 0.5, fill = "slateblue4") +
    ggplot2::geom_line(ggplot2::aes(linetype = Event, color = Event), size = 1.0) +
    ggplot2::geom_point(ggplot2::aes(shape = Event), size = 1.0) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::scale_shape_manual(values = c(7,6,5)) +
    ggplot2::scale_linetype_manual(values = c("solid", "solid", "twodash")) +
    ggplot2::scale_color_manual(values = c("blue", "orange", "red")) +
    ggplot2::theme_bw() +
    ggplot2::xlim("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday") +
    ggplot2::labs(title = chart_title,
                  subtitle = "Counts of number of patients: tested, detected and admitted\nNote: results are intended for management information only",
                  y = "Counts", x = "Days of the week", caption = "Source: GDHU, Imperial College") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
                   plot.title = ggplot2::element_text(size = 10, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 9),
                   legend.position = "bottom", legend.box = "horizontal")


  plot

  if(plot_chart == TRUE){

    plot

  }else{

    numbers_only

  }

}








