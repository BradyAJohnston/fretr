#' Bin and Summarise States for their Transitions
#'
#' @param data Data frame containing data
#' @param state Column that contains state information.
#' @param time Column that contains frame information.
#' @param frame_time Time per frame.
#'
#' @return `tibble()` with summarised transition data.
#' @export
#'
bin_transitions <- function(data, state, time, frame_time = 0.2) {
  data %>%
    dplyr::filter({{ state }} <= 1.2 & {{ state }} >= -0.2) %>%
    dplyr::mutate(
      from = {{ state }},
      to = dplyr::lead(from),
      new_state = dplyr::lag(from) != from,
      new_state = dplyr::if_else(is.na(new_state), FALSE, new_state),
      state_no = 1 + cumsum(new_state)
    ) %>%
    dplyr::group_by(rna, mol, state_no) %>%
    dplyr::summarise(
      frames = n(),
      time = n() * frame_time,
      state_from = last(from),
      state_to = last(to)
    )
}
