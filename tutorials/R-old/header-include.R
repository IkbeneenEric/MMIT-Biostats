library(learnr)
library(tidyverse)
library(openintro)
library(emo)
library(glue)
library(here)
library(shiny)
shiny_url <- "https://oferengel-posit.shinyapps.io/"
submit_hash_url <- "https://forms.gle/ajEDfePc1jcTukyB7"

gradethis::gradethis_setup(pass.praise = TRUE, fail.encourage = TRUE)
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

# Hash generation helpers
# Should ideally be loaded from the imstutorials package when it exists
is_server_context <- function(.envir) {
  # We are in the server context if there are the follow:
  # * input - input reactive values
  # * output - shiny output
  # * session - shiny session
  #
  # Check context by examining the class of each of these.
  # If any is missing then it will be a NULL which will fail.
  
  inherits(.envir$input, "reactivevalues") &
    inherits(.envir$output, "shinyoutput") &
    inherits(.envir$session, "ShinySession")
}

check_server_context <- function(.envir) {
  if (!is_server_context(.envir)) {
    calling_func <- deparse(sys.calls()[[sys.nframe() - 1]])
    err <- paste0("Function `", calling_func, "`", " must be called from an Rmd chunk where `context = \"server\"`")
    stop(err, call. = FALSE)
  }
}


encoder_logic <- function(strip_output = FALSE) {
  p <- parent.frame()
  check_server_context(p)
  # Make this var available within the local context below
  assign("strip_output", strip_output, envir = p)
  # Evaluate in parent frame to get input, output, and session
  local(
    {
      encoded_txt <- shiny::eventReactive(
        input$hash_generate,
        {
          # shiny::getDefaultReactiveDomain()$userData$tutorial_state
          state <- learnr:::get_tutorial_state()
          shiny::validate(shiny::need(length(state) > 0, "No progress yet."))
          shiny::validate(shiny::need(nchar(input$name) > 0, "No name entered."))
          shiny::validate(shiny::need(nchar(input$studentID) > 0, "Please enter your student ID"))
          user_state <- purrr::map_dfr(state, identity, .id = "label")
          user_state <- dplyr::group_by(user_state, label, type, correct)
          user_state <- dplyr::summarize(
            user_state,
            answer = list(answer),
            timestamp = dplyr::first(timestamp),
            .groups = "drop"
          )
          user_state <- dplyr::relocate(user_state, correct, .before = timestamp)
          user_info <- tibble(
            label = c("student_name", "student_id"),
            type = "identifier",
            answer = as.list(c(input$name, input$studentID)),
            timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z", tz = "UTC")
          )
          learnrhash::encode_obj(bind_rows(user_info, user_state))
        }
      )
      output$hash_output <- shiny::renderText(encoded_txt())
    },
    envir = p
  )
}

hash_encoder_ui <- {
  shiny::div("If you have completed this tutorial and are happy with all of your", "solutions, please enter your identifying information, then click the button below to generate your hash", shiny::textInput("name", "What's your name?"), shiny::textInput("studentID", "What is your student ID (Gebruikersnaam  s-/p-nummer)?"), shiny::renderText({
    input$caption
  }), )
}




question <- function (
    text, ..., 
    type = c("auto", "single", "multiple", "learnr_radio",  "learnr_checkbox", "learnr_text", "learnr_numeric"), 
    correct = "Correct!", 
    incorrect = "Incorrect", 
    try_again = NULL, 
    message = NULL, 
    post_message = NULL, 
    loading = NULL, 
    submit_button = rlang::missing_arg(), 
    try_again_button = rlang::missing_arg(), 
    allow_retry = TRUE, 
    random_answer_order = TRUE, 
    options = list()
) {
  # browser()
  correct <- if_else(correct != "Correct!", correct, random_praise())
  incorrect <- if_else(incorrect != "Incorrect", incorrect, random_encouragement())
  
  learnr::question (
    text = text, ..., 
    type = type, 
    correct = correct, 
    incorrect = incorrect,
    try_again = try_again, 
    message = message, 
    post_message = post_message, 
    loading = loading, 
    submit_button = submit_button, 
    try_again_button = try_again_button, 
    allow_retry = allow_retry, 
    random_answer_order = random_answer_order, 
    options = options
  ) 
  
}



here <- function(...) {
  v_args <- list(...)
  v_args_ext <- c(list("tutorials"), v_args)
  case_when(
    file.exists(here::here(v_args)) ~ here::here(v_args),
    file.exists(here::here(v_args_ext)) ~ here::here(v_args_ext)
    )
}