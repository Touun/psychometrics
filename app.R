# ===================================================================
# Load necessary libraries
# ===================================================================
library(shiny)
library(httr2)      
library(jsonlite)
library(shinyjs)
library(tools)
library(DBI)
library(RSQLite)

if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# ===================================================================
# UI (User Interface) - No changes needed here
# ===================================================================
ui <- fluidPage(
  useShinyjs(),
  # Add CSS for styling the report, similar to the Rails view
  tags$head(
    tags$style(HTML("
      body { background-color: #f8f9fa; }
      .container-fluid { max-width: 1400px; }
      .analysis-section {
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
        background-color: #ffffff;
        box-shadow: 0 4px 6px rgba(0,0,0,0.05);
      }
      .trait-details {
        margin-bottom: 15px;
        border-left: 3px solid #007bff;
        padding-left: 15px;
      }
      .trait-details[open] > summary {
        font-weight: 600;
        margin-bottom: 10px;
      }
      .trait-details summary {
        cursor: pointer;
        outline: none;
      }
      .trait-content {
        padding-left: 20px;
        margin-top: 10px;
      }
      .facet {
        margin-bottom: 18px;
        padding-left: 15px;
      }
      .facet blockquote {
        background: #f1f3f5;
        border-left: 5px solid #ced4da;
        margin: 10px 0;
        padding: 10px 20px;
        font-style: italic;
        color: #495057;
      }
      .facet ul {
        list-style-type: disc;
        padding-left: 20px;
      }
      .badge {
        display: inline-block;
        padding: .35em .65em;
        font-size: 85%;
        font-weight: 700;
        line-height: 1;
        text-align: center;
        white-space: nowrap;
        vertical-align: baseline;
        border-radius: .35rem;
        color: #fff;
      }
      .btn-primary {
        background-color: #007bff;
        border-color: #007bff;
      }
    "))
  ),
  titlePanel("Psycholinguistic Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Profile Management"),
      selectInput("profile_selector", "Load Saved Profile:", choices = c("None" = "")),
      hr(),
      tabsetPanel(
        id = "analysis_type",
        tabPanel(
          "Twitter Analysis",
          br(),
          textInput("twitter_handle", "Enter Twitter Handle (without @):", placeholder = "e.g., grok"),
          actionButton("run_twitter_analysis", "Analyze Handle", class = "btn-primary")
        ),
        tabPanel(
          "Text Corpus Analysis",
          br(),
          textAreaInput("corpus_text", "Paste Text Corpus Here:", rows = 10, placeholder = "Paste your text..."),
          actionButton("run_corpus_analysis", "Analyze Corpus", class = "btn-primary")
        )
      )
    ),

    mainPanel(
      width = 9,
      h4(textOutput("analysis_header")),
      div(
        style = "display: flex; gap: 10px; margin-bottom: 15px;",
        disabled(actionButton("save_profile", "Save Current Analysis", icon = icon("save")))
      ),
      uiOutput("formatted_analysis_output"),
      tags$details(
        style="margin-top: 25px;",
        tags$summary("Raw API Response (for debugging)"),
        verbatimTextOutput("raw_output")
      )
    )
  )
)

# ===================================================================
# Server Logic
# ===================================================================

# ===================================================================
# Server Logic
# ===================================================================
server <- function(input, output, session) {
  
  # --- Database Setup (Unchanged) ---
  DB_FILE <- "profiles_database.sqlite"
  db <- dbConnect(RSQLite::SQLite(), DB_FILE)
  if (!dbExistsTable(db, "profiles")) {
    dbExecute(db, "CREATE TABLE profiles (id TEXT PRIMARY KEY, analysis_json TEXT);")
  }
  load_profiles_from_db <- function() {
    df <- dbGetQuery(db, "SELECT * FROM profiles;")
    if (nrow(df) == 0) return(list())
    profiles_list <- lapply(df$analysis_json, function(json) { fromJSON(json) })
    names(profiles_list) <- df$id
    return(profiles_list)
  }
  initial_profiles <- load_profiles_from_db()
  
  # --- Reactive values (Unchanged) ---
  rv <- reactiveValues(
    saved_profiles = initial_profiles,
    current_analysis = NULL,
    current_analysis_id = NULL
  )
  if (length(initial_profiles) > 0) {
    updateSelectInput(session, "profile_selector",
                      choices = c("None" = "", names(initial_profiles)))
  }
  
  # ===================================================================
  # INTEGRATED PLUMBER LOGIC
  # ===================================================================
  
  # --- Helper function to read the system prompt file (Unchanged) ---
  get_system_prompt <- function(filepath = "system_prompt.txt") {
    if (!file.exists(filepath)) {
      stop("System prompt file not found: ", filepath)
    }
    paste(readLines(filepath, warn = FALSE), collapse = "\n")
  }
  
  # --- Core analysis function for Twitter (Unchanged) ---
  perform_xai_analysis <- function(twitter_handle) {
    xai_api_key <- Sys.getenv("XAI_API_KEY")
    if (xai_api_key == "") {
      return(list(error = "XAI_API_KEY environment variable not set.", details = "Please set the XAI_API_KEY in your .Renviron file and restart the R session."))
    }
    system_prompt <- tryCatch({ get_system_prompt() }, error = function(e) { NULL })
    if (is.null(system_prompt)) {
      return(list(error = "Could not read system_prompt.txt file.", details = "Ensure the system_prompt.txt file is in the same directory as the app."))
    }
    xai_api_url <- "https://api.x.ai/v1/chat/completions"
    request_body <- list(
      model = "grok-4-latest",
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = paste0("You have been provided with a transcript of up to 29 recent, original tweets and replies from @", twitter_handle, " posted within the last 12 months. Perform a step-by-step analysis of this transcript."))
      ),
      search_parameters = list(
        mode = "on",
        sources = list(list(type = "x", included_x_handles = list(twitter_handle))),
        max_search_results = 29,
        return_citations = FALSE,
        query = "-is:retweet -is:quote -has:links"
      ),
      response_format = list(type = "json_object"),
      stream = FALSE
    )
    response <- tryCatch({
      request(xai_api_url) |>
        req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", xai_api_key)) |>
        req_body_json(request_body) |>
        req_timeout(240) |>
        req_perform()
    }, error = function(e) {
      list(error_obj = e)
    })
    if ("error_obj" %in% names(response)) {
      return(list(error = "Failed to connect to the x.ai API.", details = response$error_obj$message))
    }
    if (resp_status(response) >= 400) {
      return(list(error = paste("x.ai API Error:", resp_status(response)), details = resp_body_string(response)))
    }
    response_content <- resp_body_json(response)
    json_string_payload <- response_content$choices[[1]]$message$content
    tryCatch({
      fromJSON(json_string_payload, flatten = TRUE)
    }, error = function(e) {
      list(error = "Failed to parse the JSON response from x.ai.", details = e$message, raw_response = json_string_payload)
    })
  }
  
  # --- NEW: Core analysis function for Text Corpus ---
  # This function integrates the logic from your second Plumber API.
  perform_corpus_analysis <- function(corpus_text) {
    # --- Validation from Plumber API ---
    word_count <- length(gregexpr("\\S+", corpus_text)[[1]])
    if (word_count > 3000) {
      return(list(
        error = "Corpus exceeds the 3000-word limit.",
        details = paste("Current word count:", word_count)
      ))
    }
    
    # --- Configuration ---
    xai_api_key <- Sys.getenv("XAI_API_KEY")
    if (xai_api_key == "") {
      return(list(error = "XAI_API_KEY environment variable not set.", details = "Please set the XAI_API_KEY in your .Renviron file and restart the R session."))
    }
    system_prompt <- tryCatch({ get_system_prompt() }, error = function(e) { NULL })
    if (is.null(system_prompt)) {
      return(list(error = "Could not read system_prompt.txt file.", details = "Ensure the system_prompt.txt file is in the same directory as the app."))
    }
    xai_api_url <- "https://api.x.ai/v1/chat/completions"
    
    # --- Build the Request Body for x.ai API (Corpus Version) ---
    user_prompt <- sprintf(
      "You have been provided with a corpus of text. Perform a step-by-step analysis of this transcript.\n\n---BEGIN CORPUS---\n%s\n---END CORPUS---",
      corpus_text
    )
    request_body <- list(
      model = "grok-4-latest",
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_prompt)
      ),
      response_format = list(type = "json_object"),
      stream = FALSE
    )
    
    # --- Make the API Call to x.ai using httr2 ---
    response <- tryCatch({
      request(xai_api_url) |>
        req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", xai_api_key)) |>
        req_body_json(request_body) |>
        req_timeout(240) |>
        req_perform()
    }, error = function(e) {
      list(error_obj = e)
    })
    if ("error_obj" %in% names(response)) {
      return(list(error = "Failed to connect to the x.ai API.", details = response$error_obj$message))
    }
    if (resp_status(response) >= 400) {
      return(list(error = paste("x.ai API Error:", resp_status(response)), details = resp_body_string(response)))
    }
    response_content <- resp_body_json(response)
    json_string_payload <- response_content$choices[[1]]$message$content
    tryCatch({
      fromJSON(json_string_payload, flatten = TRUE)
    }, error = function(e) {
      list(error = "Failed to parse the JSON response from x.ai.", details = e$message, raw_response = json_string_payload)
    })
  }
  
  # --- Twitter Analysis Event Handler (Unchanged) ---
  observeEvent(input$run_twitter_analysis, {
    handle <- trimws(input$twitter_handle)
    if (nchar(handle) == 0) {
      showNotification("Please enter a Twitter handle.", type = "warning")
      return()
    }
    id <- showNotification(
      "Analysis started, this usually takes between 2-3 minutes.",
      duration = NULL, closeButton = FALSE, type = "message"
    )
    on.exit({
      removeNotification(id)
      shinyjs::enable("run_twitter_analysis")
      shinyjs::html("run_twitter_analysis", "Analyze Handle")
    }, add = TRUE)
    shinyjs::disable("run_twitter_analysis")
    shinyjs::html("run_twitter_analysis", "Analyzing...")
    rv$current_analysis <- NULL
    rv$current_analysis_id <- NULL
    analysis_result <- perform_xai_analysis(handle)
    if (!is.null(analysis_result$error)) {
      rv$current_analysis <- analysis_result
    } else {
      rv$current_analysis <- analysis_result
      rv$current_analysis_id <- paste0("@", handle)
      shinyjs::enable("save_profile")
    }
  })
  
  # ===================================================================
  # MODIFIED & UNMODIFIED LOGIC
  # ===================================================================
  
  # --- MODIFIED: Text Corpus Analysis Event Handler ---
  observeEvent(input$run_corpus_analysis, {
    text <- trimws(input$corpus_text)
    if (nchar(text) == 0) {
      showNotification("Please paste text into the corpus field.", type = "warning")
      return()
    }
    id <- showNotification(
      "Analysis started, this can take a few minutes.",
      duration = NULL, closeButton = FALSE, type = "message"
    )
    on.exit({
      removeNotification(id)
      shinyjs::enable("run_corpus_analysis")
      shinyjs::html("run_corpus_analysis", "Analyze Corpus")
    }, add = TRUE)
    shinyjs::disable("run_corpus_analysis")
    shinyjs::html("run_corpus_analysis", "Analyzing...")
    rv$current_analysis <- NULL
    rv$current_analysis_id <- NULL
    
    # Call the new internal function
    analysis_result <- perform_corpus_analysis(text)
    
    # Process the result from the internal function
    if (!is.null(analysis_result$error)) {
      rv$current_analysis <- analysis_result
    } else {
      rv$current_analysis <- analysis_result
      rv$current_analysis_id <- "Text Corpus Analysis"
      shinyjs::enable("save_profile")
    }
  })
  
  # --- Header Logic (Unchanged) ---
  output$analysis_header <- renderText({
    if (!is.null(rv$current_analysis_id)) {
      paste("Analysis Result for:", rv$current_analysis_id)
    } else {
      "Analysis Result"
    }
  })
  
  # --- Profile Management Logic (Unchanged) ---
  observeEvent(input$save_profile, {
    req(rv$current_analysis)
    suggested_name <- rv$current_analysis_id
    if (suggested_name == "Text Corpus Analysis") {
      suggested_name <- paste("Corpus Analysis -", format(Sys.time(), "%Y-%m-%d %H:%M"))
    }
    showModal(modalDialog(
      title = "Save Analysis Profile",
      textInput("profile_name_input", "Enter a name for this profile:", value = suggested_name),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_save", "Save Profile", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })
  observeEvent(input$confirm_save, {
    profile_name <- trimws(input$profile_name_input)
    if (nchar(profile_name) == 0) {
      showNotification("Profile name cannot be empty.", type = "error")
      return()
    }
    rv$saved_profiles[[profile_name]] <- rv$current_analysis
    json_data <- toJSON(rv$current_analysis, auto_unbox = TRUE)
    dbExecute(db,
              "INSERT INTO profiles (id, analysis_json) VALUES (?, ?) ON CONFLICT(id) DO UPDATE SET analysis_json=excluded.analysis_json;",
              params = list(profile_name, json_data))
    updateSelectInput(session, "profile_selector",
                      choices = c("None" = "", names(rv$saved_profiles)),
                      selected = profile_name)
    showNotification(paste("Profile '", profile_name, "' saved!"), type = "message")
    removeModal()
  })
  observeEvent(input$profile_selector, {
    selected_id <- input$profile_selector
    req(selected_id)
    rv$current_analysis_id <- selected_id
    rv$current_analysis <- rv$saved_profiles[[selected_id]]
    shinyjs::enable("save_profile")
  })
  
  # --- Helper function for creating styled score badges (Unchanged) ---
  score_badge <- function(score) {
    score_lower <- tolower(score)
    color <- switch(score_lower,
                    "high" = "#28a745",
                    "medium" = "#ffc107",
                    "low" = "#dc3545",
                    "#6c757d"
    )
    text_color <- if (score_lower == "medium") "#000" else "#fff"
    tags$span(class = "badge", style = paste0("background-color:", color, "; color:", text_color,";"), score)
  }
  
  # --- Output Rendering (Unchanged) ---
  output$formatted_analysis_output <- renderUI({
    if (is.null(rv$current_analysis)) {
      return(wellPanel("Run an analysis or load a profile to see results here."))
    }
    if (!is.null(rv$current_analysis$error)) {
      return(
        div(class="analysis-section", style="background-color: #f8d7da; border-color: #f5c6cb;",
            h5(strong("Error Encountered")),
            p(rv$current_analysis$error),
            tags$pre(rv$current_analysis$details)
        )
      )
    }
    report <- rv$current_analysis
    report_elements <- list()
    if (!is.null(report$personality_profile)) {
      personality_content <- lapply(names(report$personality_profile), function(trait_name) {
        trait_data <- report$personality_profile[[trait_name]]
        facets_content <- lapply(names(trait_data$facets), function(facet_name) {
          facet_data <- trait_data$facets[[facet_name]]
          evidence_content <- tags$ul(
            lapply(facet_data$evidence, function(e) { tags$li(paste0("\"", e, "\"")) })
          )
          tags$div(class = "facet",
                   strong(toTitleCase(facet_name), ":"), " ", score_badge(facet_data$score),
                   tags$p(tags$em(facet_data$analysis)),
                   tags$blockquote(strong("Evidence:"), evidence_content)
          )
        })
        tags$details(class = "trait-details", open = NA,
                     tags$summary(strong(toTitleCase(trait_name))),
                     tags$div(class = "trait-content",
                              tags$p(strong("Overall Assessment:"), trait_data$assessment),
                              tags$h5("Facets:"),
                              tagList(facets_content)
                     )
        )
      })
      report_elements <- append(report_elements, list(
        tags$div(class = "analysis-section",
                 h4("Big Five Personality Profile"),
                 tagList(personality_content)
        )
      ))
    }
    if (!is.null(report$entrepreneurship_assessment)) {
      report_elements <- append(report_elements, list(
        tags$div(class = "analysis-section",
                 h4("Entrepreneurship Assessment"),
                 tags$p(report$entrepreneurship_assessment$summary)
        )
      ))
    }
    if (!is.null(report$red_flag_analysis)) {
      report_elements <- append(report_elements, list(
        tags$div(class = "analysis-section",
                 h4("Red Flag Analysis"),
                 tags$p(report$red_flag_analysis$overall_conclusion)
        )
      ))
    }
    do.call(tagList, report_elements)
  })
  output$raw_output <- renderPrint({
    req(rv$current_analysis)
    jsonlite::toJSON(rv$current_analysis, pretty = TRUE, auto_unbox = TRUE)
  })
}

# ===================================================================
# Run the application
# ===================================================================
shinyApp(ui = ui, server = server)