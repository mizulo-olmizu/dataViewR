#' View a data frame in the Data Viewer application
#'
#' This function launches the Data Viewer application and displays the given data frame.
#'
#' @param data A data frame to view.
#' @param name An optional name for the data frame in the viewer.
#' @param use_parquet A logical value indicating whether to use the Parquet format. Defaults to FALSE.
#' @param port The port number for the Data Viewer application. Defaults to 3000.
#' @param timeout The timeout in seconds for the application to start. Defaults to 3.
#' @param app_path The path to the Data Viewer application executable.
#'
#' @importFrom httr GET POST status_code content
#' @importFrom arrow write_parquet
#' @importFrom processx process
#'
#' @export
launch_data_viewer <- function(data, name = NULL, use_parquet = FALSE, port = 3000, timeout = 3, app_path = "/Applications/data_viewer.app/Contents/MacOS/data_viewer") {
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }

  if (is.null(name)) {
    name <- deparse(substitute(data))
  }

  temp_file <- tempfile(fileext = if (use_parquet) ".parquet" else ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  if (use_parquet) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("The 'arrow' package is required to use Parquet format. Please install it.")
    }
    arrow::write_parquet(data, temp_file)
  } else {
    utils::write.csv(data, temp_file, row.names = FALSE)
  }

  health_check_url <- paste0("http://127.0.0.1:", port, "/health-check")
  update_data_url <- paste0("http://127.0.0.1:", port, "/update-data")

  health_check <- function() {
    tryCatch(
      {
        httr::GET(health_check_url)
      },
      error = function(e) {
        NULL
      }
    )
  }

  update_data <- function() {
    body <- list(input = temp_file, name = name)
    httr::POST(update_data_url, body = body, encode = "json")
  }

  if (is.null(health_check())) {
    processx::process$new(app_path)
    start_time <- Sys.time()
    while (difftime(Sys.time(), start_time, units = "secs") < timeout) {
      if (!is.null(health_check())) {
        break
      }
      Sys.sleep(0.1)
    }
  }

  response <- update_data()
  if (httr::status_code(response) != 200) {
    stop("Failed to update data: ", httr::content(response, "text"))
  }

  invisible(NULL)
}
