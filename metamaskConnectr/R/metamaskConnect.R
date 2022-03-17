#' <Add Title>
#'
#' <Add Description>
#'
#' @importFrom reactR createReactShinyInput
#' @importFrom htmltools htmlDependency tags
#'
#' @export
metamaskConnect <- function(inputId, default = "") {
  reactR::createReactShinyInput(
    inputId,
    "metamaskConnect",
    htmltools::htmlDependency(
      name = "metamaskConnect-input",
      version = "1.0.0",
      src = "www/metamaskConnectr/metamaskConnect",
      package = "metamaskConnectr",
      script = "metamaskConnect.js"
    ),
    default,
    list(),
    htmltools::tags$div
  )
}
