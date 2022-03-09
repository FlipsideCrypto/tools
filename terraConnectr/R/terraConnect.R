#' <Add Title>
#'
#' <Add Description>
#'
#' @importFrom reactR createReactShinyInput
#' @importFrom htmltools htmlDependency tags
#'
#' @export
terraConnect <- function(inputId) {
  reactR::createReactShinyInput(
    inputId,
    "terraConnect",
    htmltools::htmlDependency(
      name = "terraConnect-input",
      version = "1.0.0",
      src = "www/terraConnectr/terraConnect",
      package = "terraConnectr",
      script = "terraConnect.js"
    ),
    default = 0,
    list(),
    htmltools::tags$div
  )
}
