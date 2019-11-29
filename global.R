library(shiny)
library(shinydashboard)
`%then%` <- shiny:::`%OR%`

fedTaxBrackets <- list(
  "Single" = list(
    list(cutoff = 3800, rate = 0.1, base = 0),
    list(cutoff = 13500, rate = 0.12, base = 970),
    list(cutoff = 43275, rate = 0.22, base = 4543),
    list(cutoff = 88000, rate = 0.24, base = 14382.5),
    list(cutoff = 164525, rate = 0.32, base = 32748.5),
    list(cutoff = 207900, rate = 0.35, base = 46628.5),
    list(cutoff = 514100, rate = 0.37, base = 153798.5)
  ),
  "Married" = list(
    list(cutoff = 11800, rate = 0.1, base = 0),
    list(cutoff = 31200, rate = 0.12, base = 1940),
    list(cutoff = 90750, rate = 0.22, base = 9086),
    list(cutoff = 180200, rate = 0.24, base = 28765),
    list(cutoff = 333250, rate = 0.32, base = 65497),
    list(cutoff = 420000, rate = 0.35, base = 93257),
    list(cutoff = 624150, rate = 0.37, base = 164709.50)
  )
)

calculateFedTax <- function(taxableAmount, maritalStatus = c("Single", "Married")) {
  maritalStatus <- match.arg(maritalStatus)

  taxBrackets <- fedTaxBrackets[[maritalStatus]]

  for (i in rev(seq_along(taxBrackets))) {
    bracket <- taxBrackets[[i]]
    if (taxableAmount >= bracket$cutoff) {
      bracketTax <- (taxableAmount - bracket$cutoff) * bracket$rate
      return(bracketTax + bracket$base)
    }
  }

  return(0)
}

formatNumber <- function(number, decimals = 0) {
  paste0("$", format(round(number, decimals), nsmall = decimals))
}
