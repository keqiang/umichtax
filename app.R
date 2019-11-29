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

ui <- dashboardPage(
  dashboardHeader(title = "UofM Tax Calculator"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    singleton(
      tags$head(
        tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "css/main.css"
        )
      )
    ),
    tags$h4(
      style = "color: red;",
      "EFFECTIVE January 1, 2019. The result is only an estimation of your tax. Please use with caution."
    ),
    helpText(
      "This website is based on http://www.finance.umich.edu/news/calculating-your-taxes"
    ),
    fluidRow(
      column(
        width = 6,
        fluidRow(
          box(
            width = 12,
            status = "warning",
            title = "Pay Stub Information",
            textOutput("status"),
            fluidRow(
              column(
                width = 6,
                radioButtons(
                  "payrollType",
                  label = "Payroll Type",
                  choices = c("Monthly", "Biweekly"),
                  selected = "Monthly",
                  inline = TRUE
                )
              ),
              column(
                width = 6,
                radioButtons(
                  "maritalStatus",
                  label = "Maital Status",
                  choices = c("Single", "Married"),
                  selected = "Single",
                  inline = TRUE
                )
              )
            ),
            numericInput(
              "fedTaxableGross",
              label = "Fed Taxable Gross (not total gross)",
              value = 4500,
              min = 0,
              step = 1
            ),
            helpText("Displayed as 'FED TAXABLE GROSS' in the Paycheck Summary portion of your pay stub"),
            tags$hr(),
            numericInput(
              "ficaTaxableGross",
              label = "FICA Taxable Gross",
              value = 4000,
              min = 0,
              step = 1
            ),
            helpText("Displayed as 'FICA TAXABLE GROSS' in the Paycheck Summary portion of your pay stub"),
            tags$hr(),
            numericInput(
              "numOfExemptions",
              label = "Number of Exemptions",
              value = 0,
              min = 0,
              step = 1
            ),
            helpText("Displayed at the top right of your pay stub. Each exemption claimed will result in a $4,200 deduction annually."),
            tags$hr(),
            numericInput(
              "afterTaxDeductions",
              label = "After-tax Deductions",
              value = 0,
              min = 0,
              step = 1
            ),
            helpText("Displayed at the center of your pay stub")
          )
        )
      ),
      column(
        width = 6,
        fluidRow(
          box(
            width = 12,
            status = "success",
            title = "Income Summary",
            fluidRow(
              valueBoxOutput("fedTaxableGross"),
              valueBoxOutput("annualFedTaxableGrossAfterExemptionsInfo"),
              valueBoxOutput("netPayInfo")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "danger",
            title = "Federal Tax",
            fluidRow(
              valueBoxOutput("annualFedTaxInfo", width = 6),
              valueBoxOutput("perPayrollFexTaxInfo", width = 6)
            )
          ),
          box(
            width = 12,
            status = "warning",
            title = "State Tax",
            fluidRow(
              valueBoxOutput("annualStateTaxInfo", width = 6),
              valueBoxOutput("perPayrollStateTaxInfo", width = 6)
            )
          ),
          box(
            width = 12,
            status = "danger",
            title = "FICA Tax",
            fluidRow(
              valueBoxOutput("socialSecurityTaxInfo", width = 6),
              valueBoxOutput("medicareTaxInfo", width = 6)
            )
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  payrollNums <- reactive({
    ifelse(input$payrollType == "Monthly", 12, 26)
  })

  output$status <- renderText({
    validate(
      need(
        input$fedTaxableGross, "Please enter your federal taxable gross"
      ) %then%
        need(input$fedTaxableGross >= 0, "Please use a non-negative number for federal taxable gross") %then%
        need(
          input$ficaTaxableGross, "Please enter your FICA taxable gross"
        ) %then%
        need(input$ficaTaxableGross >= 0, "Please use non-negative number for FICA taxable gross") %then%
        need(
          input$numOfExemptions, "Please enter number of your exemptions"
        ) %then%
        need(input$numOfExemptions >= 0, "Please use a non-negative number for exemptions") %then%
        need(
          input$afterTaxDeductions, "Please enter your after-tax deduction amount"
        ) %then%
        need(input$afterTaxDeductions >= 0, "Please use a non-negative number for after-tax deduction amount")
    )
  })

  fedTaxableGross <- reactive({
    req(input$fedTaxableGross, input$fedTaxableGross >= 0)
    input$fedTaxableGross
  })

  output$fedTaxableGross <- renderValueBox({
    valueBox(
      formatNumber(fedTaxableGross()),
      subtitle = "Per-payroll federal taxable gross"
    )
  })

  annualFedTaxableGross <- reactive({
    fedTaxableGross() * payrollNums()
  })

  numOfExemptions <- reactive({
    req(input$numOfExemptions, input$numOfExemptions >= 0)
  })

  annualFedTaxableGrossAfterExemptions <- reactive({
    annualFedTaxableGross() - 4200 * numOfExemptions()
  })

  output$annualFedTaxableGrossAfterExemptionsInfo <- renderValueBox({
    valueBox(
      formatNumber(annualFedTaxableGrossAfterExemptions()),
      subtitle = "Annual taxable gross"
    )
  })

  annualFedTax <- reactive({
    calculateFedTax(annualFedTaxableGrossAfterExemptions(), input$maritalStatus)
  })

  output$annualFedTaxInfo <- renderValueBox({
    valueBox(
      formatNumber(annualFedTax()),
      subtitle = "Annual federal tax",
      color = "red"
    )
  })

  perPayrollFedTax <- reactive({
    annualFedTax() / payrollNums()
  })

  perPayrollStateDeduction <- reactive({
    ifelse(input$payrollType == "Monthly", 366.66, 169.23)
  })

  perPayrollStateTax <- reactive({
    0.0425 * max(0, fedTaxableGross() - perPayrollStateDeduction())
  })

  output$annualStateTaxInfo <- renderValueBox({
    valueBox(
      formatNumber(perPayrollStateTax() * payrollNums()),
      subtitle = "Annual state tax",
      color = "yellow"
    )
  })

  output$perPayrollFexTaxInfo <- renderValueBox({
    valueBox(
      formatNumber(perPayrollFedTax()),
      subtitle = "Per-payroll federal tax",
      color = "red"
    )
  })

  output$perPayrollStateTaxInfo <- renderValueBox({
    valueBox(
      formatNumber(perPayrollStateTax()),
      subtitle = "Per-payroll state tax",
      color = "yellow"
    )
  })

  ficaTaxableGross <- reactive({
    req(input$ficaTaxableGross, input$ficaTaxableGross >= 0)
    input$ficaTaxableGross
  })

  annualFicaTaxable <- reactive({
    ficaTaxableGross() * payrollNums()
  })

  perPayrollSocialSecurityTax <- reactive({
    min(annualFicaTaxable(), 132900) * 0.062 / payrollNums()
  })

  output$socialSecurityTaxInfo <- renderValueBox({
    valueBox(
      formatNumber(perPayrollSocialSecurityTax()),
      subtitle = "Per-payroll social security tax",
      color = "maroon"
    )
  })

  perPayrollMedicareTax <- reactive({
    tax <- annualFicaTaxable() * 0.0145
    wagesOver <- annualFicaTaxable() - 200000
    if (wagesOver > 0) {
      tax <- tax + wagesOver * 0.009
    }
    tax / payrollNums()
  })

  output$medicareTaxInfo <- renderValueBox({
    valueBox(
      formatNumber(perPayrollMedicareTax()),
      subtitle = "Per-payroll medicare tax",
      color = "maroon"
    )
  })

  afterTaxDeductions <- reactive({
    req(input$afterTaxDeductions, input$afterTaxDeductions >= 0)
    input$afterTaxDeductions
  })

  netPay <- reactive({
    fedTaxableGross() - perPayrollFedTax() - perPayrollStateTax() - perPayrollSocialSecurityTax() - perPayrollMedicareTax() - afterTaxDeductions()
  })

  output$netPayInfo <- renderValueBox({
    valueBox(
      formatNumber(netPay()),
      subtitle = "Per-payroll net pay",
      color = "green"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
