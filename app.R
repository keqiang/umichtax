library(shiny)

options(digits = 2)

fedTaxBrackets <- list(
  list(cutoff = 3800, rate = 0.1, base = 0),
  list(cutoff = 13500, rate = 0.12, base = 970),
  list(cutoff = 43275, rate = 0.22, base = 4543),
  list(cutoff = 88000, rate = 0.24, base = 14382.5),
  list(cutoff = 164525, rate = 0.32, base = 32748.5),
  list(cutoff = 207900, rate = 0.35, base = 46628.5),
  list(cutoff = 514100, rate = 0.37, base = 153798.5)
)

calculateFedTax <- function(taxableAmount, maritalStatus = c("Single", "Married")) {
  maritalStatus <- match.arg(maritalStatus)

  for (i in rev(seq_along(fedTaxBrackets))) {
    bracket <- fedTaxBrackets[[i]]
    if (taxableAmount >= bracket$cutoff) {
      bracketTax <- (taxableAmount - bracket$cutoff) * bracket$rate
      return(bracketTax + bracket$base)
    }
  }

  return(0)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  # Application title
  titlePanel("University of Michigan Tax Calculator"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("EFFECTIVE January 1, 2019"),
      radioButtons(
        "payrollType",
        label = "Payroll Type",
        choices = c("Monthly", "Biweekly"),
        selected = "Monthly",
        inline = TRUE
      ),
      numericInput(
        "perPayrollGross",
        label = "Per-payroll Gross",
        value = 4500,
        min = 500,
        step = 1
      ),
      helpText("'TOTAL GROSS' on your paystub"),
      tags$hr(),
      radioButtons(
        "maritalStatus",
        label = "Maital Status",
        choices = c("Single", "Married"),
        selected = "Single",
        inline = TRUE
      ),
      numericInput(
        "numOfExemptions",
        label = "Number of Exemptions",
        value = 0,
        min = 0,
        step = 1
      ),
      helpText("Displayed at the top right of your paystub. Each exemption claimed will result in a $4,200 deduction annually."),
      tags$hr(),
      numericInput(
        "perPayrollBeforeTaxDeductions",
        label = "Per-payroll Before-tax Deductions",
        value = 0,
        min = 0,
        step = 1
      ),
      helpText("Sum of all your before-tax deductions per-payroll. For example, health care, retirement plan and FSA."),
      tags$hr(),
      numericInput(
          "perPayrollSST",
          label = "Per-payroll Social Security Tax",
          value = 0,
          min = 0,
          step = 1
      ),
      helpText("Sum of 'Fed MED/EE' and Fed 'OASDI/EE' listed on your payroll."),
      tags$hr()
    ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("ftgInfo"),
      textOutput("ftgYearlyAfterExemptionsInfo"),
      textOutput("fedTaxYearlyInfo"),
      textOutput("netPayInfo")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  payrollNums <- reactive({
    ifelse(input$payrollType == "Monthly", 12, 24)
  })

  # Per-payroll Federal Taxable Gross
  ftg <- reactive({
    req(input$perPayrollGross, input$perPayrollBeforeTaxDeductions)
    input$perPayrollGross - input$perPayrollBeforeTaxDeductions
  })

  output$ftgInfo <- renderText({
    glue::glue("Your per-payroll Federal Taxable Gross is {ftg()}")
  })

  ftgYearly <- reactive({
    req(payrollNums())
    ftg() * payrollNums()
  })

  ftgYearlyAfterExemptions <- reactive({
    req(input$numOfExemptions)
    ftgYearly() - 4200 * input$numOfExemptions
  })

  output$ftgYearlyAfterExemptionsInfo <- renderText({
    glue::glue("Your annual taxable gross is: {ftgYearlyAfterExemptions()}")
  })
  
  fedTaxYearly <- reactive({
      calculateFedTax(ftgYearlyAfterExemptions(), input$maritalStatus)
  })
 
  perPayrollFedTax <- reactive({
      fedTaxYearly() / payrollNums()
  })
  
  perPayrollStateDeduction <- reactive({
      ifelse(input$payrollType == "Monthly", 366.66, 169.23)
  })
  
  perPayrollStateTax <- reactive({
      0.0425 * (ftg() - perPayrollStateDeduction())
  })
  
  output$fedTaxYearlyInfo <- renderText({
      glue::glue("Your annual federal tax is: {fedTaxYearly()}; your per-payroll federal tax is {perPayrollFedTax()}; your per-payroll state tax is {perPayrollStateTax()}")
  })
  
  
  netPay <- reactive({
      ftg() - perPayrollFedTax() - perPayrollStateTax() - input$perPayrollSST
  })
  
  output$netPayInfo <- renderText({
      glue::glue("Your per-payroll net pay is: {netPay()}")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
