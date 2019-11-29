dashboardPage(
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
            sliderInput(
              "additionalBeforeTax",
              label = "Add more before-tax amount",
              min = 0,
              max = 5000,
              value = 0,
              step = 100
            ),
            helpText("Use this for retirement planning, such as a SRA contribution"),
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
              valueBoxOutput("perPayrollFexTaxInfo", width = 6),
              valueBoxOutput("annualFedTaxInfo", width = 6)
            )
          ),
          box(
            width = 12,
            status = "warning",
            title = "State Tax",
            fluidRow(
              valueBoxOutput("perPayrollStateTaxInfo", width = 6),
              valueBoxOutput("annualStateTaxInfo", width = 6)
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
