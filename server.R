function(input, output) {
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
