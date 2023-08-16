# Cargar la librería Shiny
library(shiny)
library(bslib)

# Definir la interfaz de usuario

data_muestra <- read.csv("/home/jesus/Bankruptcy-Prediction/Data/Data_muestra.csv")
data_muestra <- data_muestra[, -which(names(data_muestra) == "Bankrupt.")]

variable_names <- c(
  "ROA.C..before.interest.and.depreciation.before.interest",
  "ROA.A..before.interest.and...after.tax",
  "ROA.B..before.interest.and.depreciation.after.tax",
  "Operating.Gross.Margin",
  "Realized.Sales.Gross.Margin",
  "Pre.tax.net.Interest.Rate",
  "After.tax.net.Interest.Rate",
  "Non.industry.income.and.expenditure.revenue",
  "Continuous.interest.rate..after.tax.",
  "Operating.Expense.Rate",
  "Research.and.development.expense.rate",
  "Cash.flow.rate",
  "Interest.bearing.debt.interest.rate",
  "Tax.rate..A.",
  "Net.Value.Per.Share..B.",
  "Net.Value.Per.Share..A.",
  "Net.Value.Per.Share..C.",
  "Persistent.EPS.in.the.Last.Four.Seasons",
  "Cash.Flow.Per.Share",
  "Operating.Profit.Per.Share..Yuan...",
  "Per.Share.Net.profit.before.tax..Yuan...",
  "Operating.Profit.Growth.Rate",
  "After.tax.Net.Profit.Growth.Rate",
  "Regular.Net.Profit.Growth.Rate",
  "Continuous.Net.Profit.Growth.Rate",
  "Total.Asset.Growth.Rate",
  "Net.Value.Growth.Rate",
  "Total.Asset.Return.Growth.Rate.Ratio",
  "Cash.Reinvestment..",
  "Quick.Ratio",
  "Total.debt.Total.net.worth",
  "Debt.ratio..",
  "Net.worth.Assets",
  "Borrowing.dependency",
  "Contingent.liabilities.Net.worth",
  "Operating.profit.Paid.in.capital",
  "Net.profit.before.tax.Paid.in.capital",
  "Inventory.and.accounts.receivable.Net.value",
  "Total.Asset.Turnover",
  "Accounts.Receivable.Turnover",
  "Average.Collection.Days",
  "Inventory.Turnover.Rate..times.",
  "Fixed.Assets.Turnover.Frequency",
  "Net.Worth.Turnover.Rate..times.",
  "Revenue.per.person",
  "Operating.profit.per.person",
  "Working.Capital.to.Total.Assets",
  "Quick.Assets.Total.Assets",
  "Current.Assets.Total.Assets",
  "Cash.Total.Assets",
  "Cash.Current.Liability",
  "Current.Liability.to.Assets",
  "Operating.Funds.to.Liability",
  "Inventory.Current.Liability",
  "Current.Liabilities.Liability",
  "Working.Capital.Equity",
  "Current.Liabilities.Equity",
  "Retained.Earnings.to.Total.Assets",
  "Total.income.Total.expense",
  "Total.expense.Assets",
  "Quick.Asset.Turnover.Rate",
  "Cash.Turnover.Rate",
  "Fixed.Assets.to.Assets",
  "Current.Liability.to.Liability",
  "Current.Liability.to.Equity",
  "Equity.to.Long.term.Liability",
  "Cash.Flow.to.Total.Assets",
  "Cash.Flow.to.Liability",
  "CFO.to.Assets",
  "Cash.Flow.to.Equity",
  "Current.Liability.to.Current.Assets",
  "Net.Income.to.Total.Assets",
  "Total.assets.to.GNP.price",
  "Gross.Profit.to.Sales",
  "Net.Income.to.Stockholder.s.Equity",
  "Liability.to.Equity",
  "Degree.of.Financial.Leverage..DFL.",
  "Equity.to.Liability"
)

variable_descriptions <- list(
  "ROA.C..before.interest.and.depreciation.before.interest" = "Return on assets before interest and depreciation before interest. It measures a company's ability to generate profits from its assets before accounting for interest and depreciation expenses.",
  "ROA.A..before.interest.and...after.tax" = "Return on assets before interest and after tax. Similar to the previous variable but considering after-tax profits.",
  "ROA.B..before.interest.and.depreciation.after.tax" = "Return on assets before interest and depreciation after tax. Measures the company's ability to generate profits from assets after accounting for both interest and depreciation expenses.",
  "Operating.Gross.Margin" = "Operating gross margin. It represents the difference between sales revenue and the cost of goods sold, divided by sales revenue. It indicates the proportion of each sales dollar that represents gross profit.",
  "Realized.Sales.Gross.Margin" = "Realized sales gross margin. Similar to the previous variable but considering realized sales revenue.",
  "Pre.tax.net.Interest.Rate" = "Pre-tax net interest rate. Measures the company's ability to generate profits from its core operations before accounting for interest expenses.",
  "After.tax.net.Interest.Rate" = "After-tax net interest rate. Similar to the previous variable but considering after-tax profits.",
  "Non.industry.income.and.expenditure.revenue" = "Non-industry income and expenditure revenue. Represents income and expenses not directly related to the industry in which the company operates.",
  "Continuous.interest.rate..after.tax." = "Continuous interest rate after tax. Indicates the interest rate the company is paying on its debts after accounting for taxes.",
  "Operating.Expense.Rate" = "Operating expense rate. Measures the proportion of operating expenses to the total assets of the company.",
  "Research.and.development.expense.rate" = "Research and development expense rate. Represents the proportion of research and development expenses to the total assets of the company.",
  "Cash.flow.rate" = "Cash flow rate. Measures the company's ability to generate cash flow from its operations relative to its total assets.",
  "Interest.bearing.debt.interest.rate" = "Interest-bearing debt interest rate. Indicates the interest rate the company is paying on its interest-bearing debts.",
  "Tax.rate..A." = "Tax rate A. Represents the tax rate paid by the company.",
  "Net.Value.Per.Share..B." = "Net value per share B. Represents the net value of the company per share of common stock.",
  "Net.Value.Per.Share..A." = "Net value per share A. Similar to the previous variable but considering different calculations.",
  "Net.Value.Per.Share..C." = "Net value per share C. Similar to the previous variables but considering different calculations.",
  "Persistent.EPS.in.the.Last.Four.Seasons" = "Persistent EPS in the last four seasons. Measures the consistency of earnings per share over the last four seasons.",
  "Cash.Flow.Per.Share" = "Cash flow per share. Represents the cash flow generated by the company per share of common stock.",
  "Operating.Profit.Per.Share..Yuan..." = "Operating profit per share (Yuan). Measures the operating profit generated by the company per share of common stock in Yuan currency.",
  "Per.Share.Net.profit.before.tax..Yuan..." = "Per share net profit before tax (Yuan). Measures the net profit generated by the company per share of common stock before taxes in Yuan currency.",
  "Operating.Profit.Growth.Rate" = "Operating profit growth rate. Measures the growth rate of the company's operating profit over a period of time.",
  "After.tax.Net.Profit.Growth.Rate" = "After-tax net profit growth rate. Measures the growth rate of the company's net profit after taxes over a period of time.",
  "Regular.Net.Profit.Growth.Rate" = "Regular net profit growth rate. Measures the growth rate of the company's net profit over a period of time.",
  "Continuous.Net.Profit.Growth.Rate" = "Continuous net profit growth rate. Measures the continuous growth rate of the company's net profit over a period of time.",
  "Total.Asset.Growth.Rate" = "Total asset growth rate. Measures the growth rate of the company's total assets over a period of time.",
  "Net.Value.Growth.Rate" = "Net value growth rate. Measures the growth rate of the company's net value over a period of time.",
  "Total.Asset.Return.Growth.Rate.Ratio" = "Total asset return growth rate ratio. Represents the ratio of growth rates between total asset return and growth rate.",
  "Cash.Reinvestment.." = "Cash reinvestment. Measures the reinvestment of cash in the company's operations.",
  "Quick.Ratio" = "Quick ratio. Indicates the company's ability to cover short-term obligations with its most liquid assets.",
  "Total.debt.Total.net.worth" = "Total debt to total net worth. Represents the proportion of total debt relative to the company's total net worth.",
  "Debt.ratio.." = "Debt ratio. Measures the company's leverage by comparing total debt to its total assets.",
  "Net.worth.Assets" = "Net worth to assets ratio. Represents the proportion of net worth relative to the company's total assets.",
  "Borrowing.dependency" = "Borrowing dependency. Measures the company's dependency on borrowing.",
  "Contingent.liabilities.Net.worth" = "Contingent liabilities to net worth ratio. Represents the proportion of contingent liabilities relative to the company's net worth.",
  "Operating.profit.Paid.in.capital" = "Operating profit paid in capital. Represents the portion of operating profit paid as capital.",
  "Net.profit.before.tax.Paid.in.capital" = "Net profit before tax paid in capital. Represents the portion of net profit before tax paid as capital.",
  "Inventory.and.accounts.receivable.Net.value" = "Inventory and accounts receivable net value. Represents the net value of inventory and accounts receivable.",
  "Total.Asset.Turnover" = "Total asset turnover. Measures the efficiency of a company's use of its assets in generating sales.",
  "Accounts.Receivable.Turnover" = "Accounts receivable turnover. Indicates how many times a company's accounts receivable are collected within a specific time period.",
  "Average.Collection.Days" = "Average collection days. Represents the average number of days it takes to collect payments from customers.",
  "Inventory.Turnover.Rate..times." = "Inventory turnover rate (times). Measures how many times the company's inventory is sold and replaced within a specific time period.",
  "Fixed.Assets.Turnover.Frequency" = "Fixed assets turnover frequency. Measures how frequently the company's fixed assets are sold and replaced within a specific time period.",
  "Net.Worth.Turnover.Rate..times." = "Net worth turnover rate (times). Measures how many times the company's net worth is used to generate sales within a specific time period.",
  "Revenue.per.person" = "Revenue per person. Indicates the revenue generated by the company per employee.",
  "Operating.profit.per.person" = "Operating profit per person. Indicates the operating profit generated by the company per employee.",
  "Working.Capital.to.Total.Assets" = "Working capital to total assets. Represents the proportion of working capital relative to the company's total assets.",
  "Quick.Assets.Total.Assets" = "Quick assets to total assets. Represents the proportion of quick assets (liquid assets) relative to the company's total assets.",
  "Current.Aspects.Total.Assets" = "Current aspects to total assets. Represents the proportion of current aspects relative to the company's total assets.",
  "Cash.Total.Assets" = "Cash to total assets. Represents the proportion of cash and cash equivalents relative to the company's total assets.",
  "Cash.Current.Liability" = "Cash to current liability. Represents the proportion of cash and cash equivalents relative to current liabilities.",
  "Current.Liability.to.Assets" = "Current liability to assets. Represents the proportion of current liabilities relative to the company's total assets.",
  "Operating.Funds.to.Liability" = "Operating funds to liability. Measures the proportion of operating funds relative to liabilities.",
  "Inventory.Current.Liability" = "Inventory to current liability. Represents the proportion of inventory relative to current liabilities.",
  "Current.Liabilities.Liability" = "Current liabilities to liability. Represents the proportion of current liabilities relative to total liabilities.",
  "Working.Capital.Equity" = "Working capital to equity. Represents the proportion of working capital relative to the company's equity.",
  "Current.Liabilities.Equity" = "Current liabilities to equity. Represents the proportion of current liabilities relative to the company's equity.",
  "Retained.Earnings.to.Total.Assets" = "Retained earnings to total assets. Measures the proportion of retained earnings relative to the company's total assets.",
  "Total.income.Total.expense" = "Total income to total expense. Represents the proportion of total income relative to total expenses.",
  "Total.expense.Assets" = "Total expense to assets. Represents the proportion of total expenses relative to the company's total assets.",
  "Quick.Asset.Turnover.Rate" = "Quick asset turnover rate. Measures the efficiency of the company's use of quick assets in generating sales.",
  "Cash.Turnover.Rate" = "Cash turnover rate. Measures the efficiency of the company's use of cash and cash equivalents in generating sales.",
  "Fixed.Assets.to.Assets" = "Fixed assets to assets. Represents the proportion of fixed assets relative to the company's total assets.",
  "Current.Liability.to.Liability" = "Current liability to liability. Represents the proportion of current liabilities relative to total liabilities.",
  "Current.Liability.to.Equity" = "Current liability to equity. Represents the proportion of current liabilities relative to the company's equity.",
  "Equity.to.Long.term.Liability" = "Equity to long-term liability. Represents the proportion of equity relative to long-term liabilities.",
  "Cash.Flow.to.Total.Assets" = "Cash flow to total assets. Represents the proportion of cash flow relative to the company's total assets.",
  "Cash.Flow.to.Liability" = "Cash flow to liability. Represents the proportion of cash flow relative to liabilities.",
  "CFO.to.Assets" = "Cash flow from operations to assets. Measures the efficiency of generating cash flow from operations relative to total assets.",
  "Cash.Flow.to.Equity" = "Cash flow to equity. Represents the proportion of cash flow relative to the company's equity.",
  "Current.Liability.to.Current.Assets" = "Current liability to current assets. Represents the proportion of current liabilities relative to current assets.",
  "Net.Income.to.Total.Assets" = "Net income to total assets. Measures the proportion of net income relative to the company's total assets.",
  "Total.assets.to.GNP.price" = "Total assets to GNP price. Measures the proportion of total assets relative to the company's gross national product price.",
  "Gross.Profit.to.Sales" = "Gross profit to sales ratio. Represents the proportion of gross profit relative to sales revenue.",
  "Net.Income.to.Stockholder.s.Equity" = "Net income to stockholder's equity. Measures the proportion of net income relative to stockholder's equity.",
  "Liability.to.Equity" = "Liability to equity ratio. Represents the proportion of total liabilities relative to stockholder's equity.",
  "Degree.of.Financial.Leverage..DFL." = "Degree of financial leverage (DFL). Measures the sensitivity of earnings per share (EPS) to changes in operating income.",
  "Equity.to.Liability" = "Equity to liability ratio. Represents the proportion of equity relative to total liabilities."
)

input_data <- data.frame(matrix(ncol = length(variable_names), nrow = 0))
colnames(input_data) <- variable_names

shinyOptions(shiny.ui.font_scale = 2.5)

ui <- fluidPage(
  theme = bs_theme(
    bg = "#262C3C",
    fg = "#F4F4F4"
  ),
  
  h1("Bankruptcy Prediction App"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Mostrar los valores de data_muestra en campos de entrada
      lapply(1:length(variable_names), function(i) {
        var_name <- variable_names[i]
        var_description <- variable_descriptions[[var_name]]
        
        # Formatear el nombre de la variable
        formatted_var_name <- gsub("\\.", " ", var_name)
        formatted_var_name <- tools::toTitleCase(formatted_var_name)
        
        # Obtener el valor de data_muestra correspondiente a la variable
        input_value <- as.character(data_muestra[[var_name]])
        # Crear campos de entrada con los valores de data_muestra
        
        
        if (is.numeric(input_value)) {
          input_element <- numericInput(var_name, formatted_var_name, value = input_value, step = 0.01)
        } else {
          input_element <- textInput(var_name, formatted_var_name, value = input_value)
        }
        
        description_id <- paste0("desc", i)
        
        div(
          style = "display: flex; align-items: center;",
          input_element,
          div(
            style = "margin-left: 20px;",
            actionButton(description_id, "ℹ️", style = "font-size: 12px; padding: 3px 5px;"),
            conditionalPanel(
              condition = paste0("input['", description_id, "'] % 2 == 1"),
              style = "margin-left: 10px; font-size: 12px; color: #888;",
              p(var_description)
            )
          )
        )
      }),
      
      actionButton("predictBtn", "Make prediction")
    ),
    mainPanel(
      h3("Result of the prediction:"),
      verbatimTextOutput("predictionText")
    )
  )
)

server <- function(input, output, session) {
  load("/home/jesus/Bankruptcy-Prediction/Models/modelo.rda")
  
  # Mostrar los valores de data_muestra en los campos de entrada
  observeEvent(data_muestra, {
    if (!is.null(data_muestra)) {
      for (var_name in variable_names) {
        # Utilizar updateTextInput para cambiar el valor de cada campo
        updateTextInput(session, var_name, value = as.character(data_muestra[[var_name]]))
      }
    }
  })
  
  # Agregar la lógica de predicción al botón "Realizar Predicción"
  observeEvent(input$predictBtn, {
    # Obtener los valores ingresados por el usuario para todas las variables
    input_values <- sapply(variable_names, function(var_name) {
      if (is.numeric(data_muestra[[var_name]])) {
        as.numeric(input[[var_name]])
      } else {
        as.character(input[[var_name]])
      }
    })
    
    # Crear un nuevo data.frame con los valores ingresados por el usuario
    user_input_data <- data.frame(matrix(input_values, ncol = length(variable_names)))
    colnames(user_input_data) <- colnames(data_muestra)
    
    # Realizar la predicción utilizando el modelo cargado
    prediction_result <- predict(modelo, newdata = user_input_data, type = "response")
    probability_not_bankrupt <- 1 - prediction_result
    
    
    
    # Mostrar el resultado de la predicción en el texto de salida
    output$predictionText <- renderText({
      paste("Bankruptcy probability: ", round(prediction_result,7) * 100, "%","\n",
            "Probability of Non-Bankruptcy: ", round(probability_not_bankrupt,7), "%")
    })
    
    
  })
  
  # Mostrar los valores ingresados por el usuario en la tabla
}
# Crear la aplicación Shiny
shinyApp(ui = ui, server = server)



