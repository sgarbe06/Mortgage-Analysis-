############################################
# Rate to Reality
# Federal Reserve Rates and Home Affordability
# Faith Stamm & Sydney Garbe
# Final Project - 2026
############################################

# Install once if needed:
# install.packages(c('shiny','bslib','dplyr','tidyr','ggplot2','plotly','DT','scales','lubridate','sandwich','lmtest'))

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(lubridate)
library(sandwich)
library(lmtest)

# FRED CSV files are retrieved when the app starts. Internet required.
# The correct mortgage series is MORTGAGE30US (not MORTGAGE20US).
fetch_fred <- function(id) {
  url <- paste0('https://fred.stlouisfed.org/graph/fredgraph.csv?id=', id)
  x <- read.csv(url, na.strings = c('.', ''))
  names(x) <- c('date', id)
  x$date <- as.Date(x$date)
  x[[id]] <- as.numeric(x[[id]])
  x
}

# FEDFUNDS is monthly; MORTGAGE30US is weekly; DGS10 is daily.
# Monthly averages put all three series on a consistent basis.
monthly_series <- function(id) {
  fetch_fred(id) %>%
    mutate(month = floor_date(date, 'month')) %>%
    group_by(month) %>%
    summarise(value = mean(.data[[id]], na.rm = TRUE), .groups = 'drop') %>%
    mutate(value = ifelse(is.nan(value), NA_real_, value)) %>%
    rename(!!id := value)
}

load_data <- function() {
  a <- monthly_series('FEDFUNDS')
  b <- monthly_series('MORTGAGE30US')
  c <- monthly_series('DGS10')
  inner_join(a, b, by = 'month') %>%
    inner_join(c, by = 'month') %>%
    arrange(month) %>%
    filter(if_all(c(FEDFUNDS, MORTGAGE30US, DGS10), ~ !is.na(.x))) %>%
    mutate(spread = MORTGAGE30US - DGS10)
}

ui <- page_navbar(
  title = 'Rate to Reality',
  theme = bs_theme(bootswatch = 'flatly', primary = '#146c78'),
  header = div(class = 'container-fluid mt-3 mb-2',
               h4('Mortgage Rates, Treasury Yields & Home Affordability'),
               p('An interactive research project by Faith Stamm & Sydney Garbe')),

  nav_panel('Rate Trends',
    layout_sidebar(
      sidebar = sidebar(
        dateRangeInput('dates', 'Analysis period',
                       start = '2016-01-01', end = Sys.Date(),
                       min = '1971-04-01', max = Sys.Date()),
        checkboxGroupInput('series', 'Display series',
                           choices = c('30-year mortgage' = 'MORTGAGE30US',
                                       'Federal funds' = 'FEDFUNDS',
                                       '10-year Treasury' = 'DGS10'),
                           selected = c('MORTGAGE30US','FEDFUNDS','DGS10')),
        helpText('All values are monthly averages in percentage points.')
      ),
      h4('Historical interest rates'),
      plotlyOutput('trend', height = '440px'),
      h4('Mortgage–Treasury spread'),
      plotlyOutput('spread_plot', height = '270px'),
      downloadButton('download_data', 'Download monthly analysis data')
    )
  ),

  nav_panel('Statistical Analysis',
    p('These are observational associations, not evidence that one rate causes another.'),
    layout_columns(
      card(card_header('Mortgage vs. 10-year Treasury'), plotlyOutput('scatter_treasury', height = '320px')),
      card(card_header('Mortgage vs. federal funds'), plotlyOutput('scatter_funds', height = '320px'))
    ),
    layout_columns(
      card(card_header('Correlation matrix'), plotOutput('corr_plot', height = '300px')),
      card(card_header('Monthly changes (percentage points)'), plotlyOutput('changes_plot', height = '300px'))
    ),
    card(card_header('Multiple regression: levels'),
         p('Outcome: mortgage rate. Predictors: federal funds rate and 10-year Treasury yield. Newey–West standard errors help account for serial correlation.'),
         tableOutput('regression_table'),
         verbatimTextOutput('model_info')),
    card(card_header('Regression: month-to-month changes'),
         p('This second model tests whether changes in the predictors are associated with changes in mortgage rates, reducing the risk of misleading shared trends.'),
         tableOutput('change_regression'),
         verbatimTextOutput('change_model_info'))
  ),

  nav_panel('Homebuyer Calculator',
    layout_sidebar(
      sidebar = sidebar(
        numericInput('price', 'Home purchase price ($)', 300000, min = 10000, step = 10000),
        sliderInput('down', 'Down payment (%)', 0, 50, value = 10, step = 1),
        numericInput('rate', 'Mortgage annual interest rate (%)', 6.5, min = 0, max = 30, step = 0.25),
        selectInput('years', 'Loan term', choices = c('15 years' = 15, '30 years' = 30), selected = 30),
        numericInput('tax', 'Annual property tax ($)', 3600, min = 0, step = 100),
        numericInput('insurance', 'Annual homeowners insurance ($)', 1800, min = 0, step = 100),
        numericInput('pmi', 'Estimated annual PMI rate (%) if down payment < 20%', 0.6, min = 0, max = 5, step = 0.1),
        helpText('Estimates exclude HOA dues, maintenance, utilities, closing costs and lender fees.')
      ),
      h4('Estimated monthly housing payment'),
      h2(textOutput('payment_text')),
      plotOutput('payment_breakdown', height = '320px'),
      h4('What if mortgage rates change?'),
      plotlyOutput('scenario_plot', height = '350px'),
      p('Scenarios are hypothetical, not forecasts or loan offers.')
    )
  ),

  nav_panel('Career & Income Planning',
    layout_sidebar(
      sidebar = sidebar(
        numericInput('salary', 'Estimated annual gross salary ($)', 65000, min = 0, step = 1000),
        numericInput('other_debt', 'Other monthly debt payments ($)', 250, min = 0, step = 50),
        sliderInput('housing_share', 'Maximum housing cost as % of gross monthly income',
                    min = 15, max = 40, value = 28),
        helpText('Enter your own expected salary; this app does not estimate future job-market salaries.'),
        helpText('The percentage is a planning assumption, not a lender approval standard.')
      ),
      h4('Housing budget based on gross income'),
      h2(textOutput('budget_text')),
      tableOutput('income_table'),
      plotlyOutput('salary_plot', height = '340px'),
      p('This is an illustrative affordability screen. Actual underwriting depends on credit, debts, taxes, insurance, savings, loan terms and lender requirements.')
    )
  ),

  nav_panel('Data & Methods',
    h4('Source data'),
    tags$ul(
      tags$li(tags$a('FEDFUNDS — Effective Federal Funds Rate', href = 'https://fred.stlouisfed.org/series/FEDFUNDS', target = '_blank')),
      tags$li(tags$a('MORTGAGE30US — 30-Year Fixed Rate Mortgage Average', href = 'https://fred.stlouisfed.org/series/MORTGAGE30US', target = '_blank')),
      tags$li(tags$a('DGS10 — 10-Year Treasury Constant Maturity Yield', href = 'https://fred.stlouisfed.org/series/DGS10', target = '_blank'))
    ),
    p('Data are downloaded from FRED when the app launches. Weekly mortgage and daily Treasury observations are averaged by calendar month; FEDFUNDS is already monthly. Months with missing data are omitted.'),
    p('Mortgage rates are national averages and do not represent an individual loan quote. FRED data can be revised.'),
    h4('Methods and limitations'),
    tags$ol(
      tags$li('Time-series line plots and mortgage–Treasury spread.'),
      tags$li('Scatterplots and Pearson correlations.'),
      tags$li('Multiple linear regression of mortgage rates on Treasury yields and federal funds rates.'),
      tags$li('Month-to-month first-difference regression as a robustness check.'),
      tags$li('Newey–West standard errors for serial correlation; residual diagnostics should still be checked before drawing strong conclusions.')
    ),
    p('Potential limitations: correlated predictors, structural changes, omitted variables, autocorrelation, nonstationarity, and the fact that observed associations are not causal.'),
    h4('Analysis data preview'),
    DTOutput('data_table'),
    p('For the report: describe data collection, sample size, missingness, regression assumptions, model coefficients, uncertainty, and limitations. Disclose AI assistance according to your course policy.')
  )
)

server <- function(input, output, session) {
  raw_data <- tryCatch(load_data(), error = function(e) {
    showNotification(paste('FRED download failed:', conditionMessage(e)), type = 'error', duration = NULL)
    NULL
  })

  filtered <- reactive({
    req(raw_data, input$dates)
    x <- raw_data %>% filter(month >= input$dates[1], month <= input$dates[2])
    validate(need(nrow(x) >= 12, 'Select at least 12 months with complete data.'))
    x
  })

  output$trend <- renderPlotly({
    req(length(input$series) > 0)
    long <- filtered() %>% select(month, all_of(input$series)) %>%
      pivot_longer(-month, names_to = 'series', values_to = 'rate')
    ggplotly(ggplot(long, aes(month, rate, color = series)) +
      geom_line(linewidth = 0.9) + labs(x = NULL, y = 'Rate (%)', color = 'Series') + theme_minimal())
  })
  output$spread_plot <- renderPlotly({
    ggplotly(ggplot(filtered(), aes(month, spread)) + geom_line(color = '#146c78') +
      geom_hline(yintercept = 0, linetype = 'dashed') +
      labs(x = NULL, y = 'Mortgage rate minus Treasury yield (pp)') + theme_minimal())
  })
  output$scatter_treasury <- renderPlotly({
    ggplotly(ggplot(filtered(), aes(DGS10, MORTGAGE30US)) + geom_point(alpha = .65) +
      geom_smooth(method = 'lm', se = TRUE, color = '#146c78') +
      labs(x = '10-year Treasury yield (%)', y = 'Mortgage rate (%)') + theme_minimal())
  })
  output$scatter_funds <- renderPlotly({
    ggplotly(ggplot(filtered(), aes(FEDFUNDS, MORTGAGE30US)) + geom_point(alpha = .65) +
      geom_smooth(method = 'lm', se = TRUE, color = '#146c78') +
      labs(x = 'Federal funds rate (%)', y = 'Mortgage rate (%)') + theme_minimal())
  })
  output$corr_plot <- renderPlot({
    cm <- cor(filtered()[, c('FEDFUNDS','DGS10','MORTGAGE30US')])
    df <- as.data.frame(as.table(cm))
    ggplot(df, aes(Var1, Var2, fill = Freq)) + geom_tile() +
      geom_text(aes(label = sprintf('%.2f', Freq)), size = 5) +
      scale_fill_gradient2(low = '#b84a62', mid = 'white', high = '#146c78', midpoint = 0, limits = c(-1,1)) +
      coord_equal() + labs(x = NULL, y = NULL, fill = 'Pearson r') + theme_minimal()
  })
  changes <- reactive({
    filtered() %>% mutate(across(c(FEDFUNDS,DGS10,MORTGAGE30US), ~ .x - lag(.x), .names = 'd_{.col}')) %>%
      filter(if_all(starts_with('d_'), ~ !is.na(.x)))
  })
  output$changes_plot <- renderPlotly({
    long <- changes() %>% select(month, d_FEDFUNDS, d_DGS10, d_MORTGAGE30US) %>%
      pivot_longer(-month, names_to = 'series', values_to = 'change')
    ggplotly(ggplot(long, aes(month, change, color = series)) + geom_line() +
      labs(x = NULL, y = 'Change (percentage points)', color = 'Series') + theme_minimal())
  })
  fit_levels <- reactive(lm(MORTGAGE30US ~ FEDFUNDS + DGS10, data = filtered()))
  fit_changes <- reactive(lm(d_MORTGAGE30US ~ d_FEDFUNDS + d_DGS10, data = changes()))
  nw_table <- function(fit) {
    lag_n <- max(1, floor(4 * (length(residuals(fit))/100)^(2/9)))
    co <- lmtest::coeftest(fit, vcov. = sandwich::NeweyWest(fit, lag = lag_n, prewhite = FALSE, adjust = TRUE))
    data.frame(Term = rownames(co), Estimate = round(co[,1], 4),
               `NW Std. Error` = round(co[,2],4), `t value` = round(co[,3],3),
               `p value` = signif(co[,4],3), check.names = FALSE, row.names = NULL)
  }
  output$regression_table <- renderTable(nw_table(fit_levels()))
  output$change_regression <- renderTable(nw_table(fit_changes()))
  output$model_info <- renderText({
    fit <- fit_levels()
    paste('Observations:', nobs(fit), '| R-squared:', round(summary(fit)$r.squared, 3),
          '| Durbin-Watson (approx.):', round(sum(diff(resid(fit))^2)/sum(resid(fit)^2), 2))
  })
  output$change_model_info <- renderText({
    fit <- fit_changes()
    paste('Observations:', nobs(fit), '| R-squared:', round(summary(fit)$r.squared, 3),
          '| Durbin-Watson (approx.):', round(sum(diff(resid(fit))^2)/sum(resid(fit)^2), 2))
  })

  monthly_pi <- function(principal, annual_rate, years) {
    n <- as.numeric(years) * 12
    r <- annual_rate / 100 / 12
    if (r == 0) return(principal / n)
    principal * r / (1 - (1 + r)^(-n))
  }
  payment_parts <- reactive({
    principal <- input$price * (1 - input$down/100)
    pi <- monthly_pi(principal, input$rate, input$years)
    pmi <- if (input$down < 20) principal * input$pmi/100/12 else 0
    c('Principal & interest' = pi, 'Property tax' = input$tax/12,
      'Home insurance' = input$insurance/12, 'Estimated PMI' = pmi)
  })
  output$payment_text <- renderText(dollar(sum(payment_parts()), accuracy = 1))
  output$payment_breakdown <- renderPlot({
    df <- data.frame(item = names(payment_parts()), amount = as.numeric(payment_parts()))
    ggplot(df, aes(reorder(item, amount), amount)) + geom_col(fill = '#146c78') +
      coord_flip() + scale_y_continuous(labels = dollar) +
      labs(x = NULL, y = 'Monthly cost ($)') + theme_minimal()
  })
  scenarios <- reactive({
    rates <- seq(max(0, input$rate - 2), input$rate + 2, by = .25)
    principal <- input$price * (1 - input$down/100)
    fixed <- input$tax/12 + input$insurance/12 + ifelse(input$down < 20, principal * input$pmi/100/12, 0)
    data.frame(rate = rates,
               payment = vapply(rates, function(r) monthly_pi(principal, r, input$years) + fixed, numeric(1)))
  })
  output$scenario_plot <- renderPlotly({
    ggplotly(ggplot(scenarios(), aes(rate, payment)) + geom_line(color = '#146c78', linewidth = 1.2) +
      geom_vline(xintercept = input$rate, linetype = 'dashed') +
      scale_y_continuous(labels = dollar) +
      labs(x = 'Hypothetical mortgage rate (%)', y = 'Monthly housing cost ($)') + theme_minimal())
  })
  output$budget_text <- renderText(dollar(input$salary/12 * input$housing_share/100, accuracy = 1))
  output$income_table <- renderTable({
    budget <- input$salary/12 * input$housing_share/100
    data.frame(Measure = c('Gross monthly income', 'Housing budget', 'Other monthly debt',
                            'Housing + other debt as % of gross income', 'Estimated payment for selected home'),
               Value = c(dollar(input$salary/12), dollar(budget), dollar(input$other_debt),
                         percent((budget + input$other_debt)/(max(1,input$salary/12))),
                         dollar(sum(payment_parts()))))
  }, striped = TRUE)
  output$salary_plot <- renderPlotly({
    salaries <- seq(30000, 150000, by = 5000)
    df <- data.frame(salary = salaries, budget = salaries/12 * input$housing_share/100)
    ggplotly(ggplot(df, aes(salary, budget)) + geom_line(color = '#146c78', linewidth = 1.1) +
      geom_hline(yintercept = sum(payment_parts()), linetype = 'dashed', color = '#b84a62') +
      scale_x_continuous(labels = dollar) + scale_y_continuous(labels = dollar) +
      labs(x = 'Annual gross salary ($)', y = 'Monthly housing budget ($)') + theme_minimal())
  })
  output$data_table <- renderDT(datatable(filtered(), options = list(pageLength = 10, scrollX = TRUE)))
  output$download_data <- downloadHandler(
    filename = function() paste0('fred_monthly_rates_', Sys.Date(), '.csv'),
    content = function(file) write.csv(filtered(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
