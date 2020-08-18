library(shiny)
library(plotly)

shinyUI(
  fluidPage(
    titlePanel('Bayesian A/B/n test for ARPU'),
    sidebarLayout(
      sidebarPanel(
        numericInput(
          'rev_A',
          'Total Revenue for Control:',
          min = 0,
          max = 1e6,
          value = 7760.50
        ),
        numericInput(
          'success_A',
          'Control Conversions:',
          min = 0,
          max = 1e6,
          value = 349
        ),
        numericInput(
          'total_A',
          'Total Control Players:',
          min = 0,
          max = 1e6,
          value = 1490
        ),
        numericInput(
          'retention_A',
          'Retention A:',
          min = 0,
          max = 1e6,
          value = 699
        ),
        numericInput(
          'rev_B',
          'Total Revenue Treatment 1:',
          min = 0,
          max = 1e6,
          value = 9322.70
        ),
        numericInput(
          'success_B',
          'Converted Players Treatment 1:',
          min = 0,
          max = 1e6,
          value = 386
        ),
        numericInput(
          'total_B',
          'Total Players Treatment 1:',
          min = 0,
          max = 1e6,
          value = 1614
        ),
        numericInput(
          'retention_B',
          'Retention B:',
          min = 0,
          max = 1e6,
          value = 794
        ),
        numericInput(
          'sim_sample',
          'Monte-Carlo MC Samples:',
          min = 1,
          max = 1e7,
          value = 1e5
        ),
        actionButton(
          'button',
          'Calculate'
        ),
        hr(),
        tags$div(
          class='header', checked = NA,
          tags$p(
            'Bayesian test for A/B/n ARPU calculator. Modified for internal use'
          ),
          tags$br(),
          tags$a(
            href = 'https://github.com/Vidogreg/bayes-ab-testing/tree/master/bayes-arpu-test',
            'Original Code by V. Gregor'
          ),
          tags$p('')
        )
      ),
      mainPanel(
        tableOutput('table1'),
        tableOutput('table2'),
        tableOutput('table3'),
        tableOutput('table4')
      )
    )
  )
)