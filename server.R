library(shiny)
library(plotly)
source('utilities2.R')

shinyServer(function(input, output) {
  
  observeEvent(input$button, {
    req(
      input$success_A,
      input$total_A,
      input$success_B,
      input$total_B,
      input$rev_A,
      input$rev_B,
      input$retention_A,
      input$retention_B,
      input$sim_sample
    )
    if(
      input$success_A >= 0 &&
      input$total_A > 0 &&
      input$success_B >= 0 &&
      input$total_B > 0 &&
      input$success_A <= input$total_A &&
      input$success_B <= input$total_B &&
      input$rev_A >= 0 &&
      input$rev_B >= 0 &&
      input$retention_A >= 0 &&
      input$retention_B >= 0 &&
      input$retention_A <= input$total_A &&
      input$retention_B <= input$total_B &&
      input$sim_sample >= 2
    ) {
      sample_A <- isolate({input$total_A})
      sample_B <- isolate({input$total_B})
      conv_A <- isolate({input$success_A/input$total_A})
      conv_B <- isolate({input$success_B/input$total_B})
      arppu_A <- isolate({input$rev_A/input$success_A})
      arppu_B <- isolate({input$rev_B/input$success_B})
      arpu_A <- isolate({input$rev_A/input$total_A})
      arpu_B <- isolate({input$rev_B/input$total_B})
      alpha_A <- isolate({input$success_A + 1})
      alpha_B <- isolate({input$success_B + 1})
      beta_A <- isolate({input$total_A - input$success_A + 1})
      beta_B <- isolate({input$total_B - input$success_B + 1})
      k_A <- isolate({input$success_A + 1})
      k_B <- isolate({input$success_B + 1})
      theta_A <- isolate({1/(1 + input$rev_A)})
      theta_B <- isolate({1/(1 + input$rev_B)})
      convret_A <- isolate({input$retention_A/input$total_A})
      convret_B <- isolate({input$retention_B/input$total_B})
      alpharet_A <- isolate({input$retention_A + 1})
      alpharet_B <- isolate({input$retention_B + 1})
      betaret_A <- isolate({input$total_A - input$retention_A + 1})
      betaret_B <- isolate({input$total_B - input$retention_B + 1})
      retention_A <- isolate({input$retention_A})
      retention_B <- isolate({input$retention_B})

      res <- isolate({
        bayes_arpu(
          alphaA = alpha_A, betaA = beta_A,
          kA = k_A, thetaA = theta_A,
          alphaB = alpha_B, betaB = beta_B,
          kB = k_B, thetaB = theta_B,
          MSamples = input$sim_sample
        )
      })
      post_sample_A <- res$sampleLambdaA/res$sampleOmegaA
      post_sample_B <- res$sampleLambdaB/res$sampleOmegaB
      diff_post_sampleAB <- post_sample_B - post_sample_A
      hdi_A <- hdi_of_sample(post_sample_A)
      hdi_B <- hdi_of_sample(post_sample_B)
      hdi_diffAB <- hdi_of_sample(diff_post_sampleAB)
      x_lim <- {
        a <- min(hdi_A, hdi_B)
        b <- max(hdi_A, hdi_B)
        c(1.2*a - 0.2*b, 1.2*b - 0.2*a)
      }
      x_lim_diff <- {
        a <- hdi_diffAB[1]
        b <- hdi_diffAB[2]
        c(1.2*a - 0.2*b, 1.2*b - 0.2*a)
      }
     
      
      printPlot <- isolate({TRUE})
    } else {
      sample_A <- isolate({0})
      sample_B <- isolate({0})
      conv_A <- isolate({NaN})
      conv_B <- isolate({NaN})
      arppu_A <- isolate({NaN})
      arppu_B <- isolate({NaN})
      arpu_A <- isolate({NaN})
      arpu_B <- isolate({NaN})
      alpha_A <- isolate({1})
      alpha_B <- isolate({1})
      beta_A <- isolate({1})
      beta_B <- isolate({1})
      k_A <- isolate({1})
      k_B <- isolate({1})
      theta_A <- isolate({1})
      theta_B <- isolate({1})
      hdi_A <- isolate({c(NaN, NaN)})
      hdi_B <- isolate({c(NaN, NaN)})
      hdi_diffAB <- isolate({c(NaN, NaN)})
      x_lim <- isolate({c(NaN, NaN)})
      x_lim_diff <- isolate({c(NaN, NaN)})
      
      res <- isolate({
        list(
          convProbBbeatsA = NaN, convExpLossA_AB = NaN, convExpLossB_AB = NaN,
          revProbBbeatsA = NaN, revExpLossA_AB = NaN, revExpLossB_AB = NaN,
          arpuProbBbeatsA = NaN, arpuExpLossA_AB = NaN, arpuExpLossB_AB = NaN,
          convProbAbeatsB = NaN, convExpLossA_AB2 = NaN, convExpLossB_AB2 = NaN,
          revProbAbeatsB = NaN, revExpLossA_AB2 = NaN, revExpLossB_AB2 = NaN,
          arpuProbAbeatsB = NaN, arpuExpLossA_AB2 = NaN, arpuExpLossB_AB2 = NaN,
        )
      })
      printPlot <- isolate({FALSE})
    }
    
    output$table1 <- renderTable({
      tab <- data.frame(
        metric = c(
          'Sample Size', 'Retained Players','<strong>Conversion<strong>','<strong>Retention<strong>', 'ARPPU',
          'ARPU', '95% HDI'
        ),
        A = c(
          sprintf('\n%.d', sample_A),
          sprintf('\n%.d', retention_A),
          sprintf('\n%.2g%%', conv_A*100),
          sprintf('\n%.2g%%', convret_A*100),
          sprintf('\n%.2g â¬', arppu_A),
          sprintf('\n%.2g â¬', arpu_A),
          sprintf('[%.2g â¬, \n%.2g â¬]', hdi_A[1], hdi_A[2])
        ),
        B = c(
          sprintf('\n%.d', sample_B),
          sprintf('\n%.d', retention_B),
          sprintf('\n%.2g%%', conv_B*100),
          sprintf('\n%.2g%%', convret_B*100),
          sprintf('\n%.2g â¬', arppu_B),
          sprintf('\n%.2g â¬', arpu_B),
          sprintf('[%.2g â¬, \n%.2g â¬]', hdi_B[1], hdi_B[2])
        )
      )
      colnames(tab) <- c(' ', 'A', 'B')
      tab
    }, spacing = 'xs', sanitize.text.function = function(x){x})
    
    output$table2 <- renderTable({
      tab <- data.frame(
        column1 = c(
          'Probability that B is better than A'
        ),
        conversion = c(
          sprintf('\n%.1f%%', res$convProbBbeatsA*100)
        ),
        ARPPU = c(
          sprintf('\n%.1f%%', res$revProbBbeatsA*100)
        ),
        ARPU = c(
          sprintf('\n%.1f%%', res$arpuProbBbeatsA*100)
        ),
        Retention = c(
          sprintf('\n%.2g%% [\n%.2g%%]',as.numeric((convret_B-convret_A)/convret_A)*100, prob_B_beats_A(alpharet_A, betaret_A, alpharet_B, betaret_B)*100)
        )
      )
      colnames(tab) <- c(' ', 'Conversion', 'ARPPU', 'ARPU', 'Retention')
      tab
    }, spacing = 'xs')
    
    output$table3 <- renderTable({
      tab <- data.frame(
        column1 = c(
          'Probability that A is better than B'
        ),
        conversion = c(
          sprintf('\n%.1f%%', res$convProbAbeatsB*100)
        ),
        ARPPU = c(
          sprintf('\n%.1f%%', res$revProbAbeatsB*100)
        ),
        ARPU = c(
          sprintf('\n%.1f%%', res$arpuProbAbeatsB*100)
        ),
        Retention1 = c(
          sprintf('\n%.2g%% [\n%.2g%%]', as.numeric((convret_A-convret_B)/convret_B)*100, prob_A_beats_B(alpharet_B, betaret_B, alpharet_A, betaret_A)*100)
        )
      )
      colnames(tab) <- c(' ', 'Conversion', 'ARPPU', 'ARPU', 'Retention')
      tab
    }, spacing = 'xs')
    
    output$table4 <- renderTable({
      tab <- data.frame(
        column1 = c(
          '*Retention1 refers to A better than B',
          '*Retention2 refers to B better than A',
          'Report largest value (positive integer)'
        )
      )
      colnames(tab) <- c(' ')
      tab
    }, spacing = 'xs')
    
    output$table5 <- renderTable({
      tab <- data.frame(
        better = c(
          paste('On ', input$testday, ', the Control group had a ', round(res$convProbAbeatsB*100, digits=2), '% greater conversion rate than Group B.', sep=''),
          paste('On ', input$testday, ', Group B had a ', round(res$convProbBbeatsA*100, digits=2), '% greater conversion rate compared to the Control group.', sep='')
          
        )
      )
      colnames(tab) <- c(' ')
      tab
    }, spacing = 'xs')
    
    
    output$table6 <- renderTable({
      tab <- data.frame(
        better = c(
          paste('On ', input$testday, ', the Control group had a ', round(res$arpuProbAbeatsB*100,digits=2), '% greater lifetime value than Group B.', sep=''),
          paste('On ', input$testday, ', Group B had a ', round(res$arpuProbBbeatsA*100, digits=2), '% greater lifetime value compared to the Control group.', sep='')
        )
      )
      colnames(tab) <- c(' ')
      tab
    }, spacing = 'xs')
    
    
    output$table7 <- renderTable({
      tab <- data.frame(
        better = c(
          paste('On ', input$testday,', the Control group had a ', round(((convret_A-convret_B)/convret_B)*100,digits=2), '% greater retention rate compared to Group B (', round(prob_B_beats_A(alpharet_B, betaret_B, alpharet_A, betaret_A)*100,digits =0),
                '% confidence).', sep=''),
          
          paste('On ', input$testday,', Group B had a ', round(((convret_B-convret_A)/convret_A)*100,digits=2), '% greater retention rate compared to the Control group (', round(prob_A_beats_B(alpharet_A, betaret_A, alpharet_B, betaret_B)*100,digits =0),
                '% confidence).', sep='')
        )
      )
      colnames(tab) <- c(' ')
      tab
    }, spacing = 'xs')
  })
})
