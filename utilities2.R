# Calculates the probability that B is better than A and expected loss for A and B
# Also returns samples used for calculations
{
  # Arguments:
  #   Alpha and beta parameters of beta distributions for A and B
  #   k and theta parameters of gamma distributions for A and B
  #   alpha = number of successes + 1
  #   beta = number of failures + 1
  #   k = payer count + 1
  #   theta = 1/(1 + revenue)
  # Assumptions:
  #   payer ~ Bernoulli(lambda)
  #   lambda ~ Beta(alpha, beta)
  #   revenue ~ Exp(omega)
  #   omega ~ Gamma(k, theta)
  #   ARPU = E(payer)*E(revenue) = lambda*1/omega
  #   all varianles are calculated for A and B group
  # Return value:
  #   probBbeatsA = probability that the B version has higher ARPU than the A version
  #   expLossA = expected loss on condition that we select A but B is better
  #   expLossB = expected loss on condition that we select B but A is better
  #   expLossC = expected loss on condition that we select C but A is better
  # Example of use:
  #   If version A converted 180 out of 10000 users with revenue 5000 euros and
  #     version B converted 200 out of 11000 users with revenue 6000 euros then use
  #     bayes_arpu(
  #       180 + 1, 10000 - 180 + 1, 180 + 1, 1/(1 + 5000),
  #       200 + 1, 11000 - 200 + 1, 200 + 1, 1/(1 + 6000)
  #     )
  # Implemented based on VWO white paper
  # https://cdn2.hubspot.net/hubfs/310840/VWO_SmartStats_technical_whitepaper.pdf
}
bayes_arpu <- function(
  alphaA, betaA,
  kA, thetaA,
  alphaB, betaB,
  kB, thetaB,
  MSamples
) {
  if(alphaA <= 0 || betaA <= 0 || alphaB <= 0 || betaB <= 0 ||
     kA <= 0 || thetaA <= 0 || kB <= 0 || thetaB <= 0 ) {
    probBbeatsA <- 0
    expLossA <- 0
    expLossB <- 0
    lambdaA <- 0
    lambdaB <- 0
    omegaA <- 0
    omegaB <- 0
  } else {
    lambdaA <- rbeta(MSamples, alphaA, betaA)
    lambdaB <- rbeta(MSamples, alphaB, betaB)
    
    omegaA <- rgamma(MSamples, shape = kA, scale = thetaA)
    omegaB <- rgamma(MSamples, shape = kB, scale = thetaB)
    
    convProbBbeatsA <- sum(lambdaB > lambdaA)/MSamples
    diffTemp <- lambdaB - lambdaA
    convExpLossA_AB <- sum(diffTemp*(diffTemp > 0))/MSamples
    convExpLossB_AB <- sum(-diffTemp*(-diffTemp > 0))/MSamples
    
    convProbAbeatsB <- sum(lambdaA > lambdaB)/MSamples
    diffTemp <- lambdaA - lambdaB
    convExpLossA_AB2 <- sum(diffTemp*(diffTemp > 0))/MSamples
    convExpLossB_AB2 <- sum(-diffTemp*(-diffTemp > 0))/MSamples
    
    revProbBbeatsA <- sum(1/omegaB > 1/omegaA)/MSamples
    diffTemp <- 1/omegaB - 1/omegaA
    revExpLossA_AB <- sum(diffTemp*(diffTemp > 0))/MSamples
    revExpLossB_AB <- sum(-diffTemp*(-diffTemp > 0))/MSamples
    
    revProbAbeatsB <- sum(1/omegaA > 1/omegaB)/MSamples
    diffTemp <- 1/omegaA - 1/omegaB
    revExpLossA_AB2 <- sum(diffTemp*(diffTemp > 0))/MSamples
    revExpLossB_AB2 <- sum(-diffTemp*(-diffTemp > 0))/MSamples
    
    arpuProbBbeatsA <- sum(lambdaB/omegaB > lambdaA/omegaA)/MSamples
    diffTemp <- lambdaB/omegaB - lambdaA/omegaA
    arpuExpLossA_AB <- sum(diffTemp*(diffTemp > 0))/MSamples
    arpuExpLossB_AB <- sum(-diffTemp*(-diffTemp > 0))/MSamples
    
    arpuProbAbeatsB <- sum(lambdaA/omegaA > lambdaB/omegaB)/MSamples
    diffTemp <- lambdaA/omegaA - lambdaB/omegaB
    arpuExpLossA_AB2 <- sum(diffTemp*(diffTemp > 0))/MSamples
    arpuExpLossB_AB2 <- sum(-diffTemp*(-diffTemp > 0))/MSamples
    
  }
  list(
    convProbBbeatsA = convProbBbeatsA,
    convProbAbeatsB = convProbAbeatsB,
    convExpLossA_AB = convExpLossA_AB,
    convExpLossB_AB = convExpLossB_AB,
    convExpLossA_AB2 = convExpLossA_AB2,
    convExpLossB_AB2 = convExpLossB_AB2,
    
    revProbBbeatsA = revProbBbeatsA,
    revProbAbeatsB = revProbAbeatsB,
    revExpLossA_AB = revExpLossA_AB,
    revExpLossB_AB = revExpLossB_AB,
    revExpLossA_AB2 = revExpLossA_AB2,
    revExpLossB_AB2 = revExpLossB_AB2,
    
    arpuProbBbeatsA = arpuProbBbeatsA,
    arpuProbAbeatsB = arpuProbAbeatsB,
    arpuExpLossA_AB = arpuExpLossA_AB,
    arpuExpLossB_AB = arpuExpLossB_AB,
    arpuExpLossA_AB2 = arpuExpLossA_AB2,
    arpuExpLossB_AB2 = arpuExpLossB_AB2,

    sampleLambdaA = lambdaA,
    sampleLambdaB = lambdaB,
    sampleOmegaA = omegaA,
    sampleOmegaB = omegaB
  )
}

# Calculates the HDI interval from a sample of representative values
# estimated as shortest credible interval
{
  # Arguments:
  #   sampleVec is a vector of representative values from a probability
  #     distribution.
  #   credMass is a scalar between 0 and 1, indicating the mass within
  #     the credible interval that is to be estimated.
  # Return value:
  #   Highest density interval (HDI) limits in a vector
}
hdi_of_sample <- function(sampleVec, credMass = 0.95) {
  sortedPts <- sort(sampleVec)
  sortedPtsLength <- length(sortedPts)
  if(sortedPtsLength >= 3) {
    ciIdxInc <- min(ceiling(credMass*sortedPtsLength), sortedPtsLength - 1)
    nCIs <- sortedPtsLength - ciIdxInc
    ciWidth <- rep(0, nCIs)
    for (i in 1:nCIs) {
      ciWidth[i] <- sortedPts[i + ciIdxInc] - sortedPts[i]
    }
    HDImin <- sortedPts[which.min(ciWidth)]
    HDImax <- sortedPts[which.min(ciWidth) + ciIdxInc]
    HDIlim <- c(HDImin, HDImax)
  } else {
    HDIlim <- c(min(sortedPts), max(sortedPts))
  }
  return(HDIlim)
}

# Calculates the probability that B is better than A
{
  # Arguments:
  #   Alpha and beta parameters of beta distributions for A and B
  #   alpha = number of successes + 1
  #   beta = number of failures + 1
  # Assumptions:
  #   p_A ~ Beta(alpha_A, beta_A)
  #   p_B ~ Beta(alpha_B, beta_B)
  # Return value:
  #   Pr(p_B > p_A) = probability that the B version is better than the A version
  # Example of use:
  #   If version A converted 180 out of 1000 users and
  #     version B converted 200 out of 950 users type
  #     prob_B_beats_A(180 + 1, 1000 - 180 + 1, 200 + 1, 950 - 200 + 1)
  # Implemented based on Evan Miller`s blog post
  # http://www.evanmiller.org/bayesian-ab-testing.html#binary_ab
}
prob_B_beats_A <- function(alphaA, betaA, alphaB, betaB) {
  if(alphaA <= 0 || betaA <= 0 || alphaB <= 0 || betaB <= 0)
    result <- 0
  else {
    result <- 1
    for (i in 0:(alphaA - 1)) {
      result <- result - 
        exp(
          lbeta(alphaB + i, betaB + betaA) -
            log(betaA + i) -
            lbeta(1 + i, betaA) -
            lbeta(alphaB, betaB)
        )
    }
  }
  result
}

prob_A_beats_B <- function(alphaB, betaB, alphaA, betaA) {
  if(alphaB <= 0 || betaB <= 0 || alphaA <= 0 || betaA <= 0)
    result <- 0
  else {
    result <- 1
    for (i in 0:(alphaB - 1)) {
      result <- result - 
        exp(
          lbeta(alphaA + i, betaA + betaB) -
            log(betaB + i) -
            lbeta(1 + i, betaB) -
            lbeta(alphaA, betaA)
        )
    }
  }
  result
}
