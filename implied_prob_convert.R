# Odds Converter / Implied Probability Calculator
# Adam Wickwire - 2023


# A function that takes either the american odds, the decimal odds, or fraction odds 
# and returns the implied probability of winning the bet
# Input: Odds
# Output: Implied Probability of Winning Bet, American Odds, Decimal Odds, Fractional Odds


# Function to convert American, Decimal, or Fraction odds to implied probability
calculate_odds_and_probability <- function(odds) {
  # Utility function to find the Greatest Common Divisor
  gcd <- function(a, b) {
    if(b == 0) {
      return(a)
    } else {
      return(gcd(b, a %% b))
    }
  }
  
  # Internal function to convert American odds to Decimal odds
  convert_odds <- function(american_odds) {
    if (american_odds > 0) {
      return(american_odds / 100 + 1)
    } else {
      return(-100 / american_odds + 1)
    }
  }
  
  # Convert input odds to Decimal odds
  decimal_odds <- if (is.numeric(odds)) {
    if (odds %% 1 == 0) { # Checks if odds are integer (likely American odds)
      convert_odds(odds)
    } else { # Handles decimal odds
      odds
    }
  } else { # Handles fractional odds
    parts <- strsplit(as.character(odds), "/")[[1]]
    as.numeric(parts[1]) / as.numeric(parts[2]) + 1
  }
  
  # Calculate implied probability
  implied_probability <- round(1 / decimal_odds * 100, 2)
  
  # Convert decimal odds to American odds
  american_odds <- if (decimal_odds >= 2) {
    (decimal_odds - 1) * 100
  } else {
    -100 / (decimal_odds - 1)
  }
  
  # Convert decimal odds to fractional odds
  fractional_odds <- if (decimal_odds != 1) {
    numerator <- round((decimal_odds - 1) * 100)
    denominator <- 100
    gcd_value <- gcd(numerator, denominator)
    paste(round(numerator / gcd_value), "/", round(denominator / gcd_value))
  } else {
    "1/1"
  }
  
  # Return a data frame with all odds formats and the implied probability
  return(data.frame(
    Implied_Probability = implied_probability,
    Decimal_Odds = decimal_odds,
    American_Odds = round(american_odds),
    Fractional_Odds = fractional_odds
  ))
}


# Testing the function
calculate_odds_and_probability(110)      # American odds
calculate_odds_and_probability(2.1)      # Decimal odds
calculate_odds_and_probability("11/10")  # Fractional odds
