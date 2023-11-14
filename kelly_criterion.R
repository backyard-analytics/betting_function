# Kelly Criterion Calculator
# Adam Wickwire - 2023


# A function that calculates the optimal bet size using the Kelly Criterion 
# Input: Kelly Multiplier, American odds, Probability of Winning
# Output: Expected Value of Bet as percetange, Optimal Bet Size as percentage of bankroll,
#         Optimal Bet Size as dollar amount for 500, 1000, and 5000 bankrolls

# Function to calculate the optimal bet size using the Kelly Criterion
kelly_criterion_calculator <- function(kelly_multi, odds, probability_percentage) {
  # Internal function to convert American odds to Decimal odds
  convert_odds <- function(american_odds) {
    if (american_odds > 0) {
      return(american_odds / 100 + 1)
    } else {
      return(-100 / american_odds + 1)
    }
  }
  
  # Convert odds to decimal odds
  decimal_odds <- convert_odds(odds)
  
  # Convert probability from percentage to decimal
  probability <- probability_percentage / 100
  
  # Calculate optimal bet size as percentage of bankroll
  optimal_bet_size <- (decimal_odds * probability - 1) / (decimal_odds - 1)
  
  # Calculate optimal bet size as dollar amount for 500, 1,000 and 5,000 bankrolls
  optimal_bet_size_500 <- round(optimal_bet_size * 500, 2)
  optimal_bet_size_1000 <- round(optimal_bet_size * 1000, 2)
  optimal_bet_size_5000 <- round(optimal_bet_size * 5000, 2)
  
  # Calculate expected value of bet as percentage
  expected_value <- round((decimal_odds * probability - 1) * 100, 2)
  
  # Calculate expected value of bet as dollar amount for 500, 1,000 and 5,000 bankrolls
  expected_value_500 <- round(expected_value * 5, 2)
  expected_value_1000 <- round(expected_value * 10, 2)
  expected_value_5000 <- round(expected_value * 50, 2)
  
  # Turn optimal bet size into percentage
  optimal_bet_size <- round(optimal_bet_size * 100, 2)
  
  # Return results
  return(data.frame(expected_value = expected_value, 
              optimal_bet_size = optimal_bet_size,
              optimal_bet_size_500 = optimal_bet_size_500,
              optimal_bet_size_1000 = optimal_bet_size_1000, 
              optimal_bet_size_5000 = optimal_bet_size_5000,
              expected_value_500 = expected_value_500,
              expected_value_1000 = expected_value_1000,
              expected_value_5000 = expected_value_5000))
}


# Example use
kelly_criterion_calculator(1, 110, 60)

