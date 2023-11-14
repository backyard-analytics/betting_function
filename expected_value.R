# Odds Expected Value Calculator 
# Adam Wickwire - 2023

# A function that takes the odds, the probability of winning and bet amount 
# and returns the expected value of the bet
# Input: Odds, Probability of Winning, Bet Amount
# Output: Expected Value of Bet

# Function to calculate the expected value of a bet
expected_value_calculator <- function(odds, probability_percentage, bet_amount) {
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
  
  # Calculate expected value
  expected_value <- (decimal_odds * probability * bet_amount) - bet_amount
  
  return(expected_value)
}

# Example use
expected_value_calculator(110, 60, 100)