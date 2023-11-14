# Arbitrage Calculator
# Adam Wickwire - 2023


# Function to Calculate Arbitrage between two betting sites for given stakes
# Input: 2 Odds, 2 Stakes
# Output: Total Stake, Total Payout, and Net Profit

# Function to Calculate Arbitrage
# Modified Arbitrage Function
arbitrage <- function(odds1, odds2, stake1) {
  # Internal function to convert American odds to Decimal odds
  convert_odds <- function(american_odds) {
    if (american_odds > 0) {
      return(american_odds / 100 + 1)
    } else {
      return(-100 / american_odds + 1)
    }
  }
  
  # Convert American odds to Decimal odds
  decimal_odds1 <- convert_odds(odds1)
  decimal_odds2 <- convert_odds(odds2)
  
  # Calculate the stake for the second bet to equalize the payout
  stake2 <- (stake1 * decimal_odds1) / decimal_odds2
  
  # Calculate Total Stake
  total_stake <- stake1 + stake2
  
  # Calculate Total Payout (the same for either outcome)
  total_payout <- stake1 * decimal_odds1
  
  # Calculate Net Profit
  net_profit <- total_payout - total_stake
  
  # Return Results
  return(round(c(stake1, stake2, total_stake, total_payout, net_profit), 2))
}

# Example usage
arbitrage(120, 110, 100)


