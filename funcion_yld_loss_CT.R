yield_pot = 3000 
oil_percent = 50 
inc_low = 25
inc_mod = 0 
coef_reduc_weight_low = 0.817 
coef_reduc_weight_mod = 0.703 
coef_reduc_ac_low = 0.946 
coef_reduc_ac_mod = 0.897

calculate_yield_loss <- function(inc_low, inc_mod, oil_percent, yield_pot, 
                                 coef_reduc_weight_low, coef_reduc_weight_mod, 
                                 coef_reduc_ac_low, coef_reduc_ac_mod) {
  # Calculate potential bonus yield
  yield_bonif_potential <- yield_pot * (1 + ((oil_percent - 42) * 2) / 100)
  
  # Calculate healthy and diseased yields
  yield_healthy <- yield_pot * (1 - (inc_low + inc_mod) / 100)
  yield_low <- yield_pot * (inc_low / 100) * coef_reduc_weight_low
  yield_mod <- yield_pot * (inc_mod / 100) * coef_reduc_weight_mod
  yield_final <- yield_healthy + yield_low + yield_mod
  
  # Calculate bonus-adjusted yields
  yield_bonif_healthy <- yield_healthy * (1 + ((oil_percent - 42) * 2) / 100)
  yield_bonif_low <- yield_low * (1 + (((oil_percent * coef_reduc_ac_low) - 42) * 2) / 100)
  yield_bonif_mod <- yield_mod * (1 + (((oil_percent * coef_reduc_ac_mod) - 42) * 2) / 100)
  yield_bonif_diseased <- yield_bonif_healthy + yield_bonif_low + yield_bonif_mod
  
  # Calculate yield loss
  yield_loss <- (1 - yield_bonif_diseased / yield_bonif_potential) * 100
  
  return(list(yield_bonif_potential = yield_bonif_potential,
              yield_final = yield_final,
              yield_bonif_diseased = yield_bonif_diseased,
              yield_loss = yield_loss))
}

# Example usage

result <- calculate_yield_loss(
  yield_pot = 3000, 
  oil_percent = 50, 
  inc_low = 25,
  inc_mod =  25,
  coef_reduc_weight_low = 0.817, 
  coef_reduc_weight_mod = 0.703, 
  coef_reduc_ac_low = 0.946, 
  coef_reduc_ac_mod = 0.897
  )

print(result)
