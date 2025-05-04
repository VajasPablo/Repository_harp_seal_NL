
Total_pop <- 4.7 #Millions
Total_pop_2J3K <- Total_pop/2
Total_pop_3LNO <- Total_pop/2

quota <- 0.450000 # or 0.450 or 0.600

(quota/Total_pop_2J3K)*100
(quota/Total_pop_3LNO)*100

results_per <- (quota/Total_pop_2J3K)*100

# NL Shelf

# Format the data
lod <- c(0, 25, 50, 75, 99)
per <- c(48.928052, 59.502053, 69.331273, 82.133233, 97.886566)

# Create an approximation function
approximate_per <- function(lod_value) {
  # Linear interpolation
  interpolated <- approx(x = lod, y = per, xout = lod_value, method = "linear")
  return(interpolated$y)
}

# Find the value of PER for LOD = 19.15
lod_target <- results_per
per_at_lod_19_15 <- approximate_per(lod_target)

# Display the result
cat("The estimated value of PER for LOD =", lod_target, "is:", per_at_lod_19_15, "\n")

Per_obs <- 52.497389

Growth_percentage_cod_2J3K <- per_at_lod_19_15 - Per_obs

Growth_percentage_cod_2J3K

# Grand Banks

# Format the data
lod <- c(0, 25, 50, 75, 99)
per <- c(11.893697, 16.65727, 21.837024, 30.232927, 42.604460)

# Create an approximation function
approximate_per <- function(lod_value) {
  # Linear interpolation
  interpolated <- approx(x = lod, y = per, xout = lod_value, method = "linear")
  return(interpolated$y)
}

# Find the value of PER for LOD = 19.15
lod_target <- results_per
per_at_lod_19_15 <- approximate_per(lod_target)

# Display the result
cat("The estimated value of PER for LOD =", lod_target, "is:", per_at_lod_19_15, "\n")

Per_obs <- 13.682671

Growth_percentage_cod_3LNO <- per_at_lod_19_15 - Per_obs

# Results 

Growth_percentage_cod_2J3K
Growth_percentage_cod_3LNO

