# Problem 02

#01 Forward Selection Procedure

# Principles:
  # Start with no predictors and add one variable at a time.
  # Choose the predictor that results in the lowest SSE at each step.
  # Once a predictor is included, it remains in the model.

# Step 01: x4
# Step 02: x1, x4
# Step 03: x1, x2, x4
# Step 04: x1, x2, x3, x4


#02 Backward Elimination Procedure

# Principles:
  # Start with all predictors and remove one at a time.
  # Eliminate the predictor that results in the lowest SSE after removal.
  # Once a predictor is eliminated, it is no longer considered.

# Step 01: x1, x2, x3, x4
# Step 02: x1, x2, x3
# Step 03: x2, x3
# Step 04: x3
