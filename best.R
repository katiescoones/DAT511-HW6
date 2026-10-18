### 2. Finding the Best Hospital in a State

best <- function(state, outcome) {
    # Load the outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    # Identify the columns for each outcome
    outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)

    # Validate the outcome
    if (!(outcome %in% names(outcomes))) {stop("invalid outcome")}
    
    # Validate the state
    if (!(state %in% data[, 7])) {stop("invalid state")}
    
    # Convert selected mortality column to numeric
    data[, outcomes[outcome]] <- as.numeric(data[, outcomes[outcome]])

    # Keep hospitals in selected state
    state_data <- data[data[, 7] == state, c(2, outcomes[outcome])]

    # Remove hospitals with missing mortality rates
    state_data <- state_data[!is.na(state_data[,2]),]

    # Sort by mortality rate, then hospital name
    state_data <- state_data[order(state_data[, 2], state_data[, 1]),]

    # Return the hospital with the lowest mortality 
    state_data[1, 1]
}