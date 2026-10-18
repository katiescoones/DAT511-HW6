### 3. Ranking Hospitals by Outcome in a State


rankhospital <- function(state, outcome, num = "best") {

    # Load the outcome data
    data <- read.csv("outcome-of-care-measures.csv",
                     colClasses = "character")

    # Identify columns for each outcome
    outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)

    # Validate the outcome
    if (!(outcome %in% names(outcomes))) {stop("invalid outcome")}

    # Validate the state
    if (!(state %in% data[, 7])) {stop("invalid state")}

    # Convert selected mortality column to numeric
    data[, outcomes[outcome]] <- as.numeric(data[, outcomes[outcome]])

    # Keep hospitals in the selected state
    state_data <- data[data[, 7] == state, c(2, outcomes[outcome])]

    # Remove hospitals with missing mortality rates
    state_data <- state_data[!is.na(state_data[, 2]), ]

    # Sort by mortality rate, then hospital name
    state_data <- state_data[order(state_data[, 2], state_data[, 1]), ]

    # Determine the requested rank
    if (identical(num, "best")) {
        num <- 1
    } else if (identical(num, "worst")) {
        num <- nrow(state_data)
    }

    # Return NA if the rank is invalid or too large
    if (!is.numeric(num) || length(num) != 1 ||
        is.na(num) || num < 1 ||
        num != floor(num) || num > nrow(state_data)) {
        return(NA_character_)
    }

    # Return the hospital at the requested rank
    state_data[num, 1]
}
