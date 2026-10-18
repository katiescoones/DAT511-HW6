### 4. Ranking Hospitals Across All States

rankall <- function(outcome, num = "best") {

    # Load the outcome data
    data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")

    # Identify columns for each outcome
    outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)

    # Validate the outcome
    if (!(outcome %in% names(outcomes))) {stop("invalid outcome")}

    # Convert the selected mortality column to numeric
    data[, outcomes[outcome]] <-as.numeric(data[, outcomes[outcome]])

    # Get the states in alphabetical order
    states <- sort(unique(data[, 7]))

    # Create the results data frame
    results <- data.frame(hospital = rep(NA_character_, length(states)),state = states,stringsAsFactors = FALSE)

    # Rank hospitals separately within each state
    for (i in seq_along(states)) {

        state_data <- data[data[, 7] == states[i],
                           c(2, outcomes[outcome])]

        # Remove hospitals with missing mortality rates
        state_data <- state_data[!is.na(state_data[, 2]), ]

        # Sort by mortality rate, then hospital name
        state_data <- state_data[order(state_data[, 2], state_data[, 1]), ]

        # Determine the requested rank
        if (identical(num, "best")) {
            rank_num <- 1
        } else if (identical(num, "worst")) {
            rank_num <- nrow(state_data)
        } else {
            rank_num <- num
        }

        # Return the hospital if the rank exists
        if (is.numeric(rank_num) &&
            length(rank_num) == 1 &&
            !is.na(rank_num) &&
            rank_num >= 1 &&
            rank_num == floor(rank_num) &&
            rank_num <= nrow(state_data)) {

            results$hospital[i] <- state_data[rank_num, 1]
        }
    }

    # Return hospital and state
    results
}
