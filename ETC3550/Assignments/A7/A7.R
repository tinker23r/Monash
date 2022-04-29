library(fpp3, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(glue, warn.conflicts = FALSE)

ar2 <- function(n, c, phi1, phi2, sigma) {
    # Input Verifier
    # -1 < <U+03D5>_2 < 1
    if (-1 >= phi1 & phi1 >= 1) warning("Phi2 is not between -1 and 1")

    # <U+03D5>_1 + <U+03D5>_2 < 1
    if (phi1 + phi2 >= 1) warning("Phi1 + Phi2 is greater than or equal to 1")

    # <U+03D5>_1 - <U+03D5>_2 < 1
    if (phi1 - phi2 >= 1) warning("Phi1 - Phi2 is greater than or equal to 1")

    output_vect <- rep(0, n)
    output_vect[1] <- c / (1 - phi1 - phi2)
    output_vect[2] <- c / (1 - phi1 - phi2)

    for (i in 3:n) {
       output_vect[i] <- c +
           phi1 * output_vect[i - 1] + phi2 * output_vect[i - 2]
    }

    return(output_vect)
}

ar2(400, 3, 0.3, 0.2, 3)