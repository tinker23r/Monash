ar2 <- function(n, c, phi1, phi2, sigma) {
    # Input Verifier
    # -1 < phi2 < 1
    if (-1 >= phi2 | phi2 >= 1) warning("Phi2 is not between -1 and 1")

    # phi1 + phi2 < 1
    if (phi1 + phi2 >= 1) warning("Phi1 + Phi2 is greater than or equal to 1")

    # phi2 - phi1 < 1
    if (phi2 - phi1 >= 1) warning("Phi1 - Phi2 is greater than or equal to 1")

    output_vect <- rep(0, n)
    output_vect[1] <- c / (1 - phi1 - phi2)
    output_vect[2] <- c / (1 - phi1 - phi2)

    for (i in 3:n) {
        output_vect[i] <- c +
            phi1 * output_vect[i - 1] +
            phi2 * output_vect[i - 2] +
            rnorm(1, 0, sigma)
    }

    return(output_vect)
}