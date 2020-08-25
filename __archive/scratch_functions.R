expected_new_cases <- function(N, ps, pi, r0, phi, T = 1, D = 10) {
  (N - )* ps * (1 - (pi * ((1 + (r0 * T) / (phi * D))^(-phi) - 1) + 1)^N) / (1 - (1 - pi) ^ N)
}

expected_r <- function(N, ps, pi, r0, phi, T = 1, D = 10) {
  (N - 1) * ps * (1 - (1 + (r0 * T) / (phi * D))^(-phi))
}

expected_r_all <- function(N, ps, pi, r0, phi, T = 1, D = 10) {
  sum((N - 1:N) * ps / (1 - pi) * (1 - (1 + (r0 * T) / (phi * D)) ^ (-phi * 1:N)) *
        dbinom(1:N, N, pi) / sum(dbinom(1:N, N, pi)))
}

variance_r <- function(N, ps, pi, r0, phi, T = 1, D = 10) {
  N * ps * (1 - (1 + (r0 * T) / (phi * D))^(-phi)) - 
    N * ps^2 * (1 - 2 * (1 + (r0 * T) / (phi * D))^(-phi) + 
                  (1 + (r0 * T) / (phi * D))^(-2*phi))
}

variance_r2 <- function(N, ps, pi, r0, phi, T = 1, D = 10) {
  N * ps * (1 - (1 + (r0 * T) / (phi * D))^(-phi)) * 
    (1 - (1 - (1 + (r0 * T) / (phi * D))^(-phi)))
}

variance_r3 <- function(N, ps, pi, r0, phi, T = 1, D = 10) {
  (N - 1) * ps * (1 - (1 + (r0 * T) / (phi * D))^(-phi)) +
    (N-1) * (N-2) * ps^2 * (1 - 2 * (1 + (r0 * T) / (phi * D))^(-phi) + 
                              (1 + (2 * r0 * T) / (phi * D))^(-phi)) - 
    ((N - 1) * ps * (1 - (1 + (r0 * T) / (phi * D))^(-phi)))^2
}
