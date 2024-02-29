#' Maximum permissible errors for weights
#'
#' A dataset with maximum permissible errors for weights of different classes.
#' Errors are are expresses in mg.
#'
#' @format a data.frame with 30 rows and 10 columns.
#'
#' @name oiml_r111
#' @author International organization of legal metrology
#' @source Weights of classes E1, E2, F1, F2, M1, M1-2, M2,
#' M2-3, and M3. Part 1: metrological and technical requirements.
#' Edition 2004 (E). Table 1, pag. 12.
#' \url{https://www.oiml.org/en/files/pdf_r/r111-1-e04.pdf}
#' @keywords data
oiml_r111 <- data.frame(
  nominal = c(5000, 2000, 1000, 500, 200, 100, 50, 20, 10, 5, 2, 1,
              500, 200, 100, 50, 20, 10, 5, 2, 1,
              500, 200, 100, 50, 20, 10, 5, 2, 1,
              0),
  #nominal weights in grams added
  nominal_g = c(
    c(5000, 2000, 1000, 500, 200, 100, 50, 20, 10, 5, 2, 1) * 10^3, #kg
    c(500, 200, 100, 50, 20, 10, 5, 2, 1), #g
    c(500, 200, 100, 50, 20, 10, 5, 2, 1)* 10^-3, #mg
    0
    ),
  udm = c(rep("kg", 12), rep("g", 9), rep("mg", 10)),
  e1 = c(rep(NA, 6), 25, 10, 5.0, 2.5, 1.0, 0.5,
         0.25, 0.10, 0.05, 0.03, 0.025, 0.020, 0.016, 0.012, 0.010,
         0.008, 0.006, 0.005, 0.004, rep(0.003, 5), 0),
  e2 = c(rep(NA, 2), 1600, 800, 300, 160, 80, 30, 16, 8.0, 3.0, 1.6,
         0.8, 0.3, 0.16, 0.10, 0.08, 0.06, 0.05, 0.04, 0.03,
         0.025, 0.020, 0.016, 0.012, 0.010, 0.008, rep(0.006, 3), 0),
  f1 = c(25000, 10000, 5000, 2500, 1000, 500, 250, 100, 50, 25, 10, 5.0,
         2.5, 1.0, 0.5, 0.3, 0.25, 0.20, 0.16, 0.12, 0.10,
         0.08, 0.06, 0.05, 0.04, 0.03, 0.025, rep(0.020, 3), 0),
  f2 = c(80000, 10000, 16000, 8000, 3000, 1600, 800, 300, 160, 80, 30, 16,
         8.0, 3.0, 1.6, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3,
         0.25, 0.20, 0.16, 0.12, 0.10, 0.08, rep(0.06, 3), 0),
  m1 = c(250e3, 100e3, 50e3, 25e3, 10e3, 5e3, 2.5e3, 1e3, 500, 250, 100, 50,
         25, 10, 5.0, 3.0, 2.5, 2.0, 1.6, 1.2, 1.0,
         0.8, 0.6, 0.5, 0.4, 0.3, 0.25, rep(0.20, 3), 0),
  m12 = c(500e3, 200e3, 100e3, 50e3, 20e3, 10e3, 5e3, rep(NA, 24)),
  m2 = c(800e3, 300e3, 160e3, 80e3, 30e3, 16e3, 8e3, 3e3, 1.6e3, 800, 300, 160,
         80, 30, 16, 10, 8.0, 6.0, 5.0, 4.0, 3.0,
         2.5, 2.0, 1.6, rep(NA, 7)),
  m23 = c(1.6e6, 600e3, 300e3, 160e3, 60e3, 30e3, 16e3, rep(NA, 24)),
  m3 = c(2.5e6, 1.0e6, 500e3, 250e3, 100e3, 50e3, 25e3, 10e3, 5e3, 2.5e3, 1.0e3, 500,
         250, 100, 50, 30, 25, 20, 16, 12, 10, rep(NA, 10))
  )

usethis::use_data(oiml_r111, overwrite = TRUE)
