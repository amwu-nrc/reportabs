# code to prepare `nrc_colours` dataset goes here
nrc_colours <- c(
  `AMWU Orange` = "#FF5800",
  `AMWU Blue` = "#001D26",
  `Website Yellow` = "#ffe000",
  `Website Grey` = "#424242",
  `Sand` = "#F6EEE1",
  `Just White` = "#FFFFFF",
  `Soft Black` = "#2B2228",
  `Hard Black` = "#000000"
)



fof_palettes <- list(
  `main` = nrc_colours[c("AMWU Orange", "Soft Black", "Sand", "AMWU Blue")],
  `amwu` = nrc_colours[c("AMWU Orange",  "AMWU Blue", "Website Grey")],
  `website` = nrc_colours[c("Website Yellow", "Website Grey", "Just White")],
  `just orange` = nrc_colours[c("AMWU Orange", "Just White")],
  `just blue` = nrc_colours[c("AMWU Blue", "Sand")]
)

usethis::use_data(fof_palettes, fof_colours, internal = TRUE, overwrite = TRUE)


