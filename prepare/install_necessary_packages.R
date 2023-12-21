

r_files <- list.files(recursive = TRUE)
r_files <- r_files[tolower(substr(r_files, nchar(r_files) - 1, nchar(r_files))) == ".r"]

pkg_list <- c()
for (i in 1:length(r_files)){

  r_file <- readLines(r_files[i])
  x <- (1:length(r_file))[r_file == "box::use("]
  y <- (1:length(r_file))[r_file == ")"]

  if (all(!is.na(c(x, y))) & length(x) > 0 & length(y) > 0){

    y <- y[y > x[1]]
    x <- x[1]
    y <- y[1]
    pkg <- r_file[(x + 1):(y - 1)]
    pkg <-
      pkg[
        !grepl("R/", pkg) &
          grepl("[", pkg, fixed = TRUE)
      ]
    pkg <-
      pkg |>
      strsplit("[", fixed = TRUE) |>
      lapply(\(x) x[1]) |>
      unlist() |>
      trimws()

    if (!is.null(pkg)){
      if (length(pkg) > 0){

        pkg_list <- c(pkg_list, pkg)

      }


    }
  }

  rm(x, y, r_file, pkg)

}
rm(i, r_files)

pkg_list <-
  pkg_list |>
  unique() |>
  sort()

install.packages(pkg_list)
