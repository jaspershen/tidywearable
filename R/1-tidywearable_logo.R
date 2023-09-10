#' @title Show the logo tidywearable.
#' @description The tidywearable logo, using ASCII or Unicode characters
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param unicode Whether to use Unicode symbols. Default is `TRUE`
#' on UTF-8 platforms.
#' @return A ASCII log of tidywearable
#' @export
#' @importFrom dplyr filter
#' @import laggedcor
#' @import wearabledataset
#' @import masstools
#' @importFrom magrittr %>%
#' @importFrom utils packageVersion install.packages read.table
#' @examples
#' tidywearable_logo()
##https://onlineasciitools.com/convert-text-to-ascii-art

tidywearable_logo <-
  function(unicode = l10n_info()$`UTF-8`) {
    message(crayon::green("Thank you for using tidywearable!"))
    message(crayon::green("Version", tidywearable_version, "(", update_date, ')'))
    message(crayon::green("More information: tidywearable.org"))

    logo =
      c("  _   _     _    __          __                  _     _",
        " | | (_)   | |   \\ \\        / /                 | |   | |",
        " | |_ _  __| |_   \\ \\  /\\  / /__  __ _ _ __ __ _| |__ | | ___",
        " | __| |/ _` | | | \\ \\/  \\/ / _ \\/ _` | '__/ _` | '_ \\| |/ _ \\",
        " | |_| | (_| | |_| |\\  /\\  /  __/ (_| | | | (_| | |_) | |  __/",
        "  \\__|_|\\__,_|\\__, | \\/  \\/ \\___|\\__,_|_|  \\__,_|_.__/|_|\\___|",
        "               __/ |", "              |___/")


    hexa <- c("*", ".", "o", "*", ".", "*", ".", "o", ".", "*")
    if (unicode)
      hexa <- c("*" = "\u2b22", "o" = "\u2b21", "." = ".")[hexa]

    cols <- c(
      "red",
      "yellow",
      "green",
      "magenta",
      "cyan",
      "yellow",
      "green",
      "white",
      "magenta",
      "cyan"
    )

    col_hexa <- purrr::map2(hexa, cols, ~ crayon::make_style(.y)(.x))

    for (i in 0:9) {
      pat <- paste0("\\b", i, "\\b")
      logo <- sub(pat, col_hexa[[i + 1]], logo)
    }

    structure(crayon::blue(logo), class = "tidywearable_logo")
  }

#' @export

print.tidywearable_logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}

tidywearable_version <-
  as.character(utils::packageVersion("tidywearable"))
update_date <-
  as.character(Sys.time())


# library(cowsay)
# # https://onlineasciitools.com/convert-text-to-ascii-art
# # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
# art <- readLines("logo.txt")
# dput(art)
# tidywearable_logo <-
#   c("  _   _     _    __          __                  _     _",
#     " | | (_)   | |   \\ \\        / /                 | |   | |",
#     " | |_ _  __| |_   \\ \\  /\\  / /__  __ _ _ __ __ _| |__ | | ___",
#     " | __| |/ _` | | | \\ \\/  \\/ / _ \\/ _` | '__/ _` | '_ \\| |/ _ \\",
#     " | |_| | (_| | |_| |\\  /\\  /  __/ (_| | | | (_| | |_) | |  __/",
#     "  \\__|_|\\__,_|\\__, | \\/  \\/ \\___|\\__,_|_|  \\__,_|_.__/|_|\\___|",
#     "               __/ |", "              |___/")
# cat(tidywearable_logo, sep = "\n")
