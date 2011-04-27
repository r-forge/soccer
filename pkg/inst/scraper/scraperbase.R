
library("XML")

options(logger = TRUE)


logger <- function(txt) {
  if ( !is.null(options()$logger) ) {
    cat(txt, "\n")
  }
}


saveit <- function(object) {
  name <- {
    name <- substitute(object)
    if ( is.name(name) )
      as.character(name)
    else
      stop("cannot determine a usable name")
  }

  filename <- sprintf("../../../data/%s.RData", name)

  save(file = filename, list = name)

  message(gettextf("Created file named '%s'.", filename),
          domain = NA)

  invisible(filename)
}


documentit <- function(object, title, source, date, scraper) {
  paste0 <- function(...) paste(..., sep = "")

  name <- {
    name <- substitute(object)
    if ( is.name(name) )
      as.character(name)
    else
      stop("cannot determine a usable name")
  }

  filename <- sprintf("../../../man/%s.Rd", name)


  fmt <- c("\\format{",
           paste("  A data frame with", nrow(object),
                 "observations on the following",
                 ifelse(ncol(object) == 1, "variable.", paste(ncol(object), "variables."))),
           "  \\describe{")
  for (i in names(object)) {
    xi <- object[[i]]
    fmt <- c(fmt, paste0("    \\item{\\code{", i, "}}{",
                         if (inherits(xi, "factor")) {
                           paste("a factor with", nlevels(xi), "levels", collapse = " ")
                         } else if (is.vector(xi)) {
                           paste("a", data.class(xi), "vector")
                         } else if (is.matrix(xi)) {
                           paste("a matrix with", ncol(xi), "columns")
                         } else {
                           paste("a", data.class(xi))
                         }, "}"))
  }
  fmt <- c(fmt, "  }", "}")

  Rdtxt <- list(name = paste0("\\name{", name, "}"),
                aliases = paste0("\\alias{", name, "}"),
                docType = "\\docType{data}",
                title = paste0("\\title{", title, "}"),
                description = c("\\description{", paste0(title, ". Screen scraped by \\code{scraper/", scraper, "} on ", date, "."), "}"),
                usage = paste0("\\usage{data(", name, ")}"),
                format = fmt,
                #details = c("\\details{", paste("%%  ~~ If necessary, more details than the", "__description__ above ~~"), "}"),
                source = paste0("\\source{", source, "}"),
                # references = c("\\references{", "%%  ~~ possibly secondary sources and usages ~~", "}"),
                examples = c("\\examples{", paste0("data(", name, ")"), paste0("## maybe str(", name, ")"), "}"),
                keywords = "\\keyword{datasets}")

  cat(unlist(Rdtxt), file = filename, sep = "\n")

  message(gettextf("Created file named '%s'.", filename),
          domain = NA)

  invisible(filename)
}
