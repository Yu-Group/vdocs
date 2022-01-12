utils::globalVariables(c("where", ":=", "!!"))

#' Global environment variable to keep track of chunk indices for subchunkify
.chunk_globalenv <- new.env(parent = emptyenv())
