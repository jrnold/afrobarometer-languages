#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library("yaml")
  library("jsonlite")
})

in_file <- commandArgs(TRUE)[1]
out_file <- commandArgs(TRUE)[2]
if (is.na(in_file) | is.na(out_file)) {
  stop("Usage: yaml2json.R {in_file} {out_file}")
}

write_json(read_yaml(in_file), path = out_file)
