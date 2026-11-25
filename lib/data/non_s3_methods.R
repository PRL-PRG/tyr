# Regenerates non_s3_methods.txt: the functions whose name looks like an S3
# method (`generic.class`) but that are not, as R CMD check knows them
# (`tools:::nonS3methods`), for every package R lists. tyr does not take them
# for methods (cf. lib/s3.ml). Run it from this directory with a new release
# of R, and commit the result:
#   Rscript non_s3_methods.R
names <- sort(unique(tools:::nonS3methods(NULL)))
writeLines(c(
  sprintf("# tools:::nonS3methods(NULL), %s. Regenerate with non_s3_methods.R.",
          R.version.string),
  names), "non_s3_methods.txt")
