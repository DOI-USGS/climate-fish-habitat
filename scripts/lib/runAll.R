#' @import yaml
runAll <- function() {
  # TODO set working directory to root of repository
  viz.yaml <- yaml::yaml.load_file("viz.yaml")
  
  if (!dir.exists("target")){
    dir.create("target")
  }
  buildPages()
  # TODO describe this stuff in yaml
  file.copy("images", "target", recursive = TRUE)
  file.copy("layout/js", "target", recursive = TRUE)
  file.copy("layout/css", "target", recursive = TRUE)
}