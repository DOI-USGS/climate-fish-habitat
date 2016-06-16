
buildJS <- function(viz.yaml) {
  pages <- viz.yaml[["pages"]]
  
  for (page in pages) {
    template <- readLines("layout/templates/sectionAnalytics-js.mustache")
    cat(whisker.render(template = template, partials=NULL, data = page), file=paste0("target/js/", page$id, ".js"))
  }
}