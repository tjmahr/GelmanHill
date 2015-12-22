# Create markdown versions of R scripts

ch07_01 <- "examples/Ch07/07-01_SimulationOfProbabilityModels.R"
rmarkdown::render(ch07_01, output_format = "md_document",
                  output_options = list(variant = "markdown_github"))
