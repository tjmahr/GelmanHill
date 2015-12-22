# Create markdown versions of R scripts

ch07_01 <- "examples/Ch07/07-01_SimulationOfProbabilityModels.R"
rmarkdown::render(ch07_01, output_format = "md_document",
                  output_options = list(variant = "markdown_github"))

ch07_02 <- "examples/Ch07/07-02_SummarizingLinearRegressionUsingSimulation.R"
rmarkdown::render(ch07_02, output_format = "md_document",
                  output_options = list(variant = "markdown_github"))


# Note: Can use knitr::purl to reduce an Rmd file into an R script
# knitr::purl("examples/Ch07/07-01_SimulationOfProbabilityModels.Rmd", documentation = 2)
