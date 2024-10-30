## ADB
## Ecofish Research
## Oct 4, 2018

## Compile Rmarkdown report

require(here)
# compile_rmd function ----------------------------------------------------
compile_rmd <- function(file = basename(here::here()),
                        in.path = 'rmds', 
                        out.path = 'reports') {
  rmarkdown::render(input =       here::here(in.path, paste(file, '.rmd', sep ='')),
                    output_file = here::here(out.path, paste(file, '.html', sep = '') )
  )
}

