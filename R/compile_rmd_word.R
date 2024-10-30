## ADB
## Ecofish Research
## Oct 4, 2018

## Compile Rmarkdown report

# compile_rmd function ----------------------------------------------------
compile_rmd_word <- function(in.path = 'rmds', out.path = 'reports', file) {
rmarkdown::render(input =       here::here(in.path, paste(file, '.rmd', sep = '')),
                  output_file = here::here(out.path, paste(file, '.docx', sep = '') )
)
                  }

