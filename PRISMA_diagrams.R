# Create PRISMA flow diagrams

# directions for package: https://cran.r-project.org/web/packages/PRISMAstatement/vignettes/PRISMA.html
# install.packages("PRISMAstatement")

library(PRISMAstatement)

# Food Sovereignty

prisma(found = 4873,
       found_other = 152,
       no_dupes = 4596, 
       extra_dupes_box = TRUE,
       screened = 4596, 
       screen_exclusions = 4099, 
       full_text = 497,
       full_text_exclusions = 335, 
       qualitative = 162, 
       quantitative = 0,
       width = 800, height = 800)
