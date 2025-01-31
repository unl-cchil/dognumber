## Do dogs follow Weber’s Law? The role of ratio and difference in quantity preference

-   Created on 2024-09-23 by Jeffrey R. Stevens
    (<jeffrey.r.stevens@gmail.com>)
-   Finalized on 2025-01-31

This repository provides the reproducible research materials for our
project that investigates how dogs quantify amounts of food differently
depending on their numerical ratio and difference. Materials are
available at [Open Science Framework](https://osf.io/tp8ah/):

-   Data
-   R script for data analysis
-   R Markdown file for the manuscript

## Citation

If you use any of these materials, please cite:

> de Boer, H., Fitzpatrick, H., Wolff, L.M., Gatesy-Davis, A., &
> Stevens, J.R. (forthcoming). Do dogs follow Weber’s Law? The role of
> ratio and difference in quantity preference. *Psychological Topics*.
> [doi:10.31234/osf.io/rn8gq](https://doi.org/10.31234/osf.io/rn8gq)

## Summary

This study conducted 10 sessions of a food quantity preference task with
7 dogs at Uplifting Paws dog daycare center in Lincoln, Nebraska from
March-July 2023. Within each session dogs experienced one trial of each
of nine numerical pairs varying in their numerical difference
(large-small) and ratio (small/large) and two trials of a \[1,6\]
‘washout’ pair. In addition, the dataset includes data from
[Rivas-Blanco et al. (2020)](https://doi.org/10.3389/fpsyg.2020.573317)
on dog and wolf quantity discrimination. In the data file, each row
represents the information and choice for a single trial for one
subject.

## License

All materials presented here are released under the Creative Commons
Attribution 4.0 International Public License (CC BY 4.0). You are free
to:

-   Share — copy and redistribute the material in any medium or format
-   Adapt — remix, transform, and build upon the material for any
    purpose, even commercially. Under the following terms:
-   Attribution — You must give appropriate credit, provide a link to
    the license, and indicate if changes were made. You may do so in any
    reasonable manner, but not in any way that suggests the licensor
    endorses you or your use.

No additional restrictions — You may not apply legal terms or
technological measures that legally restrict others from doing anything
the license permits.

## Files

### Data files

`deboer_etal_2024_data.csv`

| Variable | Description |
|:--------------------|:--------------------------------------------------|
| study | Study (Current Study or Rivas-Blanco et al. 2020) |
| dog_id | Subject ID |
| date | Session date |
| session | Session number (includes failed sessions) |
| block | Block number (only includes completed sessions) |
| trial | Trial number |
| pair | Numerical pair (small:large) |
| small | Small amount |
| large | Large amount |
| diff | Numerical difference (large - small) |
| ratio | Numerical ratio (small / large) |
| large_side | Side of the large amount |
| choice_side | Side of chosen option |
| choice | Choice of larger amount (1 = large, 0 = small) |
| recode_side | Side of recoded choice |
| dog_age | Subject age |
| dog_sex | Subject sex |
| dog_neutered | Sex neuter status (Yes = neutered/spayed, No = intact |
| dog_weight | Subject weight |
| dias_overall | Dog Impulsivity Assessment Scale overall score |
| owner_age | Owner age |
| owner_gender | Owner gender |
| owner_marital_status | Owner marital status |
| employment_status | Owner employment status |
| household_income | Owner household income |

### R code

`deboer_etal_2024_rcode.R` - code for running computations and
generating figures

### R Markdown documents

`deboer_etal_2024.Rmd` - R Markdown document with R code embedded for
main manuscript and appendix

### Installation

To reproduce these results, first clone or unzip the Git repository into
a folder. Then, ensure that a subfolder named “figures” is in the
folder. Next, open `deboer_etal_2024_rcode.R` in
[RStudio](https://rstudio.com) or another R interface and ensure that
all packages mentioned at the top of the script are installed. Once all
packages are installed, run the script in R using
`source("deboer_etal_2024_rcode.R")`.

Once the script runs without errors, you can compile the R Markdown
document `deboer_etal_2024.Rmd.` Open this file in RStudio and ensure
that you have [{knitr}](https://yihui.org/knitr/) and
[Quarto](https://quarto.org/) installed. Once installed, render the
document (control-shift-K).

# Dataset Metadata

The following table is necessary for this dataset to be indexed by
search engines such as <a href="https://g.co/datasetsearch">Google
Dataset Search</a>.

<table>
<tr>
<th>
property
</th>
<th>
value
</th>
</tr>
<tr>
<td>
name
</td>
<td>
<code itemprop="name">Dog food quantity preference dataset</code>
</td>
</tr>
<tr>
<td>
description
</td>
<td>
<code itemprop="description">The dataset from the paper
<a href="https://doi.org/10.31234/osf.io/rn8gq">Do dogs follow Weber’s
Law? The role of ratio and difference in quantity preference</a>. This
study conducted 10 sessions of a food quantity preference task with 7
dogs at Uplifting Paws dog daycare center in Lincoln, Nebraska from
March-July 2023. Within each session dogs experienced one trial of each
of nine numerical pairs varying in their numerical difference
(large-small) and ratio (small/large) and two trials of a \[1,6\]
‘washout’ pair. In addition, the dataset includes data from
[Rivas-Blanco et al. (2020)](https://doi.org/10.3389/fpsyg.2020.573317)
on dog and wolf quantity discrimination. In the data file, each row
represents the information and choice for a single trial for one
subject.</code>
</td>
</tr>
</tr>
<tr>
<td>
url
</td>
<td>
<code itemprop="url"><https://github.com/unl-cchil/dognumber></code>
</td>
</tr>
<tr>
<td>
sameAs
</td>
<td>
<code itemprop="sameAs"><https://github.com/unl-cchil/dognumber></code>
</td>
</tr>
<tr>
<td>
citation
</td>
<td>
<code itemprop="citation"><https://doi.org/10.31234/osf.io/rn8gq></code>
</td>
</tr>
<tr>
<td>
license
</td>
<td>

<table>
<tr>
<th>
property
</th>
<th>
value
</th>
</tr>
<tr>
<td>
name
</td>
<td>
<code itemprop="name">CC BY-SA 4.0</code>
</td>
</tr>
<tr>
<td>
url
</td>
<td>
<code itemprop="url"><https://creativecommons.org/licenses/by-sa/4.0/></code>
</td>
</tr>
</table>

</td>
</tr>
</table>
