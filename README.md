Repository for the 2016 Mohonk Butterfly Biodiversity Survey. The files included in this repository allow any user to completely reproduce the report I have generated.

This work of research is citable: [![DOI](https://zenodo.org/badge/93018667.svg)](https://zenodo.org/badge/latestdoi/93018667)

There is a rendered `.html` [website](https://butterflyology.github.io/Mohonk_report/Mohonk_report.html) and also a `.pdf` that you can download.

This report has a Zenodo DOI and can be cited as:

Files and directories:
* <kbd>code/</kbd>
  * `plot_forecast_componants.R` - A function from the `prohpet` package that I extracted so that I could just plot annual temperature forecasts.
  * `simulate_gamm.R` - Function to simulate `gamm` from [Simon Wood](https://gist.github.com/gavinsimpson/d23ae67e653d5bfff652).
  * `sarima_for.R` - I manipulated the `sarima.for` code from the `astsa` package to plot a bit more cleanly.
* <kbd>data/</kbd>
  * `Mohonk_temps.csv` - Daily weather data from 1900-2016 from the Mohonk Preserve's weather station.
  * `Mohonk_traps_data.csv` - Tidy abundance data from trap survey of Mohonk Preserve in 2016.
* `evolution.csl` - The citation style guide for the journal *Evolution* that `BibTeX` uses for formatting.
* <kbd>images/</kbd> - where the images used to construct the report live (those that are not generated from the data).
* <kbd>misc/</kbd> - Where things in the `.gitignore` directory mostly live.
* `Mohonk_refs.bib` - THe `BibTeX` file that contains the references included in this report.
* <kbd>Mohonk_report_cache</kbd> - `LaTeX` cache.
* <kbd>Mohonk_report_files</kbd> - all the images generated from data through each iteration of the report for `LaTeX` and `.html`.
* `Mohonk_report.html` -  The `.html` code that is rendered for the [`gh-pages`](https://butterflyology.github.io/Mohonk_report/Mohonk_report.html) branch.
* `Mohonk_report.pdf` - The output document generated by the `.Rmd`  and `LaTeX`.
* `Mohonk_report.Rmd` - The `RMarkdown` file that generates the `.pdf` report. Please note that the statistical code used to generate the figures and analyses presented in the report are located here.
* `Mohonk.Rproj` - The `RStudio` project that houses the documents and settings to generate this report.
* <kbd>output/</kbd> - a compressed version of this repository can be found here.
* `README.md` - this file, which tells you about the things in this repository and project.


---
List of major commits:

1. 2017-05-31: Initial commit to repo (*nb* the code and analysis had been worked out for some time).
1. 2017-06-05: Added mean temp by month plot. Commit pushed from 35,000 over the Rockies.
1. 2017-06-22: Added `gamm()` models; fixed: caption issue, formula; removed: lm and annual plot.
1. 2017-09-18: Redesigned figure 3 using `plot.new = TRUE`, added `sarima` analysis.
1. 2017-12-03: Continued to refine text, added `prophet` models. Commit pushed from 31,000 over Pennsylvania.
1. 2017-12-16: Added bibtex entries, added text to report.
1. 2017-12-17: More editing.
1. 2017-12-20: Made `prophet` analysis fully Bayesian and not MAP.
1. 2017-12-21: Further text edits and `.bib` updates.
1. 2018-01-01: Complete draft assembled and converted into both `.pdf` and `.html` versions.
1. 2018-01-02: Repository assigned ZENODO ID.
