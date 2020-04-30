# Minimal detectable flux estimator

## Purpose

This app calculates the detection limit of static chamber-based soil greenhouse gas flux measurements.
There are two "minimal detectable flux" (MDF) estimates that have different purposes

* non-linear: Allowing zero-fluxes to be estimated by the non-linear HMR regression will lead to high uncertainties and hence a high MDF. This value should be used to restrict the MDL for the dynamic `kappa.max` selection method in the gasfluxes package (see methods)

* robust-linear: Use this option to always apply the robust-linear flux estimate of the `gasfluxes` function. This lower MDF is useful to describe a GHG measurement system in a publication. It represents a low estimate of the accuary of your flux measurements.

Try out the [Soil GHG Flux shiny tool](https://sae-interactive-data.ethz.ch/gasflxvis/) or check [Hueppi et al. 2018 on PLOSone](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0200876) for more details.

### Method

The [`gasfluxes` package](https://cran.r-project.org/web/packages/gasfluxes) is used to calculate the fluxes chosing HMR if available but otherwise applying linear regression. The detection limit of the chamber based greenhouse gas flux measurement is defined as 95 % confidence intervall according to [Parkin et al. 2011](https://dl.sciencesocieties.org/publications/jeq/abstracts/41/3/705). The calculation is also described in the help text of the `selectfluxes` function.

### Background concentrations

* N<sub>2</sub>O: 330 ppb
* CO<sub>2</sub>: 400'000 ppb
* CH<sub>4</sub>: 1'800 ppb

insert all GC.sd values in ppb!

### SAE interactive data shiny server

Find [this app on our SAE server](https://sae-interactive-data.ethz.ch/minflxlim/)

Issues and suggestions can be contributed on GitHub.

Feedback and questions please send to roman.hueppi@usys.ethz.ch
