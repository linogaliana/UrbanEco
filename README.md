UrbanEco
================
Lino Galiana
29 décembre 2017

Goal
====

This `github` repository has been created as a complement to the Sciences Po [Urban Economics lessons](http://formation.sciences-po.fr/enseignement/2017/KSTU/2210?_ga=2.66993710.1010278484.1514545681-872345418.1513268370) I am giving. Its first aim is to give students examples and tools to handle and represent spatial data. However, anybody interested in spatial data can find something that might interest him/her.

This `repo` is dedicated to R programming language. It shows that spatial data can be handled as any type of data. It also shows that R is as powerful to represent data as any GIS software. Though above the scope of the tutorials presented here, people that want to use QGIS can complement it with R to pre-process data (a tutorial on how to integrate R in QGIS can be found [here](https://www.sigterritoires.fr/index.php/utiliser-r-dans-les-traitements-qgis/). If you want to execute QGIS in R, see [here](http://jannes-m.github.io/RQGIS/))

All spatial data presented are open-source data, mostly coming from [data.gouv](https://www.data.gouv.fr/fr/), the French official statistics website. In this repository, you can find the following tutorials:

-   [Analyze Paris free biking system with R](https://cdn.rawgit.com/linogaliana/UrbanEco/378746f4/vignettes/leaflet_map.html?raw=true): a tutorial on how to build pedagogical maps with `leaflet`. Tutorial is illustrated with May 2017 Vélib data, the Paris free biking system.

The tutorials are available `vignettes` for the `UrbanEco` package (see below for installation). The `R Markdown` used to generate these files are available in the `/vignettes` directory of the repository. It is recommended to look at the HTML versions. Since github has some troubles to represent HTML pages, it is recommended to follow the links given above to access these pages.

To help people that would like to use some functions presented (e.g. to get *xy* locations of a French address, to easily get the shape of a city for plotting...), a few functions have been integrated in a package.

Getting started with `UrbanEco`
===============================

To install the package, you can use the following commands

``` r
install.packages("devtools")
devtools::install_github("linogaliana/UrbanEco")
```
