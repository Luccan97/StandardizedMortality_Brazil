

### What is it?
Dash link: https://6c6yvn-lucca0nielsen.shinyapps.io/StandardizedMortality_Brazil/

The dashboard offers an interactive visualization of the spatial distribution in the Federative Units of Brazil of the mortality rates standardized by sex and age group
according to the basic causes  of death grouped in the chapters of the ICD-10.

### Why is it important to standardize mortality rates?
Mortality rates are indicators widely used to diagnose the health context of certain places and periods. Understanding the underlying cause of death of populations helps to understand their context and specific characteristics in relation to the way of life and interactions with health determinants and health services.

The crude mortality rate is calculated by the ratio between the number of deaths in a  specific space and time over the population at risk in the same space and time. It can be said that the mortality rate is the incidence of deaths.

The problem with calculating the crude rate is that the comparison between populations with different age profiles becomes impossible. For example, the mortality rate from cardiovascular diseases will naturally be higher in territories with an aging population, just as, in general, the mortality rate from violence will naturally be higher in territories with a younger population.

### How to perform the standardization by age group and sex?
There are many ways to standardize a rate, the most common is to adjust the populations that will be compared by the same standard population.
The standard population used in the construction of this dashboard can be found [at this link.](https://www.opendata.nhs.scot/pt_PT/dataset/standard-populations/resource/2f493d21-fd39-48f9-ad6a-9b2c95b32e30?view_id =ce366795-6a5f-483e-8f42-a9dafe239582)

### Mortality Data from the Mortality Information System (SIM)
The mortality data were downloaded in the RStudio IDE through the microdatasus package, github with more information can be found [here](https://github.com/rfsaldanha/microdatasus).

The variables used were: Municipality of residence, age in years, year of death and underlying cause.
The underlying causes were reclassified and grouped by ICD-10 chapters.

### The annual population bases of the Federative Units
For the construction of the denominators of the mortality rates, the year-by-year projections by level of aggregation of municipalities made available by the [Laboratory of Population Estimates and Projections of the Graduate Program in Demography and Department of Demography and Actuarial Sciences at UFRN]( https://demografiaufrn.net/projecao-populacional/)

### Dash build process:
In this repository are available the scripts
* 00_prepareData.R which reads and cleans the used bases
* App.R file that builds the user interface and server of the Shiny dashboard.

By: Lucca Nielsen
my [GitHub](https://github.com/Luccan97)
my [Linkedin](https://www.linkedin.com/in/lucca-nielsen-53b2a9181/)

# References

SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pré-processamento de microdados do Departamento de Informática do SUS (DATASUS). Cad. Saúde Pública, Rio de Janeiro , v. 35, n. 9, e00032419, 2019 . Available from http://ref.scielo.org/dhcq3y.
Batra, Neale, et al. The Epidemiologist R Handbook. 2021. DOI
