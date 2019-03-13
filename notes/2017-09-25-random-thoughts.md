# 2017-09-25 random thoughts while working on the project

* Implemented colorblind categorical color pallets within DLMtool 

* Further developed the shortraker rockfish operating model

* do any of the management procedures with their embedded harvest control rules implement the DFO provisional harvest rule? http://www.dfo-mpo.gc.ca/reports-rapports/regs/sff-cpd/precaution-eng.htm

**Stock Status**
In critical zone. 
The stock is considered to be in “the critical zone” if the mature biomass, or its index, is less than or equal to 40% of BMSY. In other words:  Biomass ≤ 40% BMSY.

In cautious zone. 
The stock is considered to be in the “cautious zone” if the biomass, or its index, is higher than 40% of BMSY but lower than 80% of BMSY. In other words:  40% BMSY < Biomass < 80% BMSY.

Healthy. 
The stock is considered to be “healthy” if the biomass, or its index, is higher than 80% of BMSY. In other words:  Biomass ≥ 80% BMSY.

When the stock is in the “Healthy Zone” : Fp < FMSY

When the stock is in the “Cautious Zone” : Fp < FMSY x ( (Biomass – 40% BMSY ) / ( 80% BMSY − 40% BMSY) )

When the stock is in the “Critical Zone” : Fp = 0

**Biomass at MSY**. In absence of an estimate of BMSY from an explicit model, the provisional estimate of BMSY could be taken as follows (select the first feasible option):

The biomass corresponding to the biomass per recruit at F0.1 multiplied by the average number of recruits; or
The average biomass (or index of biomass) over a productive period; or
The biomass corresponding to 50% of the maximum historical biomass.

**Fishing mortality at MSY**. In absence of an estimate of FMSY from an explicit model, the provisional estimate of FMSY could be taken as follows (select the first feasible option):

- The fishing mortality corresponding to F0.1; or
- The average fishing mortality (or an index of fishing mortality) that did not lead to stock decline over a productive period; or
- The fishing mortality equal to natural mortality inferred from life history characteristics of the species.


* An obvious plot to make would be the two-dimensional kernel density plots in ggplot. Another one to try would be the hexagon bending plots. I'm thinking of using these for the f/fmsy an b/bmsy plots.

* The current DFO plot implemented by Tom shows the mean values over the last 5 years. I'm thinking of a multipanel plot with a panel per management procedure with either the kernel density plots or the hexagon plots Perhaps with 1 to 3 lines overlaid as examples. Not sure if this should be for the entire operating model duration. If it's just for the last 5 years and then to show the 2-dimensional element across simulation iterations. 

* For the most part I think we can work with the operating model plot elements from the package. These are fairly simple but there are a lot of them to make and layout and that work has already been done in the package. Should be usable with the option to specify the color pallets as I have now done. 

* Note that the precautionary approach document also specifies risk of decline % categories and shows how they correspond to different risk categories, for example, "moderately high" or "very low"

* Should make liberal use of viridis for continuous color spectrums
