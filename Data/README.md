## /Data

Tabular data used in the manuscript titled, "Trait responses of a grassland shrub invader to altered moisture regimes."
Authors: William A. Rutherford and Steven R. Archer

Below provides information for each `*.csv` files within the `/Data` directory:

`2017-07_ppt.csv` used in `8_SRER_2017-07_ppt_fig_SIFig1.R` script from two Santa Rita Experimental Range (SRER) Desert Grassland Enclosure fiel-based HOBO Onset RG-2M Rain Gauge systems (Onset Computer Corporation, Bourne, MA).

* date - date of recorded rainfall
* temp - mean daily temperature (celcius)
* rain - mean daily rainfall (mm)

`SRER_daily_1975-2020.csv` used in `9_SRER_1975-2020_ppt.R` script contains daily precipitation from seven rain gauges between 1975 to 2020 distributed across the ~21,000-hectare SRER (Goodrich et al. 2008).

* gage - rain gauge number
* date - date of recorded rainfall
* start_time - start time of rain event (hh:mm)
* mins - duration of rain event (minutes)
* amt_mm - rainfall of rain event (mm)

`ecophys.csv` used in `6_gh_plots.R` script contains only study ecophysiological trait measurements for creating Figure 3a-c.

* pot - greenhouse pot number
* tx - watering regime treatment
* sampling - seedling age
* anet - photosynthetic rate/net CO2 assimilation (µmol CO2 m-2 s-1) 
* cond - stomatal conductance (mol H2O m-2 s-1)
* trans - transpiration (mmol H2O m-2 s-1)

`germination.csv` used in `4_germination.R` script used for calculating seed germination percentages.

* tx - watering regime treatment
* sampling - seedling age
* seedlings - count of emerged seedlings

`gh_temp_light.csv` used for calculating greenhouse environmental conditions provided in Methods.

* date - date of recorded values
* doy - day of year (Julian Date)
* location - location of record inside, outside, or table of greenhouse 
* maxtemp - maximum daily temperature (C)
* mintemp - minimum daily temperature (C)
* avgtemp - mean daily temperature (C)
* maxlight - maximum daily light (lux)
* minlight - minimum daily light (lux)
* avglight - mean daily light (lux)
* w/m2 - spot light measurement with Line Quantum Sensor (W/m2)

`prve_gh_master_data.csv` is the master data set of trait measurements and used in the `2_two_way_manova.R`, `3_post_hoc.R`, `5_discriminant.R`, and `6_gh_plots.R` scripts.

* pot - greenhouse pot number
* tx - watering regime treatment
* sampling - seedling age
* totrootlnth - total root length (mm)
* surfarea - root surface area (mm2)
* avgdiam - average root diameter (mm)
* rootvol - total root volume (mm3)
* crossings - root crossings (#)
* forks - root forks (#)
* tips - root tips (#)
* maxht - maximum seedling height (mm)
* truelvs - true leaves (#)
* totlflts - total leaflets (#)
* lflnth - leaf length (mm)
* light - lignin height (mm)
* cnode - cotyledonary node height (mm)
* thorns - thorns (#)
* dryleaf - dry leaf mass (g)
* lfwtr - leaf water content (%)
* wetsdling - fresh weight (g)
* drysdling - dry seedling mass (g)
* sdlingwtr - seedling water content (%)
* taprootlnth - tap root length (mm)
* coarserootdiam - coarse root diameter (mm)
* fineroots - fine roots (#)
* rootwt - dry root mass (g)
* rootshoot - root to shoot ratio
* sla - specific leaf area (m2 g-1)
* wp - water potential (-MPa)
* anet - photosynthetic rate/net CO2 assimilation (µmol CO2 m-2 s-1) 
* cond - stomatal conductance (mol H2O m-2 s-1)
* trans - transpiration (mmol H2O m-2 s-1)
* fivegwc - 0-5 cm soil gravimetric water content (%)
* twentygwc - 5-20 cm soil gravimetric water content (%)
* agr - absolute growth rate (mg d-1)

`soil_character.csv` used in the `1_soil_character.R` script and contains measured soil texture and chemical variables provided by Motzz Labs, Inc. in Phoenix, AZ.

* depth - measurement/sampling depth (cm)
* pH - pH (SU)
* EC - Electrical Conductivity (dS/m)
* Ca - Calcium (ppm)
* Mg - Magnesium (ppm)
* Na - Sodium (ppm)
* K - (ppm)
* nitrate - Nitrate (ppm)
* phosphate - Phosphate (ppm)
* esp - Exchangeable Sodium Percentage (%)
* CEC - Cation Exchange Capacity (meq/100g)
* sand - sand content (%)
* silt - silt content (%)
* clay - clay content (%)
* thetar - residual volumetric water content
* type - soil texture classification

`soil_moisture.csv` used in the `7_soil_temp_moist_figs.R` script and volumetric water content was collected with HOBO Onset EC-5 Soil Moisture Smart Sensors.

* date - date of recorded soil moisture
* DOY - day of year (Julian date)
* tx - watering regime treatment
* rep - replicate number
* depth - soil depth in pot
* moisture - volumetric water content (m3/m3)

`soil_moisture_start_condition.csv` used in the `7_soil_temp_moist_figs.R` script as a subset of the larger dataset to retrieve volumetric water content at the start of the experiment and was collected with HOBO Onset EC-5 Soil Moisture Smart Sensors. 

* date - date of recorded soil moisture
* tx - watering regime treatment
* rep - replicate number
* depth - soil depth in pot
* moisture - volumetric water content (m3/m3)

`soil_temp_daily.csv` used in the `7_soil_temp_moist_figs.R` script, where pot soil temperatures were measured with HOBO Pendant® Temperature sensors.

* date - date of recorded values
* DOY - day of year (Julian Date)
* tx - watering regime treatment
* rep - replicate number
* maxtemp - maximum daily temperature (C)
* mintemp - minimum daily temperature (C)
* avgtemp - mean daily temperature (C)

