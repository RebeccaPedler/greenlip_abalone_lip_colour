# greenlip_abalone_lip_colour

Dietary manipulation of greenlip abalone (Haliotis laevigata) lip colour under optimum and thermal stress conditions
This repository stores the data and code used for this study. 

Please find the description for each folder below. Kindly contact Rebecca Pedler (Rebecca.pedler@yumbah.com) for any queries, or to reuse any data or analysis for future studies.

data

This folder contains data and metadata for the systematic evidence map: 

•	Lip_colour_data_all.csv: This csv contains all mean lip colour components extracted in Fiji using the macro_script “colour_components” 
•	Water_quality_conditioning_phase.csv: This csv contains all water quality data for the 113-day conditioning stage of the experiment.
•	Water_quality_thermal_challenge.csv: This csv contains all water quality data for the 29-day thermal challenge stage of the experiment.

scripts

This folder contains R scripts used throughout the project: 

•	3D_scatter_plots: This R script was used to generate 3D scatter plots for mean CIELAB colour components from the lip_colour_data_all.csv dataset.
•	ANOVA: This R script was used to generate descriptive statistics and undertake statistical analysis of data from the lip_colour_data_all.csv dataset.
•	Colour_components: This macro script was used to extract CIELAB, HSB and RGB colour components from images.
