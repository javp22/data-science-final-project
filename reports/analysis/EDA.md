# EDA 

### Data imbalance

the dataset is highly imbalanced, with approximately 84% of observations occurring before September 2021, while only 16% occured after

this implies that any comparison using absolute counts would be misleading, as differences could be driven by sample size rather than actual changes in accident patterns

that's why we will do all comparative analyses using proportions

### Monthly distribution of accidents

it shows a clear shift between periods
- the "Before" period presents a relatively uniform distribution throughout the year 
- the "After" period exhibits stronger seasonality, with a noticeable increase in accident proportions during autumn months, particularly September and October 

this suggests a structural change in the temporal dynamics of accidents after September 2021

### Accidents per year

there's clear structural break around 2020, followed by a gradual recovery

this suggests that accident patterns were temporarily disrupted and later began to return to previous levels

### Accidentes per day of the week

the overall weekly pattern remains stable in both periods

there is a clear and persistent structure:
- lower proportions at the beginning of the week
- higher concentration of accidents toward the weekend

### Accidents by via type

the accidents are centralized in "Carretera Convencional" y "Via urbana" in both periods

a slight increase is observed in the proportion of accidents on conventional roads, suggesting a potential shift towards interurban mobility after September 2021

urban roads also show a small increase, remaining the dominant environment for accidents

the other categories exhibits a sharp decrease in proportions, this should be interpreted with caution, as these categories have very low counts

### Severity of the accidents

there is almost not change, so the proportion between serious and fatal cases remains the same in both periods

### Hour of the accidents

in both periods we can see a patron

- low between 00:00–05:00
- progresive increase since 06:00
- main peaks between 11:00-14:00
- second peak between 17:00–19:00
- decline since 20:00

"Before" period distribution is slightly more spread across the day

"After" period shows a more concentrated distribution around central daytime hours
