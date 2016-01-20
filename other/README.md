# Other effects of sleep fragmentation

This repository contains the analytical [data](./data/Other.csv), [data analysis scripts](SleepFragmentation_Other.Rmd), and the [markdown document](SleepFragmentation_Other.md) and [figure](./figures/) outputs produced by the analysis scripts. The analysis shows summary plots, and the outputs of statistical analyses assessing the effects of two successive nights of sleep fragmentation on:  

1. Touch sensitivity using graded von Frey hairs before (assessing baseline sensitivity) and after (assessing whether ischaemia produced anaesthesia) forearm ischaemia;  
2. Pin-prick sensitivity using graded pin-prick intensities before ischaemia;  
3. Profile of Mood State (POMS) before and after each night of sleep;  
4. Pennebaker Inventory of Limbic Languidness (PILL) before and after of each night sleep _(these data are reported on here, but were not included in the manuscript because the value of the questionnaire was questionable)_;  
5. Sleep quality after each night of sleep (0-100mm VAS);  
6. Morning vigilance after each night of sleep (0-100mm VAS).

## Code book
[Other.csv](./data/Other.csv)  

|Key               |Label                                                                                                                                                                        |
|:-----------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|id                |Participant identification number                                                                                                                                            |
|period            |Intervention period (baseline night (no sleep fragmentation), and two succesive nights of sleep fragmentation (Fragmentation1 & Fragmentation2)                              |
|vF.before_mN      |Touch sensitivity before ischaemia (von Frey hairs, mN)                                                                                                                      |
|vf.during_mN      |Touch sensitivity after ischaemia (von Frey hairs, mN                                                                                                                        |
|pin.prick_mN      |Pin-prick sensitivity before ischaemia (graded pin-pricks, mN)                                                                                                               |
|poms.evening      |Profile of Mood State score before each night of sleep                                                                                                                       |
|poms.morning      |Profile of Mood State score on the morning after each night of sleep                                                                                                         |
|pill.evening      |Pennebaker Inventory of Limbic Languidness (PILL) before each night of sleep                                                                                                 |
|pill.morning      |Pennebaker Inventory of Limbic Languidness (PILL) after each night of sleep                                                                                                  |
|sleep.quality     |Subjective rating of sleep quality after each night of sleep (0-100 visual analogue scale, anchored at: 0 = worst sleep ever experienced, 100 = best sleep ever experienced) |
|morning.vigilance |Subjective rating of vigilance after each night of sleep (0-100 VAS, anchored at: 0 = worst sleep ever experienced, 100 = 100% vigilant)                                     |