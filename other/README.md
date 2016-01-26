# Other effects of sleep fragmentation

This repository contains the analytical [data](./data/other.csv), [data analysis scripts](sleep-fragmentation-other.Rmd), and the [markdown document](sleep-fragmentation-other.md) and [figure](./figures/) outputs produced by the analysis scripts. The analysis shows summary plots, and the outputs of statistical analyses assessing the effects of two successive nights of sleep fragmentation on:  

1. Touch sensitivity using graded von Frey hairs before (assessing baseline sensitivity) and after (assessing whether ischaemia produced anaesthesia) forearm ischaemia;  
2. Pin-prick sensitivity using graded pin-prick intensities before ischaemia;  
3. Profile of Mood State (POMS) before and after each night of sleep;  
4. Sleep quality after each night of sleep (0-100mm VAS);  
5. Morning vigilance after each night of sleep (0-100mm VAS).

Pennebaker Inventory of Limbic Languidness (PILL) was collected before and after of each night sleep, but was not analysed because we were unsure what exactly was being measured. Data are available on request (stella.iacovides@wits.ac.za).

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
|sleep.quality     |Subjective rating of sleep quality after each night of sleep (0-100 visual analogue scale, anchored at: 0 = worst sleep ever experienced, 100 = best sleep ever experienced) |
|morning.vigilance |Subjective rating of vigilance after each night of sleep (0-100 VAS, anchored at: 0 = worst sleep ever experienced, 100 = 100% vigilant)                                     |
