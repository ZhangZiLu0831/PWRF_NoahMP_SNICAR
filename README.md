#  <center> WRF Modifications to add a new snow albedo shceme in PWRF/Noah-MP

##  <center> Zilu Zhang
##  <center> zhangzilu21@mails.ucas.ac.cn
##  <center> The Institute of Atmospheric Physics， Chinese Academy of Sciences, Beijing, China

## Introduction

The PWRF model system improved by PMG and built on previous success with the Pennsylvania State University (PSU)-National Center for Atmospheric Research (NCAR) Fifth-generation Mesoscale Model has been modified for use in polar regions (Polar MM5).A recent version of the Polar Weather Research and Forecasting model (Polar WRF) has been upgraded to the version 4.X era with an improved NoahMP Land Surface Model (LSM). We plan to use the PWRF model to simulate the land-air energy exchange process and the impacts of snow albedo reduciton induced by BC deposition in the Arctic. In order to study the impacts of snow darkening by deposition of BC, we needed to add a new snow schemes in Noah-MP to the PWRF system. We describe the process in this paper.  

We followed the SNICAR snow albedo scheme in CLM for most of the work in creating the new subroutines and adding them to the PWRF modelling system. The detailed descriptions of PWRF, Noah-MP and SNICAR can been found in relevant papers.

## Modified WRF Files
```ts
phys/module_sf_noahmplsm.F
phys/module_sf_noahmpdrv.F
phys/module_physics_init.F
phys/module_surface_driver.F
Registry/Registry.EM_COMMON
dyn_em/module_first_rk_step_part1.F
dyn_em/module_initialize_real.F
share/module_wps_io_arw.F
share/moudle_optional_input.F
```
## Add a new input variable

We created a new global state variable called, ‘mbc’, by adding a record describing the new state variable with ij dimensions to the "Registry/Registry.EM_COMMON" file. 

The mbc is the BC concentration in SNOW (unit:ng/g), it can been read from the met_em* files and can been found in wrfinput_d* after the initialize of 'real'.

## Creat an new subroutines

We creat an new subroutines name SNOWALB_SNICAR_DIR, it will use when set the "opt_alb=3' in &noahmp section of namelist.input.

This new subroutines is a simplify edition of SNICAR coupled with Noah-Mp. Some process have been neglected:
1. No calculation of snow effective grain size.
2.  Only one kind of BC been considered (no coated).
3. Only consider the clear sky days.

## Usage
Replace all files on the relative directories and recomile the WRF. Add the MBC in met_em*
Then set :
```
 sf_surface_physics                  =  4,   
  opt_alb                             =3,

```
Now you can use the new snow albedo schemes.

PS:**It may cause a much longer time when use this new shcemes, So we are not suggest to run with a long period of simuition. Just choose some cases with 5-10 days.**
