*Unión de bases de datos
clear all
cd "D:\OneDrive - Universidad de los Andes\Intersemestral 2\Big Data\Trabajo Final\GitHub\Proyecto-Final\stores"
use PANEL_CARACTERISTICAS_GENERALES_2021, clear
merge 1:1 codmpio ano using PANEL_BUEN_GOBIERNO_2021
drop _merge
merge 1:1 codmpio ano using PANEL_AGRICULTURA_Y_TIERRA_2021
save municipios_final, replace

use mean_mpio_1992_2021_harmonized, replace
rename cod_mpio codmpio
rename year ano
save mean_mpio_1992_2021_harmonized, replace 

*Tratamiento de datos
use municipios_final, replace
drop _merge
merge m:1 codmpio ano using mean_mpio_1992_2021_harmonized
rename mean_mpio lights_mean
keep codmpio ano municipio depto lights_mean pobl_tot g_cap finan_credito discapital vrf_peq_productor areaoficialkm2 pib_total pib_cons
keep if ano>=2005
drop if codmpio==.
drop if pobl_tot==.
save municipios_final, replace 

**Train base de datos
keep if ano>=2005 & ano<2010
drop if pib_cons==.
save train, replace 

**Test base de datos
keep if ano>=2010
drop pib_total pib_cons
drop if finan_credito==.
drop if lights_mean==.
save test, replace 
