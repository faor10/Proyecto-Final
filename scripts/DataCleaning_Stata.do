/*
Proyecto Final
Realizado por: Carlos Avilán y Francisco Ortiz
Se presenta el tratamiento y limpieza de los datos utilizados
*/

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
keep codmpio ano municipio depto lights_mean mean_rural mean_urban pobl_tot g_cap finan_credito discapital vrf_peq_productor areaoficialkm2 pib_total pib_cons
keep if ano>=2005
drop if codmpio==.
drop if pobl_tot==.
save municipios_final, replace 

replace depto="Chocó" if depto=="Choco"
replace depto="Guainía" if depto=="Guainia"
replace depto="Atlántico" if depto=="Atlantico"
replace depto="Bogotá, D.C." if depto=="Bogota Dc"
replace depto="Bolívar" if depto=="Bolivar"
replace depto="Boyacá" if depto=="Boyaca"
replace depto="Guainía" if depto=="Guainia" 
replace depto="Córdoba" if depto=="Cordoba" 
replace depto="Nariño" if depto=="Narino" 
replace depto="Norte de Santander" if depto=="Norte Santander"
replace depto="Valle del Cauca" if depto=="Valle Cauca"
replace depto="Vaupés" if depto=="Vaupes"
replace depto="Caquetá" if depto=="Caqueta"

save municipios_final, replace 


**Generación Train base de datos
use municipios_final, replace
keep if ano>=2005 & ano<2010
drop if pib_cons==.
drop if vrf_peq_productor==.
drop pib_total
save train, replace 

**Generació Test base de datos
use municipios_final, replace
keep if ano>=2010
drop pib_total pib_cons
drop if finan_credito==.
drop if lights_mean==.
drop if vrf_peq_productor==.
save test, replace 

use municipios_final, replace

*Generación de gráficas para análisis

collapse (mean) lights_mean, by (depto ano)
collapse (mean) pib_cons, by (depto ano)
collapse (sum) pib_cons, by (depto ano)


twoway line lights_mean ano, ///
   by(depto, row(3) title("Dynamics of coverage by year of mandate")) ///
   lcolor(red pink blue) xsize(12) ysize(8)
   
twoway line  lights_mean ano,  by(depto)
twoway line  pib_cons ano,  by(depto)

twoway line  lights_mean ano 