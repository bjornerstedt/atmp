# atmp

## TODO:

* Interpretation of replication of random state

* Skapa tester

* How should probabilities be defined?

  * Life table - Omit p_death and it is set with real hazard

* state_table

  * If state is omitted, it is set to the next value. Useful for example 3 and for omitting state completely.
  
  * generate QoL if it is omitted
  
  * Allow only 1 random state
  
  * Check joins
  
  * Två eller flera payments genom kommaseparering. (De gäller för alla perioder som tabellen specificerar, med periods anges hur betalning sker.)

* Döp om variabler: contract

* Döp om tabeller i indata.

* Hjälptext

* Inputkoll: 

    * Sannolikheter måste summera till 1
    
* Visa genererad rapport i appen.

* lägg till armval i globals: control_states = 2

* Separera fältbeskrivningar från data

## Genomfört -------------------------------------------------------------

* Lägg till nedladdning av rapport till ATMP app, med indata som parameter

* Kalla arm new old

* random_state = 0 bör finnas för Jämförelse

* BUG: Visa DT tabeller enbart om indata är laddat

* Inputkoll: 

    * Kolla att states i contracts inte överskrider antal states i treatments

    * Sannolikheter måste summera till 1
    
    * Lägg till min max gränser i tabeller

* Skapa rapport som sammanfattar modellerna

* Hjälpmeny med dokumentation i appen

* Kolla varför Exempel B blir konstigt med 6 states

* Gör om atmp projekt till att inte vara ett paket

* BUG: Ny fil laddas inte i DT

* Skapa exempelfiler

* Lägg till plottar i Model

* Analysis rubriker: name, plan contract -> treatment, arm, payment

* Horisontell bar chart på Model sidan

* Skriv Upload excel input file.

* Flytta Model: filnamn ned

* Input tag: Lägg till text om att modify

* Döp om Contracts till payment plan

* Ta bort active och control plan i globals

* Lägg till inmatning till tabellerna i atmp.R

* Lägg till globals till tabellerna och snygga till

* Konvertera tabeller till och från text

* Spara ändringar i tabeller
