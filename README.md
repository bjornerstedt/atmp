# atmp

## TODO:

* Företaget måste kompenseras för dödsfall vid framtida betalning. Gör real beräkning optional.

* Integrera komparatativ statik

* Multipla betalningar: sjukhuskostnader, betalning till ATMP fortsätter trots att annan behandling påbörjas 

* Döp om state -> treatment etc i kod

* Globals använder inte default values om de saknas

* pstart och pend bör korrigeras för kont behandling, döp om till start och end

* Skapa tester:

  * Bättre felkoll - tänk igenom
  
  * state måste öka, om inte specificerat på intervall måste nästa värde vara stort nog för att tillåta numrering

  * Allow only 1 random state
  
* Life table - Omit p_death and it is set with real hazard

* state_table

  * Interpretation of replication of random state

  * If state is omitted, it is set to the previous row + 1. Useful for example 3 and for omitting state completely.
  
  * generate QoL if it is omitted
  
  * Två eller flera payments genom kommaseparering. (De gäller för alla perioder som tabellen specificerar, med periods anges hur betalning sker.)

* Betalning i death state?
  
* Hjälptext

* Visa genererad rapport i appen.

* Separera fältbeskrivningar från data

## Genomfört --------------------------------------------------------------------------------------

* Navbar och logga

* Döp om variabler: contract

* Sannolikheter måste summera till 1
    
* How should probabilities be defined?

* Döp om tabeller i indata.

* lägg till armval i globals: control_states = 2

  * Check joins
  
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
