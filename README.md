
# Constrained clustering
## Projekt pri predmetu Finančni prektikum
### Avtorja: Primož Durcik in Ana Golob


Gručenje že dolgo predstavlja enega temeljnih delov kombinatorične optimizacije in analize podatkov. V pričujoči projektni nalogi se bomo najprej seznanili z osnovnim teoretičnim ozadjem omejenega gručenja. Le-to bo v drugem delu predstavljalo osnovo za pisanje algoritma za optimalno gručenje, ki bo ravno reševanje linearnega programa (Nahaja se v datoteki LinearniProgram.R).

V grobem si pri omejenem gručenju želimo dano množico obteženih točk množice $X$ v določenem prostoru $\mathcal{X}$ razdeliti na dano število $k$ gruč z vnaprej določeno težo.

Algoritem je uporabljen na dveh eksperimentih:

V prvem eksperimentu rešujemo problem oblikovanja volilnih enote. Zanima nas, kako razčleniti notranje območje države na posamezne volilne okraje. Pri tem zahtevamo, da okraji pokrivajo skoraj enake populacije volivcev in imajo »razumno« obliko. (Nahaja se v datoteki eksperiment_volitve.R.)

V drugem eksperimentu imamo podatke o stanovanjih, ki se prodajajo v okolici Ljubljane. Glede na njihove lastnosti jih želimo razdeliti v gruče, tako da bo vsaka gruča vsebovala stanovanja s čim bolj podobnimi lastnostmi. (Nahaja se v datoteki eksperimentStanovanja.R.)

Več si lahko preberete v poročilu (datoteka porocilo.Rmd).


### Napotki za uporabo:
* Poročilo projekta se nahaja v datoteki porocilo.Rmd.
* Algoritem za reševanje lineatnega programa, ki reši problem se nahaja v datoteki LinearniProgram.R.
* V mapah z imenoma eksperiment_volitve, ter eksperimentStanovanja se nahajata problema, ki smo jih reševali z zgornjim algoritmom.
* Rezultati eksperimenta volilnih okrajev, ter opis obeh problemov je zapisan v poročilu.
* Z zaganjanjem datoteke stanovanjaShiny.R, pridete do amplikacije z rezultati problema s stanovanji.

PROGRAM UPORABLJA NASLEDNJE KNJIŽNJICE:
* lpSolveAPI
* ggplot2
* plotly
* lpSolveAPI
* ggmap
* readr



