---
output:
  pdf_document: default
  html_document: default
---
# Constrained clustering via diagrams: A unified theory and its application to electoral district design
## Projekt pri predmetu Finančni prektikum
### Avtorja: Primož Durcik in Ana Golob


## Teoretični opis omejenega gručenja
### Definicije

Naj bosta $k, n \in \mathbb{N}$ in $\mathcal{X}$ poljuben prostor. Z $X= \{x_1,\ldots x_m\}\subset \mathcal{X}$ označimo množico s pripadajočimi utežmi $\Omega = (\omega_1,\ldots \omega_m)\in \mathbb{R}_{>0}^m$. Nadalje, naj bo $\mathcal{K}=(\kappa_1,\ldots ,\kappa_k)\in\mathbb{R}_{>0}^k$, tako da $\sum_{i=1}^k\kappa_i = \sum_{j=1}^m\omega_j.$ $\mathcal{K}$ je vektor željenih "velikosti" gruč. Naš cilj je poiskati take particije množice $X$, da bo skupna teža gruče $C_i$, v prej določeni normi "čim bližje" prepisanemu $\kappa_i.$\\

Uporabljali bomo spodnjo predpostavko, ki problem nekoliko poenostavi:\\

Naj bo $$C=(\xi_{i,j})_{\substack{i=1,\ldots ,k\\ j=1,\ldots ,m}} \in [0,1]^{k\times m},$$ tako da je vsota $\sum_{i=1}^k\xi_{i,j} = 1$ za vsak $j$. $C$ imenujemo delno gručenje množice $X$ in $\xi_{i,j}$ je del enote $j$ predpisane gruči $i$. Tako definirane $C_i=(\xi_{i,1},\ldots ,\xi_{i,m})$ imenujemo gruča $i$. Nadalje označimo še množico $supp(C_i)=\{x_j \in X: \xi_{i,j}>0\}$ tistih elementov iz $X$, ki imajo pri $i$ pozitiven delež. Če je $C\in \{ 0,1\}^{k\times m}$, imenujemo gručenje celoštevilsko.

Teža gruče je podana z $\omega (C_i)=\sum_{j=1}^m \xi_{i,j}\omega_j$. Gručenje $C$ je močno uravnoteženo, če je $\omega (C_i)=\kappa_i$ za vsak $i$. Če so za teže grozdov dane zgornje in spodnje meje $\kappa_i^-, \kappa_i^+$ in velja: $\kappa_i^- < \sum_{j=1}^m\xi_{i,j}\omega_j < \kappa_i^+$ za vsak $i$, potem pravimo, da je gručenje $C$ šibko uravnoteženo. V posebnem primeru vzamemo $\kappa_i^-=(1-\epsilon)\kappa_i$ in  $\kappa_i^+=(1+\epsilon)\kappa_i$ za vsak $i$ in dan $\epsilon >0$ in tako gručenje imenujemo $\epsilon$-gručenje. Z $BC$ in $BC^{\epsilon}$ označujemo množici vseh močno uravnoteženih in $\epsilon$-uravnoteženih delnih gručenj.\\

### Omejeno gručenje s posplošenimi Voronojevimi diagrami

Posplošeni Varonojev Diagram za dane funkcije $f_i:\mathcal{X} \to \mathbb{R}$, $i=$1,$\ldots$,k določi $C_i$, ki je podmnožica od $\mathcal{X}$ tako, da za vsak $x\in \mathcal{X}$ velja: če $f_i(x)$ je minimalen, potem $x\in C_i$.

Če želimo dobiti ustrezne diagrame je ključnega pomena, da dobro definiramo funkcije $f_i$. Za nabor parametrov $(\mathcal{D}, h, \mathcal{S}, \mathcal{M})$ tako definiramo $k$-terico funkcij $\mathcal{F}(\mathcal{D}, h, \mathcal{S}, \mathcal{M})=(f_i,\ldots ,f_k)$ s predpisom:
$$f_i(x)=h(d_i(s_i,x))+\mu_i,$$
kjer je:
\begin{itemize}

\item $\mathcal{D}=(d_1,\ldots ,d_k)$ $k$-terica metrik, oziroma predpisov za merjenje razdalj na prostoru $\mathcal{X}$, 
\item $h: \mathbb{R}_{\geq 0}\to\mathbb{R}_{\geq 0}$ monotono naraščajoča funkcija, 
\item $\mathcal{S}=(s_1,\ldots ,s_k)$ $k$-terica točk iz $\mathcal{X}$,
\item $\mathcal{M}=(\mu_1,\ldots ,\mu_k)\in \mathbb{R}^k.$

\end{itemize}
Če metrike $d_i$ niso identične, pravimo končnemu diagramu anizotropen. 

Primeri diagramov dobljenih z izbiro posameznih prostorov in na njih podanih metrik:
\begin{enumerate}
\item[1.] Za Evklidski prostor nam izbira $f_i=\lVert x-s_i\rVert_2^2 + \mu_i$ porodi $\textit{power diagrams}$.
\item[2.] Za izbiro $f_i=\lVert x-s_i\rVert_2 + \mu_i$ dobimo $\textit{additively weighted Varonoi diagrams}$
\item[3.] V diskretnem primeru, kjer velja $\mathcal{X}=X$, je dan povezan graf $G= (X, E, \delta)$; kjer $\delta: E \to \mathbb{R}_{>0}$ priredi vsaki povezavi neko pozitivno vrednost. Če je $d_G(x,y)$ definitana kot najkrajša pot od $x$ do $y$, to inducira metriko na $\mathcal{X}$. Torej dobimo funkcije $f_i$ oblike: $f_i = d_G(s_i, x) + \mu_i.$
S tem dobimo $\textit{diagram najkrajših poti (angl. shotrest-path diagram)}$

\end{enumerate}

Videli bomo, da parametra $\mathcal{D}$ in $h$ v glavnem določata karakteristike končnih diagramov. Točke si služijo kot referenčne točke za določanje gruč.

Da se pokazati, da za vsako izbiro $\mathcal{D}$, $h$ in $\mathcal{S}$, obstaja izbira dodatnih parametrov $\mathcal{M}$ takšna, da so porojene gruče tako predpisanih tež kot tudi optimalno porazdeljene.

Parametre  $\mathcal{D}$, $h$ in $\mathcal{S}$ imenujemo strukturni parametri,  $\mathcal{M}$ pa izbirni parameter. Za vsako izbiro strukturnih parametrov izbirni parameter $\mathcal{M}$ dobimo z rešitvijo linearnega programa.
