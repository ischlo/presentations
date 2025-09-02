
## Where we left off



## Next steps

Ex with human heights ?

MCMC paragraph

Table review and slide ameliorate


## BHM theory

- Inverse Problem Theory and Methods for Model Parameter 


(a) randomness is induced by known variability and (b) randomness is induced by variability that is itself unknown. 
* Dvorzak, M., Magnien, J., Kleb, U., Kraker, E., Mücke, M., 2022. Bayesian Hierarchical Modelling for Uncertainty Quantification in Operational Thermal Resistance of LED Systems. Applied Sciences 12, 10063. https://doi.org/10.3390/app121910063

"Everything can be everywhere with a certain probability"

High correlations between predictor variables at low resolutions -> bad

- downscaling : Under-constrained inverse problems

- Bayesian vs Frequentist


\pi - prior on the parameter

Practical thing : define the prior in a way to make sure it does not turn to 0 in potentially observable values. This is more important than defining it to be be close to the 'actual' value.


$$
p(x,y) = p(x)p(y|x) = p(y)p(x|y)
$$


From : https://pmarchand1.github.io/ECL8202/notes_cours/08-Intro_Bayes.html#introduction

Selon l’interprétation fréquentiste, les probabilités représent la fréquence d’événements après un grand nombre de répétitions d’une observation ou d’une expérience. Dans ce cas-ci, nous pouvons assigner une probabilité à 𝑀1 car le patient provient d’une population et la maladie a une certaine fréquence dans cette population.

L’interprétation fréquentiste est à la base de la plupart des cours d’introduction aux statistiques, car elle permet notamment de définir des tests d’hypothèse et des intervalles de confiance. Dans cette approche, on peut associer une probabilité aux statistiques basées sur les données, comme la moyenne d’un échantillon 𝑥¯, mais pas aux paramètres d’un modèle comme la moyenne de la population 𝜇. Quand on définit un intervalle de confiance à 95% autour de 𝑥¯, ce n’est pas cet intervalle particulier qui a une probabilité de 95% de contenir 𝜇 (après l’échantillonnage, l’intervalle et 𝜇 sont tous les deux fixes), mais c’est 95% des échantillons possibles de 𝑥 qui produiraient un intervalle contenant la valeur de 𝜇.

Selon l’interprétation bayésienne, les probabilités représentent notre incertitude sur la valeur d’une quantité. On peut donc parler d’une distribution de probabilité même pour une valeur présumée fixe, ex.: un paramètre d’un modèle.

Historiquement, les débats entre les deux approches ont souvent été acrimonieux. Aujourd’hui, les mêmes statisticiens peuvent employer l’approche fréquentiste ou l’approche bayésienne selon la nature du problème. Cependant, il faut s’assurer de toujours interpréter les résultats en fonction de l’approche utilisée. Il faut se rappeler, par exemple, qu’un intervalle de confiance fréquentiste ne représente pas une distribution de probabilité du paramètre, ou qu’une vérification de l’ajustement d’un modèle bayésien n’est pas équivalente à un test d’hypothèse nulle.

Une des critiques courantes de l’inférence bayésienne est que l’assignation d’une distribution a priori ajoute un biais à l’analyse. Cependant, si le choix de cette distribution est justifiée par le besoin de pénaliser des valeurs trop extrêmes des paramètres, le rôle de la distribution a priori n’est pas si différent de celui d’un effet aléatoire qui resserre les moyennes de groupes vers la moyenne générale, ou du paramètre de lissage dans un modèle additif qui pénalise les courbes trop complexes. Toutes ces méthodes sont des exemples de **régularisation**, c’est-à-dire l’imposition de contraintes permettant de contrôler le risque de surajustement dans un modèle complexe, sans avoir à fixer complètement certains paramètres et certains effets.

Puisque l’inférence bayésienne s’applique à plusieurs types de modèles, les statistiques utilisées pour vérifier l’ajustement varient d’un modèle à l’autre. Cependant, une stratégie générale consiste à simuler des jeux de données à partir de la distribution a posteriori des paramètres et vérifier si les caractéristiques des données observées sont bien représentées par ces simulations. Cette technique s’appelle la vérification des prédictions a posteriori (ou posterior predictive check).


## Log normal prior

Motivation :
0 becomes unlikely as soon as there is something.
0 is highly likely when looking at the whole surface of earth.

Will have a mode in the relatively small values, but will also leave room for extreme values -> high concentration of productivity, say, in urban clusters.
This behaviour is well described by fat tailed distributions, such as log-normal.


Resources : 

- https://bayesball.github.io/BOOK/probability-a-measurement-of-uncertainty.html 
- https://bookdown.org/kevin_davisross/bayesian-reasoning-and-methods/prior.html
- https://bayesiancomputationbook.com/markdown/chp_01.html#conjugate-priors
- https://distribution-explorer.github.io/continuous/exponential.html
- https://www.pymc.io/projects/examples/en/latest/howto/model_builder.html#saving-model-to-file
- https://www.bayesrulesbook.com/chapter-5
- https://ourcodingclub.github.io/tutorials/brms/
- https://pmarchand1.github.io/ECL8202/notes_cours/08-Intro_Bayes.html#introduction
- https://paulbuerkner.com/brms/articles/brms_nonlinear.html
- https://python.arviz.org/en/stable/examples/plot_bpv_tstat.html


### Pros and Cons 
Of Bayesian (hierarchical) Modelling

- Pros :
    - Can handle complex data structures
    - Allows for uncertainty quantification
    - Can incorporate prior knowledge
    - Flexibility

- Cons :
    - Computationally intensive
    - Requires **careful** prior selection
    - Sensitive to model specification
    - Flexibility

- Bayesian is generally complex : Theory and formalism (Bayes, Monte Carlo, Information theory), software ecosystem (world of it's own), PPLs (Probabilistic Programming Languages)

- Data whose observations are considered to be exact in the scope of an analysis are termed hard data. In contrast, soft data represent observed data that might carry nontrivial uncertainty in the context of a study

## Intractability