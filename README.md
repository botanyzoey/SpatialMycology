# SpatialMycology
R code used to conduct my Master's thesis research which explored mapping the distributions patterns of the Pacific Golden Chanterelle

Thesis Abstract:
The Pacific Golden Chanterelle (Cantharellus formosus) is a widely sought-after mushroom most abundant in the forests of Washington and Oregon, USA. This project used the species to investigate how accurately the species distribution could be modeled using natural history (herbarium) as model training data and citizen science (iNaturalist) as validation data. To combat the potential sampling bias towards population centers an effort variable weighting scheme was used to consider observations in harder to reach areas more than those in easier to access areas. Four models were created and run using the natural history data as training points: Random Forests (RF), Maxent, General Linear Model (GLM), and Artificial Neural Network (ANN); the effort variable was only applied to the ANN and GLM models. Out of these four, RF was found to perform the best with an equitable skill score (ETS) 0.987 when tested against the iNaturalist citizen science validation points. Overall, this project provides a good proof of concept and framework for the use of herbarium and citizen science data for use in biogeographical modeling projects in the future.

Environmental layer resolution: 250m
