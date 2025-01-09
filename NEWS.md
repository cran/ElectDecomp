### Package changes from previous ElectDecomp version 0.0.1-1

The package contains three new functions called **inequality()**, **traditional_indexes()** and 
**malapportionment_index()**, and includes some changes in the output of the **distorsion()** function. Furthermore, it has been created a **plot** method.

The new function **inequality()** computes the inequalities in votes-to-seats conversion, including the conversion ratios (Kedar et al., 2016) for each combination of district and party, from which it can be calculated the Representational Inequality Index (RI) and drawn the representational inequality (Lorenz) curve of the election.

The new function **traditional_indexes()** provides as output the values of a list of indexes proposed in the literature as deviations from proportionality, including: the MAL index (previously available in **distorsion()**), the Gallagher index (Gallagher, 1991) or the Representational Inequality Index (Kedar et al., 2016), among others.  

The new function **malapportionment_index()** computes the malapportionment index as defined by Samuels and Snyder (2001).

Three changes has been made to the outputs of the **distorsion()**. First, a mismatch between labels and numbers of party distortion effects in the output `party.distortions` of the function **distorsion()** has been corrected. In the previous version, the correct order of labels had been "Total deviation, D_i", "Electoral system effect, A_i", "Population effect, B_i", "Mean effect, d_i", "Variance effect, e_i", " Malapportionment effect, f_i" and "Turnout effect, g_i". In the new version, we have maintained the order of the labels and reorder the rows. Second, the MAL index is not longer available in the output `aggregate.distortions` of the function **distorsion()**. 
Third, a new output `district2party.contributions`, which contains a matrix of order kxp with the district contributions to the total seat-to-vote deviation of each party, has been included.


A plot method has been included to draw the seat-to-vote descomposition for every party from the **distorsion()** output and the district contributions to the total seat-to-vote deviation of each party from the **distorsion()** output, as well as the representational inequality (Lorenz) curve of the election from the **inequality()** output.





