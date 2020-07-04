# What is the relationship between size of a gathering and transmission risk? An exercise for EPI260, Spring 2 2020

* Compare 2, 4, 8, 16 people.
* What do we mean by transmission risk? 
* What is the comparison? No gathering, smaller gatherings? 
* What are the important inputs to the calculation? 
* What simplifying assumptions can we make? 
* What data exist to inform our inputs? 


__groupsim.R__ calculates the number of infections that occur during a gathering. The user specifies _R<sub>0</sub>_, the dispersion of infectiousness (scale parameter of the gamma distribution), the duration of infectiousness, the duration of the gathering, the group size, and the proportion of the population that's susceptible, infectious, or recovered. 

Steps: 

* Assign each person in the group into an epidemiological compartment (S, I, or R) based on the poulation prevalence 
* For each infectious person, draw an infectiousness _&nu;<sub>i</sub>_ according to a gamma distribution with mean _R<sub>0</sub>_ and overdispersion parameter _k_
* Calculate the individual infection rate _&beta;<sub>i</sub>_ contributed by each infectious person as _&nu;<sub>i</sub> / D_, where _D_ is the duration of infectiousness
* Calculate the probability that a susceptible person gets infected according to an exponential distribution: _p<sub>inf</sub> = 1-exp(-sum(&beta;<sub>i</sub>) T)_, where _T_ is the duration of the gathering. 
* Calculate how many of the susceptibles become infected using a binomial draw with this _p<sub>inf</sub>_


<!--- Test formula: --->
<!--- <img src="https://render.githubusercontent.com/render/math?math=e^{i \pi} = -1"> --->
