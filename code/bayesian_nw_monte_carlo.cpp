#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
  
// calculate species' marginal persistence probabilities
// Input:
// - A: adjacency matrix of the food web
// - Pb: vector of baseline extinction probabilities
// - alpha: first parameter of the Beta distribution
// - beta: second parameter of the Beta distribution
// - nreps: number of iterations for Monte Carlo sim of Bayesian network
// Output:
// - vector of marginal persistence probabilities
// [[Rcpp::export]]

// Operatorer:
// ++: adderar 1 till variabeln
// ?: condition ? result1 : result2. Om condition är TRUE körs result 1, om FALSE körs result 2.
// =: assign
// +=: addera och assign:a

NumericVector marginal_persistence_probs(NumericMatrix A, NumericVector Pb,
                                         double alpha, double beta, int nreps) {
  int S = Pb.size(), ind_basal = 0, i, j, rep;//Pb.size() = antalet arter som är kopplade till det aktuella hotet.
  double A_times_extant, frac, p_ext;
  NumericVector rowsums(S), marginal(S), extant(S), rand(S);
  for (i = 0; i < S; i++) { // i = 0 körs en gång innan loopen; i < S är condition för att köra loopen; i++ körs varje gång efter loopen.
    rowsums(i) = sum(A(i,_)); //Plats i i vektorn rowsums sätts till summan av rad i i A. Så rowsums blir en lista över hur många arter varje art (i) äter.
    if (rowsums(i) == 0.0) ind_basal++; // ind_basal blir antalet basal producers i näringsväven
  }
   for (rep = 0; rep < nreps; rep++) {
    rand = Rcpp::runif(S); //Slumpar ett tal  mellan 0 och 1 per art
    for (i = 0; i < ind_basal; i++) extant(i) = (rand(i) < Pb(i) ? 0.0 : 1.0); // Hur funkar det här iom att basal producers ligger högst upp i matrisen?? Körs den här loopen bara för basal producers
    // Om det slumpmässiga talet är mindre än baseline extinction prob för i sätts extant(i) till 0, aka arten dör ut.
    for (i = ind_basal; i < S; i++) { //För alla som inte är basal producers?
      A_times_extant = 0.0;
      for (j = 0; j < S; j++) A_times_extant += A(i,j) * extant(j); // j är de som blir ätna.
      frac = 1.0 - A_times_extant / rowsums(i); //The fraction of i's resources that are extinct.
      p_ext = Pb(i) + (1 - Pb(i)) * R::pbeta(frac, alpha, beta, true, false);
      extant(i) = (rand(i) < p_ext ? 0.0 : 1.0);
    }
    marginal += extant;// marginal är nu lika med marginal + extant; om nreps är 10 000 kommer lika många extant-värden adderas ihop
  }
  return(marginal/nreps); //Ger sannolikheten. T.ex. om extant blivit 1 varje gång och nreps är 10 000 => 10000/10000 = 1 => 100% chans att överleva.
}
