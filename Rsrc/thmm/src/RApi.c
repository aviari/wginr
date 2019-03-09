// $Id: RApi.c 396 2019-01-02 22:53:10Z viari $
// tinyhmm : C implementation
//
#include <R.h>
#include <math.h>

//
// reminder : access to R 2x2 matrix in C : matrix[irow + icol*nrow]
//

//
// forward
//
void C_forward(int *nobs, int *nstate,
               double *logdens, double *trans, double *init,
               double *logalpha, double *loglike) {
  int i, j, k, n=*nobs, ns=*nstate;
  double logscale, maxlog, logsum;
  double phi[ns], logphi[ns];

  logscale = 0;
  for (j = 0; j < ns; j++) phi[j] = init[j];

  for (i = 0 ; i < n ; i++) {
    for (j = 0; j < ns; j++) {
      logphi[j] = log(phi[j]) + logdens[i + j*n];
      logalpha[i + j*n] = logphi[j] + logscale;
    }
    maxlog = logphi[0];
    for (j = 1 ; j < ns ; j++) if (logphi[j] > maxlog) maxlog = logphi[j];
    logsum = 0;
    for (j = 0; j < ns; j++) logsum += exp(logphi[j] - maxlog);
    logsum = maxlog + log(logsum);
    logscale += logsum;
    for (j = 0; j < ns; j++) logphi[j] = exp(logphi[j] - logsum);
    for (j = 0; j < ns; j++) {
      phi[j] = 0;
      for (k = 0 ; k < ns ; k++)
          phi[j] += logphi[k] * trans[k + j*ns];
    }
  }
  *loglike = logscale;
}

//
// backward
//
void C_backward(int *nobs, int *nstate,
               double *logdens, double *trans,
               double *logbeta) {
  int i, j, k, n=*nobs, ns=*nstate;
  double logscale, maxlog, sumphi;
  double phi[ns], logphi[ns];

  logscale = log(ns);
  for (j = 0; j < ns; j++) phi[j] = 1.0/(double)ns;  //
  for (j = 0; j < ns; j++) logbeta[n - 1 + j*n] = 0; //

  for (i = n-2 ; i >= 0 ; i--) {
    for (j = 0; j < ns; j++) logphi[j] = log(phi[j]) + logdens[i + 1 + j*n];
    maxlog = logphi[0];
    for (j = 1; j < ns; j++) if (logphi[j] > maxlog) maxlog = logphi[j];
    sumphi = 0;
    for (j = 0; j < ns; j++) {
      phi[j] = 0;
      for (k = 0 ; k < ns ; k++)
        phi[j] += exp(logphi[k]-maxlog) * trans[j + k*ns];
      logbeta[i + j*n] = log(phi[j]) + maxlog + logscale;
      sumphi += phi[j];
    }
    logscale = logscale + log(sumphi) + maxlog;
    for (j = 0; j < ns; j++) phi[j] = phi[j] / sumphi;
  }
}

//
// viterbi
//
void C_viterbi(int *nobs, int *nstate,
             double *logdens, double *logtrans, double *init,
             double *nu, int *states) {
  int i, j, k, l, n=*nobs, ns=*nstate;
  double mujk, maxmuj;

  if (n > 0) {
    for (j = 0 ; j < ns ; j++)
      nu[0 + j*n] = log(init[j]) + logdens[0 + j*n];
  }

  for (i = 1 ; i < n ; i++) {
    for (j = 0 ; j < ns ; j++) {
      maxmuj = nu[i-1 + 0*n] + logtrans[0 + j*ns];
      for (k = 1 ; k < ns ; k++) {
        mujk = nu[i-1 + k*n] + logtrans[k + j*ns];
        if (mujk > maxmuj) maxmuj = mujk;
      }
      nu[i + j*n] = maxmuj + logdens[i + j*n];
    }
  }

  if (n > 0) {
    k = 0;
    for (j = 1 ; j < ns ; j++)
      if (nu[n - 1 + j*n] > nu[n - 1 + k*n]) k = j;
    states[n-1] = k+1;
  }

  for (i = n-2 ; i >= 0 ; i--) {
    k = 0;
    l = states[i + 1] - 1;
    for (j = 1 ; j < ns ; j++)
      if ((nu[i + j*n] + logtrans[j + l*ns]) > (nu[i + k*n] + logtrans[k + l*ns])) k = j;
    states[i] = k+1;
  }
}
