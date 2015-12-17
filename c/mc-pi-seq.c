// sequential version 
// estimate PI using Monte-Carlo simulation
//
// compile: gcc -Wall -O3 -c mc-pi-seq.c
// link:    gcc -Wall -O3 -lm -o mc-pi-seq utils.o mc-pi-seq.o
// run:     ./mc-pi-seq
//
// ideas: 
//   - add argument for a timer value to stop execution after
//     a specified number of seconds    
//   - accept n as command line argument (avoids recompilation when changing n)
//     (could also set an environmental variable instead)
//   - create a Makefile (oe better: cmake file)

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "utils.h" // for gtod_diff_ms()

double dartboard(long long ndarts); // estimates pi using monte carlo method

int main(int argc, char *argv[]) {
   double pi, ms;
   struct timeval tStart, tEnd;
   const short SEED       = 23;
   long long MAX_TOSSES   = 10; //10000000000; // use 1e9 or 1e10 for testing, 1e11 for measurements (later use 12)

   if (argc < 2) {
       fprintf(stderr, "usage: ./mc-pi-seq <NUM_SAMPLES>\n");
       exit(EXIT_FAILURE);
   }

   MAX_TOSSES = atoll(argv[1]); // ? TODO check out strtoll()

   srandom(SEED);               // seed the PRNG
   gettimeofday(&tStart, NULL); // start timer
   pi = dartboard(MAX_TOSSES);  // estimate pi using monte carlo
   gettimeofday(&tEnd, NULL);   // stop timer
   ms = gtod_diff_ms(&tStart, &tEnd); // execution tim in ms

   printf("estimated pi (n=%lld):\t%lf\n", MAX_TOSSES, pi);
   printf("error (vs math.h)\t\t%lf\n", pi - M_PI); // negative -> underestimated; positive -> overestimated
   printf("execution time:\t\t\t%ld s %ld ms\n", (long)(ms/1000), ((long)ms%1000));  

   return EXIT_SUCCESS;
}

double dartboard(long long ndarts) {
  double x, y, pi; 
  long long i, hits;
  hits = 0;

  for (i = 0; i < ndarts; ++i)  {                  // throw ndarts darts
    x = (2.0 * ((double)random()/RAND_MAX)) - 1.0; // (x,y) are random between -1 and 1
    y = (2.0 * ((double)random()/RAND_MAX)) - 1.0;
  
    if ((x*x + y*y) <= 1.0) {
      ++hits; // if our random dart landed inside the unit circle, increment the score
    }
  }

  pi = 4.0 * (double)hits / (double)ndarts; 
  return(pi);
} 

// TODO add timer to about after a given number of second and show intermediate result

/*

 seq:     n            err        sec
      100000        0.002367     0.005
      1000000      -0.001353     0.023
      10000000     -0.000673     0.191
      100000000     0.000257     1.875     -> 10*n => 10*RT in sec
      1000000000    0.000034    18.627
      10000000000  -0.000020   186.397
      100000000000  0.000007  1858.491     -> good initial input size: 1e11, then move to 1e12 (can get 30 samples)
 est. 1000000000000 0.000000 18585 sec -> ca 5hours  (too long to get 100 samples, get 5)
 */
