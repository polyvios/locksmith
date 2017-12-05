/*
 *
 * Copyright (c) 2004-2006, 
 *  Polyvios Pratikakis <polyvios@cs.umd.edu>
 *  Michael Hicks       <mwh@cs.umd.edu>
 *  Jeff Foster         <jfoster@cs.umd.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#include "nonspec.h"
#include "dyckcfl.h"

double gettime() {
  double t;
  struct timeval tv;

  gettimeofday(&tv, NULL);
  t = ((double) tv.tv_usec);
  t = t / 1000000;
  t += (double) tv.tv_sec;
  return t;
}

#define NUMNODES 300000

dyck_node nodes[NUMNODES];

int main() {
  FILE *fp;
  char line[100];
  char buf[20];
  int n1, n2, i, j;
  region r;
  //double t1, t2;

  nonspec_init();
  dyck_init(TRUE);

  for(i = 0; i < 200000; i++) {
    nodes[i] = NULL;
  }

  //t1 = gettime();
  fp = fopen("graph.txt", "r");
  if (fp == NULL) exit(EXIT_FAILURE);
  while ((fgets(line, 100, fp)) != NULL) {
    if (j % 1000 == 0) {
      //t2 = gettime();
      //printf("1000 lines in %f seconds: ", t2 - t1); fflush(stdout);
      printf("%s", line); fflush(stdout);
      //t1 = t2;
    }
    j++;
    switch(line[0]) {
      case '\n' :
        break;
      case '(' :
        sscanf(line+1, "%d %d %d", &n1, &n2, &i);
        make_dyck_open_edge(nodes[n1],nodes[n2],i);
        break;
      case ')' :
        sscanf(line+1, "%d %d %d", &n1, &n2, &i);
        make_dyck_close_edge(nodes[n1],nodes[n2],i);
        break;
      case 's' :
        sscanf(line+1, "%d %d", &n1, &n2);
        make_dyck_subtype_edge(nodes[n1], nodes[n2]);
        break;
      case 'g' :
        sscanf(line+1, "%d", &n1);
        mark_dyck_node_global(nodes[n1]);
        break;
      case 'c' :
        sscanf(line+1, "%d", &n1);
	if (n1 >= NUMNODES) {
	  printf("Aborting:  Increase NUMNODES\n");
	  exit(1);
	}
        sprintf(buf, "n%d", n1);
        nodes[n1] = make_tagged_dyck_node(buf);
        break;
      case 'm' :
        sscanf(line+1, "%d %d", &n1, &n2);
        dyck_check_reaches(nodes[n1], nodes[n2]);
        break;
      case 'p' :
        sscanf(line+1, "%d %d", &n1, &n2);
        dyck_check_pn_reaches(nodes[n1], nodes[n2]);
        break;
      case 'M' :
        sscanf(line+1, "%d", &n1);
        r = newregion();
        rdyck_reaches(r, nodes[n1]);
        deleteregion(r);
        //dyck_reaches(nodes[n1]);
        break;
      case 'P' :
        sscanf(line+1, "%d", &n1);
        r = newregion();
        rdyck_pn_reaches(r, nodes[n1]);
        deleteregion(r);
        break;
    }
  }
  return EXIT_SUCCESS;
}

