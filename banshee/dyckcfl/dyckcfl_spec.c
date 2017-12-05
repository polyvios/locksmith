/*
 * Copyright (c) 2006
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#include <assert.h>	
#include "dyckcfl.h"
#include "dyckcfl_terms.h"
#include "hash.h"

//  #define DYCK_DOT_DEBUG		

/*****************************************************************************
 *                                                                           *
 *   Type Declarations                                                       *
 *                                                                           *
 *****************************************************************************/

struct dyck_node_ {
  node_T node_constant;
  node_T node_variable;
  char *unique_name;
};

DEFINE_NONPTR_LIST(dyck_node_list, dyck_node);

/*****************************************************************************
 *                                                                           *
 *   Global Variables                                                        *
 *                                                                           *
 *****************************************************************************/

#ifdef DYCK_DOT_DEBUG
static FILE *dotfile = NULL;
#endif

// A map from indices to the clustered open constructor containing that index 
static hash_table constant_to_node_map = NULL;

static region dyckregion = NULL;
static bool pn_reach = FALSE;

int flag_dyck_print_constraints = 0;

static dyck_node_list all_nodes = NULL;

/*****************************************************************************
 *                                                                           *
 *   Static Utility Functions                                                *
 *                                                                           *
 *****************************************************************************/

static void my_call_node_T_inclusion(node_T e1, node_T e2)
{
  if (flag_dyck_print_constraints) {
    node_T_print(stdout, e1);
    printf(" <= " );
    node_T_print(stdout, e2);
    printf("\n");
  }
  node_T_inclusion(e1, e2);
} 

/*****************************************************************************
 *                                                                           *
 *   Initialization/ Resetting                                               *
 *                                                                           *
 *****************************************************************************/

void dyck_init(bool pn)
{
  dyckcfl_terms_init();
  dyckregion = newregion();
  
  flag_merge_projections = FALSE;
#ifdef DYCK_DOT_DEBUG
  dotfile = fopen("dyckcfl-test.dot","w");
  fprintf(dotfile,"digraph G {\n");
  fprintf(dotfile,"size=\"8,10\";\n");
#endif

  pn_reach = pn;
  constant_to_node_map = make_hash_table(dyckregion, 32, ptr_hash, ptr_eq);
  all_nodes = new_dyck_node_list(dyckregion);
}

void dyck_reset(void)
{
  deleteregion(dyckregion);
  dyckregion = NULL;
  pn_reach = FALSE;
  all_nodes = NULL;
}

/*****************************************************************************
 *                                                                           *
 *   Graph Construction API                                                  *
 *                                                                           *
 *****************************************************************************/

dyck_node make_tagged_dyck_node(const char *name)
{
  static int count = 0;
  char unique_name[512];
  dyck_node result = ralloc(dyckregion,struct dyck_node_);

  snprintf(unique_name,512,"%s_%d",name,count++);
  result->node_constant = node_T_constant(unique_name);
  result->node_variable = node_T_fresh(name);
  result->unique_name = rstrdup(dyckregion,unique_name);
  my_call_node_T_inclusion(result->node_constant, result->node_variable);

  dyck_node_list_cons(result, all_nodes);
  hash_table_insert(constant_to_node_map, (hash_key) result->node_constant,
		    (hash_data)result);

  return result;
}

dyck_node make_untagged_dyck_node(const char *name)
{
  static int count = 0;
  char unique_name[512];
  dyck_node result = ralloc(dyckregion,struct dyck_node_);

  snprintf(unique_name,512,"%s_%d",name,count++);
  result->node_constant = node_T_constant(unique_name);
  result->node_variable = node_T_fresh(name);
  result->unique_name = rstrdup(dyckregion,unique_name);

  dyck_node_list_cons(result, all_nodes);
  hash_table_insert(constant_to_node_map, (hash_key) result->node_constant,
		    (hash_data)result);
  return result;
}

dyck_node make_tagged_empty_dyck_node(const char *name)
{
  dyck_node result = ralloc(dyckregion,struct dyck_node_);
  result->node_constant = node_T_zero();
  result->node_variable = node_T_fresh(name);
  // switch the inclusion, since putting 0 in the lower bounds has no effect
  my_call_node_T_inclusion(result->node_variable, result->node_constant);

  dyck_node_list_cons(result, all_nodes);

  return result;
}

dyck_node make_tagged_universal_dyck_node(const char *name)
{
  dyck_node result = ralloc(dyckregion,struct dyck_node_);
  result->node_constant = node_T_one();
  result->node_variable = node_T_fresh(name);
  my_call_node_T_inclusion(result->node_constant, result->node_variable);

  dyck_node_list_cons(result, all_nodes);

  return result;
}

void mark_dyck_node_global(dyck_node n)
{
  assert(n);

  my_call_node_T_inclusion(NGroup(n->node_variable),n->node_variable); 		
  my_call_node_T_inclusion(n->node_variable,NGroup_pat0(n->node_variable));
}

void make_dyck_subtype_edge(dyck_node n1, dyck_node n2)
{
  my_call_node_T_inclusion(n1->node_variable, n2->node_variable);

#ifdef DYCK_DOT_DEBUG
  fprintf(dotfile,"\"%s\" -> \"%s\" [label=\"s\"];\n",n1->unique_name,n2->unique_name);
#endif
}

void make_dyck_open_edge(dyck_node n1, dyck_node n2, int index)
{
  // Make the constructed term and add the inclusion constraint
  my_call_node_T_inclusion( OPos(index, n1->node_variable), n2->node_variable);

#ifdef DYCK_DOT_DEBUG
  fprintf(dotfile,"\"%s\" -> \"%s\" [label=\"(_%d\"];\n",n1->unique_name,n2->unique_name,index);
#endif
}

// Make an (_{index} contravariant edge between n1 and n2
void make_dyck_contra_open_edge(dyck_node n1, dyck_node n2, int index) {
  // Make the constructed term and add the inclusion constraint
  my_call_node_T_inclusion(ONeg(index,n1->node_variable), n2->node_variable);

#ifdef DYCK_DOT_DEBUG
  fprintf(dotfile,"\"%s\" -> \"%s\" [label=\"(^%d\"];\n",n1->unique_name,n2->unique_name,index);
#endif

}

// n-edges for pn reachability (until a better method is devised)
static void make_dyck_p_edge(dyck_node n1, dyck_node n2)
{
  my_call_node_T_inclusion(Pos(n1->node_variable),n2->node_variable);
}


void make_dyck_close_edge(dyck_node n1, dyck_node n2, int index)
{
  my_call_node_T_inclusion(n1->node_variable,OPos_pat0(n2->node_variable,index));
  if (pn_reach) make_dyck_p_edge(n1, n2);

#ifdef DYCK_DOT_DEBUG
  fprintf(dotfile,"\"%s\" -> \"%s\" [label=\")_%d\"];\n",n1->unique_name,n2->unique_name,index);
#endif
}

void make_dyck_contra_close_edge(dyck_node n1, dyck_node n2, int index) 
{
  my_call_node_T_inclusion(n1->node_variable,ONeg_proj0(n2->node_variable,index));
  /* TODO : check */
  // if (pn_reach) make_dyck_p_edge(n1, n2);

#ifdef DYCK_DOT_DEBUG
  fprintf(dotfile,"\"%s\" -> \"%s\" [label=\")^%d\"];\n",n1->unique_name,n2->unique_name,index);
#endif
}

/*****************************************************************************
 *                                                                           *
 *   Query API                                                               *
 *                                                                           *
 *****************************************************************************/

void dyck_finished_adding()
{

#ifdef DYCK_DOT_DEBUG
  fprintf(dotfile,"\n}");
  fclose(dotfile);
#endif

  return;
}

bool dyck_check_reaches(dyck_node n1, dyck_node n2)
{  
  node_T_list_scanner scan;
  node_T next_lb;
  // compute the transitive lower bounds of n2
  node_T_list tlb = node_T_tlb(n2->node_variable);
  
  // check whether n1's constant is a member of n2
  node_T_list_scan(tlb,&scan);
  while (node_T_list_next(&scan,&next_lb)) {
    if (node_T_eq(n1->node_constant,next_lb)) return TRUE;
  }

  return FALSE;
}

// Search for target's pn reachability starting from current, assuming
// that nodes in visited have already been searched
static bool dyck_check_pn_reaches_aux(node_T target, node_T current,
	hash_table visited_n, 
	hash_table visited_p,
	bool seen_p,
	dyck_node_list all_constants)
{
	hash_table visited = seen_p ? visited_p : visited_n;

// Found the target
	if (target == NULL && node_T_is_constant(current,NULL)) {
		dyck_node n = NULL;
		assert(all_constants);
		insist(hash_table_lookup(constant_to_node_map,
			(hash_key)current,
			(hash_data *)&n));
		assert(n);
		dyck_node_list_cons(n, all_constants);
	}
	else if (target && node_T_eq(target,current)) return TRUE;

// Already searched from this point
	if (hash_table_lookup(visited,(void *)node_T_get_stamp(current),NULL)) return FALSE;

// Otherwise, mark this node visited
	hash_table_insert(visited,(void *)node_T_get_stamp(current),(void *)node_T_get_stamp(current));

// Compute this node's transitive lower bounds
	{
		node_T_list_scanner scan;
		node_T next_lb;
		node_T_list tlb = node_T_tlb(current);

	// Scan the lower bounds
		node_T_list_scan(tlb,&scan);

		while (node_T_list_next(&scan,&next_lb)) {
			struct OPos_decon o_contents;
			struct Pos_decon p_contents;

	// If target is null, we're finding all PN reaches
			if (target == NULL && node_T_is_constant(next_lb,NULL)) {
				dyck_node n = NULL;
				assert(all_constants);
				insist(hash_table_lookup(constant_to_node_map,
					(hash_key)next_lb,
					(hash_data *)&n));
				assert(n);
				dyck_node_list_cons(n, all_constants);
			}

	// Again, we've found the target
			else if (target != NULL && node_T_eq(target,next_lb)) return TRUE;

	// Deconstruct any constructors
	// and search their contents
			o_contents = OPos_decon(next_lb);
			p_contents = Pos_decon(next_lb);
			if (!seen_p && o_contents.f0 ) {
				if (dyck_check_pn_reaches_aux(target,o_contents.f0,visited_n,
				visited_p,FALSE, all_constants)) {
					if (target) return TRUE;
				}
			}
			else if (p_contents.f0) {
				if (dyck_check_pn_reaches_aux(target,p_contents.f0,visited_n,
				visited_p,TRUE, all_constants)) {
					if (target) return TRUE;
				}
			}


		}
	} 
// We didn't find the target on this search
	return FALSE;
}

bool dyck_check_pn_reaches(dyck_node n1, dyck_node n2)
{
  region scratch = NULL;
  hash_table visited = NULL;
  hash_table visited_p = NULL;
  bool result;


  if (!pn_reach) fail("PN reachability not enabled.");

  scratch = newregion();
  visited = make_hash_table(scratch, 32, ptr_hash, ptr_eq);
  visited_p = make_hash_table(scratch, 32, ptr_hash, ptr_eq);

  result = dyck_check_pn_reaches_aux(n1->node_constant,n2->node_variable,visited,
				   visited_p, FALSE, NULL);
  deleteregion(scratch);

  return result;
}

dyck_node_list rdyck_reaches(region r, dyck_node n)
{
  dyck_node_list result;
  node_T_list_scanner scan;
  node_T next_lb;
  // compute the transitive lower bounds of n
  node_T_list tlb = node_T_tlb(n->node_variable);
  
  
  result = new_dyck_node_list(r);

  node_T_list_scan(tlb,&scan);
  
  while (node_T_list_next(&scan,&next_lb)) {
    if (node_T_is_constant(next_lb,NULL)) {
      dyck_node next_node = NULL;
      insist(hash_table_lookup(constant_to_node_map, 
			       (hash_key)next_lb, (hash_data *)&next_node));
      assert(next_node);
      dyck_node_list_cons(next_node,result);
    }
  }

  return result;
}

dyck_node_list dyck_reaches(dyck_node n) {
	return rdyck_reaches(dyckregion, n);
}


dyck_node_list rdyck_pn_reaches(region r, dyck_node n)
{
  region scratch = NULL;
  hash_table visited = NULL;
  hash_table visited_p = NULL;
  dyck_node_list all_constants = NULL;


  if (!pn_reach) fail("PN reachability not enabled.");

  scratch = newregion();
  visited = make_hash_table(scratch, 32, ptr_hash, ptr_eq);
  visited_p = make_hash_table(scratch, 32, ptr_hash, ptr_eq);
  all_constants = new_dyck_node_list(r);

  dyck_check_pn_reaches_aux(NULL, n->node_variable,visited,
			    visited_p, FALSE, all_constants);

  deleteregion(scratch);

  return all_constants;
}

dyck_node_list dyck_pn_reaches(dyck_node n) {
	return rdyck_pn_reaches(dyckregion, n);
}

// Print (in dot format) a representation of the closed CFL graph, w/o
// PN edges
void dyck_print_closed_graph(FILE *f)
{
  dyck_node_list_scanner anscan;
  dyck_node next_node;

  fprintf(f,"digraph G {\n");
  fprintf(f,"size=\"8,10\";\n");


  dyck_node_list_scan(all_nodes,&anscan);
  
  while(dyck_node_list_next(&anscan,&next_node)) {
    node_T_list_scanner scan;
    node_T next_lb;
    
    node_T_list tlb = node_T_tlb(next_node->node_variable);
    node_T_list_scan(tlb,&scan);
    while (node_T_list_next(&scan,&next_lb)) {
      if (node_T_is_constant(next_lb,NULL)) {
	fprintf(f,"\"");
	node_T_print(f,next_lb);
	fprintf(f,"\" -> \"");
	node_T_print(f,next_node->node_constant);
	fprintf(f,"\" [label=\"%s\"];\n","s");
      }
    }
  }
  fprintf(f,"\n}");
}

