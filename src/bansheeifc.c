#define NONSPEC 1
#define deletes
#include <nonspec.h>
#include <dyckcfl.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <usage.h>
#include <regions.h>
#include <stdint.h>
#include <signal.h>

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_MALLOC_MALLOC_H
#include <malloc/malloc.h>
#endif


value banshee_make_tagged_node(value tag) {
  dyck_node d;
  CAMLparam1(tag);
  d = make_tagged_dyck_node(String_val(tag));
  CAMLreturn((value)d);
}

value banshee_make_untagged_node(value tag) {
  dyck_node d;
  CAMLparam1(tag);
  d = make_untagged_dyck_node(String_val(tag));
  CAMLreturn((value)d);
}

value banshee_make_tagged_empty_node(value tag) {
  dyck_node d;
  CAMLparam1(tag);
  d = make_tagged_empty_dyck_node(String_val(tag));
  CAMLreturn((value)d);
}

value banshee_make_tagged_universal_node(value tag) {
  dyck_node d;
  CAMLparam1(tag);
  d = make_tagged_universal_dyck_node(String_val(tag));
  CAMLreturn((value)d);
}

value banshee_mark_node_global(value node) {
  dyck_node d;

  CAMLparam1(node);
  d = (dyck_node) node;
  mark_dyck_node_global(d);
  CAMLreturn(Val_unit);
}

value banshee_make_subtype_edge(value t1, value t2) {
  dyck_node d1, d2;
  CAMLparam2(t1, t2);
  d1 = (dyck_node) t1;
  d2 = (dyck_node) t2;
  make_dyck_subtype_edge(d1,d2);
  CAMLreturn(Val_unit);
}

value banshee_initcfl(value dopn) {
  CAMLparam1(dopn);
  nonspec_init();
  dyck_init(Bool_val(dopn));
  flag_merge_projections = FALSE;
  //flag_dyck_print_constraints = TRUE;
  CAMLreturn(Val_unit);
}

value banshee_open_edge(value node1, value node2, value i) {
  dyck_node d1, d2;
  CAMLparam3(node1, node2, i);
  d1 = (dyck_node) node1;
  d2 = (dyck_node) node2;
	make_dyck_open_edge(d1,d2,Int_val(i));
  CAMLreturn(Val_unit);
}

value banshee_close_edge(value node1, value node2, value i) {
  dyck_node d1, d2;
  CAMLparam3(node1, node2, i);
  d1 = (dyck_node) node1;
  d2 = (dyck_node) node2;
	make_dyck_close_edge(d1,d2,Int_val(i));
  CAMLreturn(Val_unit);
}

value banshee_print_graph(value unit) {
  CAMLparam1(unit);
  FILE *f = fopen("graph.dot","w");
  assert(f);
  dyck_print_closed_graph(f);
  fclose(f);
  CAMLreturn(Val_unit);
}

value banshee_reaches_m(value n1, value n2) {
  dyck_node d1, d2;
  CAMLparam2(n1, n2);
  d1 = (dyck_node) n1;
  d2 = (dyck_node) n2;
  if(dyck_check_reaches(d1, d2)) CAMLreturn(Val_true);
  CAMLreturn(Val_false);
}

value banshee_reachespn(value n1, value n2) {
  dyck_node d1, d2;
  CAMLparam2(n1, n2);
  d1 = (dyck_node) n1;
  d2 = (dyck_node) n2;
  if(dyck_check_pn_reaches(d1, d2)) CAMLreturn(Val_true);
  CAMLreturn(Val_false);
}

//value banshee_reset(value unit) {
//  CAMLparam1(unit);
//  dyck_reset();
//  CAMLreturn(Val_unit);
//}

static value add_to_list(value list, value val) {
  CAMLparam2(list,val);
  CAMLlocal1(r);
  r = alloc(2, 0);
  Store_field(r, 0, val);
  Store_field(r, 1, list);              /* cdr = the first cons cell */
  CAMLreturn(r);
}

value banshee_reaches_m_list(value node) {
  dyck_node d;
  dyck_node_list dlist;
  dyck_node_list_scanner dscan;
  dyck_node next_node;
  region r;
  CAMLparam1(node);
  CAMLlocal1(resultlist);

  r = newregion();
  d = (dyck_node) node;
  dlist = rdyck_reaches(r,d);

  resultlist = Val_int(0);
  dyck_node_list_scan(dlist,&dscan);
  while(dyck_node_list_next(&dscan,&next_node)) {
    resultlist = add_to_list(resultlist, (value)next_node);
  }
  deleteregion(r);
  CAMLreturn(resultlist);
}

value banshee_reaches_pn_list(value node) {
  dyck_node d;
  dyck_node_list dlist;
  dyck_node_list_scanner dscan;
  dyck_node next_node;
  region r;
  CAMLparam1(node);
  CAMLlocal1(resultlist);

  r = newregion();
  d = (dyck_node) node;
  dlist = rdyck_pn_reaches(r,d);

  resultlist = Val_int(0);
  dyck_node_list_scan(dlist,&dscan);
  while(dyck_node_list_next(&dscan,&next_node)) {
    resultlist = add_to_list(resultlist, (value)next_node);
  }
  deleteregion(r);
  CAMLreturn(resultlist);
}

value get_usage(value ign) {
  long l;
  CAMLparam1(ign);
  l = get_memusage();
  CAMLreturn(Val_long(l));
}

value get_profile_mem(value ign) {
  long s;
  CAMLparam1(ign);
  s = region_profile_total_mem();
  CAMLreturn(Val_long(s));
}

value get_usage2(value ign) {
  CAMLparam1(ign);
  long result = 0;
#ifdef HAVE_MALLOC_ZONE_STATISTICS
  malloc_statistics_t zstats;
  struct mstats stats = mstats();
  malloc_zone_statistics(NULL, &zstats);
  result = zstats.size_in_use;
#elif HAVE_MALLINFO
  struct mallinfo info = mallinfo();
  result = info.uordblks;
#endif
  CAMLreturn(Val_long(result));
}

value banshee_hash(value node) {
  dyck_node d;
  intptr_t i;

  d = (dyck_node) node;
  i = (intptr_t) d;
  return Val_int(i >> 1); /* lsb is always zero, so just shift off */
}
