
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 58 "c-parse.y"

#include <stdio.h>
#include <errno.h>
#include <setjmp.h>

#include "parser.h"
#include "c-parse.h"
#include "c-lex.h"
#include "semantics.h"
#include "input.h"
#include "expr.h"
#include "stmt.h"

int yyparse(void) deletes;

void yyerror();

/* Like YYERROR but do call yyerror.  */
#define YYERROR1 { yyerror ("syntax error"); YYERROR; }

/* Cause the `yydebug' variable to be defined.  */
#define YYDEBUG 1

/* Line 189 of yacc.c  */
#line 187 "c-parse.y"

/* Region in which to allocate parse structures. Idea: the AST user can set
   this to different regions at appropriate junctures depending on what's
   being done with the AST */
region parse_region;
/* We'll see this a LOT below */
#define pr parse_region

/* Number of statements (loosely speaking) and compound statements 
   seen so far.  */
static int stmt_count;
static int compstmt_count;
  
/* List of types and structure classes of the current declaration.  */
static type_element current_declspecs = NULL;
static attribute prefix_attributes = NULL;

/* >0 if currently parsing an expression that will not be evaluated (argument
   to alignof, sizeof. Currently not typeof though that could be considered
   a bug) */
int unevaluated_expression;

#ifdef RC_ADJUST
static size_t rc_adjust_yystype(void *x, int by) 
{
  struct yystype *p = x;
  RC_ADJUST_PREAMBLE;

  RC_ADJUST(p->u.ptr, by);
  RC_ADJUST(p->idtoken.location.filename, by);
  RC_ADJUST(p->idtoken.id.data, by);
  RC_ADJUST(p->idtoken.decl, by);

  return sizeof *p;
}

static void rc_update_yystype(struct yystype *old, struct yystype *new)
{
  regionid base = regionidof(old);

  RC_UPDATE(base, old->u.ptr, new->u.ptr);
  RC_UPDATE(base, old->idtoken.location.filename, new->idtoken.location.filename);
  RC_UPDATE(base, old->idtoken.id.data, new->idtoken.id.data);
  RC_UPDATE(base, old->idtoken.decl, new->idtoken.decl);
}
#endif

/* A stack of declspecs and attributes for use during parsing */
typedef struct spec_stack *spec_stack;
struct spec_stack { 
  type_element parentptr declspecs;
  attribute parentptr attributes;
  spec_stack sameregion next;
};

/* Stack of saved values of current_declspecs and prefix_attributes.  */
/* In an ideal world, we would be able to eliminate most rc ops for
   declspec_stack and ds_region assignments. Seems tricky though. */
static spec_stack declspec_stack;
static region ds_region;

/* Pop top entry of declspec_stack back into current_declspecs,
   prefix_attributes */
static void pop_declspec_stack(void) deletes
{
  current_declspecs = declspec_stack->declspecs;
  prefix_attributes = declspec_stack->attributes;
  declspec_stack = declspec_stack->next;
  if (!declspec_stack)
    deleteregion_ptr(&ds_region);
}

static void push_declspec_stack(void)
{
  spec_stack news;

  if (!ds_region) ds_region = newsubregion(parse_region);
  news = ralloc(ds_region, struct spec_stack);
  news->declspecs = current_declspecs;
  news->attributes = prefix_attributes;
  news->next = declspec_stack;
  declspec_stack = news;
}

/* Tell yyparse how to print a token's value, if yydebug is set.  */

#define YYPRINT(FILE,YYCHAR,YYLVAL) yyprint(FILE,YYCHAR,YYLVAL)
void yyprint();


/* Line 189 of yacc.c  */
#line 189 "c-parse.tab.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     IDENTIFIER = 258,
     TYPENAME = 259,
     SCSPEC = 260,
     TYPESPEC = 261,
     TYPE_QUAL = 262,
     FN_QUAL = 263,
     CONSTANT = 264,
     STRING = 265,
     MAGIC_STRING = 266,
     ELLIPSIS = 267,
     SIZEOF = 268,
     ENUM = 269,
     STRUCT = 270,
     UNION = 271,
     IF = 272,
     ELSE = 273,
     WHILE = 274,
     DO = 275,
     FOR = 276,
     SWITCH = 277,
     CASE = 278,
     DEFAULT = 279,
     BREAK = 280,
     CONTINUE = 281,
     RETURN = 282,
     GOTO = 283,
     ASM_KEYWORD = 284,
     TYPEOF = 285,
     ALIGNOF = 286,
     ATTRIBUTE = 287,
     EXTENSION = 288,
     LABEL = 289,
     REALPART = 290,
     IMAGPART = 291,
     VA_ARG = 292,
     ASSIGN = 293,
     OROR = 294,
     ANDAND = 295,
     EQCOMPARE = 296,
     ARITHCOMPARE = 297,
     RSHIFT = 298,
     LSHIFT = 299,
     MINUSMINUS = 300,
     PLUSPLUS = 301,
     UNARY = 302,
     HYPERUNARY = 303,
     POINTSAT = 304
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 279 "c-parse.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2640

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  72
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  148
/* YYNRULES -- Number of rules.  */
#define YYNRULES  390
/* YYNRULES -- Number of states.  */
#define YYNSTATES  673

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   304

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    65,     2,     2,     2,    55,    46,     2,
      61,    68,    53,    51,    70,    52,    60,    54,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    41,    67,
       2,    38,     2,    40,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    62,     2,    71,    45,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    66,    44,    69,    64,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    39,    42,    43,    47,    48,    49,    50,
      56,    57,    58,    59,    63
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     6,     7,    10,    11,    15,    17,
      19,    25,    28,    32,    37,    42,    45,    49,    52,    55,
      57,    58,    59,    67,    72,    73,    74,    82,    87,    88,
      89,    96,   100,   102,   104,   106,   108,   110,   112,   114,
     116,   118,   120,   122,   124,   126,   128,   129,   131,   133,
     137,   139,   142,   145,   148,   151,   154,   159,   162,   167,
     169,   171,   173,   178,   179,   187,   189,   193,   197,   201,
     205,   209,   213,   217,   221,   225,   229,   233,   237,   241,
     245,   251,   252,   258,   262,   266,   268,   270,   272,   276,
     280,   281,   286,   291,   298,   303,   307,   311,   314,   317,
     319,   321,   324,   326,   328,   329,   331,   334,   336,   338,
     341,   344,   349,   354,   358,   361,   363,   365,   368,   371,
     372,   373,   378,   383,   387,   391,   395,   398,   401,   404,
     408,   409,   412,   415,   418,   421,   425,   426,   429,   432,
     434,   436,   439,   442,   444,   446,   449,   452,   455,   459,
     460,   463,   465,   467,   469,   474,   479,   481,   483,   485,
     487,   491,   493,   497,   498,   503,   504,   511,   515,   516,
     523,   527,   528,   530,   532,   535,   542,   544,   548,   549,
     551,   556,   563,   568,   570,   572,   574,   576,   578,   582,
     584,   585,   588,   590,   594,   596,   600,   602,   610,   616,
     621,   625,   630,   631,   632,   638,   639,   640,   646,   648,
     650,   654,   659,   664,   668,   672,   676,   678,   683,   688,
     692,   696,   700,   702,   707,   711,   715,   720,   724,   728,
     730,   732,   733,   741,   747,   750,   751,   759,   765,   768,
     769,   778,   779,   787,   790,   791,   793,   794,   796,   798,
     801,   802,   806,   809,   813,   816,   820,   823,   825,   828,
     830,   834,   837,   842,   846,   848,   852,   854,   856,   860,
     863,   866,   867,   869,   871,   874,   875,   878,   882,   886,
     889,   894,   899,   903,   907,   911,   914,   916,   918,   921,
     924,   925,   927,   930,   931,   932,   934,   936,   939,   943,
     945,   948,   950,   954,   961,   967,   973,   976,   979,   984,
     985,   990,   992,   995,   997,   999,  1001,  1004,  1005,  1010,
    1012,  1016,  1017,  1018,  1026,  1032,  1035,  1036,  1037,  1038,
    1051,  1052,  1059,  1062,  1065,  1068,  1072,  1079,  1088,  1099,
    1112,  1116,  1121,  1123,  1127,  1133,  1136,  1139,  1140,  1142,
    1143,  1145,  1146,  1148,  1150,  1154,  1159,  1161,  1165,  1166,
    1169,  1172,  1173,  1178,  1181,  1182,  1184,  1186,  1190,  1192,
    1196,  1201,  1206,  1211,  1216,  1221,  1222,  1225,  1227,  1230,
    1232,  1236,  1238,  1240,  1244,  1245,  1247,  1249,  1251,  1253,
    1255
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      73,     0,    -1,    -1,    74,    -1,    -1,    75,    77,    -1,
      -1,    74,    76,    77,    -1,    79,    -1,    78,    -1,    29,
      61,    90,    68,    67,    -1,   215,    77,    -1,   109,   123,
      67,    -1,   116,   109,   123,    67,    -1,   112,   109,   122,
      67,    -1,   116,    67,    -1,   112,   109,    67,    -1,     1,
      67,    -1,     1,    69,    -1,    67,    -1,    -1,    -1,   112,
     109,   145,    80,   105,    81,   177,    -1,   112,   109,   145,
       1,    -1,    -1,    -1,   116,   109,   148,    82,   105,    83,
     177,    -1,   116,   109,   148,     1,    -1,    -1,    -1,   109,
     148,    84,   105,    85,   177,    -1,   109,   148,     1,    -1,
       3,    -1,     4,    -1,    86,    -1,    86,    -1,    46,    -1,
      52,    -1,    51,    -1,    57,    -1,    56,    -1,    64,    -1,
      65,    -1,    35,    -1,    36,    -1,    92,    -1,    -1,    92,
      -1,    98,    -1,    92,    70,    98,    -1,   100,    -1,    53,
      96,    -1,   215,    96,    -1,    89,    96,    -1,    43,    87,
      -1,    94,    93,    -1,    94,    61,   164,    68,    -1,    95,
      93,    -1,    95,    61,   164,    68,    -1,    13,    -1,    31,
      -1,    93,    -1,    61,   164,    68,    96,    -1,    -1,    61,
     164,    68,    66,    97,   136,    69,    -1,    96,    -1,    98,
      51,    98,    -1,    98,    52,    98,    -1,    98,    53,    98,
      -1,    98,    54,    98,    -1,    98,    55,    98,    -1,    98,
      50,    98,    -1,    98,    49,    98,    -1,    98,    48,    98,
      -1,    98,    47,    98,    -1,    98,    46,    98,    -1,    98,
      44,    98,    -1,    98,    45,    98,    -1,    98,    43,    98,
      -1,    98,    42,    98,    -1,    98,    40,    90,    41,    98,
      -1,    -1,    98,    40,    99,    41,    98,    -1,    98,    38,
      98,    -1,    98,    39,    98,    -1,     3,    -1,     9,    -1,
     102,    -1,    61,    90,    68,    -1,    61,     1,    68,    -1,
      -1,    61,   101,   179,    68,    -1,   100,    61,    91,    68,
      -1,    37,    61,    98,    70,   164,    68,    -1,   100,    62,
      90,    71,    -1,   100,    60,    86,    -1,   100,    63,    86,
      -1,   100,    57,    -1,   100,    56,    -1,   103,    -1,   104,
      -1,   103,   104,    -1,    10,    -1,    11,    -1,    -1,   106,
      -1,   106,    12,    -1,   107,    -1,   172,    -1,   106,   107,
      -1,   107,   172,    -1,   114,   109,   122,    67,    -1,   117,
     109,   123,    67,    -1,   114,   109,    67,    -1,   117,    67,
      -1,   111,    -1,   172,    -1,   108,   111,    -1,   111,   172,
      -1,    -1,    -1,   112,   109,   122,    67,    -1,   116,   109,
     123,    67,    -1,   112,   109,   139,    -1,   116,   109,   142,
      -1,   112,   109,    67,    -1,   116,    67,    -1,   215,   111,
      -1,   120,   113,    -1,   116,   120,   113,    -1,    -1,   113,
     121,    -1,   113,   216,    -1,   113,   130,    -1,   120,   115,
      -1,   117,   120,   115,    -1,    -1,   115,   121,    -1,   115,
     216,    -1,   117,    -1,   130,    -1,   116,   117,    -1,   116,
     130,    -1,   217,    -1,   216,    -1,   117,   217,    -1,   117,
     216,    -1,   120,   119,    -1,   166,   120,   119,    -1,    -1,
     119,   121,    -1,   219,    -1,   150,    -1,     4,    -1,    30,
      61,    90,    68,    -1,    30,    61,   164,    68,    -1,   219,
      -1,   217,    -1,   150,    -1,   125,    -1,   122,    70,   125,
      -1,   127,    -1,   123,    70,   125,    -1,    -1,    29,    61,
     102,    68,    -1,    -1,   145,   124,   129,    38,   126,   135,
      -1,   145,   124,   129,    -1,    -1,   148,   124,   129,    38,
     128,   135,    -1,   148,   124,   129,    -1,    -1,   130,    -1,
     131,    -1,   130,   131,    -1,    32,    61,    61,   132,    68,
      68,    -1,   133,    -1,   132,    70,   133,    -1,    -1,   134,
      -1,   134,    61,     3,    68,    -1,   134,    61,     3,    70,
      92,    68,    -1,   134,    61,    91,    68,    -1,    88,    -1,
     216,    -1,   219,    -1,   217,    -1,    98,    -1,    66,   136,
      69,    -1,     1,    -1,    -1,   137,   155,    -1,   138,    -1,
     137,    70,   138,    -1,    98,    -1,    66,   136,    69,    -1,
       1,    -1,    62,    98,    12,    98,    71,    38,   138,    -1,
      62,    98,    71,    38,   138,    -1,    62,    98,    71,   138,
      -1,    88,    41,   138,    -1,    60,    88,    38,   138,    -1,
      -1,    -1,   145,   140,   105,   141,   179,    -1,    -1,    -1,
     148,   143,   105,   144,   179,    -1,   146,    -1,   148,    -1,
      61,   146,    68,    -1,   146,    61,   208,   214,    -1,   146,
      62,    90,    71,    -1,   146,    62,    71,    -1,    53,   167,
     146,    -1,   130,   110,   146,    -1,     4,    -1,   147,    61,
     208,   214,    -1,   147,    62,    90,    71,    -1,   147,    62,
      71,    -1,    53,   167,   147,    -1,   130,   110,   147,    -1,
       4,    -1,   148,    61,   208,   214,    -1,    61,   148,    68,
      -1,    53,   167,   148,    -1,   148,    62,    90,    71,    -1,
     148,    62,    71,    -1,   130,   110,   148,    -1,     3,    -1,
      86,    -1,    -1,    15,   149,    66,   151,   157,    69,   129,
      -1,    15,    66,   157,    69,   129,    -1,    15,   149,    -1,
      -1,    16,   149,    66,   152,   157,    69,   129,    -1,    16,
      66,   157,    69,   129,    -1,    16,   149,    -1,    -1,    14,
     149,    66,   153,   162,   156,    69,   129,    -1,    -1,    14,
      66,   154,   162,   156,    69,   129,    -1,    14,   149,    -1,
      -1,    70,    -1,    -1,    70,    -1,   158,    -1,   158,   159,
      -1,    -1,   158,   159,    67,    -1,   158,    67,    -1,   118,
     109,   160,    -1,   118,   109,    -1,   166,   109,   160,    -1,
     166,   109,    -1,     1,    -1,   215,   159,    -1,   161,    -1,
     160,    70,   161,    -1,   145,   129,    -1,   145,    41,    98,
     129,    -1,    41,    98,   129,    -1,   163,    -1,   162,    70,
     163,    -1,     1,    -1,    86,    -1,    86,    38,    98,    -1,
     118,   165,    -1,   166,   165,    -1,    -1,   168,    -1,   217,
      -1,   166,   217,    -1,    -1,   167,   217,    -1,    61,   168,
      68,    -1,    53,   167,   168,    -1,    53,   167,    -1,   168,
      61,   201,   214,    -1,   168,    62,    90,    71,    -1,   168,
      62,    71,    -1,    61,   201,   214,    -1,    62,    90,    71,
      -1,    62,    71,    -1,   170,    -1,   185,    -1,   170,   185,
      -1,   170,   172,    -1,    -1,   169,    -1,     1,    67,    -1,
      -1,    -1,   175,    -1,   176,    -1,   175,   176,    -1,    34,
     213,    67,    -1,   179,    -1,     1,   179,    -1,    66,    -1,
     178,   173,    69,    -1,   178,   173,   174,   108,   171,    69,
      -1,   178,   173,   174,     1,    69,    -1,   178,   173,   174,
     169,    69,    -1,   181,   184,    -1,   181,     1,    -1,    17,
      61,    90,    68,    -1,    -1,    20,   183,   184,    19,    -1,
     186,    -1,   194,   184,    -1,   186,    -1,   194,    -1,   179,
      -1,    90,    67,    -1,    -1,   180,    18,   187,   184,    -1,
     180,    -1,   180,    18,     1,    -1,    -1,    -1,    19,   188,
      61,    90,    68,   189,   184,    -1,   182,    61,    90,    68,
      67,    -1,   182,     1,    -1,    -1,    -1,    -1,    21,    61,
     196,    67,   190,   196,    67,   191,   196,    68,   192,   184,
      -1,    -1,    22,    61,    90,    68,   193,   184,    -1,    25,
      67,    -1,    26,    67,    -1,    27,    67,    -1,    27,    90,
      67,    -1,    29,   195,    61,    90,    68,    67,    -1,    29,
     195,    61,    90,    41,   197,    68,    67,    -1,    29,   195,
      61,    90,    41,   197,    41,   197,    68,    67,    -1,    29,
     195,    61,    90,    41,   197,    41,   197,    41,   200,    68,
      67,    -1,    28,    87,    67,    -1,    28,    53,    90,    67,
      -1,    67,    -1,    23,    98,    41,    -1,    23,    98,    12,
      98,    41,    -1,    24,    41,    -1,    87,    41,    -1,    -1,
     217,    -1,    -1,    90,    -1,    -1,   198,    -1,   199,    -1,
     198,    70,   199,    -1,    10,    61,    90,    68,    -1,   102,
      -1,   200,    70,   102,    -1,    -1,   202,   203,    -1,   205,
      68,    -1,    -1,   206,    67,   204,   203,    -1,     1,    68,
      -1,    -1,    12,    -1,   206,    -1,   206,    70,    12,    -1,
     207,    -1,   206,    70,   207,    -1,   112,   109,   147,   129,
      -1,   112,   109,   148,   129,    -1,   112,   109,   165,   129,
      -1,   116,   109,   148,   129,    -1,   116,   109,   165,   129,
      -1,    -1,   209,   210,    -1,   203,    -1,   211,    68,    -1,
     212,    -1,   211,    70,   212,    -1,     3,    -1,    87,    -1,
     213,    70,    87,    -1,    -1,   218,    -1,    33,    -1,     5,
      -1,     7,    -1,     8,    -1,     6,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   279,   279,   283,   294,   294,   295,   295,   300,   301,
     302,   307,   313,   321,   324,   327,   329,   333,   334,   335,
     343,   347,   342,   351,   355,   359,   354,   363,   367,   371,
     366,   375,   381,   382,   386,   390,   392,   394,   396,   398,
     400,   402,   404,   406,   408,   412,   421,   422,   426,   428,
     433,   434,   437,   440,   447,   452,   462,   465,   468,   474,
     478,   482,   483,   486,   485,   531,   532,   534,   536,   538,
     540,   542,   544,   546,   548,   550,   552,   554,   556,   558,
     560,   563,   562,   568,   570,   575,   581,   582,   583,   585,
     588,   587,   602,   604,   606,   608,   610,   613,   615,   621,
     624,   625,   630,   631,   638,   639,   640,   652,   653,   654,
     655,   663,   666,   669,   674,   684,   685,   686,   687,   695,
     705,   715,   718,   721,   724,   727,   731,   733,   743,   745,
     750,   751,   753,   758,   763,   765,   771,   772,   774,   787,
     788,   789,   791,   796,   797,   798,   800,   813,   815,   820,
     821,   829,   830,   831,   835,   837,   843,   844,   845,   849,
     850,   854,   855,   860,   861,   868,   867,   873,   881,   880,
     886,   893,   894,   899,   901,   906,   911,   913,   919,   920,
     922,   925,   929,   937,   938,   939,   940,   946,   947,   951,
     958,   961,   965,   966,   972,   973,   975,   979,   981,   983,
     985,   987,   993,  1000,   992,  1013,  1020,  1012,  1035,  1036,
    1042,  1044,  1046,  1048,  1050,  1057,  1059,  1068,  1070,  1072,
    1074,  1081,  1083,  1090,  1092,  1094,  1096,  1098,  1105,  1107,
    1111,  1116,  1115,  1121,  1125,  1128,  1127,  1131,  1135,  1138,
    1137,  1142,  1141,  1145,  1149,  1151,  1154,  1156,  1161,  1163,
    1169,  1170,  1172,  1188,  1191,  1197,  1200,  1206,  1208,  1214,
    1215,  1220,  1223,  1226,  1232,  1233,  1235,  1241,  1243,  1248,
    1250,  1256,  1257,  1261,  1262,  1268,  1269,  1274,  1277,  1279,
    1281,  1283,  1285,  1287,  1289,  1291,  1302,  1318,  1319,  1321,
    1326,  1327,  1330,  1334,  1340,  1341,  1348,  1349,  1353,  1360,
    1361,  1364,  1366,  1368,  1370,  1373,  1379,  1382,  1386,  1397,
    1396,  1408,  1410,  1415,  1417,  1423,  1425,  1429,  1428,  1436,
    1445,  1448,  1450,  1447,  1459,  1466,  1469,  1470,  1472,  1469,
    1479,  1478,  1487,  1492,  1497,  1501,  1505,  1510,  1515,  1519,
    1523,  1528,  1534,  1541,  1544,  1547,  1550,  1559,  1560,  1565,
    1566,  1572,  1573,  1577,  1578,  1583,  1590,  1592,  1599,  1599,
    1607,  1609,  1608,  1619,  1626,  1627,  1637,  1639,  1644,  1645,
    1652,  1656,  1660,  1664,  1668,  1678,  1678,  1685,  1686,  1691,
    1693,  1698,  1703,  1704,  1711,  1712,  1716,  1723,  1727,  1731,
    1735
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IDENTIFIER", "TYPENAME", "SCSPEC",
  "TYPESPEC", "TYPE_QUAL", "FN_QUAL", "CONSTANT", "STRING", "MAGIC_STRING",
  "ELLIPSIS", "SIZEOF", "ENUM", "STRUCT", "UNION", "IF", "ELSE", "WHILE",
  "DO", "FOR", "SWITCH", "CASE", "DEFAULT", "BREAK", "CONTINUE", "RETURN",
  "GOTO", "ASM_KEYWORD", "TYPEOF", "ALIGNOF", "ATTRIBUTE", "EXTENSION",
  "LABEL", "REALPART", "IMAGPART", "VA_ARG", "'='", "ASSIGN", "'?'", "':'",
  "OROR", "ANDAND", "'|'", "'^'", "'&'", "EQCOMPARE", "ARITHCOMPARE",
  "RSHIFT", "LSHIFT", "'+'", "'-'", "'*'", "'/'", "'%'", "MINUSMINUS",
  "PLUSPLUS", "UNARY", "HYPERUNARY", "'.'", "'('", "'['", "POINTSAT",
  "'~'", "'!'", "'{'", "';'", "')'", "'}'", "','", "']'", "$accept",
  "program", "extdefs", "@1", "@2", "extdef", "datadef", "fndef", "$@3",
  "$@4", "$@5", "$@6", "$@7", "$@8", "identifier", "id_label", "idword",
  "unop", "expr", "exprlist", "nonnull_exprlist", "unary_expr", "sizeof",
  "alignof", "cast_expr", "$@9", "expr_no_commas", "$@10", "primary",
  "$@11", "string", "string_list", "string_component",
  "old_style_parm_decls", "datadecls", "datadecl", "decls", "setspecs",
  "setattrs", "decl", "typed_declspecs", "reserved_declspecs",
  "typed_declspecs_no_prefix_attr", "reserved_declspecs_no_prefix_attr",
  "declmods", "declmods_no_prefix_attr", "typed_typespecs",
  "reserved_typespecquals", "typespec", "typespecqual_reserved",
  "initdecls", "notype_initdecls", "maybeasm", "initdcl", "@12",
  "notype_initdcl", "@13", "maybe_attribute", "attributes", "attribute",
  "attribute_list", "attrib", "any_word", "init", "initlist_maybe_comma",
  "initlist1", "initelt", "nested_function", "$@14", "$@15",
  "notype_nested_function", "$@16", "$@17", "declarator",
  "after_type_declarator", "parm_declarator", "notype_declarator", "tag",
  "structsp", "@18", "@19", "@20", "@21", "maybecomma", "maybecomma_warn",
  "component_decl_list", "component_decl_list2", "component_decl",
  "components", "component_declarator", "enumlist", "enumerator",
  "typename", "absdcl", "nonempty_type_quals", "type_quals", "absdcl1",
  "stmts", "stmt_or_labels", "xstmts", "errstmt", "pushlevel",
  "maybe_label_decls", "label_decls", "label_decl", "compstmt_or_error",
  "compstmt_start", "compstmt", "simple_if", "if_prefix", "do_stmt_start",
  "@22", "labeled_stmt", "stmt_or_label", "stmt", "$@23", "$@24", "@25",
  "$@26", "$@27", "@28", "@29", "label", "maybe_type_qual", "xexpr",
  "asm_operands", "nonnull_asm_operands", "asm_operand", "asm_clobbers",
  "parmlist", "$@30", "parmlist_1", "$@31", "parmlist_2", "parms", "parm",
  "parmlist_or_identifiers", "$@32", "parmlist_or_identifiers_1",
  "identifiers", "old_parameter", "identifiers_or_typenames", "fn_quals",
  "extension", "scspec", "type_qual", "fn_qual", "type_spec", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,    61,   293,
      63,    58,   294,   295,   124,    94,    38,   296,   297,   298,
     299,    43,    45,    42,    47,    37,   300,   301,   302,   303,
      46,    40,    91,   304,   126,    33,   123,    59,    41,   125,
      44,    93
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    72,    73,    73,    75,    74,    76,    74,    77,    77,
      77,    77,    78,    78,    78,    78,    78,    78,    78,    78,
      80,    81,    79,    79,    82,    83,    79,    79,    84,    85,
      79,    79,    86,    86,    87,    88,    89,    89,    89,    89,
      89,    89,    89,    89,    89,    90,    91,    91,    92,    92,
      93,    93,    93,    93,    93,    93,    93,    93,    93,    94,
      95,    96,    96,    97,    96,    98,    98,    98,    98,    98,
      98,    98,    98,    98,    98,    98,    98,    98,    98,    98,
      98,    99,    98,    98,    98,   100,   100,   100,   100,   100,
     101,   100,   100,   100,   100,   100,   100,   100,   100,   102,
     103,   103,   104,   104,   105,   105,   105,   106,   106,   106,
     106,   107,   107,   107,   107,   108,   108,   108,   108,   109,
     110,   111,   111,   111,   111,   111,   111,   111,   112,   112,
     113,   113,   113,   113,   114,   114,   115,   115,   115,   116,
     116,   116,   116,   117,   117,   117,   117,   118,   118,   119,
     119,   120,   120,   120,   120,   120,   121,   121,   121,   122,
     122,   123,   123,   124,   124,   126,   125,   125,   128,   127,
     127,   129,   129,   130,   130,   131,   132,   132,   133,   133,
     133,   133,   133,   134,   134,   134,   134,   135,   135,   135,
     136,   136,   137,   137,   138,   138,   138,   138,   138,   138,
     138,   138,   140,   141,   139,   143,   144,   142,   145,   145,
     146,   146,   146,   146,   146,   146,   146,   147,   147,   147,
     147,   147,   147,   148,   148,   148,   148,   148,   148,   148,
     149,   151,   150,   150,   150,   152,   150,   150,   150,   153,
     150,   154,   150,   150,   155,   155,   156,   156,   157,   157,
     158,   158,   158,   159,   159,   159,   159,   159,   159,   160,
     160,   161,   161,   161,   162,   162,   162,   163,   163,   164,
     164,   165,   165,   166,   166,   167,   167,   168,   168,   168,
     168,   168,   168,   168,   168,   168,   169,   170,   170,   170,
     171,   171,   172,   173,   174,   174,   175,   175,   176,   177,
     177,   178,   179,   179,   179,   179,   180,   180,   181,   183,
     182,   184,   184,   185,   185,   186,   186,   187,   186,   186,
     186,   188,   189,   186,   186,   186,   190,   191,   192,   186,
     193,   186,   186,   186,   186,   186,   186,   186,   186,   186,
     186,   186,   186,   194,   194,   194,   194,   195,   195,   196,
     196,   197,   197,   198,   198,   199,   200,   200,   202,   201,
     203,   204,   203,   203,   205,   205,   205,   205,   206,   206,
     207,   207,   207,   207,   207,   209,   208,   210,   210,   211,
     211,   212,   213,   213,   214,   214,   215,   216,   217,   218,
     219
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     1,     0,     2,     0,     3,     1,     1,
       5,     2,     3,     4,     4,     2,     3,     2,     2,     1,
       0,     0,     7,     4,     0,     0,     7,     4,     0,     0,
       6,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     1,     3,
       1,     2,     2,     2,     2,     2,     4,     2,     4,     1,
       1,     1,     4,     0,     7,     1,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       5,     0,     5,     3,     3,     1,     1,     1,     3,     3,
       0,     4,     4,     6,     4,     3,     3,     2,     2,     1,
       1,     2,     1,     1,     0,     1,     2,     1,     1,     2,
       2,     4,     4,     3,     2,     1,     1,     2,     2,     0,
       0,     4,     4,     3,     3,     3,     2,     2,     2,     3,
       0,     2,     2,     2,     2,     3,     0,     2,     2,     1,
       1,     2,     2,     1,     1,     2,     2,     2,     3,     0,
       2,     1,     1,     1,     4,     4,     1,     1,     1,     1,
       3,     1,     3,     0,     4,     0,     6,     3,     0,     6,
       3,     0,     1,     1,     2,     6,     1,     3,     0,     1,
       4,     6,     4,     1,     1,     1,     1,     1,     3,     1,
       0,     2,     1,     3,     1,     3,     1,     7,     5,     4,
       3,     4,     0,     0,     5,     0,     0,     5,     1,     1,
       3,     4,     4,     3,     3,     3,     1,     4,     4,     3,
       3,     3,     1,     4,     3,     3,     4,     3,     3,     1,
       1,     0,     7,     5,     2,     0,     7,     5,     2,     0,
       8,     0,     7,     2,     0,     1,     0,     1,     1,     2,
       0,     3,     2,     3,     2,     3,     2,     1,     2,     1,
       3,     2,     4,     3,     1,     3,     1,     1,     3,     2,
       2,     0,     1,     1,     2,     0,     2,     3,     3,     2,
       4,     4,     3,     3,     3,     2,     1,     1,     2,     2,
       0,     1,     2,     0,     0,     1,     1,     2,     3,     1,
       2,     1,     3,     6,     5,     5,     2,     2,     4,     0,
       4,     1,     2,     1,     1,     1,     2,     0,     4,     1,
       3,     0,     0,     7,     5,     2,     0,     0,     0,    12,
       0,     6,     2,     2,     2,     3,     6,     8,    10,    12,
       3,     4,     1,     3,     5,     2,     2,     0,     1,     0,
       1,     0,     1,     1,     3,     4,     1,     3,     0,     2,
       2,     0,     4,     2,     0,     1,     1,     3,     1,     3,
       4,     4,     4,     4,     4,     0,     2,     1,     2,     1,
       3,     1,     1,     3,     0,     1,     1,     1,     1,     1,
       1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       4,     0,     6,     0,     1,     0,     0,   153,   387,   390,
     388,     0,     0,     0,     0,     0,     0,   386,    19,     5,
       9,     8,     0,   119,   119,   139,   130,   140,   173,   152,
       0,   144,   143,   151,     7,    17,    18,    32,    33,   241,
     230,   243,   250,   234,   250,   238,     0,     0,     0,   229,
     275,     0,     0,   161,   120,     0,     0,    15,     0,   141,
     130,   142,   146,   145,   128,   174,    11,     0,   239,     0,
       0,   231,     0,   235,    85,    86,   102,   103,    59,    60,
      43,    44,     0,     0,    36,    38,    37,     0,    40,    39,
       0,    41,    42,     0,     0,    45,    61,     0,     0,    65,
      48,    50,    87,    99,   100,     0,     0,   271,   149,     0,
     271,   273,   178,     0,     0,    12,     0,     0,    31,     0,
     375,     0,     0,   171,   216,   275,     0,    16,     0,   159,
     120,     0,   208,   209,     0,     0,   129,   131,   133,   158,
     132,   157,   156,   266,   267,   246,   264,     0,   171,   257,
     252,   119,   249,   119,     0,   250,   171,   250,     0,    34,
      54,    51,     0,     0,     0,     0,    53,     0,     0,     0,
      55,     0,    57,     0,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      98,    97,     0,    46,     0,     0,   101,    52,   154,   275,
     358,     0,   269,   272,   147,   155,   149,   270,   274,    35,
     183,     0,   176,   179,   184,   186,   185,   225,   276,   224,
     162,   163,   228,     0,   384,     0,   227,     0,     0,    29,
     105,     0,   119,   119,   136,   108,   170,   172,     0,     0,
      14,     0,     0,    23,     0,   171,   375,     0,    13,    27,
       0,     0,   247,     0,   246,   233,   254,   251,   256,   258,
       0,   237,     0,     0,    89,    88,   301,   293,     0,     0,
      10,    49,     0,     0,    83,    84,     0,     0,    79,    78,
      76,    77,    75,    74,    73,    72,    71,    66,    67,    68,
      69,    70,    95,     0,    47,     0,    96,   279,     0,   384,
       0,   285,     0,   358,     0,   150,   148,     0,   178,    46,
       0,   389,   223,   385,     0,   381,   365,   119,   119,   377,
       0,   366,   368,   376,     0,   379,   226,   292,     0,   106,
     109,   110,     0,   114,     0,   136,   134,   168,   214,   210,
     160,   215,    21,   167,   384,   213,     0,    25,   268,   265,
     171,     0,     0,   171,   253,   259,   255,   171,   171,     0,
     294,    91,    63,    62,    56,    58,     0,     0,    92,    94,
     278,   277,   283,   359,   284,   384,   282,     0,   175,   177,
      85,     0,   164,   363,   271,   271,   360,   361,     0,   378,
       0,     0,    30,   299,   113,     0,     0,   163,   135,   137,
     138,     0,     0,   165,   211,   212,     0,   242,   171,   171,
       0,   261,     0,   232,   236,     0,     0,   302,     0,   295,
     296,     0,    80,    82,   280,   281,   180,     0,   182,   222,
     275,   358,   120,   171,   171,   171,   275,   171,   171,     0,
     367,   369,   380,   300,   111,   112,   189,     0,   187,   169,
      22,     0,    26,   240,   263,   171,   260,    93,   382,     0,
       0,    85,   153,     0,   321,   309,     0,     0,     0,     0,
       0,     0,     0,     0,   347,   342,     0,     0,   290,     0,
     119,   119,     0,     0,   116,   315,   319,     0,     0,   287,
     313,   314,     0,   297,   196,     0,     0,     0,     0,   194,
       0,   244,   192,     0,   279,     0,   375,     0,   370,   371,
     372,   279,   373,   374,   362,     0,   166,   262,   298,     0,
     304,     0,     0,     0,   349,     0,     0,   345,   332,   333,
     334,     0,     0,     0,     0,   348,   346,   316,   117,   291,
       0,   118,     0,   126,     0,   305,   289,   288,     0,   307,
     306,   311,     0,   325,     0,   127,     0,     0,     0,     0,
      64,     0,   191,   181,   220,   275,   221,   384,   219,     0,
     188,   383,     0,     0,     0,   350,     0,     0,     0,   343,
     335,     0,   340,     0,   303,   125,     0,   123,   202,     0,
     124,   205,   320,     0,   312,     0,     0,     0,     0,   195,
     200,   193,     0,   217,   218,   308,     0,   310,   326,   330,
       0,   341,     0,   121,     0,   122,     0,   318,     0,   201,
       0,     0,   199,   322,   349,     0,   344,   351,     0,   203,
     206,   324,     0,   198,     0,     0,   331,     0,     0,   352,
     353,   336,     0,     0,     0,   323,   327,     0,   351,     0,
       0,   204,   207,   197,   349,     0,     0,   337,   354,     0,
     355,     0,     0,   328,   356,     0,   338,     0,     0,     0,
     329,   339,   357
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     3,     5,    19,    20,    21,   244,   402,
     250,   406,   122,   328,   159,   476,   498,    93,   477,   293,
      95,    96,    97,    98,    99,   421,   100,   277,   101,   164,
     102,   103,   104,   229,   230,   231,   478,    22,   117,   479,
     317,    64,   232,   336,   318,    25,   107,   204,    26,   137,
     128,    52,   123,   129,   451,    53,   401,   236,   237,    28,
     211,   212,   213,   449,   500,   501,   502,   587,   614,   642,
     590,   616,   643,   221,   132,   564,   133,    41,    29,   155,
     157,   147,    67,   562,   253,    69,    70,   152,   354,   355,
     145,   146,   109,   202,   110,   113,   203,   482,   483,   540,
     235,   360,   418,   419,   420,   392,   267,   485,   486,   487,
     488,   523,   550,   489,   551,   593,   522,   634,   624,   654,
     667,   625,   552,   534,   576,   638,   639,   640,   665,   299,
     300,   319,   439,   320,   321,   322,   224,   225,   323,   324,
     325,   459,   312,   105,    31,    32,   313,    33
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -545
static const yytype_int16 yypact[] =
{
      73,    84,    95,  1580,  -545,  1580,    38,  -545,  -545,  -545,
    -545,    83,    87,   140,    52,    57,   104,  -545,  -545,  -545,
    -545,  -545,   170,  -545,  1167,   260,  -545,   137,  -545,  -545,
    1580,  -545,  -545,  -545,  -545,  -545,  -545,  -545,  -545,  -545,
    -545,   109,  -545,   130,  -545,   135,  2229,  2172,   147,  -545,
    -545,   170,    30,  -545,   137,   653,   280,  -545,   170,   260,
    -545,   137,  -545,  -545,   855,  -545,  -545,   289,  -545,   152,
     374,  -545,   158,  -545,  -545,  -545,  -545,  -545,  -545,  -545,
    -545,  -545,   195,   323,  -545,  -545,  -545,  2229,  -545,  -545,
    1613,  -545,  -545,  2229,   203,   211,  -545,  2286,  2343,  -545,
    2585,  1332,  -545,   181,  -545,  2229,   227,   256,  -545,   239,
    1682,  -545,   669,   384,   262,  -545,   707,   170,  -545,   250,
    -545,  1098,  1011,   137,  -545,  -545,   707,  -545,   287,  -545,
     137,   740,   324,   334,   306,   721,   855,  -545,   137,  -545,
    -545,  -545,  -545,  -545,   277,   270,  -545,   289,   137,  -545,
    -545,  -545,   253,   797,  1280,  -545,   137,  -545,  2229,  -545,
    -545,  -545,   264,   276,   282,   293,  -545,   285,  2229,  1613,
    -545,  1613,  -545,  2229,  2229,  2229,  2229,  2229,  2229,  2229,
    2229,  2229,  2229,  2229,  2229,  2229,  2229,  2229,  2229,  2229,
    -545,  -545,   323,  2229,  2229,   323,  -545,  -545,  -545,  -545,
     256,  1155,  -545,   456,   494,  -545,  -545,  -545,  -545,  -545,
    -545,   233,  -545,   313,  -545,  -545,  -545,   334,  -545,  -545,
    -545,   338,   334,   181,   371,   563,  -545,   332,   341,  -545,
    1585,  1109,  -545,   682,  -545,  -545,   375,   137,   196,   281,
    -545,   707,   707,  -545,  1011,   137,  -545,  1212,  -545,  -545,
    1011,  2229,   323,   380,   270,  -545,   331,  -545,   331,  -545,
     400,  -545,   408,  2520,  -545,  -545,  -545,  -545,   358,  2045,
    -545,  2585,   376,   385,  2585,  2585,   452,   483,  1957,  2015,
    1002,  2080,  2142,  2195,  1553,   813,   813,   180,   180,  -545,
    -545,  -545,  -545,   447,   211,   460,  -545,   191,   309,   371,
     585,  -545,   468,  -545,  1269,  -545,   494,   470,   669,  2400,
     475,  -545,  -545,  -545,   479,  -545,  -545,  -545,  1748,  -545,
     481,   357,  -545,  -545,   268,  -545,  -545,  -545,    59,  -545,
    -545,  -545,   379,  -545,   170,  -545,   770,  -545,   324,  -545,
    -545,   324,  -545,   504,   371,  -545,   480,  -545,  2585,  -545,
     137,   485,  2229,   188,   482,  -545,   482,   137,   137,   797,
      34,  -545,  -545,  -545,  -545,  -545,  2229,  2229,  -545,  -545,
     456,  -545,  -545,  -545,  -545,   371,  -545,   486,  -545,  -545,
     344,   493,  -545,  -545,   484,   284,  -545,  -545,  1338,  -545,
     550,   282,  -545,  -545,  -545,   364,   368,   121,   770,  -545,
    -545,  1804,    59,  -545,  -545,  -545,    59,  -545,   137,  2549,
    2229,  -545,   331,  -545,  -545,   505,   323,  -545,  1395,   528,
    -545,   789,  1776,  1776,  -545,  -545,  -545,  2229,  -545,  -545,
    -545,   284,   137,    31,   156,   137,  -545,   156,   137,   585,
    -545,  -545,  -545,  -545,  -545,  -545,  -545,   789,  2585,  -545,
    -545,  1804,  -545,  -545,  -545,  2549,  -545,  -545,  -545,   381,
     403,   531,   535,   522,  -545,  -545,   527,   533,  2229,   541,
     536,   540,  1986,   163,   595,  -545,   575,   554,  1868,   875,
    -545,  1224,   555,   944,  -545,  -545,   607,  1454,    60,  -545,
    -545,  -545,  2109,  -545,  -545,   323,  2229,   789,   587,  2585,
     560,   562,  -545,   406,   552,   762,  -545,  1326,  -545,  -545,
    -545,   207,  -545,  -545,  -545,   568,  -545,  -545,  -545,   323,
    -545,  2229,   591,  1927,  2229,  2229,  2472,  -545,  -545,  -545,
    -545,   588,  2229,   589,   600,  -545,  -545,  -545,  -545,  -545,
     593,  -545,   418,  -545,   170,  -545,  -545,  -545,  1513,  -545,
    -545,  -545,  1927,  -545,  2229,  -545,   625,  2428,   596,  1738,
    -545,  1033,  -545,  -545,   467,  -545,   467,   371,  -545,   599,
    -545,  -545,   598,  2229,   660,  -545,   623,   624,  2229,  -545,
    -545,   626,  -545,  2229,  -545,  -545,   388,  -545,   542,   395,
    -545,   474,  -545,  1927,  -545,   627,  1738,  2229,  1672,  -545,
    -545,  -545,   677,  -545,  -545,  -545,   631,  -545,  -545,  -545,
    2567,  -545,    48,  -545,  1011,  -545,  1011,  -545,   633,  -545,
    2490,  1738,  -545,  -545,  2229,  1927,  -545,   684,   634,  -545,
    -545,  -545,   667,  -545,  1927,   641,  -545,   655,    55,   647,
    -545,  -545,   282,   282,  1738,  -545,  -545,  2229,   684,   651,
     684,  -545,  -545,  -545,  2229,   661,    86,  -545,  -545,   663,
    -545,   181,   654,  -545,  -545,   437,  -545,  1927,   665,   181,
    -545,  -545,  -545
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -545,  -545,  -545,  -545,  -545,   167,  -545,  -545,  -545,  -545,
    -545,  -545,  -545,  -545,   -11,   -58,   -93,  -545,   -43,   424,
    -149,   321,  -545,  -545,   -64,  -545,   462,  -545,  -545,  -545,
    -202,  -545,   637,  -217,  -545,   512,  -545,    42,  -110,  -302,
       2,   674,  -545,   413,    12,   -18,   -24,   537,    -8,  -150,
    -304,   -46,  -113,   -63,  -545,  -545,  -545,   269,    21,    10,
    -545,   444,  -545,   307,  -398,  -545,  -316,  -545,  -545,  -545,
    -545,  -545,  -545,   -47,   -74,  -353,    16,   521,   -30,  -545,
    -545,  -545,  -545,  -545,   503,    32,  -545,   609,   506,   349,
     620,   519,   -60,   -99,   -13,  -111,  -142,   295,  -545,  -545,
    -181,  -545,  -545,  -545,   355,  -271,  -545,  -151,  -545,  -545,
    -545,  -545,  -454,   296,  -363,  -545,  -545,  -545,  -545,  -545,
    -545,  -545,  -359,  -545,  -544,   132,  -545,   131,  -545,   492,
    -545,  -260,  -545,  -545,  -545,   401,  -198,  -545,  -545,  -545,
     407,  -545,  -263,     5,    58,   160,  -545,   -42
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -365
static const yytype_int16 yytable[] =
{
      40,    40,    40,    94,   106,    23,    59,    23,    30,   131,
      30,   207,   134,   268,   238,    24,    60,    24,   245,   210,
     242,   310,   142,   161,    27,   160,    27,   342,   395,   166,
     165,   433,    23,   347,   139,    30,   372,    65,    55,   108,
     373,   197,    24,    54,   294,    61,   151,   163,   344,   515,
     331,    27,   239,   220,   305,   490,   144,   153,   298,   491,
     391,   553,   108,    16,    65,    56,    58,   114,   416,   574,
     216,    65,    54,    -2,   135,   154,    72,   130,   227,    54,
     635,   404,   108,    62,     4,   138,    37,    38,   297,   627,
      37,    38,   506,   507,   142,    -3,   648,   115,   594,   558,
     116,   209,   206,   417,   233,    35,   139,    36,   245,   272,
     659,   273,   424,    46,   234,   490,   628,    62,    47,   491,
     490,   554,   140,   649,   491,   266,   163,   661,   163,   217,
     151,   450,   276,   222,    54,   452,   144,   130,    54,   617,
      65,   153,   114,    37,    38,   206,   108,   130,    65,    39,
     119,   295,   566,    42,   662,   370,   305,   138,   302,   154,
     294,   108,   142,   108,   338,    48,    37,    38,   341,    16,
     214,   636,    34,    49,   139,    68,   538,   393,   340,   514,
     645,   292,   120,   121,   296,    63,   399,   260,    16,   262,
     555,    76,    77,   256,   140,   258,    71,    66,    10,    49,
     124,    73,    16,    10,   346,   363,    44,   111,   112,   353,
      49,   353,   233,   670,    10,   210,   532,   120,   121,    63,
      16,   148,   234,    50,   141,   335,   233,   156,    16,   410,
     111,    51,   233,   187,   188,   189,   234,   484,   586,    16,
     443,   144,   234,   600,   199,   601,    27,    65,   399,   125,
     111,   393,   200,   201,   217,   393,   158,   126,   222,   130,
     436,   377,   130,   130,   142,     8,   216,    10,   431,   201,
     208,   167,   215,   218,   332,   334,   139,   130,   503,   130,
     619,   168,   622,    49,   124,   435,   438,    49,   396,   298,
     143,    62,    37,    38,   142,   198,   141,   209,   541,   415,
      59,   307,   546,   308,   603,   633,   139,   205,   567,   199,
      60,   223,    16,   208,   111,   251,    16,   200,   201,   504,
     257,    27,   505,   120,   121,   511,    37,    38,   653,   111,
     219,   111,   264,   125,    49,   124,   389,   436,   390,    61,
     252,   126,   246,   247,   265,   431,   201,   127,   266,   339,
     397,   108,   270,   130,   240,    54,   142,   241,   458,   384,
     385,   269,   370,    16,   141,   353,   214,   119,   139,   370,
     303,   304,   352,   248,   309,   149,   116,   371,     7,   311,
       9,    10,    49,   124,   125,   246,   247,    49,    11,    12,
      13,    10,   126,    63,   400,   120,   121,   629,   218,   630,
     434,   437,   556,   326,    15,   432,    54,    17,   327,    27,
     209,    16,   426,   337,   427,   533,    16,   255,   170,   172,
     480,    49,   124,   492,   387,   261,   361,   388,   197,   531,
     481,   444,   125,   130,   241,   445,   209,    50,   116,    27,
     126,   150,    65,  -248,   364,    51,   394,   114,   518,   350,
      16,   519,    54,   365,   602,   613,   400,   218,   241,   664,
      27,   571,   615,    59,   569,   116,   141,   672,   215,   357,
     327,   125,   520,    60,   563,   245,   168,   358,   572,   126,
     480,   575,   577,   492,   209,   585,   209,    49,   429,   581,
     481,   651,   652,   366,   480,   588,   141,   492,   589,    27,
       9,    10,    61,   119,   481,   668,  -163,   669,    11,    12,
      13,   595,  -163,    27,   343,   368,    16,   303,   304,   111,
     217,   222,   542,   544,   367,   432,   432,   217,   506,   507,
     606,   369,    54,    43,    45,   120,   121,   430,   378,   374,
     612,  -163,   403,   382,  -163,   431,   201,   383,   209,   386,
     209,   405,   412,   315,   408,    49,   429,   425,   141,    10,
     591,   428,   416,   130,   314,    54,   315,     7,     8,     9,
      10,   119,   -32,   457,  -163,   316,   -33,    11,    12,    13,
    -163,   575,   527,   521,    16,   209,   314,   209,   524,     7,
       8,     9,    10,    15,   525,    16,   233,   316,   233,    11,
      12,    13,    10,   528,   655,   430,   234,   529,   234,  -163,
     209,   575,  -163,   431,   201,    15,   536,    16,   217,   407,
     263,   537,   411,   432,   545,   548,   413,   414,   559,   560,
     271,  -364,   561,   209,   535,   274,   275,   570,   278,   279,
     280,   281,   282,   283,   284,   285,   286,   287,   288,   289,
     290,   291,   573,  -364,   118,   580,   582,   -28,   -28,   -28,
     -28,   583,   584,   596,   218,   599,   605,   -28,   -28,   -28,
     604,   218,    37,    38,     8,     9,    10,   453,   454,   607,
      49,   429,   119,   -28,    10,  -163,     7,     8,     9,    10,
     608,  -163,   609,   611,   637,   618,    11,    12,    13,   623,
     631,   641,   508,   509,   510,   644,   512,   513,   646,    16,
      49,   124,    15,   348,   120,   121,   647,   650,   657,   -28,
    -163,   666,   249,  -163,   517,   -24,   -24,   -24,   -24,   660,
     565,   663,   671,   381,   136,   -24,   -24,   -24,    51,    16,
     196,   243,   330,   306,   -20,   -20,   -20,   -20,   398,   333,
     119,   -24,   379,  -163,   -20,   -20,   -20,   351,   516,  -163,
     125,   456,   218,   259,   356,    49,   429,   254,   126,   119,
     -20,   349,  -163,   539,   493,     8,     9,    10,  -163,   547,
     656,   658,   120,   121,    11,    12,    13,   -24,  -163,   441,
     494,  -163,   461,    38,    16,   375,     0,   442,    75,    76,
      77,     7,    78,     9,    10,     0,   -20,  -163,     0,     0,
    -163,    11,    12,    13,   409,   565,     0,     0,     0,     0,
      79,     0,    17,    51,    80,    81,    82,    15,   422,   423,
       0,     0,    83,     0,     0,    84,     0,     0,     0,     0,
      85,    86,    87,     0,     0,    88,    89,     0,     0,   495,
      90,   496,     0,    91,    92,   497,     0,     0,  -190,     0,
       8,     9,    10,   448,   185,   186,   187,   188,   189,    11,
      12,    13,   455,     0,     0,     0,   228,     0,  -115,  -115,
    -115,  -115,  -115,   499,  -115,  -115,  -115,    16,  -115,  -115,
    -115,  -115,  -115,     0,  -115,  -115,  -115,  -115,  -115,  -115,
    -115,  -115,  -115,  -115,  -115,  -115,  -115,  -115,  -115,   499,
    -115,  -115,  -115,   448,     0,     0,     0,     0,  -115,     0,
       0,  -115,     0,     0,     0,     0,  -115,  -115,  -115,     0,
     526,  -115,  -115,     0,     0,     0,  -115,     0,     0,  -115,
    -115,  -115,  -115,     0,  -115,   228,     0,   461,    38,     0,
       0,     0,     0,    75,    76,    77,     0,    78,   557,   499,
       0,   463,     0,   464,   465,   466,   467,   468,   469,   470,
     471,   472,   473,   474,     0,    79,     0,    17,     0,    80,
      81,    82,     0,     0,     0,     0,     0,    83,     0,     0,
      84,     0,     0,     0,     0,    85,    86,    87,     0,     0,
      88,    89,     0,     0,     0,    90,     0,     0,    91,    92,
     266,   475,   228,  -286,     0,     7,     8,     9,    10,     0,
       0,   499,     0,   499,     0,    11,    12,    13,     0,     0,
       0,     0,     0,     0,   494,     0,   461,    38,     0,     0,
     610,    15,    75,    76,    77,     0,    78,   179,   180,   181,
     182,   183,   184,   185,   186,   187,   188,   189,   499,   620,
     499,     0,     0,     0,    79,     0,    17,     0,    80,    81,
      82,     0,     0,     0,     0,     0,    83,  -104,     0,    84,
       0,     0,     0,   499,    85,    86,    87,     0,     0,    88,
      89,     0,     0,   495,    90,   496,     0,    91,    92,   497,
       0,    74,  -245,     0,     0,     0,   499,    75,    76,    77,
     228,    78,     0,  -107,  -107,  -107,  -107,     0,     0,     0,
       0,  -107,     0,  -107,  -107,  -107,     0,     0,     0,    79,
       0,    17,     0,    80,    81,    82,     0,     0,     0,  -107,
       0,    83,     0,     0,    84,     0,     0,     0,     0,    85,
      86,    87,     0,     0,    88,    89,     0,     0,    74,    90,
       0,     0,    91,    92,    75,    76,    77,     0,    78,   226,
       0,     7,     8,     9,    10,  -107,     0,     0,     0,     0,
       0,    11,    12,    13,     0,     0,    79,     0,    17,     0,
      80,    81,    82,     0,     0,     0,     0,    15,    83,    16,
       0,    84,     0,     0,     0,     0,    85,    86,    87,     0,
       0,    88,    89,     0,     0,    74,    90,     0,     0,    91,
      92,    75,    76,    77,     0,    78,   301,     0,     7,     8,
       9,    10,     0,     0,    57,     0,     0,     0,    11,    12,
      13,     0,     0,    79,     0,    17,     0,    80,    81,    82,
       0,     0,     0,     0,    15,    83,    16,     0,    84,     0,
       0,     0,     0,    85,    86,    87,     0,     0,    88,    89,
       0,     0,    74,    90,     0,     0,    91,    92,    75,    76,
      77,   149,    78,   345,     7,     0,     9,    10,     0,     0,
       0,   543,     0,     0,    11,    12,    13,     0,     0,     0,
      79,     0,    17,     0,    80,    81,    82,     0,     0,     0,
      15,     0,    83,    17,     0,    84,     0,     0,     0,     0,
      85,    86,    87,     0,     0,    88,    89,     0,     0,    74,
      90,     0,     0,    91,    92,    75,    76,    77,     0,    78,
     376,     0,     7,     8,     9,    10,     0,     0,     0,     0,
     440,     0,    11,    12,    13,     0,     0,    79,     0,    17,
       0,    80,    81,    82,     0,     0,     0,     0,    15,    83,
      16,     0,    84,     0,     0,     0,     0,    85,    86,    87,
       0,     0,    88,    89,     0,     0,     0,    90,   190,   191,
      91,    92,   192,   193,   194,   195,   460,   568,   461,   462,
       8,     9,    10,     0,    75,    76,    77,     0,    78,    11,
      12,    13,   463,     0,   464,   465,   466,   467,   468,   469,
     470,   471,   472,   473,   474,    15,    79,    16,    17,     0,
      80,    81,    82,     0,     0,     0,     0,     0,    83,     0,
       0,    84,     0,     0,     0,     0,    85,    86,    87,     0,
       0,    88,    89,     0,     0,   549,    90,   461,    38,    91,
      92,   266,   475,    75,    76,    77,     0,    78,     0,     0,
       0,   463,     0,   464,   465,   466,   467,   468,   469,   470,
     471,   472,   473,   474,     0,    79,     0,    17,     0,    80,
      81,    82,     0,     0,     0,     0,     0,    83,     0,     0,
      84,     0,     0,     0,     0,    85,    86,    87,     0,     0,
      88,    89,     0,     0,   592,    90,  -317,  -317,    91,    92,
     266,   475,  -317,  -317,  -317,     0,  -317,     0,     0,     0,
    -317,     0,  -317,  -317,  -317,  -317,  -317,  -317,  -317,  -317,
    -317,  -317,  -317,     0,  -317,     0,  -317,     0,  -317,  -317,
    -317,     0,     0,     0,     0,     0,  -317,     0,     0,  -317,
       0,     0,     0,     0,  -317,  -317,  -317,     0,     0,  -317,
    -317,     0,     0,     0,  -317,     0,     0,  -317,  -317,  -317,
    -317,     6,     0,  -119,     7,     8,     9,    10,     0,     7,
       8,     9,    10,     0,    11,    12,    13,   329,     0,    11,
      12,    13,   183,   184,   185,   186,   187,   188,   189,    14,
      15,     0,    16,    17,   162,    15,    74,     7,     0,     9,
      10,     0,    75,    76,    77,     0,    78,    11,    12,    13,
       0,     0,     0,  -119,     0,     0,     0,     0,     0,     0,
       0,  -119,     0,    15,    79,     0,    17,    18,    80,    81,
      82,     0,     0,     0,     0,     0,    83,     0,     0,    84,
       0,     0,     0,     0,    85,    86,    87,     0,     0,    88,
      89,     0,     0,   494,    90,   461,    38,    91,    92,   -90,
       0,    75,    76,    77,     0,    78,     7,     0,     9,    10,
       0,     0,     0,     0,     0,     0,    11,    12,    13,     0,
       0,     0,     0,    79,     0,    17,     0,    80,    81,    82,
     621,     0,    15,     0,     0,    83,     0,     0,    84,     0,
       0,     0,     0,    85,    86,    87,     0,     0,    88,    89,
       0,     0,   495,    90,   496,   199,    91,    92,   497,   494,
       0,   461,    38,   200,   201,     0,     0,    75,    76,    77,
       0,    78,     7,     8,     9,    10,     0,     0,     0,     0,
       0,     0,    11,    12,    13,     0,     0,     0,     0,    79,
       0,    17,     0,    80,    81,    82,     0,     0,    15,     0,
      16,    83,     0,     0,    84,     0,     0,     0,     0,    85,
      86,    87,     0,     0,    88,    89,     0,     0,   495,    90,
     496,     0,    91,    92,   497,   446,     0,    74,     0,     0,
       0,     0,     0,    75,    76,    77,   175,    78,   176,   177,
     178,   179,   180,   181,   182,   183,   184,   185,   186,   187,
     188,   189,     0,     0,     0,    79,     0,    17,     0,    80,
      81,    82,     0,     0,     0,     0,     0,    83,     0,     0,
      84,     0,     0,     0,     0,    85,    86,    87,     0,     0,
      88,    89,     0,     0,     0,    90,     0,     0,    91,    92,
     447,   461,   462,     8,     9,    10,     0,    75,    76,    77,
       0,    78,    11,    12,    13,   463,     0,   464,   465,   466,
     467,   468,   469,   470,   471,   472,   473,   474,    15,    79,
      16,    17,     0,    80,    81,    82,     0,     0,     0,     0,
       0,    83,     0,     0,    84,     0,     0,     0,     0,    85,
      86,    87,     0,     0,    88,    89,     0,     0,     0,    90,
     461,    38,    91,    92,   266,   475,    75,    76,    77,     0,
      78,     0,     0,     0,   463,     0,   464,   465,   466,   467,
     468,   469,   470,   471,   472,   473,   474,     0,    79,     0,
      17,     0,    80,    81,    82,     0,     0,     0,     0,     0,
      83,     0,     0,    84,     0,     0,     0,     0,    85,    86,
      87,     0,     0,    88,    89,     0,     0,     0,    90,    74,
       0,    91,    92,   266,   475,    75,    76,    77,     0,    78,
     177,   178,   179,   180,   181,   182,   183,   184,   185,   186,
     187,   188,   189,     0,     0,     0,     0,    79,     0,    17,
       0,    80,    81,    82,     0,     0,     0,     0,     0,    83,
       0,     0,    84,     0,     0,     0,     0,    85,    86,    87,
       0,     0,    88,    89,     0,     0,     0,    90,    74,     0,
      91,    92,     0,   530,    75,    76,    77,     0,    78,   178,
     179,   180,   181,   182,   183,   184,   185,   186,   187,   188,
     189,     0,     0,     0,     0,     0,    79,     0,    17,     0,
      80,    81,    82,     0,     0,     0,     0,     0,    83,     0,
       0,    84,     0,     0,     0,     0,    85,    86,    87,     0,
       0,    88,    89,     0,     0,     0,    90,     0,     0,    91,
      92,   362,    74,     7,     8,     9,    10,     0,    75,    76,
      77,     0,    78,    11,    12,    13,   180,   181,   182,   183,
     184,   185,   186,   187,   188,   189,     0,     0,     0,    15,
      79,    16,    17,     0,    80,    81,    82,     0,     0,     0,
       0,     0,    83,     0,     0,    84,     0,     0,     0,     0,
      85,    86,    87,     0,     0,    88,    89,     0,     0,     0,
      90,     0,     0,    91,    92,    74,     7,     0,     9,    10,
       0,    75,    76,    77,     0,    78,    11,    12,    13,   181,
     182,   183,   184,   185,   186,   187,   188,   189,     0,     0,
       0,     0,    15,    79,     0,    17,     0,    80,    81,    82,
       0,     0,     0,     0,     0,    83,     0,     0,    84,     0,
       0,     0,     0,    85,    86,    87,     0,     0,    88,    89,
       0,     0,    74,    90,     0,     0,    91,    92,    75,    76,
      77,     0,    78,   182,   183,   184,   185,   186,   187,   188,
     189,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      79,     0,    17,     0,    80,    81,    82,     0,     0,     0,
       0,     0,    83,     0,     0,    84,     0,     0,     0,     0,
      85,    86,    87,     0,     0,    88,    89,     0,     0,    74,
      90,     0,     0,    91,    92,    75,    76,    77,     0,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    79,     0,    17,
       0,    80,    81,    82,     0,     0,     0,     0,     0,    83,
       0,     0,    84,     0,     0,     0,     0,    85,    86,    87,
       0,     0,    88,    89,     0,     0,    74,   169,     0,     0,
      91,    92,    75,    76,    77,     0,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    79,     0,    17,     0,    80,    81,
      82,     0,     0,     0,     0,     0,    83,     0,     0,    84,
       0,     0,     0,     0,    85,    86,    87,     0,     0,    88,
      89,     0,     0,   380,   171,     0,     0,    91,    92,    75,
      76,    77,     0,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    79,     0,    17,     0,    80,    81,    82,     0,     0,
     597,     0,     0,    83,     0,     0,    84,     0,     0,     0,
       0,    85,    86,    87,     0,     0,    88,    89,     0,     0,
       0,    90,     0,     0,    91,    92,   173,   174,   175,     0,
     176,   177,   178,   179,   180,   181,   182,   183,   184,   185,
     186,   187,   188,   189,   578,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   598,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     173,   174,   175,   579,   176,   177,   178,   179,   180,   181,
     182,   183,   184,   185,   186,   187,   188,   189,   173,   174,
     175,     0,   176,   177,   178,   179,   180,   181,   182,   183,
     184,   185,   186,   187,   188,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   173,   174,
     175,   632,   176,   177,   178,   179,   180,   181,   182,   183,
     184,   185,   186,   187,   188,   189,     0,     0,     0,     0,
       0,    16,     0,     0,     0,     0,     0,   173,   174,   175,
     359,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   187,   188,   189,   173,   174,   175,   626,   176,
     177,   178,   179,   180,   181,   182,   183,   184,   185,   186,
     187,   188,   189,   173,   174,   175,     0,   176,   177,   178,
     179,   180,   181,   182,   183,   184,   185,   186,   187,   188,
     189
};

static const yytype_int16 yycheck[] =
{
      11,    12,    13,    46,    47,     3,    24,     5,     3,    56,
       5,   110,    58,   164,   125,     3,    24,     5,   131,   112,
     130,   223,    64,    87,     3,    83,     5,   244,   332,    93,
      90,   384,    30,   250,    64,    30,   299,    27,    22,    47,
     300,   105,    30,    22,   193,    24,    70,    90,   246,   447,
     231,    30,   126,   116,   204,   418,    67,    70,   200,   418,
       1,     1,    70,    32,    54,    23,    24,    51,    34,   523,
     112,    61,    51,     0,    58,    70,    44,    56,   121,    58,
     624,   344,    90,    25,     0,    64,     3,     4,   199,    41,
       3,     4,    61,    62,   136,     0,    41,    67,   552,   497,
      70,   112,   110,    69,   122,    67,   136,    69,   221,   169,
     654,   171,   375,    61,   122,   478,    68,    59,    61,   478,
     483,    61,    64,    68,   483,    66,   169,    41,   171,   113,
     154,   402,   175,   117,   113,   406,   147,   116,   117,   593,
     130,   154,   126,     3,     4,   153,   154,   126,   138,    66,
      29,   194,   505,    66,    68,   297,   306,   136,   201,   154,
     309,   169,   204,   171,   238,    61,     3,     4,   242,    32,
     112,   625,     5,     3,   204,    66,   478,   328,   241,   439,
     634,   192,    61,    62,   195,    25,   336,   155,    32,   157,
     492,    10,    11,   151,   136,   153,    66,    30,     7,     3,
       4,    66,    32,     7,   247,   269,    66,    47,    61,   256,
       3,   258,   230,   667,     7,   308,    53,    61,    62,    59,
      32,    69,   230,    53,    64,   233,   244,    69,    32,    41,
      70,    61,   250,    53,    54,    55,   244,   418,   542,    32,
     391,   252,   250,   559,    53,   561,   225,   237,   398,    53,
      90,   402,    61,    62,   238,   406,    61,    61,   242,   238,
      53,   304,   241,   242,   306,     5,   308,     7,    61,    62,
     110,    68,   112,   113,   232,   233,   306,   256,   427,   258,
     596,    70,   598,     3,     4,   384,   385,     3,   334,   431,
       1,   233,     3,     4,   336,    68,   136,   308,   479,   359,
     318,    68,   483,    70,   567,   621,   336,    68,   506,    53,
     318,    61,    32,   153,   154,    38,    32,    61,    62,   430,
      67,   300,   432,    61,    62,   436,     3,     4,   644,   169,
      68,   171,    68,    53,     3,     4,    68,    53,    70,   318,
      70,    61,    61,    62,    68,    61,    62,    67,    66,    68,
     334,   359,    67,   332,    67,   334,   398,    70,   416,   317,
     318,    68,   504,    32,   204,   412,   308,    29,   398,   511,
      61,    62,    41,    67,    61,     1,    70,    68,     4,     8,
       6,     7,     3,     4,    53,    61,    62,     3,    14,    15,
      16,     7,    61,   233,   336,    61,    62,   614,   238,   616,
     384,   385,   495,    71,    30,   384,   385,    33,    67,   388,
     421,    32,    68,    38,    70,   473,    32,   148,    97,    98,
     418,     3,     4,   418,    67,   156,    68,    70,   492,   472,
     418,    67,    53,   412,    70,    67,   447,    53,    70,   418,
      61,    67,   432,    69,    68,    61,    67,   431,    67,    69,
      32,    70,   431,    68,   565,    67,   398,   297,    70,   661,
     439,   519,    67,   481,   507,    70,   306,   669,   308,    69,
      67,    53,    69,   481,    68,   588,    70,    69,   521,    61,
     478,   524,   525,   478,   495,    67,   497,     3,     4,   532,
     478,   642,   643,    41,   492,   542,   336,   492,   544,   478,
       6,     7,   481,    29,   492,    68,    32,    70,    14,    15,
      16,   554,    38,   492,   245,    68,    32,    61,    62,   359,
     504,   505,   480,   481,    41,   504,   505,   511,    61,    62,
     573,    71,   511,    12,    13,    61,    62,    53,    68,    71,
     583,    67,    38,    68,    70,    61,    62,    68,   559,    68,
     561,    71,    70,     3,    69,     3,     4,    71,   398,     7,
     544,    68,    34,   542,     1,   544,     3,     4,     5,     6,
       7,    29,    41,    68,    32,    12,    41,    14,    15,    16,
      38,   624,    41,    61,    32,   596,     1,   598,    61,     4,
       5,     6,     7,    30,    61,    32,   614,    12,   616,    14,
      15,    16,     7,    67,   647,    53,   614,    67,   616,    67,
     621,   654,    70,    61,    62,    30,    41,    32,   602,   350,
     158,    67,   353,   602,    69,    18,   357,   358,    41,    69,
     168,    68,    70,   644,   474,   173,   174,    69,   176,   177,
     178,   179,   180,   181,   182,   183,   184,   185,   186,   187,
     188,   189,    61,    68,     1,    67,    67,     4,     5,     6,
       7,    61,    69,    38,   504,    69,    68,    14,    15,    16,
      71,   511,     3,     4,     5,     6,     7,   408,   409,    19,
       3,     4,    29,    30,     7,    32,     4,     5,     6,     7,
      67,    38,    68,    67,    10,    68,    14,    15,    16,    68,
      67,    67,   433,   434,   435,    38,   437,   438,    67,    32,
       3,     4,    30,   251,    61,    62,    61,    70,    67,    66,
      67,    67,     1,    70,   455,     4,     5,     6,     7,    68,
      53,    68,    67,   309,    60,    14,    15,    16,    61,    32,
     103,     1,   230,   206,     4,     5,     6,     7,   335,    67,
      29,    30,   308,    32,    14,    15,    16,   254,   451,    38,
      53,   412,   602,   154,   258,     3,     4,   147,    61,    29,
      30,   252,    32,   478,   419,     5,     6,     7,    38,   483,
     648,   650,    61,    62,    14,    15,    16,    66,    67,   388,
       1,    70,     3,     4,    32,   303,    -1,   390,     9,    10,
      11,     4,    13,     6,     7,    -1,    66,    67,    -1,    -1,
      70,    14,    15,    16,   352,    53,    -1,    -1,    -1,    -1,
      31,    -1,    33,    61,    35,    36,    37,    30,   366,   367,
      -1,    -1,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      51,    52,    53,    -1,    -1,    56,    57,    -1,    -1,    60,
      61,    62,    -1,    64,    65,    66,    -1,    -1,    69,    -1,
       5,     6,     7,   401,    51,    52,    53,    54,    55,    14,
      15,    16,   410,    -1,    -1,    -1,     1,    -1,     3,     4,
       5,     6,     7,   421,     9,    10,    11,    32,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,   447,
      35,    36,    37,   451,    -1,    -1,    -1,    -1,    43,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    -1,
     468,    56,    57,    -1,    -1,    -1,    61,    -1,    -1,    64,
      65,    66,    67,    -1,    69,     1,    -1,     3,     4,    -1,
      -1,    -1,    -1,     9,    10,    11,    -1,    13,   496,   497,
      -1,    17,    -1,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    -1,    31,    -1,    33,    -1,    35,
      36,    37,    -1,    -1,    -1,    -1,    -1,    43,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    51,    52,    53,    -1,    -1,
      56,    57,    -1,    -1,    -1,    61,    -1,    -1,    64,    65,
      66,    67,     1,    69,    -1,     4,     5,     6,     7,    -1,
      -1,   559,    -1,   561,    -1,    14,    15,    16,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,     3,     4,    -1,    -1,
     578,    30,     9,    10,    11,    -1,    13,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,   596,   597,
     598,    -1,    -1,    -1,    31,    -1,    33,    -1,    35,    36,
      37,    -1,    -1,    -1,    -1,    -1,    43,    66,    -1,    46,
      -1,    -1,    -1,   621,    51,    52,    53,    -1,    -1,    56,
      57,    -1,    -1,    60,    61,    62,    -1,    64,    65,    66,
      -1,     3,    69,    -1,    -1,    -1,   644,     9,    10,    11,
       1,    13,    -1,     4,     5,     6,     7,    -1,    -1,    -1,
      -1,    12,    -1,    14,    15,    16,    -1,    -1,    -1,    31,
      -1,    33,    -1,    35,    36,    37,    -1,    -1,    -1,    30,
      -1,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,
      52,    53,    -1,    -1,    56,    57,    -1,    -1,     3,    61,
      -1,    -1,    64,    65,     9,    10,    11,    -1,    13,    71,
      -1,     4,     5,     6,     7,    66,    -1,    -1,    -1,    -1,
      -1,    14,    15,    16,    -1,    -1,    31,    -1,    33,    -1,
      35,    36,    37,    -1,    -1,    -1,    -1,    30,    43,    32,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    -1,
      -1,    56,    57,    -1,    -1,     3,    61,    -1,    -1,    64,
      65,     9,    10,    11,    -1,    13,    71,    -1,     4,     5,
       6,     7,    -1,    -1,    67,    -1,    -1,    -1,    14,    15,
      16,    -1,    -1,    31,    -1,    33,    -1,    35,    36,    37,
      -1,    -1,    -1,    -1,    30,    43,    32,    -1,    46,    -1,
      -1,    -1,    -1,    51,    52,    53,    -1,    -1,    56,    57,
      -1,    -1,     3,    61,    -1,    -1,    64,    65,     9,    10,
      11,     1,    13,    71,     4,    -1,     6,     7,    -1,    -1,
      -1,    67,    -1,    -1,    14,    15,    16,    -1,    -1,    -1,
      31,    -1,    33,    -1,    35,    36,    37,    -1,    -1,    -1,
      30,    -1,    43,    33,    -1,    46,    -1,    -1,    -1,    -1,
      51,    52,    53,    -1,    -1,    56,    57,    -1,    -1,     3,
      61,    -1,    -1,    64,    65,     9,    10,    11,    -1,    13,
      71,    -1,     4,     5,     6,     7,    -1,    -1,    -1,    -1,
      12,    -1,    14,    15,    16,    -1,    -1,    31,    -1,    33,
      -1,    35,    36,    37,    -1,    -1,    -1,    -1,    30,    43,
      32,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,
      -1,    -1,    56,    57,    -1,    -1,    -1,    61,    56,    57,
      64,    65,    60,    61,    62,    63,     1,    71,     3,     4,
       5,     6,     7,    -1,     9,    10,    11,    -1,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    -1,
      35,    36,    37,    -1,    -1,    -1,    -1,    -1,    43,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    -1,
      -1,    56,    57,    -1,    -1,     1,    61,     3,     4,    64,
      65,    66,    67,     9,    10,    11,    -1,    13,    -1,    -1,
      -1,    17,    -1,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    -1,    31,    -1,    33,    -1,    35,
      36,    37,    -1,    -1,    -1,    -1,    -1,    43,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    51,    52,    53,    -1,    -1,
      56,    57,    -1,    -1,     1,    61,     3,     4,    64,    65,
      66,    67,     9,    10,    11,    -1,    13,    -1,    -1,    -1,
      17,    -1,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    -1,    31,    -1,    33,    -1,    35,    36,
      37,    -1,    -1,    -1,    -1,    -1,    43,    -1,    -1,    46,
      -1,    -1,    -1,    -1,    51,    52,    53,    -1,    -1,    56,
      57,    -1,    -1,    -1,    61,    -1,    -1,    64,    65,    66,
      67,     1,    -1,     3,     4,     5,     6,     7,    -1,     4,
       5,     6,     7,    -1,    14,    15,    16,    12,    -1,    14,
      15,    16,    49,    50,    51,    52,    53,    54,    55,    29,
      30,    -1,    32,    33,     1,    30,     3,     4,    -1,     6,
       7,    -1,     9,    10,    11,    -1,    13,    14,    15,    16,
      -1,    -1,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    61,    -1,    30,    31,    -1,    33,    67,    35,    36,
      37,    -1,    -1,    -1,    -1,    -1,    43,    -1,    -1,    46,
      -1,    -1,    -1,    -1,    51,    52,    53,    -1,    -1,    56,
      57,    -1,    -1,     1,    61,     3,     4,    64,    65,    66,
      -1,     9,    10,    11,    -1,    13,     4,    -1,     6,     7,
      -1,    -1,    -1,    -1,    -1,    -1,    14,    15,    16,    -1,
      -1,    -1,    -1,    31,    -1,    33,    -1,    35,    36,    37,
      38,    -1,    30,    -1,    -1,    43,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    51,    52,    53,    -1,    -1,    56,    57,
      -1,    -1,    60,    61,    62,    53,    64,    65,    66,     1,
      -1,     3,     4,    61,    62,    -1,    -1,     9,    10,    11,
      -1,    13,     4,     5,     6,     7,    -1,    -1,    -1,    -1,
      -1,    -1,    14,    15,    16,    -1,    -1,    -1,    -1,    31,
      -1,    33,    -1,    35,    36,    37,    -1,    -1,    30,    -1,
      32,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,
      52,    53,    -1,    -1,    56,    57,    -1,    -1,    60,    61,
      62,    -1,    64,    65,    66,     1,    -1,     3,    -1,    -1,
      -1,    -1,    -1,     9,    10,    11,    40,    13,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    -1,    -1,    -1,    31,    -1,    33,    -1,    35,
      36,    37,    -1,    -1,    -1,    -1,    -1,    43,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    51,    52,    53,    -1,    -1,
      56,    57,    -1,    -1,    -1,    61,    -1,    -1,    64,    65,
      66,     3,     4,     5,     6,     7,    -1,     9,    10,    11,
      -1,    13,    14,    15,    16,    17,    -1,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    -1,    35,    36,    37,    -1,    -1,    -1,    -1,
      -1,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,
      52,    53,    -1,    -1,    56,    57,    -1,    -1,    -1,    61,
       3,     4,    64,    65,    66,    67,     9,    10,    11,    -1,
      13,    -1,    -1,    -1,    17,    -1,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    -1,    31,    -1,
      33,    -1,    35,    36,    37,    -1,    -1,    -1,    -1,    -1,
      43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,
      53,    -1,    -1,    56,    57,    -1,    -1,    -1,    61,     3,
      -1,    64,    65,    66,    67,     9,    10,    11,    -1,    13,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    -1,    -1,    -1,    -1,    31,    -1,    33,
      -1,    35,    36,    37,    -1,    -1,    -1,    -1,    -1,    43,
      -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,
      -1,    -1,    56,    57,    -1,    -1,    -1,    61,     3,    -1,
      64,    65,    -1,    67,     9,    10,    11,    -1,    13,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    -1,    -1,    -1,    -1,    -1,    31,    -1,    33,    -1,
      35,    36,    37,    -1,    -1,    -1,    -1,    -1,    43,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    -1,
      -1,    56,    57,    -1,    -1,    -1,    61,    -1,    -1,    64,
      65,    66,     3,     4,     5,     6,     7,    -1,     9,    10,
      11,    -1,    13,    14,    15,    16,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    -1,    -1,    -1,    30,
      31,    32,    33,    -1,    35,    36,    37,    -1,    -1,    -1,
      -1,    -1,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      51,    52,    53,    -1,    -1,    56,    57,    -1,    -1,    -1,
      61,    -1,    -1,    64,    65,     3,     4,    -1,     6,     7,
      -1,     9,    10,    11,    -1,    13,    14,    15,    16,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    -1,    -1,
      -1,    -1,    30,    31,    -1,    33,    -1,    35,    36,    37,
      -1,    -1,    -1,    -1,    -1,    43,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    51,    52,    53,    -1,    -1,    56,    57,
      -1,    -1,     3,    61,    -1,    -1,    64,    65,     9,    10,
      11,    -1,    13,    48,    49,    50,    51,    52,    53,    54,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      31,    -1,    33,    -1,    35,    36,    37,    -1,    -1,    -1,
      -1,    -1,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      51,    52,    53,    -1,    -1,    56,    57,    -1,    -1,     3,
      61,    -1,    -1,    64,    65,     9,    10,    11,    -1,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    31,    -1,    33,
      -1,    35,    36,    37,    -1,    -1,    -1,    -1,    -1,    43,
      -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,
      -1,    -1,    56,    57,    -1,    -1,     3,    61,    -1,    -1,
      64,    65,     9,    10,    11,    -1,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    31,    -1,    33,    -1,    35,    36,
      37,    -1,    -1,    -1,    -1,    -1,    43,    -1,    -1,    46,
      -1,    -1,    -1,    -1,    51,    52,    53,    -1,    -1,    56,
      57,    -1,    -1,     3,    61,    -1,    -1,    64,    65,     9,
      10,    11,    -1,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    31,    -1,    33,    -1,    35,    36,    37,    -1,    -1,
      12,    -1,    -1,    43,    -1,    -1,    46,    -1,    -1,    -1,
      -1,    51,    52,    53,    -1,    -1,    56,    57,    -1,    -1,
      -1,    61,    -1,    -1,    64,    65,    38,    39,    40,    -1,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    38,    39,
      40,    -1,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    38,    39,
      40,    71,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    -1,    -1,    -1,    -1,
      -1,    32,    -1,    -1,    -1,    -1,    -1,    38,    39,    40,
      70,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    38,    39,    40,    -1,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    73,    74,    75,     0,    76,     1,     4,     5,     6,
       7,    14,    15,    16,    29,    30,    32,    33,    67,    77,
      78,    79,   109,   112,   116,   117,   120,   130,   131,   150,
     215,   216,   217,   219,    77,    67,    69,     3,     4,    66,
      86,   149,    66,   149,    66,   149,    61,    61,    61,     3,
      53,    61,   123,   127,   130,   148,   109,    67,   109,   117,
     120,   130,   216,   217,   113,   131,    77,   154,    66,   157,
     158,    66,   157,    66,     3,     9,    10,    11,    13,    31,
      35,    36,    37,    43,    46,    51,    52,    53,    56,    57,
      61,    64,    65,    89,    90,    92,    93,    94,    95,    96,
      98,   100,   102,   103,   104,   215,    90,   118,   120,   164,
     166,   217,    61,   167,   148,    67,    70,   110,     1,    29,
      61,    62,    84,   124,     4,    53,    61,    67,   122,   125,
     130,   145,   146,   148,   123,   148,   113,   121,   130,   150,
     216,   217,   219,     1,    86,   162,   163,   153,    69,     1,
      67,   118,   159,   166,   215,   151,    69,   152,    61,    86,
      87,    96,     1,    90,   101,   164,    96,    68,    70,    61,
      93,    61,    93,    38,    39,    40,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    60,    61,    62,    63,   104,    96,    68,    53,
      61,    62,   165,   168,   119,    68,   120,   165,   217,    86,
      88,   132,   133,   134,   216,   217,   219,   148,   217,    68,
     125,   145,   148,    61,   208,   209,    71,    90,     1,   105,
     106,   107,   114,   117,   120,   172,   129,   130,   167,   146,
      67,    70,   110,     1,    80,   124,    61,    62,    67,     1,
      82,    38,    70,   156,   162,   129,   109,    67,   109,   159,
     157,   129,   157,    98,    68,    68,    66,   178,   179,    68,
      67,    98,   164,   164,    98,    98,    90,    99,    98,    98,
      98,    98,    98,    98,    98,    98,    98,    98,    98,    98,
      98,    98,    86,    91,    92,    90,    86,   167,   168,   201,
     202,    71,    90,    61,    62,   121,   119,    68,    70,    61,
     102,     8,   214,   218,     1,     3,    12,   112,   116,   203,
     205,   206,   207,   210,   211,   212,    71,    67,    85,    12,
     107,   172,   109,    67,   109,   120,   115,    38,   146,    68,
     125,   146,   105,   129,   208,    71,    90,   105,    98,   163,
      69,   156,    41,   145,   160,   161,   160,    69,    69,    70,
     173,    68,    66,    96,    68,    68,    41,    41,    68,    71,
     168,    68,   214,   203,    71,   201,    71,    90,    68,   133,
       3,    91,    68,    68,   109,   109,    68,    67,    70,    68,
      70,     1,   177,   179,    67,   122,   123,   148,   115,   121,
     216,   128,    81,    38,   214,    71,    83,   129,    69,    98,
      41,   129,    70,   129,   129,   164,    34,    69,   174,   175,
     176,    97,    98,    98,   214,    71,    68,    70,    68,     4,
      53,    61,   130,   147,   148,   165,    53,   148,   165,   204,
      12,   207,   212,   179,    67,    67,     1,    66,    98,   135,
     177,   126,   177,   129,   129,    98,   161,    68,    87,   213,
       1,     3,     4,    17,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    67,    87,    90,   108,   111,
     112,   116,   169,   170,   172,   179,   180,   181,   182,   185,
     186,   194,   215,   176,     1,    60,    62,    66,    88,    98,
     136,   137,   138,    92,   167,   110,    61,    62,   129,   129,
     129,   167,   129,   129,   203,   136,   135,   129,    67,    70,
      69,    61,   188,   183,    61,    61,    98,    41,    67,    67,
      67,    90,    53,    87,   195,   217,    41,    67,   111,   169,
     171,   172,   109,    67,   109,    69,   172,   185,    18,     1,
     184,   186,   194,     1,    61,   111,    88,    98,   136,    41,
      69,    70,   155,    68,   147,    53,   147,   208,    71,    90,
      69,    87,    90,    61,   184,    90,   196,    90,    12,    41,
      67,    90,    67,    61,    69,    67,   122,   139,   145,   123,
     142,   148,     1,   187,   184,    90,    38,    12,    71,    69,
     138,   138,   167,   214,    71,    68,    90,    19,    67,    68,
      98,    67,    90,    67,   140,    67,   143,   184,    68,   138,
      98,    38,   138,    68,   190,   193,    41,    41,    68,   105,
     105,    67,    71,   138,   189,   196,   184,    10,   197,   198,
     199,    67,   141,   144,    38,   184,    67,    61,    41,    68,
      70,   179,   179,   138,   191,    90,   197,    67,   199,   196,
      68,    41,    68,    68,   102,   200,    67,   192,    68,    70,
     184,    67,   102
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{


    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

/* Line 1455 of yacc.c  */
#line 279 "c-parse.y"
    { if (pedantic)
		    pedwarn("ANSI C forbids an empty source file");
		  the_program = NULL;
		;}
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 284 "c-parse.y"
    {
		  the_program = (yyvsp[(1) - (1)].u.decl);
		;}
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 294 "c-parse.y"
    { (yyval.u.telement) = NULL; ;}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 294 "c-parse.y"
    { (yyval.u.decl) = (yyvsp[(2) - (2)].u.decl); ;}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 295 "c-parse.y"
    { (yyval.u.telement) = NULL; ;}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 296 "c-parse.y"
    { (yyval.u.decl) = declaration_chain((yyvsp[(1) - (3)].u.decl), (yyvsp[(3) - (3)].u.decl)); ;}
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 303 "c-parse.y"
    { 
		  (yyval.u.decl) = CAST(declaration, new_asm_decl
		    (pr, (yyvsp[(1) - (5)].u.itoken).location,
		     new_asm_stmt(pr, (yyvsp[(1) - (5)].u.itoken).location, (yyvsp[(3) - (5)].u.expr), NULL, NULL, NULL, NULL))); ;}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 308 "c-parse.y"
    { pedantic = (yyvsp[(1) - (2)].u.itoken).i; 
		  (yyval.u.decl) = CAST(declaration, new_extension_decl(pr, (yyvsp[(1) - (2)].u.itoken).location, (yyvsp[(2) - (2)].u.decl))); ;}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 314 "c-parse.y"
    { if (pedantic)
		    error("ANSI C forbids data definition with no type or storage class");
		  else if (!flag_traditional)
		    warning("data definition has no type or storage class"); 

		  (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[(2) - (3)].u.decl)->location, NULL, NULL, (yyvsp[(2) - (3)].u.decl)));
		  pop_declspec_stack(); ;}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 322 "c-parse.y"
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[(1) - (4)].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[(3) - (4)].u.decl)));
		  pop_declspec_stack(); ;}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 325 "c-parse.y"
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[(1) - (4)].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[(3) - (4)].u.decl)));
		  pop_declspec_stack(); ;}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 328 "c-parse.y"
    { pedwarn("empty declaration"); ;}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 330 "c-parse.y"
    { shadow_tag((yyvsp[(1) - (3)].u.telement)); 
	    (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[(1) - (3)].u.telement)->location, current_declspecs, prefix_attributes, NULL));
	    pop_declspec_stack(); ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 333 "c-parse.y"
    { (yyval.u.decl) = new_error_decl(pr, last_location); ;}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 334 "c-parse.y"
    { (yyval.u.decl) = new_error_decl(pr, last_location); ;}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 336 "c-parse.y"
    { if (pedantic)
		    pedwarn("ANSI C does not allow extra `;' outside of a function");
		  (yyval.u.decl) = NULL; ;}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 343 "c-parse.y"
    { if (!start_function(current_declspecs, (yyvsp[(3) - (3)].u.declarator),
				      prefix_attributes, 0))
		    YYERROR1; ;}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 347 "c-parse.y"
    { store_parm_decls((yyvsp[(5) - (5)].u.decl)); ;}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 349 "c-parse.y"
    { (yyval.u.decl) = finish_function((yyvsp[(7) - (7)].u.stmt));
		  pop_declspec_stack(); ;}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 352 "c-parse.y"
    { (yyval.u.decl) = new_error_decl(pr, last_location);
		  pop_declspec_stack(); ;}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 355 "c-parse.y"
    { if (!start_function(current_declspecs, (yyvsp[(3) - (3)].u.declarator),
				      prefix_attributes, 0))
		    YYERROR1; ;}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 359 "c-parse.y"
    { store_parm_decls((yyvsp[(5) - (5)].u.decl)); ;}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 361 "c-parse.y"
    { (yyval.u.decl) = finish_function((yyvsp[(7) - (7)].u.stmt)); 
		  pop_declspec_stack(); ;}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 364 "c-parse.y"
    { (yyval.u.decl) = new_error_decl(pr, last_location);
		  pop_declspec_stack(); ;}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 367 "c-parse.y"
    { if (!start_function(NULL, (yyvsp[(2) - (2)].u.declarator),
				      prefix_attributes, 0))
		    YYERROR1; ;}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 371 "c-parse.y"
    { store_parm_decls((yyvsp[(4) - (4)].u.decl)); ;}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 373 "c-parse.y"
    { (yyval.u.decl) = finish_function((yyvsp[(6) - (6)].u.stmt)); 
		  pop_declspec_stack(); ;}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 376 "c-parse.y"
    { (yyval.u.decl) = new_error_decl(pr, last_location);
		  pop_declspec_stack(); ;}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 386 "c-parse.y"
    { (yyval.u.id_label) = new_id_label(pr, (yyvsp[(1) - (1)].idtoken).location, (yyvsp[(1) - (1)].idtoken).id); ;}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 390 "c-parse.y"
    { (yyval.u.word) = new_word(pr, (yyvsp[(1) - (1)].idtoken).location, (yyvsp[(1) - (1)].idtoken).id); ;}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 393 "c-parse.y"
    { (yyval.u.itoken) = (yyvsp[(1) - (1)].u.itoken); (yyval.u.itoken).i = kind_address_of; ;}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 395 "c-parse.y"
    { (yyval.u.itoken) = (yyvsp[(1) - (1)].u.itoken); (yyval.u.itoken).i = kind_unary_minus; ;}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 397 "c-parse.y"
    { (yyval.u.itoken) = (yyvsp[(1) - (1)].u.itoken); (yyval.u.itoken).i = kind_unary_plus; ;}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 399 "c-parse.y"
    { (yyval.u.itoken) = (yyvsp[(1) - (1)].u.itoken); (yyval.u.itoken).i = kind_preincrement; ;}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 401 "c-parse.y"
    { (yyval.u.itoken) = (yyvsp[(1) - (1)].u.itoken); (yyval.u.itoken).i = kind_predecrement; ;}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 403 "c-parse.y"
    { (yyval.u.itoken) = (yyvsp[(1) - (1)].u.itoken); (yyval.u.itoken).i = kind_bitnot; ;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 405 "c-parse.y"
    { (yyval.u.itoken) = (yyvsp[(1) - (1)].u.itoken); (yyval.u.itoken).i = kind_not; ;}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 407 "c-parse.y"
    { (yyval.u.itoken) = (yyvsp[(1) - (1)].u.itoken); (yyval.u.itoken).i = kind_realpart; ;}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 409 "c-parse.y"
    { (yyval.u.itoken) = (yyvsp[(1) - (1)].u.itoken); (yyval.u.itoken).i = kind_imagpart; ;}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 413 "c-parse.y"
    { if ((yyvsp[(1) - (1)].u.expr)->next)
		    (yyval.u.expr) = make_comma((yyvsp[(1) - (1)].u.expr)->location, (yyvsp[(1) - (1)].u.expr));
		  else
		    (yyval.u.expr) = (yyvsp[(1) - (1)].u.expr); ;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 421 "c-parse.y"
    { (yyval.u.expr) = NULL; ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 427 "c-parse.y"
    { (yyval.u.expr) = (yyvsp[(1) - (1)].u.expr); ;}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 429 "c-parse.y"
    { (yyval.u.expr) = expression_chain((yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 435 "c-parse.y"
    { (yyval.u.expr) = make_dereference((yyvsp[(1) - (2)].u.itoken).location, (yyvsp[(2) - (2)].u.expr)); ;}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 438 "c-parse.y"
    { (yyval.u.expr) = make_extension_expr((yyvsp[(1) - (2)].u.itoken).location, (yyvsp[(2) - (2)].u.expr));
		  pedantic = (yyvsp[(1) - (2)].u.itoken).i; ;}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 441 "c-parse.y"
    { (yyval.u.expr) = make_unary((yyvsp[(1) - (2)].u.itoken).location, (yyvsp[(1) - (2)].u.itoken).i, (yyvsp[(2) - (2)].u.expr));
#if 0
		  overflow_warning((yyval.u.expr)); 
#endif
		;}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 448 "c-parse.y"
    {
		  (yyval.u.expr) = CAST(expression, make_label_address((yyvsp[(1) - (2)].u.itoken).location, (yyvsp[(2) - (2)].u.id_label)));
		  use_label((yyvsp[(2) - (2)].u.id_label));
		;}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 453 "c-parse.y"
    { 
#if 0
		  if (TREE_CODE ((yyvsp[(2) - (2)].u.expr)) == COMPONENT_REF
		      && DECL_C_BIT_FIELD (TREE_OPERAND ((yyvsp[(2) - (2)].u.expr), 1)))
		    error("`sizeof' applied to a bit-field");
		  (yyval.u.expr) = c_sizeof (TREE_TYPE ((yyvsp[(2) - (2)].u.expr))); 
#endif
		  (yyval.u.expr) = make_sizeof_expr((yyvsp[(1) - (2)].u.itoken).location, (yyvsp[(2) - (2)].u.expr));
		  unevaluated_expression--; ;}
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 463 "c-parse.y"
    { (yyval.u.expr) = make_sizeof_type((yyvsp[(1) - (4)].u.itoken).location, (yyvsp[(3) - (4)].u.type));
		  unevaluated_expression--; ;}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 466 "c-parse.y"
    { (yyval.u.expr) = make_alignof_expr((yyvsp[(1) - (2)].u.itoken).location, (yyvsp[(2) - (2)].u.expr));
		  unevaluated_expression--; ;}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 469 "c-parse.y"
    { (yyval.u.expr) = make_alignof_type((yyvsp[(1) - (4)].u.itoken).location, (yyvsp[(3) - (4)].u.type)); 
		  unevaluated_expression--; ;}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 474 "c-parse.y"
    { unevaluated_expression++; (yyval.u.itoken) = (yyvsp[(1) - (1)].u.itoken); ;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 478 "c-parse.y"
    { unevaluated_expression++; (yyval.u.itoken) = (yyvsp[(1) - (1)].u.itoken); ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 484 "c-parse.y"
    { (yyval.u.expr) = make_cast((yyvsp[(1) - (4)].u.itoken).location, (yyvsp[(2) - (4)].u.type), (yyvsp[(4) - (4)].u.expr)); ;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 486 "c-parse.y"
    { 
#if 0
		  start_init (NULL, NULL, 0);
		  (yyvsp[(2) - (4)].u.type) = groktypename ((yyvsp[(2) - (4)].u.type));
		  really_start_incremental_init ((yyvsp[(2) - (4)].u.type)); 
#endif
		;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 494 "c-parse.y"
    { 
		  (yyval.u.expr) = CAST(expression, new_cast_list(pr, (yyvsp[(1) - (7)].u.itoken).location, (yyvsp[(2) - (7)].u.type), CAST(expression, new_init_list(pr, (yyvsp[(6) - (7)].u.expr)->location, (yyvsp[(6) - (7)].u.expr)))));
		  (yyval.u.expr)->type = (yyvsp[(2) - (7)].u.type)->type;
		  /* XXX: Evil hack for foo((int[5]) {1, 2, 3}) */
		  /* XXX: what does gcc do ? */
		  if (type_array((yyval.u.expr)->type))
		    (yyval.u.expr)->lvalue = TRUE;

		  if (pedantic)
		    pedwarn("ANSI C forbids constructor expressions");
#if 0
		  char *name;
		  tree result = pop_init_level (0);
		  tree type = (yyvsp[(2) - (7)].u.type);
		  finish_init ();

		  if (TYPE_NAME (type) != 0)
		    {
		      if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
			name = IDENTIFIER_POINTER (TYPE_NAME (type));
		      else
			name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
		    }
		  else
		    name = "";
		  (yyval.u.expr) = result;
		  if (TREE_CODE (type) == ARRAY_TYPE && TYPE_SIZE (type) == 0)
		    {
		      int failure = complete_array_type (type, (yyval.u.expr), 1);
		      if (failure)
			abort ();
		    }
#endif
		;}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 533 "c-parse.y"
    { (yyval.u.expr) = make_binary((yyvsp[(2) - (3)].u.itoken).location, kind_plus, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 535 "c-parse.y"
    { (yyval.u.expr) = make_binary((yyvsp[(2) - (3)].u.itoken).location, kind_minus, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 537 "c-parse.y"
    { (yyval.u.expr) = make_binary((yyvsp[(2) - (3)].u.itoken).location, kind_times, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 539 "c-parse.y"
    { (yyval.u.expr) = make_binary((yyvsp[(2) - (3)].u.itoken).location, kind_divide, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 541 "c-parse.y"
    { (yyval.u.expr) = make_binary((yyvsp[(2) - (3)].u.itoken).location, kind_modulo, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 543 "c-parse.y"
    { (yyval.u.expr) = make_binary((yyvsp[(2) - (3)].u.itoken).location, kind_lshift, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 545 "c-parse.y"
    { (yyval.u.expr) = make_binary((yyvsp[(2) - (3)].u.itoken).location, kind_rshift, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 547 "c-parse.y"
    { (yyval.u.expr) = make_binary((yyvsp[(2) - (3)].u.itoken).location, (yyvsp[(2) - (3)].u.itoken).i, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 549 "c-parse.y"
    { (yyval.u.expr) = make_binary((yyvsp[(2) - (3)].u.itoken).location, (yyvsp[(2) - (3)].u.itoken).i, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 551 "c-parse.y"
    { (yyval.u.expr) = make_binary((yyvsp[(2) - (3)].u.itoken).location, kind_bitand, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 553 "c-parse.y"
    { (yyval.u.expr) = make_binary((yyvsp[(2) - (3)].u.itoken).location, kind_bitor, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 555 "c-parse.y"
    { (yyval.u.expr) = make_binary((yyvsp[(2) - (3)].u.itoken).location, kind_bitxor, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 557 "c-parse.y"
    { (yyval.u.expr) = make_binary((yyvsp[(2) - (3)].u.itoken).location, kind_andand, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 559 "c-parse.y"
    { (yyval.u.expr) = make_binary((yyvsp[(2) - (3)].u.itoken).location, kind_oror, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 561 "c-parse.y"
    { (yyval.u.expr) = make_conditional((yyvsp[(2) - (5)].u.itoken).location, (yyvsp[(1) - (5)].u.expr), (yyvsp[(3) - (5)].u.expr), (yyvsp[(5) - (5)].u.expr)); ;}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 563 "c-parse.y"
    { if (pedantic)
		    pedwarn("ANSI C forbids omitting the middle term of a ?: expression"); 
		;}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 567 "c-parse.y"
    { (yyval.u.expr) = make_conditional((yyvsp[(2) - (5)].u.itoken).location, (yyvsp[(1) - (5)].u.expr), NULL, (yyvsp[(5) - (5)].u.expr)); ;}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 569 "c-parse.y"
    { (yyval.u.expr) = make_assign((yyvsp[(2) - (3)].u.itoken).location, kind_assign, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 571 "c-parse.y"
    { (yyval.u.expr) = make_assign((yyvsp[(2) - (3)].u.itoken).location, (yyvsp[(2) - (3)].u.itoken).i, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 576 "c-parse.y"
    { 
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  (yyval.u.expr) = make_identifier((yyvsp[(1) - (1)].idtoken).location, (yyvsp[(1) - (1)].idtoken).id, yychar == '('); 
		;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 581 "c-parse.y"
    { (yyval.u.expr) = CAST(expression, (yyvsp[(1) - (1)].u.constant)); ;}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 582 "c-parse.y"
    { (yyval.u.expr) = CAST(expression, (yyvsp[(1) - (1)].u.string)); ;}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 584 "c-parse.y"
    { (yyval.u.expr) = (yyvsp[(2) - (3)].u.expr); ;}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 586 "c-parse.y"
    { (yyval.u.expr) = make_error_expr(last_location); ;}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 588 "c-parse.y"
    { if (current_function_decl == 0)
		    {
		      error("braced-group within expression allowed only inside a function");
		      YYERROR;
		    }
		    push_label_level();
		;}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 596 "c-parse.y"
    { 
		  pop_label_level();
		  if (pedantic)
		    pedwarn("ANSI C forbids braced-groups within expressions");
		  (yyval.u.expr) = make_compound_expr((yyvsp[(1) - (4)].u.itoken).location, (yyvsp[(3) - (4)].u.stmt));
		;}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 603 "c-parse.y"
    { (yyval.u.expr) = make_function_call((yyvsp[(2) - (4)].u.itoken).location, (yyvsp[(1) - (4)].u.expr), (yyvsp[(3) - (4)].u.expr)); ;}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 605 "c-parse.y"
    { (yyval.u.expr) = make_va_arg((yyvsp[(1) - (6)].u.itoken).location, (yyvsp[(3) - (6)].u.expr), (yyvsp[(5) - (6)].u.type)); ;}
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 607 "c-parse.y"
    { (yyval.u.expr) = make_array_ref((yyvsp[(2) - (4)].u.itoken).location, (yyvsp[(1) - (4)].u.expr), (yyvsp[(3) - (4)].u.expr)); ;}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 609 "c-parse.y"
    { (yyval.u.expr) = make_field_ref((yyvsp[(2) - (3)].u.itoken).location, (yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].idtoken).id); ;}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 611 "c-parse.y"
    { (yyval.u.expr) = make_field_ref((yyvsp[(2) - (3)].u.itoken).location, make_dereference((yyvsp[(2) - (3)].u.itoken).location, (yyvsp[(1) - (3)].u.expr)),
				      (yyvsp[(3) - (3)].idtoken).id); ;}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 614 "c-parse.y"
    { (yyval.u.expr) = make_postincrement((yyvsp[(2) - (2)].u.itoken).location, (yyvsp[(1) - (2)].u.expr)); ;}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 616 "c-parse.y"
    { (yyval.u.expr) = make_postdecrement((yyvsp[(2) - (2)].u.itoken).location, (yyvsp[(1) - (2)].u.expr)); ;}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 621 "c-parse.y"
    { (yyval.u.string) = make_string((yyvsp[(1) - (1)].u.expr)->location, (yyvsp[(1) - (1)].u.expr)); ;}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 624 "c-parse.y"
    { (yyval.u.expr) = (yyvsp[(1) - (1)].u.expr); ;}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 626 "c-parse.y"
    { (yyval.u.expr) = expression_chain((yyvsp[(1) - (2)].u.expr), (yyvsp[(2) - (2)].u.expr)); ;}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 630 "c-parse.y"
    { (yyval.u.expr) = CAST(expression, (yyvsp[(1) - (1)].u.expr)); ;}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 632 "c-parse.y"
    { (yyval.u.expr) = make_identifier((yyvsp[(1) - (1)].idtoken).location, (yyvsp[(1) - (1)].idtoken).id, FALSE);
	  ;}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 638 "c-parse.y"
    { (yyval.u.decl) = NULL; ;}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 642 "c-parse.y"
    { if (pedantic)
		    pedwarn("ANSI C does not permit use of `varargs.h'"); 
		  (yyval.u.decl) = declaration_chain((yyvsp[(1) - (2)].u.decl), CAST(declaration, new_ellipsis_decl(pr, (yyvsp[(2) - (2)].u.itoken).location)));
		;}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 653 "c-parse.y"
    { (yyval.u.decl) = new_error_decl(pr, last_location); ;}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 654 "c-parse.y"
    { (yyval.u.decl) = declaration_chain((yyvsp[(1) - (2)].u.decl), (yyvsp[(2) - (2)].u.decl)); ;}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 655 "c-parse.y"
    { (yyval.u.decl) = new_error_decl(pr, last_location); ;}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 664 "c-parse.y"
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[(1) - (4)].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[(3) - (4)].u.decl)));
		  pop_declspec_stack(); ;}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 667 "c-parse.y"
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[(1) - (4)].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[(3) - (4)].u.decl)));
		  pop_declspec_stack(); ;}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 670 "c-parse.y"
    { shadow_tag_warned((yyvsp[(1) - (3)].u.telement), 1);
		  (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[(1) - (3)].u.telement)->location, current_declspecs, prefix_attributes, NULL));
		  pop_declspec_stack();
		  pedwarn("empty declaration"); ;}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 675 "c-parse.y"
    { pedwarn("empty declaration"); 
		  (yyval.u.decl) = NULL; ;}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 685 "c-parse.y"
    { (yyval.u.decl) = new_error_decl(pr, last_location); ;}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 686 "c-parse.y"
    { (yyval.u.decl) = declaration_chain((yyvsp[(1) - (2)].u.decl), (yyvsp[(2) - (2)].u.decl)); ;}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 687 "c-parse.y"
    { (yyval.u.decl) = new_error_decl(pr, last_location); ;}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 695 "c-parse.y"
    { 
		  push_declspec_stack();
		  pending_xref_error();
		  split_type_elements((yyvsp[(0) - (0)].u.telement),
				      &current_declspecs, &prefix_attributes);
		;}
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 705 "c-parse.y"
    { prefix_attributes = attribute_chain(prefix_attributes,
						      (yyvsp[(0) - (0)].u.attribute)); 
		/* This syntax is broken as it will apply to all remaining
		   declarations, not just the current declarator 
		   (this is broken in base GCC too) */
		/* XXX: Used extensively in the linux kernel. YUCK. */
		/*error("Unsupported attribute syntax");*/ ;}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 716 "c-parse.y"
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[(1) - (4)].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[(3) - (4)].u.decl)));
		  pop_declspec_stack(); ;}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 719 "c-parse.y"
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[(1) - (4)].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[(3) - (4)].u.decl)));
		  pop_declspec_stack(); ;}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 722 "c-parse.y"
    { (yyval.u.decl) = (yyvsp[(3) - (3)].u.decl);
		  pop_declspec_stack(); ;}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 725 "c-parse.y"
    { (yyval.u.decl) = (yyvsp[(3) - (3)].u.decl);
		  pop_declspec_stack(); ;}
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 728 "c-parse.y"
    { shadow_tag((yyvsp[(1) - (3)].u.telement));
		  (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[(1) - (3)].u.telement)->location, current_declspecs, prefix_attributes, NULL));
		  pop_declspec_stack(); ;}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 732 "c-parse.y"
    { pedwarn("empty declaration"); ;}
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 734 "c-parse.y"
    { pedantic = (yyvsp[(1) - (2)].u.itoken).i; 
		  (yyval.u.decl) = CAST(declaration, new_extension_decl(pr, (yyvsp[(1) - (2)].u.itoken).location, (yyvsp[(2) - (2)].u.decl))); ;}
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 744 "c-parse.y"
    { (yyval.u.telement) = (yyvsp[(1) - (2)].u.telement); (yyvsp[(1) - (2)].u.telement)->next = CAST(node, (yyvsp[(2) - (2)].u.telement)); ;}
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 746 "c-parse.y"
    { (yyval.u.telement) = type_element_chain((yyvsp[(1) - (3)].u.telement), (yyvsp[(2) - (3)].u.telement)); (yyvsp[(2) - (3)].u.telement)->next = CAST(node, (yyvsp[(3) - (3)].u.telement)); ;}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 750 "c-parse.y"
    { (yyval.u.telement) = NULL; ;}
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 752 "c-parse.y"
    { (yyval.u.telement) = type_element_chain((yyvsp[(1) - (2)].u.telement), (yyvsp[(2) - (2)].u.telement)); ;}
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 754 "c-parse.y"
    { if (extra_warnings)
		    warning("`%s' is not at beginning of declaration",
			    rid_name(CAST(rid, (yyvsp[(2) - (2)].u.telement))));
		  (yyval.u.telement) = type_element_chain((yyvsp[(1) - (2)].u.telement), (yyvsp[(2) - (2)].u.telement)); ;}
    break;

  case 133:

/* Line 1455 of yacc.c  */
#line 759 "c-parse.y"
    { (yyval.u.telement) = type_element_chain((yyvsp[(1) - (2)].u.telement), CAST(type_element, (yyvsp[(2) - (2)].u.attribute))); ;}
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 764 "c-parse.y"
    { (yyval.u.telement) = (yyvsp[(1) - (2)].u.telement); (yyvsp[(1) - (2)].u.telement)->next = CAST(node, (yyvsp[(2) - (2)].u.telement)); ;}
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 766 "c-parse.y"
    { (yyval.u.telement) = type_element_chain((yyvsp[(1) - (3)].u.telement), (yyvsp[(2) - (3)].u.telement)); (yyvsp[(2) - (3)].u.telement)->next = CAST(node, (yyvsp[(3) - (3)].u.telement)); ;}
    break;

  case 136:

/* Line 1455 of yacc.c  */
#line 771 "c-parse.y"
    { (yyval.u.telement) = NULL; ;}
    break;

  case 137:

/* Line 1455 of yacc.c  */
#line 773 "c-parse.y"
    { (yyval.u.telement) = type_element_chain((yyvsp[(1) - (2)].u.telement), (yyvsp[(2) - (2)].u.telement)); ;}
    break;

  case 138:

/* Line 1455 of yacc.c  */
#line 775 "c-parse.y"
    { if (extra_warnings)
		    warning("`%s' is not at beginning of declaration",
			    rid_name(CAST(rid, (yyvsp[(2) - (2)].u.telement))));
		  (yyval.u.telement) = type_element_chain((yyvsp[(1) - (2)].u.telement), (yyvsp[(2) - (2)].u.telement)); ;}
    break;

  case 140:

/* Line 1455 of yacc.c  */
#line 788 "c-parse.y"
    { (yyval.u.telement) = CAST(type_element, (yyvsp[(1) - (1)].u.attribute)); ;}
    break;

  case 141:

/* Line 1455 of yacc.c  */
#line 790 "c-parse.y"
    { (yyval.u.telement) = type_element_chain((yyvsp[(1) - (2)].u.telement), (yyvsp[(2) - (2)].u.telement)); ;}
    break;

  case 142:

/* Line 1455 of yacc.c  */
#line 792 "c-parse.y"
    { (yyval.u.telement) = type_element_chain((yyvsp[(1) - (2)].u.telement), CAST(type_element, (yyvsp[(2) - (2)].u.attribute))); ;}
    break;

  case 145:

/* Line 1455 of yacc.c  */
#line 799 "c-parse.y"
    { (yyval.u.telement) = type_element_chain((yyvsp[(1) - (2)].u.telement), (yyvsp[(2) - (2)].u.telement)); ;}
    break;

  case 146:

/* Line 1455 of yacc.c  */
#line 801 "c-parse.y"
    { if (extra_warnings /*&& TREE_STATIC ($1)*/)
		    warning("`%s' is not at beginning of declaration",
			    rid_name(CAST(rid, (yyvsp[(2) - (2)].u.telement))));
		  (yyval.u.telement) = type_element_chain((yyvsp[(1) - (2)].u.telement), (yyvsp[(2) - (2)].u.telement)); ;}
    break;

  case 147:

/* Line 1455 of yacc.c  */
#line 814 "c-parse.y"
    { (yyval.u.telement) = (yyvsp[(1) - (2)].u.telement); (yyvsp[(1) - (2)].u.telement)->next = CAST(node, (yyvsp[(2) - (2)].u.telement)); ;}
    break;

  case 148:

/* Line 1455 of yacc.c  */
#line 816 "c-parse.y"
    { (yyval.u.telement) = type_element_chain((yyvsp[(1) - (3)].u.telement), (yyvsp[(2) - (3)].u.telement)); (yyvsp[(2) - (3)].u.telement)->next = CAST(node, (yyvsp[(3) - (3)].u.telement)); ;}
    break;

  case 149:

/* Line 1455 of yacc.c  */
#line 820 "c-parse.y"
    { (yyval.u.telement) = NULL; ;}
    break;

  case 150:

/* Line 1455 of yacc.c  */
#line 822 "c-parse.y"
    { (yyval.u.telement) = type_element_chain((yyvsp[(1) - (2)].u.telement), (yyvsp[(2) - (2)].u.telement)); ;}
    break;

  case 153:

/* Line 1455 of yacc.c  */
#line 832 "c-parse.y"
    { /* For a typedef name, record the meaning, not the name.
		     In case of `foo foo, bar;'.  */
		  (yyval.u.telement) = CAST(type_element, new_typename(pr, (yyvsp[(1) - (1)].idtoken).location, (yyvsp[(1) - (1)].idtoken).decl)); ;}
    break;

  case 154:

/* Line 1455 of yacc.c  */
#line 836 "c-parse.y"
    { (yyval.u.telement) = CAST(type_element, new_typeof_expr(pr, (yyvsp[(1) - (4)].u.itoken).location, (yyvsp[(3) - (4)].u.expr))); ;}
    break;

  case 155:

/* Line 1455 of yacc.c  */
#line 838 "c-parse.y"
    { (yyval.u.telement) = CAST(type_element, new_typeof_type(pr, (yyvsp[(1) - (4)].u.itoken).location, (yyvsp[(3) - (4)].u.type))); ;}
    break;

  case 160:

/* Line 1455 of yacc.c  */
#line 850 "c-parse.y"
    { (yyval.u.decl) = declaration_chain((yyvsp[(1) - (3)].u.decl), (yyvsp[(3) - (3)].u.decl)); ;}
    break;

  case 161:

/* Line 1455 of yacc.c  */
#line 854 "c-parse.y"
    { (yyval.u.decl) = (yyvsp[(1) - (1)].u.decl); ;}
    break;

  case 162:

/* Line 1455 of yacc.c  */
#line 855 "c-parse.y"
    { (yyval.u.decl) = declaration_chain((yyvsp[(1) - (3)].u.decl), (yyvsp[(3) - (3)].u.decl)); ;}
    break;

  case 163:

/* Line 1455 of yacc.c  */
#line 860 "c-parse.y"
    { (yyval.u.asm_stmt) = NULL; ;}
    break;

  case 164:

/* Line 1455 of yacc.c  */
#line 862 "c-parse.y"
    { (yyval.u.asm_stmt) = new_asm_stmt(pr, (yyvsp[(1) - (4)].u.itoken).location, CAST(expression, (yyvsp[(3) - (4)].u.string)),
				    NULL, NULL, NULL, NULL); ;}
    break;

  case 165:

/* Line 1455 of yacc.c  */
#line 868 "c-parse.y"
    { (yyval.u.decl) = start_decl((yyvsp[(1) - (4)].u.declarator), (yyvsp[(2) - (4)].u.asm_stmt), current_declspecs, 1,
					(yyvsp[(3) - (4)].u.attribute), prefix_attributes); ;}
    break;

  case 166:

/* Line 1455 of yacc.c  */
#line 872 "c-parse.y"
    { (yyval.u.decl) = finish_decl((yyvsp[(5) - (6)].u.decl), (yyvsp[(6) - (6)].u.expr)); ;}
    break;

  case 167:

/* Line 1455 of yacc.c  */
#line 874 "c-parse.y"
    { declaration d = start_decl((yyvsp[(1) - (3)].u.declarator), (yyvsp[(2) - (3)].u.asm_stmt), current_declspecs, 0,
					     (yyvsp[(3) - (3)].u.attribute), prefix_attributes);
		  (yyval.u.decl) = finish_decl(d, NULL); ;}
    break;

  case 168:

/* Line 1455 of yacc.c  */
#line 881 "c-parse.y"
    { (yyval.u.decl) = start_decl((yyvsp[(1) - (4)].u.declarator), (yyvsp[(2) - (4)].u.asm_stmt), current_declspecs, 1,
					 (yyvsp[(3) - (4)].u.attribute), prefix_attributes); ;}
    break;

  case 169:

/* Line 1455 of yacc.c  */
#line 885 "c-parse.y"
    { (yyval.u.decl) = finish_decl((yyvsp[(5) - (6)].u.decl), (yyvsp[(6) - (6)].u.expr)); ;}
    break;

  case 170:

/* Line 1455 of yacc.c  */
#line 887 "c-parse.y"
    { declaration d = start_decl((yyvsp[(1) - (3)].u.declarator), (yyvsp[(2) - (3)].u.asm_stmt), current_declspecs, 0,
					     (yyvsp[(3) - (3)].u.attribute), prefix_attributes);
		  (yyval.u.decl) = finish_decl(d, NULL); ;}
    break;

  case 171:

/* Line 1455 of yacc.c  */
#line 893 "c-parse.y"
    { (yyval.u.attribute) = NULL; ;}
    break;

  case 172:

/* Line 1455 of yacc.c  */
#line 895 "c-parse.y"
    { (yyval.u.attribute) = (yyvsp[(1) - (1)].u.attribute); ;}
    break;

  case 173:

/* Line 1455 of yacc.c  */
#line 900 "c-parse.y"
    { (yyval.u.attribute) = (yyvsp[(1) - (1)].u.attribute); ;}
    break;

  case 174:

/* Line 1455 of yacc.c  */
#line 902 "c-parse.y"
    { (yyval.u.attribute) = attribute_chain((yyvsp[(1) - (2)].u.attribute), (yyvsp[(2) - (2)].u.attribute)); ;}
    break;

  case 175:

/* Line 1455 of yacc.c  */
#line 907 "c-parse.y"
    { (yyval.u.attribute) = (yyvsp[(4) - (6)].u.attribute); ;}
    break;

  case 176:

/* Line 1455 of yacc.c  */
#line 912 "c-parse.y"
    { (yyval.u.attribute) = (yyvsp[(1) - (1)].u.attribute); ;}
    break;

  case 177:

/* Line 1455 of yacc.c  */
#line 914 "c-parse.y"
    { (yyval.u.attribute) = attribute_chain((yyvsp[(1) - (3)].u.attribute), (yyvsp[(3) - (3)].u.attribute)); ;}
    break;

  case 178:

/* Line 1455 of yacc.c  */
#line 919 "c-parse.y"
    { (yyval.u.attribute) = NULL; ;}
    break;

  case 179:

/* Line 1455 of yacc.c  */
#line 921 "c-parse.y"
    { (yyval.u.attribute) = new_attribute(pr, (yyvsp[(1) - (1)].u.word)->location, (yyvsp[(1) - (1)].u.word), NULL, NULL); ;}
    break;

  case 180:

/* Line 1455 of yacc.c  */
#line 923 "c-parse.y"
    { (yyval.u.attribute) = new_attribute
		    (pr, (yyvsp[(1) - (4)].u.word)->location, (yyvsp[(1) - (4)].u.word), new_word(pr, (yyvsp[(3) - (4)].idtoken).location, (yyvsp[(3) - (4)].idtoken).id), NULL); ;}
    break;

  case 181:

/* Line 1455 of yacc.c  */
#line 926 "c-parse.y"
    { (yyval.u.attribute) = new_attribute
		    (pr, (yyvsp[(2) - (6)].u.itoken).location, (yyvsp[(1) - (6)].u.word), new_word(pr, (yyvsp[(3) - (6)].idtoken).location, (yyvsp[(3) - (6)].idtoken).id), (yyvsp[(5) - (6)].u.expr));
		;}
    break;

  case 182:

/* Line 1455 of yacc.c  */
#line 930 "c-parse.y"
    { (yyval.u.attribute) = new_attribute(pr, (yyvsp[(2) - (4)].u.itoken).location, (yyvsp[(1) - (4)].u.word), NULL, (yyvsp[(3) - (4)].u.expr)); ;}
    break;

  case 184:

/* Line 1455 of yacc.c  */
#line 938 "c-parse.y"
    { (yyval.u.word) = new_word(pr, (yyvsp[(1) - (1)].u.telement)->location, str2cstring(pr, rid_name(CAST(rid, (yyvsp[(1) - (1)].u.telement))))); ;}
    break;

  case 185:

/* Line 1455 of yacc.c  */
#line 939 "c-parse.y"
    { (yyval.u.word) = new_word(pr, (yyvsp[(1) - (1)].u.telement)->location, str2cstring(pr, rid_name(CAST(rid, (yyvsp[(1) - (1)].u.telement))))); ;}
    break;

  case 186:

/* Line 1455 of yacc.c  */
#line 940 "c-parse.y"
    { (yyval.u.word) = new_word(pr, (yyvsp[(1) - (1)].u.telement)->location, str2cstring(pr, qualifier_name(CAST(qualifier, (yyvsp[(1) - (1)].u.telement))->id))); ;}
    break;

  case 188:

/* Line 1455 of yacc.c  */
#line 949 "c-parse.y"
    { (yyval.u.expr) = CAST(expression, new_init_list(pr, (yyvsp[(1) - (3)].u.itoken).location, (yyvsp[(2) - (3)].u.expr))); 
		  (yyval.u.expr)->type = error_type; ;}
    break;

  case 189:

/* Line 1455 of yacc.c  */
#line 952 "c-parse.y"
    { (yyval.u.expr) = make_error_expr(last_location); ;}
    break;

  case 190:

/* Line 1455 of yacc.c  */
#line 958 "c-parse.y"
    { if (pedantic)
		    pedwarn("ANSI C forbids empty initializer braces"); 
		  (yyval.u.expr) = NULL; ;}
    break;

  case 191:

/* Line 1455 of yacc.c  */
#line 961 "c-parse.y"
    { (yyval.u.expr) = (yyvsp[(1) - (2)].u.expr); ;}
    break;

  case 193:

/* Line 1455 of yacc.c  */
#line 966 "c-parse.y"
    { (yyval.u.expr) = expression_chain((yyvsp[(1) - (3)].u.expr), (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 194:

/* Line 1455 of yacc.c  */
#line 972 "c-parse.y"
    { (yyval.u.expr) = (yyvsp[(1) - (1)].u.expr); ;}
    break;

  case 195:

/* Line 1455 of yacc.c  */
#line 974 "c-parse.y"
    { (yyval.u.expr) = CAST(expression, new_init_list(pr, (yyvsp[(1) - (3)].u.itoken).location, (yyvsp[(2) - (3)].u.expr))); ;}
    break;

  case 196:

/* Line 1455 of yacc.c  */
#line 975 "c-parse.y"
    { (yyval.u.expr) = make_error_expr(last_location); ;}
    break;

  case 197:

/* Line 1455 of yacc.c  */
#line 980 "c-parse.y"
    { (yyval.u.expr) = CAST(expression, new_init_index(pr, (yyvsp[(1) - (7)].u.itoken).location, (yyvsp[(2) - (7)].u.expr), (yyvsp[(4) - (7)].u.expr), (yyvsp[(7) - (7)].u.expr))); ;}
    break;

  case 198:

/* Line 1455 of yacc.c  */
#line 982 "c-parse.y"
    { (yyval.u.expr) = CAST(expression, new_init_index(pr, (yyvsp[(1) - (5)].u.itoken).location, (yyvsp[(2) - (5)].u.expr), NULL, (yyvsp[(5) - (5)].u.expr))); ;}
    break;

  case 199:

/* Line 1455 of yacc.c  */
#line 984 "c-parse.y"
    { (yyval.u.expr) = CAST(expression, new_init_index(pr, (yyvsp[(1) - (4)].u.itoken).location, (yyvsp[(2) - (4)].u.expr), NULL, (yyvsp[(4) - (4)].u.expr))); ;}
    break;

  case 200:

/* Line 1455 of yacc.c  */
#line 986 "c-parse.y"
    { (yyval.u.expr) = CAST(expression, new_init_field(pr, (yyvsp[(1) - (3)].u.word)->location, (yyvsp[(1) - (3)].u.word), (yyvsp[(3) - (3)].u.expr))); ;}
    break;

  case 201:

/* Line 1455 of yacc.c  */
#line 988 "c-parse.y"
    { (yyval.u.expr) = CAST(expression, new_init_field(pr, (yyvsp[(1) - (4)].u.itoken).location, (yyvsp[(2) - (4)].u.word), (yyvsp[(4) - (4)].u.expr))); ;}
    break;

  case 202:

/* Line 1455 of yacc.c  */
#line 993 "c-parse.y"
    { if (!start_function(current_declspecs, (yyvsp[(1) - (1)].u.declarator),
				      prefix_attributes, 1))
		    {
		      YYERROR1;
		    }
		  ;}
    break;

  case 203:

/* Line 1455 of yacc.c  */
#line 1000 "c-parse.y"
    { store_parm_decls((yyvsp[(3) - (3)].u.decl)); ;}
    break;

  case 204:

/* Line 1455 of yacc.c  */
#line 1008 "c-parse.y"
    { (yyval.u.decl) = finish_function((yyvsp[(5) - (5)].u.stmt)); ;}
    break;

  case 205:

/* Line 1455 of yacc.c  */
#line 1013 "c-parse.y"
    { if (!start_function(current_declspecs, (yyvsp[(1) - (1)].u.declarator),
				      prefix_attributes, 1))
		    {
		      YYERROR1;
		    }
		;}
    break;

  case 206:

/* Line 1455 of yacc.c  */
#line 1020 "c-parse.y"
    { store_parm_decls((yyvsp[(3) - (3)].u.decl)); ;}
    break;

  case 207:

/* Line 1455 of yacc.c  */
#line 1028 "c-parse.y"
    { (yyval.u.decl) = finish_function((yyvsp[(5) - (5)].u.stmt)); ;}
    break;

  case 210:

/* Line 1455 of yacc.c  */
#line 1043 "c-parse.y"
    { (yyval.u.declarator) = (yyvsp[(2) - (3)].u.declarator); ;}
    break;

  case 211:

/* Line 1455 of yacc.c  */
#line 1045 "c-parse.y"
    { (yyval.u.declarator) = make_function_declarator((yyvsp[(2) - (4)].u.itoken).location, (yyvsp[(1) - (4)].u.declarator), (yyvsp[(3) - (4)].u.decl), (yyvsp[(4) - (4)].u.telement)); ;}
    break;

  case 212:

/* Line 1455 of yacc.c  */
#line 1047 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[(2) - (4)].u.itoken).location, (yyvsp[(1) - (4)].u.declarator), (yyvsp[(3) - (4)].u.expr))); ;}
    break;

  case 213:

/* Line 1455 of yacc.c  */
#line 1049 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[(2) - (3)].u.itoken).location, (yyvsp[(1) - (3)].u.declarator), NULL)); ;}
    break;

  case 214:

/* Line 1455 of yacc.c  */
#line 1051 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_pointer_declarator(pr, (yyvsp[(1) - (3)].u.itoken).location, (yyvsp[(3) - (3)].u.declarator), (yyvsp[(2) - (3)].u.telement))); ;}
    break;

  case 215:

/* Line 1455 of yacc.c  */
#line 1058 "c-parse.y"
    { (yyval.u.declarator) = (yyvsp[(3) - (3)].u.declarator); ;}
    break;

  case 216:

/* Line 1455 of yacc.c  */
#line 1059 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_identifier_declarator(pr, (yyvsp[(1) - (1)].idtoken).location, (yyvsp[(1) - (1)].idtoken).id)); ;}
    break;

  case 217:

/* Line 1455 of yacc.c  */
#line 1069 "c-parse.y"
    { (yyval.u.declarator) = make_function_declarator((yyvsp[(2) - (4)].u.itoken).location, (yyvsp[(1) - (4)].u.declarator), (yyvsp[(3) - (4)].u.decl), (yyvsp[(4) - (4)].u.telement)); ;}
    break;

  case 218:

/* Line 1455 of yacc.c  */
#line 1071 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[(2) - (4)].u.itoken).location, (yyvsp[(1) - (4)].u.declarator), (yyvsp[(3) - (4)].u.expr))); ;}
    break;

  case 219:

/* Line 1455 of yacc.c  */
#line 1073 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[(2) - (3)].u.itoken).location, (yyvsp[(1) - (3)].u.declarator), NULL)); ;}
    break;

  case 220:

/* Line 1455 of yacc.c  */
#line 1075 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_pointer_declarator(pr, (yyvsp[(1) - (3)].u.itoken).location, (yyvsp[(3) - (3)].u.declarator), (yyvsp[(2) - (3)].u.telement))); ;}
    break;

  case 221:

/* Line 1455 of yacc.c  */
#line 1082 "c-parse.y"
    { (yyval.u.declarator) = (yyvsp[(3) - (3)].u.declarator); ;}
    break;

  case 222:

/* Line 1455 of yacc.c  */
#line 1083 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_identifier_declarator(pr, (yyvsp[(1) - (1)].idtoken).location, (yyvsp[(1) - (1)].idtoken).id)); ;}
    break;

  case 223:

/* Line 1455 of yacc.c  */
#line 1091 "c-parse.y"
    { (yyval.u.declarator) = make_function_declarator((yyvsp[(2) - (4)].u.itoken).location, (yyvsp[(1) - (4)].u.declarator), (yyvsp[(3) - (4)].u.decl), (yyvsp[(4) - (4)].u.telement)); ;}
    break;

  case 224:

/* Line 1455 of yacc.c  */
#line 1093 "c-parse.y"
    { (yyval.u.declarator) = (yyvsp[(2) - (3)].u.declarator); ;}
    break;

  case 225:

/* Line 1455 of yacc.c  */
#line 1095 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_pointer_declarator(pr, (yyvsp[(1) - (3)].u.itoken).location, (yyvsp[(3) - (3)].u.declarator), (yyvsp[(2) - (3)].u.telement))); ;}
    break;

  case 226:

/* Line 1455 of yacc.c  */
#line 1097 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[(2) - (4)].u.itoken).location, (yyvsp[(1) - (4)].u.declarator), (yyvsp[(3) - (4)].u.expr))); ;}
    break;

  case 227:

/* Line 1455 of yacc.c  */
#line 1099 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[(2) - (3)].u.itoken).location, (yyvsp[(1) - (3)].u.declarator), NULL)); ;}
    break;

  case 228:

/* Line 1455 of yacc.c  */
#line 1106 "c-parse.y"
    { (yyval.u.declarator) = (yyvsp[(3) - (3)].u.declarator); ;}
    break;

  case 229:

/* Line 1455 of yacc.c  */
#line 1107 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_identifier_declarator(pr, (yyvsp[(1) - (1)].idtoken).location, (yyvsp[(1) - (1)].idtoken).id)); ;}
    break;

  case 230:

/* Line 1455 of yacc.c  */
#line 1111 "c-parse.y"
    { (yyval.u.word) = new_word(pr, (yyvsp[(1) - (1)].idtoken).location, (yyvsp[(1) - (1)].idtoken).id); ;}
    break;

  case 231:

/* Line 1455 of yacc.c  */
#line 1116 "c-parse.y"
    { (yyval.u.telement) = start_struct((yyvsp[(1) - (3)].u.itoken).location, kind_struct_ref, (yyvsp[(2) - (3)].u.word));
		  /* Start scope of tag before parsing components.  */
		;}
    break;

  case 232:

/* Line 1455 of yacc.c  */
#line 1120 "c-parse.y"
    { (yyval.u.telement) = finish_struct((yyvsp[(4) - (7)].u.telement), (yyvsp[(5) - (7)].u.decl), (yyvsp[(7) - (7)].u.attribute)); ;}
    break;

  case 233:

/* Line 1455 of yacc.c  */
#line 1122 "c-parse.y"
    { (yyval.u.telement) = finish_struct(start_struct((yyvsp[(1) - (5)].u.itoken).location, kind_struct_ref, NULL),
				     (yyvsp[(3) - (5)].u.decl), (yyvsp[(5) - (5)].u.attribute));
		;}
    break;

  case 234:

/* Line 1455 of yacc.c  */
#line 1126 "c-parse.y"
    { (yyval.u.telement) = xref_tag((yyvsp[(1) - (2)].u.itoken).location, kind_struct_ref, (yyvsp[(2) - (2)].u.word)); ;}
    break;

  case 235:

/* Line 1455 of yacc.c  */
#line 1128 "c-parse.y"
    { (yyval.u.telement) = start_struct ((yyvsp[(1) - (3)].u.itoken).location, kind_union_ref, (yyvsp[(2) - (3)].u.word)); ;}
    break;

  case 236:

/* Line 1455 of yacc.c  */
#line 1130 "c-parse.y"
    { (yyval.u.telement) = finish_struct((yyvsp[(4) - (7)].u.telement), (yyvsp[(5) - (7)].u.decl), (yyvsp[(7) - (7)].u.attribute)); ;}
    break;

  case 237:

/* Line 1455 of yacc.c  */
#line 1132 "c-parse.y"
    { (yyval.u.telement) = finish_struct(start_struct((yyvsp[(1) - (5)].u.itoken).location, kind_union_ref, NULL),
				     (yyvsp[(3) - (5)].u.decl), (yyvsp[(5) - (5)].u.attribute));
		;}
    break;

  case 238:

/* Line 1455 of yacc.c  */
#line 1136 "c-parse.y"
    { (yyval.u.telement) = xref_tag((yyvsp[(1) - (2)].u.itoken).location, kind_union_ref, (yyvsp[(2) - (2)].u.word)); ;}
    break;

  case 239:

/* Line 1455 of yacc.c  */
#line 1138 "c-parse.y"
    { (yyval.u.telement) = start_enum((yyvsp[(1) - (3)].u.itoken).location, (yyvsp[(2) - (3)].u.word)); ;}
    break;

  case 240:

/* Line 1455 of yacc.c  */
#line 1140 "c-parse.y"
    { (yyval.u.telement) = finish_enum((yyvsp[(4) - (8)].u.telement), (yyvsp[(5) - (8)].u.decl), (yyvsp[(8) - (8)].u.attribute)); ;}
    break;

  case 241:

/* Line 1455 of yacc.c  */
#line 1142 "c-parse.y"
    { (yyval.u.telement) = start_enum((yyvsp[(1) - (2)].u.itoken).location, NULL); ;}
    break;

  case 242:

/* Line 1455 of yacc.c  */
#line 1144 "c-parse.y"
    { (yyval.u.telement) = finish_enum((yyvsp[(3) - (7)].u.telement), (yyvsp[(4) - (7)].u.decl), (yyvsp[(7) - (7)].u.attribute)); ;}
    break;

  case 243:

/* Line 1455 of yacc.c  */
#line 1146 "c-parse.y"
    { (yyval.u.telement) = xref_tag((yyvsp[(1) - (2)].u.itoken).location, kind_enum_ref, (yyvsp[(2) - (2)].u.word)); ;}
    break;

  case 247:

/* Line 1455 of yacc.c  */
#line 1157 "c-parse.y"
    { if (pedantic) pedwarn("comma at end of enumerator list"); ;}
    break;

  case 248:

/* Line 1455 of yacc.c  */
#line 1162 "c-parse.y"
    { (yyval.u.decl) = (yyvsp[(1) - (1)].u.decl); ;}
    break;

  case 249:

/* Line 1455 of yacc.c  */
#line 1164 "c-parse.y"
    { (yyval.u.decl) = declaration_chain((yyvsp[(1) - (2)].u.decl), (yyvsp[(2) - (2)].u.decl));
		  pedwarn("no semicolon at end of struct or union"); ;}
    break;

  case 250:

/* Line 1455 of yacc.c  */
#line 1169 "c-parse.y"
    { (yyval.u.decl) = NULL; ;}
    break;

  case 251:

/* Line 1455 of yacc.c  */
#line 1171 "c-parse.y"
    { (yyval.u.decl) = declaration_chain((yyvsp[(1) - (3)].u.decl), (yyvsp[(2) - (3)].u.decl)); ;}
    break;

  case 252:

/* Line 1455 of yacc.c  */
#line 1173 "c-parse.y"
    { if (pedantic)
		    pedwarn("extra semicolon in struct or union specified"); 
		   (yyval.u.decl) = (yyvsp[(1) - (2)].u.decl); ;}
    break;

  case 253:

/* Line 1455 of yacc.c  */
#line 1189 "c-parse.y"
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[(1) - (3)].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[(3) - (3)].u.decl)));
		  pop_declspec_stack(); ;}
    break;

  case 254:

/* Line 1455 of yacc.c  */
#line 1192 "c-parse.y"
    { if (pedantic)
		    pedwarn("ANSI C forbids member declarations with no members");
		  shadow_tag((yyvsp[(1) - (2)].u.telement));
		  (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[(1) - (2)].u.telement)->location, current_declspecs, prefix_attributes, NULL));
		  pop_declspec_stack(); ;}
    break;

  case 255:

/* Line 1455 of yacc.c  */
#line 1198 "c-parse.y"
    { (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[(1) - (3)].u.telement)->location, current_declspecs, prefix_attributes, (yyvsp[(3) - (3)].u.decl)));
		  pop_declspec_stack(); ;}
    break;

  case 256:

/* Line 1455 of yacc.c  */
#line 1201 "c-parse.y"
    { if (pedantic)
		    pedwarn("ANSI C forbids member declarations with no members");
		  shadow_tag((yyvsp[(1) - (2)].u.telement));
		  (yyval.u.decl) = CAST(declaration, new_data_decl(pr, (yyvsp[(1) - (2)].u.telement)->location, current_declspecs, prefix_attributes, NULL));	
		  pop_declspec_stack(); ;}
    break;

  case 257:

/* Line 1455 of yacc.c  */
#line 1207 "c-parse.y"
    { (yyval.u.decl) = new_error_decl(pr, last_location); ;}
    break;

  case 258:

/* Line 1455 of yacc.c  */
#line 1209 "c-parse.y"
    { pedantic = (yyvsp[(1) - (2)].u.itoken).i;
		  (yyval.u.decl) = CAST(declaration, new_extension_decl(pr, (yyvsp[(1) - (2)].u.itoken).location, (yyvsp[(2) - (2)].u.decl))); ;}
    break;

  case 260:

/* Line 1455 of yacc.c  */
#line 1216 "c-parse.y"
    { (yyval.u.decl) = declaration_chain((yyvsp[(1) - (3)].u.decl), (yyvsp[(3) - (3)].u.decl)); ;}
    break;

  case 261:

/* Line 1455 of yacc.c  */
#line 1221 "c-parse.y"
    { (yyval.u.decl) = make_field((yyvsp[(1) - (2)].u.declarator), NULL, current_declspecs,
				  (yyvsp[(2) - (2)].u.attribute), prefix_attributes); ;}
    break;

  case 262:

/* Line 1455 of yacc.c  */
#line 1224 "c-parse.y"
    { (yyval.u.decl) = make_field((yyvsp[(1) - (4)].u.declarator), (yyvsp[(3) - (4)].u.expr), current_declspecs,
				  (yyvsp[(4) - (4)].u.attribute), prefix_attributes); ;}
    break;

  case 263:

/* Line 1455 of yacc.c  */
#line 1227 "c-parse.y"
    { (yyval.u.decl) = make_field(NULL, (yyvsp[(2) - (3)].u.expr), current_declspecs,
				  (yyvsp[(3) - (3)].u.attribute), prefix_attributes); ;}
    break;

  case 265:

/* Line 1455 of yacc.c  */
#line 1234 "c-parse.y"
    { (yyval.u.decl) = declaration_chain((yyvsp[(1) - (3)].u.decl), (yyvsp[(3) - (3)].u.decl)); ;}
    break;

  case 266:

/* Line 1455 of yacc.c  */
#line 1236 "c-parse.y"
    { (yyval.u.decl) = NULL; ;}
    break;

  case 267:

/* Line 1455 of yacc.c  */
#line 1242 "c-parse.y"
    { (yyval.u.decl) = make_enumerator((yyvsp[(1) - (1)].idtoken).location, (yyvsp[(1) - (1)].idtoken).id, NULL); ;}
    break;

  case 268:

/* Line 1455 of yacc.c  */
#line 1244 "c-parse.y"
    { (yyval.u.decl) = make_enumerator((yyvsp[(1) - (3)].idtoken).location, (yyvsp[(1) - (3)].idtoken).id, (yyvsp[(3) - (3)].u.expr)); ;}
    break;

  case 269:

/* Line 1455 of yacc.c  */
#line 1249 "c-parse.y"
    { (yyval.u.type) = make_type((yyvsp[(1) - (2)].u.telement), (yyvsp[(2) - (2)].u.declarator)); ;}
    break;

  case 270:

/* Line 1455 of yacc.c  */
#line 1251 "c-parse.y"
    { (yyval.u.type) = make_type((yyvsp[(1) - (2)].u.telement), (yyvsp[(2) - (2)].u.declarator)); ;}
    break;

  case 271:

/* Line 1455 of yacc.c  */
#line 1256 "c-parse.y"
    { (yyval.u.declarator) = NULL; ;}
    break;

  case 274:

/* Line 1455 of yacc.c  */
#line 1263 "c-parse.y"
    { (yyval.u.telement) = type_element_chain((yyvsp[(1) - (2)].u.telement), (yyvsp[(2) - (2)].u.telement)); ;}
    break;

  case 275:

/* Line 1455 of yacc.c  */
#line 1268 "c-parse.y"
    { (yyval.u.telement) = NULL; ;}
    break;

  case 276:

/* Line 1455 of yacc.c  */
#line 1270 "c-parse.y"
    { (yyval.u.telement) = type_element_chain((yyvsp[(1) - (2)].u.telement), (yyvsp[(2) - (2)].u.telement)); ;}
    break;

  case 277:

/* Line 1455 of yacc.c  */
#line 1275 "c-parse.y"
    { (yyval.u.declarator) = (yyvsp[(2) - (3)].u.declarator); ;}
    break;

  case 278:

/* Line 1455 of yacc.c  */
#line 1278 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_pointer_declarator(pr, (yyvsp[(1) - (3)].u.itoken).location, (yyvsp[(3) - (3)].u.declarator), (yyvsp[(2) - (3)].u.telement))); ;}
    break;

  case 279:

/* Line 1455 of yacc.c  */
#line 1280 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_pointer_declarator(pr, (yyvsp[(1) - (2)].u.itoken).location, NULL, (yyvsp[(2) - (2)].u.telement))); ;}
    break;

  case 280:

/* Line 1455 of yacc.c  */
#line 1282 "c-parse.y"
    { (yyval.u.declarator) = make_function_declarator((yyvsp[(2) - (4)].u.itoken).location, (yyvsp[(1) - (4)].u.declarator), (yyvsp[(3) - (4)].u.decl), (yyvsp[(4) - (4)].u.telement)); ;}
    break;

  case 281:

/* Line 1455 of yacc.c  */
#line 1284 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[(2) - (4)].u.itoken).location, (yyvsp[(1) - (4)].u.declarator), (yyvsp[(3) - (4)].u.expr))); ;}
    break;

  case 282:

/* Line 1455 of yacc.c  */
#line 1286 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[(2) - (3)].u.itoken).location, (yyvsp[(1) - (3)].u.declarator), NULL)); ;}
    break;

  case 283:

/* Line 1455 of yacc.c  */
#line 1288 "c-parse.y"
    { (yyval.u.declarator) = make_function_declarator((yyvsp[(1) - (3)].u.itoken).location, NULL, (yyvsp[(2) - (3)].u.decl), (yyvsp[(3) - (3)].u.telement)); ;}
    break;

  case 284:

/* Line 1455 of yacc.c  */
#line 1290 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[(1) - (3)].u.itoken).location, NULL, (yyvsp[(2) - (3)].u.expr))); ;}
    break;

  case 285:

/* Line 1455 of yacc.c  */
#line 1292 "c-parse.y"
    { (yyval.u.declarator) = CAST(declarator, new_array_declarator(pr, (yyvsp[(1) - (2)].u.itoken).location, NULL, NULL)); ;}
    break;

  case 286:

/* Line 1455 of yacc.c  */
#line 1303 "c-parse.y"
    {
		  if (pedantic && (yyvsp[(1) - (1)].u.istmt).i)
		    pedwarn("ANSI C forbids label at end of compound statement");
		  /* Add an empty statement to last label if stand-alone */
		  if ((yyvsp[(1) - (1)].u.istmt).i)
		    {
		      statement last_label = CAST(statement, last_node(CAST(node, (yyvsp[(1) - (1)].u.istmt).stmt)));

		      chain_with_labels(last_label, CAST(statement, new_empty_stmt(pr, last_label->location)));
		    }
		  (yyval.u.stmt) = (yyvsp[(1) - (1)].u.istmt).stmt;
		;}
    break;

  case 288:

/* Line 1455 of yacc.c  */
#line 1320 "c-parse.y"
    { (yyval.u.istmt).i = (yyvsp[(2) - (2)].u.istmt).i; (yyval.u.istmt).stmt = chain_with_labels((yyvsp[(1) - (2)].u.istmt).stmt, (yyvsp[(2) - (2)].u.istmt).stmt); ;}
    break;

  case 289:

/* Line 1455 of yacc.c  */
#line 1322 "c-parse.y"
    { (yyval.u.istmt).i = 0; (yyval.u.istmt).stmt = new_error_stmt(pr, last_location); ;}
    break;

  case 290:

/* Line 1455 of yacc.c  */
#line 1326 "c-parse.y"
    { (yyval.u.stmt) = NULL; ;}
    break;

  case 293:

/* Line 1455 of yacc.c  */
#line 1334 "c-parse.y"
    { pushlevel(FALSE); ;}
    break;

  case 294:

/* Line 1455 of yacc.c  */
#line 1340 "c-parse.y"
    { (yyval.u.id_label) = NULL; ;}
    break;

  case 295:

/* Line 1455 of yacc.c  */
#line 1342 "c-parse.y"
    { if (pedantic)
		    pedwarn("ANSI C forbids label declarations"); 
		  (yyval.u.id_label) = (yyvsp[(1) - (1)].u.id_label); ;}
    break;

  case 297:

/* Line 1455 of yacc.c  */
#line 1349 "c-parse.y"
    { (yyval.u.id_label) = id_label_chain((yyvsp[(1) - (2)].u.id_label), (yyvsp[(2) - (2)].u.id_label)); ;}
    break;

  case 298:

/* Line 1455 of yacc.c  */
#line 1354 "c-parse.y"
    { (yyval.u.id_label) = (yyvsp[(2) - (3)].u.id_label); ;}
    break;

  case 300:

/* Line 1455 of yacc.c  */
#line 1361 "c-parse.y"
    { (yyval.u.stmt) = (yyvsp[(2) - (2)].u.stmt); ;}
    break;

  case 301:

/* Line 1455 of yacc.c  */
#line 1364 "c-parse.y"
    { (yyval.u.itoken) = (yyvsp[(1) - (1)].u.itoken); compstmt_count++; ;}
    break;

  case 302:

/* Line 1455 of yacc.c  */
#line 1367 "c-parse.y"
    { (yyval.u.stmt) = CAST(statement, new_compound_stmt(pr, (yyvsp[(1) - (3)].u.itoken).location, NULL, NULL, NULL, poplevel())); ;}
    break;

  case 303:

/* Line 1455 of yacc.c  */
#line 1369 "c-parse.y"
    { (yyval.u.stmt) = CAST(statement, new_compound_stmt(pr, (yyvsp[(1) - (6)].u.itoken).location, (yyvsp[(3) - (6)].u.id_label), (yyvsp[(4) - (6)].u.decl), (yyvsp[(5) - (6)].u.stmt), poplevel())); ;}
    break;

  case 304:

/* Line 1455 of yacc.c  */
#line 1371 "c-parse.y"
    { poplevel();
		  (yyval.u.stmt) = new_error_stmt(pr, last_location); ;}
    break;

  case 305:

/* Line 1455 of yacc.c  */
#line 1374 "c-parse.y"
    { (yyval.u.stmt) = CAST(statement, new_compound_stmt(pr, (yyvsp[(1) - (5)].u.itoken).location, (yyvsp[(3) - (5)].u.id_label), NULL, (yyvsp[(4) - (5)].u.stmt), poplevel())); ;}
    break;

  case 306:

/* Line 1455 of yacc.c  */
#line 1380 "c-parse.y"
    { (yyval.u.istmt).stmt = CAST(statement, new_if_stmt(pr, (yyvsp[(1) - (2)].u.iexpr).expr->location, (yyvsp[(1) - (2)].u.iexpr).expr, (yyvsp[(2) - (2)].u.stmt), NULL));
		  (yyval.u.istmt).i = (yyvsp[(1) - (2)].u.iexpr).i; ;}
    break;

  case 307:

/* Line 1455 of yacc.c  */
#line 1382 "c-parse.y"
    { (yyval.u.istmt).i = (yyvsp[(1) - (2)].u.iexpr).i; (yyval.u.istmt).stmt = new_error_stmt(pr, last_location); ;}
    break;

  case 308:

/* Line 1455 of yacc.c  */
#line 1387 "c-parse.y"
    { (yyval.u.iexpr).i = stmt_count;
		  (yyval.u.iexpr).expr = (yyvsp[(3) - (4)].u.expr);
		  check_condition("if", (yyvsp[(3) - (4)].u.expr)); ;}
    break;

  case 309:

/* Line 1455 of yacc.c  */
#line 1397 "c-parse.y"
    { stmt_count++;
		  compstmt_count++; 
		  (yyval.u.cstmt) = CAST(conditional_stmt,
				   new_dowhile_stmt(pr, (yyvsp[(1) - (1)].u.itoken).location, NULL, NULL));
		 push_loop(CAST(statement, (yyval.u.cstmt))); ;}
    break;

  case 310:

/* Line 1455 of yacc.c  */
#line 1403 "c-parse.y"
    { (yyval.u.cstmt) = (yyvsp[(2) - (4)].u.cstmt); 
		  (yyval.u.cstmt)->stmt = (yyvsp[(3) - (4)].u.stmt); ;}
    break;

  case 311:

/* Line 1455 of yacc.c  */
#line 1409 "c-parse.y"
    { (yyval.u.stmt) = (yyvsp[(1) - (1)].u.stmt); ;}
    break;

  case 312:

/* Line 1455 of yacc.c  */
#line 1411 "c-parse.y"
    { (yyval.u.stmt) = CAST(statement, new_labeled_stmt(pr, (yyvsp[(1) - (2)].u.label)->location, (yyvsp[(1) - (2)].u.label), (yyvsp[(2) - (2)].u.stmt))); ;}
    break;

  case 313:

/* Line 1455 of yacc.c  */
#line 1416 "c-parse.y"
    { (yyval.u.istmt).i = 0; (yyval.u.istmt).stmt = (yyvsp[(1) - (1)].u.stmt); ;}
    break;

  case 314:

/* Line 1455 of yacc.c  */
#line 1418 "c-parse.y"
    { (yyval.u.istmt).i = 1; (yyval.u.istmt).stmt = CAST(statement, new_labeled_stmt(pr, (yyvsp[(1) - (1)].u.label)->location, (yyvsp[(1) - (1)].u.label), NULL)); ;}
    break;

  case 315:

/* Line 1455 of yacc.c  */
#line 1424 "c-parse.y"
    { stmt_count++; (yyval.u.stmt) = (yyvsp[(1) - (1)].u.stmt); ;}
    break;

  case 316:

/* Line 1455 of yacc.c  */
#line 1426 "c-parse.y"
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_expression_stmt(pr, (yyvsp[(1) - (2)].u.expr)->location, (yyvsp[(1) - (2)].u.expr))); ;}
    break;

  case 317:

/* Line 1455 of yacc.c  */
#line 1429 "c-parse.y"
    { (yyvsp[(1) - (2)].u.istmt).i = stmt_count; ;}
    break;

  case 318:

/* Line 1455 of yacc.c  */
#line 1431 "c-parse.y"
    { if (extra_warnings && stmt_count == (yyvsp[(1) - (4)].u.istmt).i)
		    warning("empty body in an else-statement");
		  (yyval.u.stmt) = (yyvsp[(1) - (4)].u.istmt).stmt;
		  CAST(if_stmt, (yyval.u.stmt))->stmt2 = (yyvsp[(4) - (4)].u.stmt);
		;}
    break;

  case 319:

/* Line 1455 of yacc.c  */
#line 1437 "c-parse.y"
    { /* This warning is here instead of in simple_if, because we
		     do not want a warning if an empty if is followed by an
		     else statement.  Increment stmt_count so we don't
		     give a second error if this is a nested `if'.  */
		  if (extra_warnings && stmt_count++ == (yyvsp[(1) - (1)].u.istmt).i)
		    warning_with_location ((yyvsp[(1) - (1)].u.istmt).stmt->location,
					   "empty body in an if-statement");
		  (yyval.u.stmt) = (yyvsp[(1) - (1)].u.istmt).stmt; ;}
    break;

  case 320:

/* Line 1455 of yacc.c  */
#line 1446 "c-parse.y"
    { (yyval.u.stmt) = new_error_stmt(pr, last_location); ;}
    break;

  case 321:

/* Line 1455 of yacc.c  */
#line 1448 "c-parse.y"
    { stmt_count++; ;}
    break;

  case 322:

/* Line 1455 of yacc.c  */
#line 1450 "c-parse.y"
    { check_condition("while", (yyvsp[(4) - (5)].u.expr)); 
		  (yyval.u.cstmt) = CAST(conditional_stmt,
			           new_while_stmt(pr, (yyvsp[(1) - (5)].u.itoken).location, (yyvsp[(4) - (5)].u.expr), NULL));
		  /* The condition is not "in the loop" for break or continue */
		  push_loop(CAST(statement, (yyval.u.cstmt))); ;}
    break;

  case 323:

/* Line 1455 of yacc.c  */
#line 1456 "c-parse.y"
    { (yyval.u.stmt) = CAST(statement, (yyvsp[(6) - (7)].u.cstmt));
		  (yyvsp[(6) - (7)].u.cstmt)->stmt = (yyvsp[(7) - (7)].u.stmt); 
		  pop_loop(); ;}
    break;

  case 324:

/* Line 1455 of yacc.c  */
#line 1460 "c-parse.y"
    { (yyval.u.stmt) = CAST(statement, (yyvsp[(1) - (5)].u.cstmt));
		  (yyvsp[(1) - (5)].u.cstmt)->condition = (yyvsp[(3) - (5)].u.expr);
		  check_condition("do-while", (yyvsp[(3) - (5)].u.expr)); 
		  /* Note that pop_loop should be before the expr to be consistent
		     with while, but GCC is inconsistent. See loop1.c */
		  pop_loop(); ;}
    break;

  case 325:

/* Line 1455 of yacc.c  */
#line 1467 "c-parse.y"
    { (yyval.u.stmt) = new_error_stmt(pr, last_location); 
		  pop_loop(); ;}
    break;

  case 326:

/* Line 1455 of yacc.c  */
#line 1469 "c-parse.y"
    { stmt_count++; ;}
    break;

  case 327:

/* Line 1455 of yacc.c  */
#line 1470 "c-parse.y"
    { if ((yyvsp[(6) - (7)].u.expr)) check_condition("for", (yyvsp[(6) - (7)].u.expr)); ;}
    break;

  case 328:

/* Line 1455 of yacc.c  */
#line 1472 "c-parse.y"
    { (yyval.u.for_stmt) = new_for_stmt(pr, (yyvsp[(1) - (10)].u.itoken).location, (yyvsp[(3) - (10)].u.expr), (yyvsp[(6) - (10)].u.expr), (yyvsp[(9) - (10)].u.expr), NULL);
		  push_loop(CAST(statement, (yyval.u.for_stmt))); ;}
    break;

  case 329:

/* Line 1455 of yacc.c  */
#line 1475 "c-parse.y"
    { (yyval.u.stmt) = CAST(statement, (yyvsp[(11) - (12)].u.for_stmt));
		  (yyvsp[(11) - (12)].u.for_stmt)->stmt = (yyvsp[(12) - (12)].u.stmt); 
		  pop_loop(); ;}
    break;

  case 330:

/* Line 1455 of yacc.c  */
#line 1479 "c-parse.y"
    { stmt_count++; check_switch((yyvsp[(3) - (4)].u.expr)); 
		  (yyval.u.cstmt) = CAST(conditional_stmt,
			           new_switch_stmt(pr, (yyvsp[(1) - (4)].u.itoken).location, (yyvsp[(3) - (4)].u.expr), NULL)); 
		  push_loop(CAST(statement, (yyval.u.cstmt))); ;}
    break;

  case 331:

/* Line 1455 of yacc.c  */
#line 1484 "c-parse.y"
    { (yyval.u.stmt) = CAST(statement, (yyvsp[(5) - (6)].u.cstmt)); 
		  (yyvsp[(5) - (6)].u.cstmt)->stmt = (yyvsp[(6) - (6)].u.stmt);
		  pop_loop(); ;}
    break;

  case 332:

/* Line 1455 of yacc.c  */
#line 1488 "c-parse.y"
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_break_stmt(pr, (yyvsp[(1) - (2)].u.itoken).location));
		  check_break((yyval.u.stmt));
		;}
    break;

  case 333:

/* Line 1455 of yacc.c  */
#line 1493 "c-parse.y"
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_continue_stmt(pr, (yyvsp[(1) - (2)].u.itoken).location));
		  check_continue((yyval.u.stmt));
		;}
    break;

  case 334:

/* Line 1455 of yacc.c  */
#line 1498 "c-parse.y"
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_return_stmt(pr, (yyvsp[(1) - (2)].u.itoken).location, NULL)); 
		  check_void_return(); ;}
    break;

  case 335:

/* Line 1455 of yacc.c  */
#line 1502 "c-parse.y"
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_return_stmt(pr, (yyvsp[(1) - (3)].u.itoken).location, (yyvsp[(2) - (3)].u.expr))); 
		  check_return((yyvsp[(2) - (3)].u.expr)); ;}
    break;

  case 336:

/* Line 1455 of yacc.c  */
#line 1506 "c-parse.y"
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_asm_stmt(pr, (yyvsp[(1) - (6)].u.itoken).location, (yyvsp[(4) - (6)].u.expr), NULL,
					       NULL, NULL, (yyvsp[(2) - (6)].u.telement))); ;}
    break;

  case 337:

/* Line 1455 of yacc.c  */
#line 1511 "c-parse.y"
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_asm_stmt(pr, (yyvsp[(1) - (8)].u.itoken).location, (yyvsp[(4) - (8)].u.expr), (yyvsp[(6) - (8)].u.asm_operand), NULL,
					       NULL, (yyvsp[(2) - (8)].u.telement))); ;}
    break;

  case 338:

/* Line 1455 of yacc.c  */
#line 1516 "c-parse.y"
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_asm_stmt(pr, (yyvsp[(1) - (10)].u.itoken).location, (yyvsp[(4) - (10)].u.expr), (yyvsp[(6) - (10)].u.asm_operand), (yyvsp[(8) - (10)].u.asm_operand), NULL, (yyvsp[(2) - (10)].u.telement))); ;}
    break;

  case 339:

/* Line 1455 of yacc.c  */
#line 1521 "c-parse.y"
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_asm_stmt(pr, (yyvsp[(1) - (12)].u.itoken).location, (yyvsp[(4) - (12)].u.expr), (yyvsp[(6) - (12)].u.asm_operand), (yyvsp[(8) - (12)].u.asm_operand), (yyvsp[(10) - (12)].u.string), (yyvsp[(2) - (12)].u.telement))); ;}
    break;

  case 340:

/* Line 1455 of yacc.c  */
#line 1524 "c-parse.y"
    { stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_goto_stmt(pr, (yyvsp[(1) - (3)].u.itoken).location, (yyvsp[(2) - (3)].u.id_label)));
		  use_label((yyvsp[(2) - (3)].u.id_label));
		;}
    break;

  case 341:

/* Line 1455 of yacc.c  */
#line 1529 "c-parse.y"
    { if (pedantic)
		    pedwarn("ANSI C forbids `goto *expr;'");
		  stmt_count++;
		  (yyval.u.stmt) = CAST(statement, new_computed_goto_stmt(pr, (yyvsp[(1) - (4)].u.itoken).location, (yyvsp[(3) - (4)].u.expr))); 
		  check_computed_goto((yyvsp[(3) - (4)].u.expr)); ;}
    break;

  case 342:

/* Line 1455 of yacc.c  */
#line 1534 "c-parse.y"
    { (yyval.u.stmt) = CAST(statement, new_empty_stmt(pr, (yyvsp[(1) - (1)].u.itoken).location)); ;}
    break;

  case 343:

/* Line 1455 of yacc.c  */
#line 1542 "c-parse.y"
    { (yyval.u.label) = CAST(label, new_case_label(pr, (yyvsp[(1) - (3)].u.itoken).location, (yyvsp[(2) - (3)].u.expr), NULL)); 
		  check_case((yyval.u.label)); ;}
    break;

  case 344:

/* Line 1455 of yacc.c  */
#line 1545 "c-parse.y"
    { (yyval.u.label) = CAST(label, new_case_label(pr, (yyvsp[(1) - (5)].u.itoken).location, (yyvsp[(2) - (5)].u.expr), (yyvsp[(4) - (5)].u.expr))); 
		  check_case((yyval.u.label)); ;}
    break;

  case 345:

/* Line 1455 of yacc.c  */
#line 1548 "c-parse.y"
    { (yyval.u.label) = CAST(label, new_default_label(pr, (yyvsp[(1) - (2)].u.itoken).location)); 
		  check_default((yyval.u.label)); ;}
    break;

  case 346:

/* Line 1455 of yacc.c  */
#line 1551 "c-parse.y"
    { (yyval.u.label) = CAST(label, (yyvsp[(1) - (2)].u.id_label)); 
		  define_label((yyvsp[(1) - (2)].u.id_label)); ;}
    break;

  case 347:

/* Line 1455 of yacc.c  */
#line 1559 "c-parse.y"
    { (yyval.u.telement) = NULL; ;}
    break;

  case 349:

/* Line 1455 of yacc.c  */
#line 1565 "c-parse.y"
    { (yyval.u.expr) = NULL; ;}
    break;

  case 351:

/* Line 1455 of yacc.c  */
#line 1572 "c-parse.y"
    { (yyval.u.asm_operand) = NULL; ;}
    break;

  case 354:

/* Line 1455 of yacc.c  */
#line 1579 "c-parse.y"
    { (yyval.u.asm_operand) = asm_operand_chain((yyvsp[(1) - (3)].u.asm_operand), (yyvsp[(3) - (3)].u.asm_operand)); ;}
    break;

  case 355:

/* Line 1455 of yacc.c  */
#line 1584 "c-parse.y"
    { (yyval.u.asm_operand) = new_asm_operand(pr, (yyvsp[(1) - (4)].u.expr)->location,
				       make_string((yyvsp[(1) - (4)].u.expr)->location, CAST(expression, (yyvsp[(1) - (4)].u.expr))),
				       (yyvsp[(3) - (4)].u.expr));  ;}
    break;

  case 356:

/* Line 1455 of yacc.c  */
#line 1591 "c-parse.y"
    { (yyval.u.string) = (yyvsp[(1) - (1)].u.string); ;}
    break;

  case 357:

/* Line 1455 of yacc.c  */
#line 1593 "c-parse.y"
    { (yyval.u.string) = string_chain((yyvsp[(1) - (3)].u.string), (yyvsp[(3) - (3)].u.string)); ;}
    break;

  case 358:

/* Line 1455 of yacc.c  */
#line 1599 "c-parse.y"
    { pushlevel(TRUE); ;}
    break;

  case 359:

/* Line 1455 of yacc.c  */
#line 1601 "c-parse.y"
    { (yyval.u.decl) = (yyvsp[(2) - (2)].u.decl);
		  /* poplevel() is done when building the declarator */
		;}
    break;

  case 360:

/* Line 1455 of yacc.c  */
#line 1607 "c-parse.y"
    { (yyval.u.decl) = (yyvsp[(1) - (2)].u.decl); ;}
    break;

  case 361:

/* Line 1455 of yacc.c  */
#line 1609 "c-parse.y"
    { if (pedantic)
		    pedwarn("ANSI C forbids forward parameter declarations");
#if 0
		  /* Mark the forward decls as such.  */
		  for (parm = getdecls (); parm; parm = TREE_CHAIN (parm))
		    TREE_ASM_WRITTEN (parm) = 1;
#endif
		  ;}
    break;

  case 362:

/* Line 1455 of yacc.c  */
#line 1618 "c-parse.y"
    { (yyval.u.decl) = (yyvsp[(4) - (4)].u.decl); ;}
    break;

  case 363:

/* Line 1455 of yacc.c  */
#line 1620 "c-parse.y"
    { (yyval.u.decl) = new_error_decl(pr, last_location); ;}
    break;

  case 364:

/* Line 1455 of yacc.c  */
#line 1626 "c-parse.y"
    { (yyval.u.decl) = NULL; ;}
    break;

  case 365:

/* Line 1455 of yacc.c  */
#line 1628 "c-parse.y"
    { (yyval.u.decl) = new_error_decl(pr, last_location);
		  /* Gcc used to allow this as an extension.  However, it does
		     not work for all targets, and thus has been disabled.
		     Also, since func (...) and func () are indistinguishable,
		     it caused problems with the code in expand_builtin which
		     tries to verify that BUILT_IN_NEXT_ARG is being used
		     correctly.  */
		  error("ANSI C requires a named argument before `...'");
		;}
    break;

  case 366:

/* Line 1455 of yacc.c  */
#line 1638 "c-parse.y"
    { (yyval.u.decl) = (yyvsp[(1) - (1)].u.decl); ;}
    break;

  case 367:

/* Line 1455 of yacc.c  */
#line 1640 "c-parse.y"
    { (yyval.u.decl) = declaration_chain((yyvsp[(1) - (3)].u.decl), CAST(declaration, new_ellipsis_decl(pr, (yyvsp[(3) - (3)].u.itoken).location))); ;}
    break;

  case 369:

/* Line 1455 of yacc.c  */
#line 1646 "c-parse.y"
    { (yyval.u.decl) = declaration_chain((yyvsp[(1) - (3)].u.decl), (yyvsp[(3) - (3)].u.decl)); ;}
    break;

  case 370:

/* Line 1455 of yacc.c  */
#line 1653 "c-parse.y"
    { (yyval.u.decl) = declare_parameter((yyvsp[(3) - (4)].u.declarator), current_declspecs, (yyvsp[(4) - (4)].u.attribute),
					 prefix_attributes, FALSE);
		  pop_declspec_stack(); ;}
    break;

  case 371:

/* Line 1455 of yacc.c  */
#line 1657 "c-parse.y"
    { (yyval.u.decl) = declare_parameter((yyvsp[(3) - (4)].u.declarator), current_declspecs, (yyvsp[(4) - (4)].u.attribute),
					 prefix_attributes, FALSE);
		  pop_declspec_stack(); ;}
    break;

  case 372:

/* Line 1455 of yacc.c  */
#line 1661 "c-parse.y"
    { (yyval.u.decl) = declare_parameter((yyvsp[(3) - (4)].u.declarator), current_declspecs, (yyvsp[(4) - (4)].u.attribute),
					 prefix_attributes, TRUE);
		pop_declspec_stack(); ;}
    break;

  case 373:

/* Line 1455 of yacc.c  */
#line 1665 "c-parse.y"
    { (yyval.u.decl) = declare_parameter((yyvsp[(3) - (4)].u.declarator), current_declspecs, (yyvsp[(4) - (4)].u.attribute),
					 prefix_attributes, FALSE);
		  pop_declspec_stack(); ;}
    break;

  case 374:

/* Line 1455 of yacc.c  */
#line 1669 "c-parse.y"
    { (yyval.u.decl) = declare_parameter((yyvsp[(3) - (4)].u.declarator), current_declspecs, (yyvsp[(4) - (4)].u.attribute),
					 prefix_attributes, TRUE);
		  pop_declspec_stack(); ;}
    break;

  case 375:

/* Line 1455 of yacc.c  */
#line 1678 "c-parse.y"
    { pushlevel(TRUE); ;}
    break;

  case 376:

/* Line 1455 of yacc.c  */
#line 1680 "c-parse.y"
    { (yyval.u.decl) = (yyvsp[(2) - (2)].u.decl);
		  /* poplevel is done when building the declarator */ ;}
    break;

  case 378:

/* Line 1455 of yacc.c  */
#line 1686 "c-parse.y"
    { (yyval.u.decl) = (yyvsp[(1) - (2)].u.decl); ;}
    break;

  case 379:

/* Line 1455 of yacc.c  */
#line 1692 "c-parse.y"
    { (yyval.u.decl) = (yyvsp[(1) - (1)].u.decl); ;}
    break;

  case 380:

/* Line 1455 of yacc.c  */
#line 1694 "c-parse.y"
    { (yyval.u.decl) = declaration_chain((yyvsp[(1) - (3)].u.decl), (yyvsp[(3) - (3)].u.decl)); ;}
    break;

  case 381:

/* Line 1455 of yacc.c  */
#line 1698 "c-parse.y"
    { (yyval.u.decl) = declare_old_parameter((yyvsp[(1) - (1)].idtoken).location, (yyvsp[(1) - (1)].idtoken).id); ;}
    break;

  case 382:

/* Line 1455 of yacc.c  */
#line 1703 "c-parse.y"
    { (yyval.u.id_label) = (yyvsp[(1) - (1)].u.id_label); declare_label((yyvsp[(1) - (1)].u.id_label)); ;}
    break;

  case 383:

/* Line 1455 of yacc.c  */
#line 1705 "c-parse.y"
    { (yyval.u.id_label) = id_label_chain((yyvsp[(1) - (3)].u.id_label), (yyvsp[(3) - (3)].u.id_label));
		  declare_label((yyvsp[(3) - (3)].u.id_label)); ;}
    break;

  case 384:

/* Line 1455 of yacc.c  */
#line 1711 "c-parse.y"
    { (yyval.u.telement) = NULL; ;}
    break;

  case 385:

/* Line 1455 of yacc.c  */
#line 1712 "c-parse.y"
    { (yyval.u.telement) = (yyvsp[(1) - (1)].u.telement); ;}
    break;

  case 386:

/* Line 1455 of yacc.c  */
#line 1717 "c-parse.y"
    { (yyval.u.itoken).location = (yyvsp[(1) - (1)].u.itoken).location;
		  (yyval.u.itoken).i = pedantic;
		  pedantic = 0; ;}
    break;

  case 387:

/* Line 1455 of yacc.c  */
#line 1723 "c-parse.y"
    { (yyval.u.telement) = CAST(type_element, new_rid(pr, (yyvsp[(1) - (1)].u.itoken).location, (yyvsp[(1) - (1)].u.itoken).i)); ;}
    break;

  case 388:

/* Line 1455 of yacc.c  */
#line 1727 "c-parse.y"
    { (yyval.u.telement) = CAST(type_element, new_qualifier(pr, (yyvsp[(1) - (1)].u.itoken).location, (yyvsp[(1) - (1)].u.itoken).i)); ;}
    break;

  case 389:

/* Line 1455 of yacc.c  */
#line 1731 "c-parse.y"
    { (yyval.u.telement) = CAST(type_element, new_qualifier(pr, (yyvsp[(1) - (1)].u.itoken).location, (yyvsp[(1) - (1)].u.itoken).i)); ;}
    break;

  case 390:

/* Line 1455 of yacc.c  */
#line 1735 "c-parse.y"
    { (yyval.u.telement) = CAST(type_element, new_rid(pr, (yyvsp[(1) - (1)].u.itoken).location, (yyvsp[(1) - (1)].u.itoken).i)); ;}
    break;



/* Line 1455 of yacc.c  */
#line 5277 "c-parse.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 1675 of yacc.c  */
#line 1739 "c-parse.y"


