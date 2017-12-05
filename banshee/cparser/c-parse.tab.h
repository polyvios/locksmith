
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
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

extern YYSTYPE yylval;


