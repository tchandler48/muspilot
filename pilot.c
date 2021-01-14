/sys region=4096
/file obj1 n(pilot.obj) new(repl)
/inc watc
/job nogo

/* pilot.c - main() function for RPilot   */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>
#include <string.h>

#include "rpilot.h"
#include "pmain.h"
#include "rline.h"
#include "rstack.h"
#include "rlabel.h"
#include "rcondex.h"
#include "rvar.h"
#include "rcmds.h"
#include "rdebug.h"
#include "rerr.h"
#include "rstring.h"
#include "rpinfo.h"
#include "rbind.h"
#include "rinter.h"
#include "rmath.h"
#include "rcalc.h"

struct rpinfo 
{
  struct line *currline;  	/* code that is currently being executed */
  struct line *linehead;  	/* the head of the line list */
  struct label *lblhead;  	/* head of the label list */
  struct numvar *numhead;    
  struct strvar *strhead;
  struct stack *stk;
  int error;          	/* the last error that occured */
  int status;         /* the status of the program (one of the STAT_ values) */
  char *lastacc;      /* the name of the last variable to be accepted */
  int strict;         /* should we be strict about undeclared variables? */
  char *filename;     /* name of file being run */
};

struct rpinfo *rpi;

  int showban  = TRUE;
  int inter 	 = FALSE;
  int bind 	 = FALSE;
  int mainfunc = FALSE;
  int showhelp = FALSE;
  char *filename=NULL, *outfile=NULL, *dataname=NULL, *funcname=NULL;

#define NO_INTER 0

int next_num( char *str, int *pos, int *status );
char next_tok( char *str, int *pos );
int find_match( char *str, int pos, char what, char match );
int read_num( char *str, int *pos );
int read_var( char *str, int *pos );
void run_bound( char *code[] );
int calc( char *expr, int *status );



int main(int argc, char *argv[])
{
  int i;

  for( i=1; i < argc; i++ ) 
  {

    /* printf("argv[%d] = %s\n", i, argv[i] ); */

    if( !strcasecmp(argv[i], "-b") ) 
    {
      showban = FALSE;
    } else if( !strcasecmp(argv[i], "-i") ) {
      inter = TRUE;
    } else if( !strcasecmp(argv[i], "-?") ) {
      showhelp = TRUE;
    } else if( !strcasecmp(argv[i], "-c") ) {
      bind = TRUE;
    } else if( !strcasecmp(argv[i], "-o") ) {

      if( argc == i+1 ) 
      {
	err( NO_CLARGS, "-o" );
      } else {
	outfile = new_str( argv[i+1] );
	i++;
      }
    } else if( !strcasecmp(argv[i], "-d") ) {
      if( argc == i+1 ) {
	err( NO_CLARGS, "-d" );
      } else {
	dataname = new_str( argv[i+1] );
	i++;
      }
    } else if( !strcasecmp(argv[i], "-f") ) {
      if( argc == i+1 ) {
	err( NO_CLARGS, "-f" );
      } else {
	funcname = new_str( argv[i+1] );
	i++;
      }
    } else if( !strcasecmp(argv[i], "-m") ) {
      mainfunc = TRUE;
    } else {
      filename = new_str( argv[i] );
    }
  }

/*
printf("filename=\"%s\", outfile=\"%s\", dataname=\"%s\", funcname=\"%s\"\n", 
                     filename, outfile, dataname, funcname ); 
printf("showban=%d, inter=%d, mainfunc=%d\n", showban, inter, mainfunc ); 
*/

  if( argc == 1 )   			/* no arguments */
  {
    banner();
  } else {  				/* there were arguments */
    if( showhelp == TRUE ) {
      if( showban == TRUE ) { banner(); }
      help(); 
    } else if( inter == TRUE ) {
      if( showban == TRUE ) { banner(); }
      interact();
    } else if( filename == NULL ) {
      if( showban == TRUE ) { banner(); }
      err( NO_FILE, "" );
    } else if( bind == TRUE ) {
      bindfile( filename, outfile, dataname, funcname, mainfunc );
    } else {
      run( filename );
    }
  }

  return 0;
}


void help() 
{
  printf( "Usage: rpilot [switch] filename\n" );
  printf( "Switch \t Action\n" );
  printf( "-b \t Suppress banner printing on startup\n" );
  printf( "-i \t Enter interactive mode when no file names are given\n");
  /*  printf( "-c \t Bind a file as a C program\n" ); */
  printf( "-? \t Print this help message, then exit\n\n" );

}

void banner()
{
  printf( "RPilot: Rob's PILOT Interpreter, version %s\n", VERSION );
  printf( "Copyright 1998,2002 Rob Linwood (rcl211@nyu.edu)\n" );
  printf( "RPilot is Free Software. Please see http://rpilot.sf.net/\n" );
  printf( "For help, try `rpilot -?'\n\n" );
}


/* rbind.c   */

char *filebase( char *filename )
{
  int dotpos, i;

  dotpos = -1;
  for( i=strlen(filename)-1; i>=0; i-- ) 
  {
    if( filename[i] == '.' ) {
      dotpos = i;
      break;
    }
  }

  if( dotpos == -1 ) 
  {
    return filename;
  } else { 
    return new_strf( filename, 0, dotpos );
  }    
}

void bindfile( char *filename, char *outfile, char *dataname, char *funcname, \
	       int mainfunc )
{
  FILE *header, *source;
  char *base, *hfile, *srcfile;
  line *curr;
  char *buffer;
  int i;

  if( outfile == NULL ) 
  {
    base = filebase( filename );
  } else {
    base = outfile;
  }


  hfile = (char *)malloc(strlen(base)+3);
  strcpy( hfile, base );
  strcat( hfile, ".h" );
  srcfile = (char *)malloc(strlen(base)+3);
  strcpy( srcfile, base );
  strcat( srcfile, ".c" );

  if( (header = fopen(hfile, "w")) == NULL ) 
  {
    err( ERR_FILE, hfile );
  }
  if( (source = fopen(srcfile, "w")) == NULL ) 
  {
    err( ERR_FILE, srcfile );
  }

  if( dataname == NULL ) 
  {
    dataname = new_str( "pilot_data" );
  }
  if( funcname == NULL ) 
  {
    funcname = new_str( "run_pilot" );
  }
    

  init();
  readfile( filename );

  /* start outputting to files */
  fprintf( header, "PILOT code datafile - generated by RPilot %s \n\n", \
	   VERSION );
  fprintf( header, "#ifndef _%s_h_\n#define _%s_h_\n\n", base, base );
  fprintf( header, "#include <stdio.h>\n\n" );
  fprintf( header, "void %s(void);\n", funcname );
  fprintf( header, "\n\n#endif" );
  
  fclose( header );

  fprintf( source, "PILOT code source file - generated by RPilot %s \n\n", \
	   VERSION );
  fprintf( source, "#include \"%s\"\n\n", hfile );
  fprintf( source, "extern void run_bound( char *code[] );\n\n" );
  fprintf( source, "void %s(void)\n{\n", funcname );
  fprintf( source, "char *%s[] = {\n", dataname );
 
  curr = (line *)rpi->linehead;
  curr = (line *)curr->next;  

  while( curr != NULL ) 
  {

    prt_ln( curr );
    printf( "\n" );

    buffer = get_line( curr );
    fprintf( source, "\"" );
    for( i = 0; i<strlen(buffer); i++ ) {
      switch( buffer[i] ) {
      case '\"' : 
	fprintf( source, "\\\"" );
	break;
      case '\'' :
	fprintf( source, "\\\'" );
	break;
      case '\\' : 
	fprintf( source, "\\\\" );
	break;
      default:
	fprintf( source, "%c",  buffer[i] );
	break;
      }
    }
    fprintf( source, "\",\n" );
    free( buffer );  
    /*
        fprintf( source, "  \"" );
        print_line_to( curr, source );
        fprintf( source, "\",\n" );
    */
    curr = (line *)curr->next;
  }
    
  fprintf( source, "NULL\n};\n\n" );

  fprintf( source, "run_bound( %s );\n}\n\n", dataname );
  
  if( mainfunc == TRUE ) {
    fprintf( source, "int main( int argc, char *argv[] )\n{\n" );
    fprintf( source, "  %s();\n  return 0;\n}\n\n", funcname );
  }

  fclose( source );

}


/* run some bound code */
void run_bound( char *code[] )
{
  line *curr;
  int i = 0;

  init();
  rpi->filename = new_str( "<<Bound Code>>" );

  curr = (line *)rpi->linehead;
  
  while( code[i] != NULL ) 
  {
    printf( "code[%d] is \"%s\"\n", i, code[i] );
    curr->next = (struct line *)new_line( code[i], '!', i+1 );
    curr = (line *)curr->next;
    i++;
  }

  curr = (line *)rpi->linehead;
  rpi->linehead = curr->next;
  interp();
}


/*
 * calc.c - handle simple mathematical expressions
 * rob - started july.25.2000
 * 
 * updates:
 *   - got around to finishing it - aug.11.2000
 *   - RPilot special code - aug.11.2000
 */

int calc( char *expr, int *status )
{
  int pos = 0;
  int num = 0, result = 0;
  char op = 0; 

  total_trim( expr );
  result = next_num( expr, &pos, status );

  while( pos < strlen(expr) ) 
  {
    op = next_tok( expr, &pos );
    num = next_num( expr, &pos, status );


    switch( op ) 
    {
    case 0 :  	/* invalid operand */
      *status = CALC_NO_OP;
      return 0;
      break;
    case '+' :
      result += num;
      break;
    case '-' :
      result -= num;
      break;
    case '/' :
      result /= num;
      break;
    case '*' :
      result *= num;
      break;
    case '%' :
      result %= num;
      break;
    case '&' : 
      result &= num;
      break;
    case '|' :
      result |= num;
      break;
    case '^' :
      result ^= num;
      break;
    default:
      *status = CALC_BAD_OP;
      return 0;
      break;
    }
  }

  *status = CALC_SUCCESS;
  return result;
}


int next_num( char *str, int *pos, int *status )
{
  char *inparen, *tempstr;
  int result, rparen;
  int mult = 1;

  *pos = wspace( str, *pos );
  
  if( str[*pos] == '-' ) 
  {
    mult = -1;
    *pos += 1;
  }
  if( str[*pos] == '(' ) 
  {
    rparen = find_match( str, *pos+1, ')', '(' );
    inparen = new_strf( str, *pos+1, rparen-*pos-1 );
    
    *pos = rparen+1;

    result = calc( inparen, status );
    free( inparen );
  } else if( str[*pos] == '#' ) {  	
    result = read_var( str, pos );
  } else {
    result = read_num( str, pos );
  }

  return result * mult;
}

/*
 * find_match()
 * Returns the position in the string `str' of the matching character.
 * Example: find_match( "((8*8)+9)/2", 1, ')', '(' ) => 8
 */

int find_match( char *str, int pos, char what, char match )
{
  int levels = 1;
  int i = pos;

  do {
    if( str[i] == what ) {
      levels--;
    } else if( str[i] == match ) {
      levels++;
    }
    i++;
  } while( levels != 0 );

  return i-1;
}


int read_num( char *str, int *pos )
{
  int start;
  int numchars;
  char *num;
  int result;

  start = wspace( str, *pos );
  numchars = start;

  while( isdigit(str[numchars]) ) 
  {
    numchars++;
  }

  num = new_strf( str, start, numchars - start );

  *pos = numchars;

  result = atoi( num );
  free( num );

  return result;
}
  

char next_tok( char *str, int *pos )
{
  int nows = wspace( str, *pos );
  *pos = nows;

  *pos += 1;  	
  return str[nows];
}


int read_var( char *str, int *pos )
{
  int start;
  int numchars;
  char *var;
  int result;

  start = *pos;
  numchars = start + 1;

  while( isalpha(str[numchars]) ) 
  {
    numchars++;
  }

  var = new_strf( str, start, numchars - start );

  /*  printf( "*** read_var(): var = \"%s\"\n", var ); */

  *pos = numchars;

  result = get_nr( var );
  free( var );

  return result;
}

 
/* cmds.c - RPilot commands functions */

/*
 * internal_type takes a string and formats it like it would be for a T:
 * statement, but returning a string rather than printing it to the screen 
 */

char *internal_type( char *str )
{
  int i;
  int nextsp;
  char *varname;
  char output[500], *ret;


 if( str == NULL ) 
 {
   return new_str("");
 }


 /* output = (char *)malloc(5000); */
 strcpy( output, "" );
 
  for( i=0; i<strlen(str); i++ ) 
  {
   /* if( (str[i] == '$') && (!isblank(str[i+1])) )  */
      if( (str[i] == '$') && ((str[i+1] != ' ')) )
      {
      if( str[i+1] == '$' ) 
      {
	i+=2;
      } else {
	nextsp = find( str, " \t", i );  	
	if( nextsp == -1 ) {   
	  varname = new_strf( str, i, strlen(str)-i );
	} else {
	  varname = new_strf( str, i, nextsp-i );
	}
	sprintf( output, "%s%s ", output, get_stvr( varname) );
	i+=strlen( varname );
	free( varname );
      }
 	/* if( (str[i] == '#') && (!isblank(str[i+1])) ) */
    } else if( (str[i] == '#') && ((str[i+1] != ' ')) ) 
      {
      if( str[i+1] == '#' ) {
	i+=2;
      } else {
	nextsp = find( str, " \t", i );  
	if( nextsp == -1 ) {  
	  varname = new_strf( str, i, strlen(str)-i );
	} else {
	  varname = new_strf( str, i, nextsp-i );
	}
	sprintf( output, "%s%d ", output, get_nr( varname) );
	i+=strlen( varname );
	free( varname );
      }
    } else {
      sprintf( output, "%s%c", output, str[i] );
    }
    /* printf( "rcmds.c output=\"%s\"\n", output ); */

  }

  ret = new_str( output );

/*
  free( output );
  printf("rcmds.c !!output=\"%s\", len=%d\n", output, strlen(output) );
  printf("rcmds.c !!ret=\"%s\", len=%d\n", ret, strlen(ret) );
*/
  
  return ret;
}


/* the cmd_ functions implement the PILOT statements they're named after */

void cmd_use( char *str )
{
  use( str );  
}

void cmd_compute( char *str )
{
  char *lside, *rside;
  int eqlpos;
  int i;

  if( str == NULL ) 
  {
    return;
  }

  trim( str );

  for( eqlpos=0; eqlpos<strlen(str); eqlpos++ ) 
  {
    if( str[eqlpos] == '=' ) 
    {
      break;
    }
  }

  if( eqlpos == strlen(str) ) 
  {
    err( NO_EQL, str );
  }

  lside = new_strf( str, 0, eqlpos );
  rside = new_strf( str, eqlpos+1, strlen(str)-eqlpos-1 );
  trim( lside );
  trim( rside );
  
  if( lside[0] == '#' ) { 
    set_numvar( lside, express(rside) );
  } else if( lside[0] == '$' ) { 
    set_strvar( lside, internal_type(rside) );
  } else {
    err( CONS_ASGN, lside );
  }
  free( lside );
  free( rside );
}



void cmd_accept( char *str )
{
  char inbuf[MAXLINE];
  int i;

  fflush( stdin );
  printf( "%s ", get_stvr("$PROMPT") );  

  trim( str );

  if( !strcmp(str, "") ) { 
    fgets( inbuf, MAXLINE-1, stdin );
    chop( inbuf );  		
    set_strvar( "$ACCEPT", inbuf );
    free( rpi->lastacc );
    rpi->lastacc = new_str( "$ACCEPT" );
  } else if( str[0] == '$' ) { 	
    fgets( inbuf, MAXLINE-1, stdin );
    chop( inbuf );
    if( !strcmp(inbuf, "") ) {  	
      strcpy( inbuf, "[BLANK]" );  
    }
    set_strvar( str, inbuf );
    free( rpi->lastacc );
    rpi->lastacc = new_str( str );
  } else if( str[0] == '#' ) { 	
    fgets( inbuf, MAXLINE-1, stdin );
    chop( inbuf );
    i = atoi( inbuf );
    /*    scanf( "%d", &i ); */
    set_numvar( str, i );
    free( rpi->lastacc );
    rpi->lastacc = new_str( str );
  }

  fflush(stdin);
}


void cmd_type( char *str )
{
/*   int i; */
/*   int nextsp; */
/*   char *varname; */

/*  if( str == NULL ) { */
/* //puts( "T: empty arguments (str == NULL)" ); */
/*    return; */
/*  } */

/*   for( i=0; i<strlen(str); i++ ) { */
/*     if( (str[i] == '$') && (!isblank(str[i+1])) ) { */
/*       if( str[i+1] == '$' ) { */
/* 	i+=2; */
/*       } else { */
/* 	nextsp = find( str, " \t", i );  // find next space */
/* 	if( nextsp == -1 ) {  // no more spaces, use the rest of the line */
/* 	  varname = new_strf( str, i, strlen(str)-i ); */
/* 	} else { */
/* 	  varname = new_strf( str, i, nextsp-i ); */
/* 	} */
/* 	printf( "%s ", get_stvr( varname) ); */
/* 	i+=strlen( varname ); */
/* 	free( varname ); */
/*       } */
/*     } else if( (str[i] == '#') && (!isblank(str[i+1])) ) { */
/*       if( str[i+1] == '#' ) { */
/* 	i+=2; */
/*       } else { */
/* 	nextsp = find( str, " \t", i );  // find next space; */
/* 	if( nextsp == -1 ) {  // no more spaces, use the rest of the line */
/* 	  varname = new_strf( str, i, strlen(str)-i ); */
/* 	} else { */
/* 	  varname = new_strf( str, i, nextsp-i ); */
/* 	} */
/* 	printf( "%d ", get_nr( varname) ); */
/* 	i+=strlen( varname ); */
/* 	free( varname ); */
/*       } */
/*     } else { */
/*       putchar( str[i] ); */
/*     } */
/*   } */
  
  char *output;

  output = internal_type( str );
  printf( "%s\n", output );
  free( output );
  
}


void cmd_end( char *str )
{
  rpi->currline = (struct line *)stk_pop( (stack *)rpi->stk );
}


void cmd_match( char *str )
{
  int count = numstr( str );
  int i;
  /*  char *temp = (char *)malloc( strlen(str)+1 ); */
  char *temp = NULL;

  strupr( str );
  for( i=1; i<count+1; i++ ) { 	
    /*    strset( temp, 0 ); */
    temp = parse( str, i );
    if( !strcmp(strupr(get_stvr(rpi->lastacc)), temp) ) {
      set_numvar( "#MATCHED", 1 );
      set_numvar( "#WHICH", i );
      return;
    }
  }
  
  free( temp );

  set_numvar( "#MATCHED", 0 );
  set_numvar( "#WHICH", 0 );  
}


void cmd_jump( char *str  )
{
  jump( str );
}


void cmd_execute( char *str )
{
  execute( str );
}

/* Types a line if #matched == TRUE */
void cmd_yes( char *str )
{
  if( get_nr( "#MATCHED") == TRUE ) 
  {
    cmd_type( str );
  }
}


/* Types a line if #matched == FALSE */
void cmd_no( char *str )
{
  if( get_nr("#MATCHED") == FALSE ) 
  {
    cmd_type( str );
  }
}


/* call a shell */
void cmd_shell( char *str )
{
  int retcode;

  retcode = system( get_stvl(str) );
  set_numvar( "#RETCODE", retcode );
}


void cmd_debug( char *str )
{
  debug();
}


void cmd_generate( char *str )
{
  int upper, lower, rnd;
  char *exp, *var;

  if( !strcmp(str, "") ) 
  {
    /* do what? */
  } else {
    trim( str );
  }

  var = parse( str, 1 );
  exp = parse( str, 2 );
  lower = get_nv( exp );
  free( exp );
  exp = parse( str, 3 );
  upper = get_nv( exp );

  srand( (unsigned)time(NULL) );

  /*  rnd = lower + (int)( (upper * rand()) / RAND_MAX ); */
  rnd = lower+(int) ((float)upper*rand()/(RAND_MAX+1.0));

  set_numvar(  var, rnd );
  free( exp );
  free( var );
}


/* condex.c - conditional expression handling */

condex *new_condex( char *str )
{
  int i = -1;
  condex *c;

  c = (condex *)malloc( sizeof(condex) );
  c->next = NULL;

  if( !strcmp(str, "") ) 
  {
    c->lside = NULL;
    c->rside = NULL;
    c->op = OP_NULL;
    return c;
  }

  if( !strcmp(str, "Y") ) 
  {
    c->op = OP_YES;
    return c;
  } else if( !strcmp(str, "N") ) {
    c->op = OP_NO;
    return c;
  }

  for(i=0; i<strlen(str); i++) {
    if( (str[i]=='=') || (str[i]=='>') || (str[i]=='<') || (str[i]=='!') ) 
    {
      break;
    }
  }

  /* we can't find a relational operator */
  if( i == strlen(str) ) {
    printf("str=\"%s\"\n", str );
    err( NO_RELAT, str );
    return NULL;
  }

  if( str[i] == '=' ) {
    c->op = OP_EQL;
  } else if( (str[i] == '>') && (str[i+1] != '=') ) {
    c->op = OP_GT;
  } else if( (str[i] == '<') && (str[i+1] != '=') && (str[i+1] != '>') ) {
    c->op = OP_LT;
  } else if( (str[i] == '>') && (str[i+1] == '=') ) {
    c->op = OP_GE;
  } else if( (str[i] == '<') && (str[i+1] == '=') ) {
    c->op = OP_LE;
  } else if( (str[i] == '<') && (str[i+1] == '>') ) {
    c->op = OP_NEQL;
  } else { 		
    err( BAD_RELAT, str );
  }

  if( (c->op==OP_EQL) || (c->op==OP_GT) || (c->op==OP_LT) ) {
    c->lside = new_strf( str, 0, i );
    c->rside = new_strf( str, i+1, strlen(str)-i );
  } else {
    c->lside = new_strf( str, 0, i );
    c->rside = new_strf( str, i+2, strlen(str)-i-1 );
  }

  return c;
}

/* hack to support binding */
void prt_cd_t( condex *curr, FILE *stream )
{
  fprintf( stream, "%s ", curr->lside );
  switch( curr->op ) {
  case OP_EQL : fprintf( stream, "=" );
    break;
  case OP_NEQL : fprintf( stream, "<>" );
    break;
  case OP_GT : fprintf( stream, ">" );
    break;
  case OP_LT : fprintf( stream, "<" );
    break;
  case OP_GE : fprintf( stream, ">=" );
    break;
  case OP_LE : fprintf( stream, "<=" );
    break;
  case OP_YES : fprintf( stream, "Y" );
    break;
  case OP_NO : fprintf( stream, "N" );
    break;
  }
  fprintf( stream, " %s", curr->rside );
}


char *get_condex( condex *curr )
{
  char *buffer;

  buffer = (char *)malloc(1024);
  
  sprintf( buffer, "%s ", curr->lside );
  
  switch( curr->op ) {
  case OP_EQL : strcat( buffer, "=" );
    break;
  case OP_NEQL : strcat( buffer, "<>" );
    break;
  case OP_GT : strcat( buffer, ">" );
    break;
  case OP_LT : strcat( buffer, "<" );
    break;
  case OP_GE : strcat( buffer, ">=" );
    break;
  case OP_LE : strcat( buffer, "<=" );
    break;
  case OP_YES : strcat( buffer, "Y" );
    break;
  case OP_NO : strcat( buffer, "N" );
    break;
  }
  sprintf( buffer, " %s", curr->rside );

  return buffer;
}

void prt_cd( condex *curr )
{
  prt_cd_t( curr, stdout );
}


void prt_cd_l( condex *head )
{
  condex *curr = head;
  
  while( curr ) {
    prt_cd( curr );
    printf( "\n" );
    curr = (condex *)curr->next;
  }
}


#ifdef TEST
int main( int argc, char *argv[] )
{

  FILE *f;
  char buf[256];
  condex *head, *curr;

  if( argc < 2 ) {
    puts( "condex: Usage condex filename" );
    return 0;
  }

  f = fopen( argv[1], "r" );

  head = new_condex("");
  curr = head;

  do {
    strset( buf, 0 );
    fgets( buf, 255, f );
    chop( buf );
    if( strcmp(buf, "") ) {
      curr->next = (struct condex *)new_condex( buf );
      curr = (condex *)curr->next;
    }
  } while( !feof(f) );

  
  prt_cd_l( head );

  return 0;
}
#endif


/* debug.c - RPilot's interactive debugger */

void debug(void)
{
  char inbuf[MAXLINE];
  char lastcmd[MAXLINE];

  do {
    strset( inbuf, 0 );
    printf( "debug> " );
    fgets( inbuf, MAXLINE-1, stdin );
    proc( inbuf, lastcmd );
  } while( strcasecmp(inbuf, "exit") );

}

void proc( char *inbuf, char *lastcmd )
{

  char *tok;

  if( !strcmp(inbuf, "" ) ) {
    strncpy( inbuf, lastcmd, MAXLINE-1 );
  }

  tok = parse( inbuf, 1 );
  
  if( !strcasecmp(tok, "step") ) {
    /*    step( rpi ); */
  } else if( !strcasecmp(tok, "skip") ) {
    /*    skip( rpi ); */
  } else if( !strcasecmp(tok, "print") ) {
    /*   print( rpi, inbuf ); */
  } else if( !strcasecmp(tok, "set") ) {
    /*    set( rpi, inbuf ); */
  } else if( !strcasecmp(tok, "list") ) {
    /*    list( rpi, inbuf ); */
  } else if( !strcasecmp(tok, "run") ) {
    /*    run( rpi, inbuf ); */
  } else if( !strcasecmp(tok, "stop") ) {
    /*    stop( rpi ); */
  } else if( !strcasecmp(tok, "help") ) {
    /*    help(); */
  } else if( !strcasecmp(tok, "jump") ) {
    /*    jump( rpi, inbuf ); */
  } else if( !strcasecmp(tok, "exec" ) ) {
    /*    exec( rpi, inbuf ); */
  } else if( !strcasecmp(tok, "use") ) {
    /*    use( rpi, inbuf ); */
  } else if( !strcasecmp(tok, "restart") ) {
    /*    restart( rpi ); */
  }

  free( tok );

}


void dump_numvars(void) 
{
  numvar *n = (numvar *)rpi->numhead;
  while( n != NULL ) 
  {
    printf( "\"%s\" = %d\n", n->name, n->val );
    n = (numvar *)n->next;
  }

}


/*  err.c - Error handling code for RPilot  */

int yesno( char *msg )
{
  char buf[MAXLINE];

  printf( "%s (Y/N) ", msg );
  fgets( buf, MAXLINE-1, stdin );

  chop( buf );
  if( (!strcmp(strupr(buf), "Y")) || (!strcmp(strupr(buf), "YES")) ) {
    return YES;
  } else {
    return NO;
  }
}

int err( int errnum, char *msg )
{
  char *errbuf;
  int linenum;
  line *l;
  
  if( rpi != NULL ) {
    l = (line *)rpi->currline;
    linenum = l->linenum;
  } else {
    linenum = -1;
  }

  errbuf = (char *)malloc( strlen(errstr(errnum)) + strlen(msg) );
  sprintf( errbuf, errstr(errnum), msg );

  if( rpi != NULL ) {
    printf( "RPilot Error in %s, line %d\n   %s\n", 
	    rpi->filename, linenum, errbuf );
  } else {
    printf( "RPilot Error : %s\n", errbuf );
  }
  exit( errnum );
}

char *errstr( int errnum )
{

  char *errlist[] = {
    "Duplicate label `%s'",                                /* DUP_LABEL  */
    "No file name specified",                              /* NO_FILE    */
    "Can't open file `%s'",                                /* ERR_FILE   */ 
    "Unknown command `%s'",                                /* UNKNWN_CMD */
    "Out of memory!",                                      /* NO_MEM     */
    "Duplicate variable `%s'",                             /* DUP_VAR    */
    "Unknown variable `%s'",                               /* BAD_VAR    */
    "Expected math symbol, not `%s'",                      /* EXP_MATH   */
    "Missing relational operator",                         /* NO_RELAT   */
    "Missing colon in statement",                          /* NO_COLON   */
    "Unknown label: `%s'",                                 /* BAD_LABEL  */
    "Unknown relational operator: `%s'",                   /* BAD_RELAT  */
    "No equal sign in assignment",                         /* NO_EQL     */
    "Missing right parentheses in conditional expression", /* NO_RPAREN  */
    "Cannot assign value to constant `%s'",                /* CONS_ASGN  */
    "Missing command line argument after `%s'"             /* NO_CLARG   */
  };

  if( errnum >= 0 ) {
    return errlist[errnum];
  } else {
    return "";
  }
}


/* interact.c - handle interactive mode */

#ifndef NO_INTER

  #include "rinter.h"

#endif

void interact()
{
  FILE *f;

  if( (f = fopen("interact.p", "r")) == NULL ) {
    if( (f = fopen(getenv(ENV_VAR), "r")) == NULL ) {
#ifndef NO_INTER
      inter();
#else
      puts( "Can't run interactive mode. RPilot was compiled with NO_INTER." );
      exit( 1 );
#endif
    } else {
      run( getenv(ENV_VAR) );
    }
  } else {
    run( "interact.p" );
  }
}



/* label.c - label handling code */

label *new_label( char *name, line *lne, int linenum )
{
  label *l;

  l = (label *)malloc( sizeof(label) );
  l->linenum = linenum;
  l->stmnt = lne;
  l->name = new_str( name );
  l->next = NULL;

  return l;
}


label *get_label( char *name )
{
  label *l = (label *)rpi->lblhead;

  while( l != NULL ) {
    if( !strcasecmp(l->name, name) ) {
      return l; 
    }
    l = (label *)l->next;
  }
  err( BAD_LABEL, name );
}


void prt_lb( label *l )
{
    printf( "[line %d] %s: ", l->linenum, l->name );
    prt_ln( l->stmnt );
    printf( "\n" );
}

void prt_lb_l()
{
  label *l = (label *)rpi->lblhead;
  while( l != NULL ) {
    prt_lb( l );
    l = (label *)l->next;
  }

}


/*  * line.c - RPilot syntax handling routines  */


/* the input str should already be trim()ed */

line *new_line( char *str, char lastcmd, int linenum )
{
  line *l = (line *)malloc( sizeof(line) );
  int colon; 
  int rparen, lparen;
  int i;
  char *contemp = NULL;  /* temp variable for condex string */


  colon = findchar( str, ':' );  /* get the position of the colon */
  l->next = NULL;
  l->linenum = linenum;

  /* is it a blank string? */
  if( !strcmp(str, "") ) {
    l->cmd = lastcmd;
    l->args = NULL;
    l->cond = NULL;
    return l;
  }
  
  if( colon == -1 ) {  /* if there is no colon, it's an error */
    err( NO_COLON, str );
  }


  /* Notes for the 3/3/00 rewrite
   * There are 3 different possiblities for what will be the first character
   * in the line:
   *
   * 1) A letter signifing the command name (this includes 'Y' and 'N')
   * 2) A colon, meaning that we should use the last command for this one, too
   * 3) A left parentheses, like above, but with a conditional expression
   *
   */

  if( colon != strlen(str)-1 ) {
    l->args = new_strf( str, colon+1, strlen(str)-colon+1 );
    ltrim( l->args );
  } else {
    l->args = new_str( "" );
  }


  /* 
   * Here, we'll handle the second case first, because it is the easiest.
   * The command is the same as the last command, and everything after the
   * first character constitutes the arguments.
   */

  if( str[0] == ':' ) {
    l->cmd = lastcmd;
    l->cond = NULL;
    return l;
  }


  /*
   * Now the third case.  Like the second case, everything after the colon
   * forms the arguments.  But we have to find the right parentheses in order
   * to create a conditional expression.  The command is the same as the
   * previous one.
   */

  if( str[0] == '(' ) {
    l->cmd = lastcmd;

    rparen = findchar( str, ')' );
    if( rparen == -1 ) {
      /* If there is no right parentheses, we signal an error */
      err( NO_RPAREN, str );
    }
/* 
     otherwise, create a temporary string, and make a new condex
     contemp = new_strf( str, 1, rparen-1 ); 
*/
    contemp = new_strf( str, 1, rparen-1 );
    l->cond = new_condex( contemp );
    free( contemp );

    return l;
  }


  /*
   * Finally, the first case.  This itself has four possibilities:
   *
   * 1) There is no conditional expression
   * 2) There is a conditional expression in parentheses
   * 3) There is a 'Y' as the conditional
   * 4) There is an 'N' as the conditional
   */
  
  l->cmd = str[0];
  /*  l->args = new_strf( str, colon+1, strlen(str)-colon+1 ); */

  lparen = findchar( str, '(' );
  if( (lparen == -1)|| (lparen > colon) ) {
    /* Now, we can see if there is a Y or N somehwere.. */
    for( i=1; i<colon; i++ ) {
      if( toupper(str[i]) == 'Y' ) {
	l->cond = new_condex( "Y" );
	return l;
      } else if( toupper(str[i]) == 'N' ) {
	l->cond = new_condex( "N" );
	return l;
      }
    }
    /* If we didn't find Y or N, there is no condex, so just return */
    l->cond = NULL;
    return l;
  }

  /* Now we have a conditional expression, so create a new condex and return */
  rparen = findchar( str, ')' );
  if( rparen == -1 ) {
    err( NO_RPAREN, str );
  }
  /*  contemp = new_strf( str, lparen+1, rparen-1 ); */
  contemp = new_strf( str, lparen+1, rparen-2 );
  l->cond = new_condex( contemp );
  free( contemp );

  return l;
}

/* support binding */
void prt_ln_t( line *curr, FILE *stream )
{
  if( curr == NULL ) {
    fprintf( stream, "[NULL]" );
    return;
  }
  fprintf( stream, "%c", curr->cmd );
  if( curr->cond != NULL ) {
    if( (curr->cond->op == OP_YES) || (curr->cond->op == OP_NO) ) {
      prt_cd_t( curr->cond, stream );
    } else {
      fprintf( stream, "(" );
      prt_cd_t( curr->cond, stream );
      fprintf( stream, ")" );
    }
  }
  fprintf( stream, ": %s", curr->args );
}

char *get_line( line *curr )
{
  char *buffer;
  char *cond;

  buffer = (char *)malloc(1024);
  
  if( curr == NULL ) {
    return "(null)";
  }

  sprintf( buffer, "%c", curr->cmd );
  if( curr->cond != NULL ) {
    if( (curr->cond->op == OP_YES) || (curr->cond->op == OP_NO) ) {
      sprintf( buffer, "%c", curr->cond->op );
    } else {
      cond = get_condex( curr->cond );
      sprintf( buffer, "(%s)", cond );
      free( cond );
    }
  }

  sprintf( buffer, ": %s", curr->args );

  return buffer;
}
      

void prt_ln( line *curr )
{
  prt_ln_t( curr, stdout );
}


void prt_ln_l( line *head ) 
{
  line *curr = head;
  
  while( curr ) {
    printf( "#%d ", curr->linenum );
    prt_ln( curr );
    printf( "\n" );
    curr = (line *)curr->next;
  }
}


#ifdef TEST

int main( int argc, char *argv[] )
{

  FILE *f;
  char buf[256];
  line *head, *curr;
  int linenum = 0;

  if( argc < 2 ) {
    puts( "line: Usage line filename" );
    return 0;
  }

  f = fopen( argv[1], "r" );

  head = new_line("", ' ', 0 );
  curr = head;

  do {
    strset( buf, 0 );
    fgets( buf, 255, f );
    chop( buf );
    trim( buf );
    puts( buf );
    if( strcmp(buf, "") ) {
      curr->next = (struct line *)new_line( buf, 'Q', ++linenum );
      curr = (line *)curr->next;
    }
  } while( !feof(f) );

  
  prt_ln_l( head );

  return 0;
}
#endif


/* math.c - mathematical functions for RPilot
 * 
 * major update - aug.11.2000
 *  - remove all garbage, express() remains only as a wrapper to calc()
 */

int express( char *form )
{
  int result, status;

  result = calc( form, &status );

  if( status != 0 )		/* error */
  { 
    err( EXP_MATH, "" );
  }
  return result;
}



/*  rpinfo.c   */

rpinfo *new_rpinfo()
{
  rpinfo *r;

  r = (rpinfo *)malloc( sizeof(rpinfo) );

  r->currline = NULL;
  r->linehead = NULL;
  r->lblhead = NULL;
  r->numhead = NULL;
  r->strhead = NULL;
  r->stk = NULL;
  r->error = ERR_NONE;
  r->status = 0;
  r->lastacc = NULL;
  r->strict = TRUE;
  r->filename = NULL;

  return r;
}


/*  rstack.c  */

stkitem *new_stkitem( line *lne )
{
  stkitem *si;

  si = (stkitem *)malloc( sizeof(stkitem) );
  si->item = lne;
  si->prev = NULL;
}


stack *new_stack( line *lne )
{
  stack *s;
  stkitem *si;

  s = (stack *)malloc( sizeof(stack) );
  si = new_stkitem( lne );

  s->head = si;
  s->tail = si;

  return s;
}


/* returns the new tail of the list */
stack *stk_push( stack *stk, line *lne )
{
  stkitem *si;

  si = new_stkitem( lne );
  (stkitem *)si->prev = stk->tail;  // worthless cast
  stk->tail = si;
  
  return stk;
}


line *stk_pop( stack *stk )
{
  stkitem *si;
  line *lne;

  si = stk->tail;

  stk->tail = (stkitem *)stk->tail->prev;  // worthless cast
  lne = si->item;
  free( si );

  return lne;
}



/*
 * rstring.c - generic string-handling functions
 * as of version 1.49, this contains the contents of the `parse' library
 */

/*
 * Name    : strupr
 * Descrip : Changes s to all uppercase
 * Input   : s = pointer to a string to uppercase
 * Output  : returns a pointer to s
 * Notes   : none
 * Example : strupr( "proper-noun" ) - returns "PROPER-NOUN"
 */

char *strupr( char *s )
{
  int i;

  for(i=0; i<strlen(s); i++ ) {
    s[i] = toupper(s[i]);
  }
  
  return s;
}

/*
 * Name    : strset
 * Descrip : Sets all characters in s to ch
 * Input   : s = pointer to a string to set
 *           ch = character to set all positions in s to 
 * Output  : returns a pointer to s
 * Notes   : None
 * Example : char cstring[10];
             strset( cstring, 'C' ) - returns "CCCCCCCCC"
 */

char *strset( char *s, int ch )
{
  unsigned char c;
  
  for(c=0; s[c] != 0; c++)
    s[c] = ch;
  
  return s;
}

char *new_str( char *src ) 		/* make a copy of a string */
{
  char *str;

  str = (char *)malloc( strlen(src)+1 );
  strncpy( str, src, strlen(src) );

  /* junk from being tacked on the end */
  if( strlen(str) > strlen(src) ) 
  {
    str[strlen(src)] = 0;
  }
  
  return str;
}

/* make a new string from src, starting at char index
   and going for count chars */
char *new_strf( char *src, int index, int count )
{
  char *str;

  str = (char *)malloc( count + 1 );
  memset( str, 0, count+1 );
  strncpy( str, src+index, count );

  /*
    str[strlen(str)+1] = 0;
    str[count+1] = 0;
  */

  return str;
}

/*
 * Name: firstnot - Finds first instance of a character which is not the
 *                     one specified
 * Input: d - Pointer to string to search through
 *        e - Char to search against
 *        first - Position in d to start the search at
 * Output: Returns the position of the first character which is not e.
 *         Returns -1 if there was an error
 * Example: firstnot( "ggggXgggX", "g", 0 ) returns 4
 */

int firstnot( const char *d, const char e, int first )
{
  char k;
  
  for(k=first; k<strlen(d); k++) {
    if( d[k] != e )
      return k;
  }
  return -1;
}

/*
 * Name: neither -  Finds the first character after the two specified
 * Input: d - Pointer to the string to search through
 *        e - The first char to search against
 *        f - The second char to search against
 *        first - The location in d to start searching at
 * Output: Returns the location of the first char which is not e or f.
 *         Returns -1 on errors
 * Notes: This is just like firstnot() except it takes to chars as args.
 * Example: neither( "ggggXgggXzgg", 'g', 'X', 0 ) returns 9.
 */

int neither( const char *d, const char e, const char f, char first )
{
  char k;
  
  for(k=first; k<strlen(d); k++) {
    if( (d[k] != e) && (d[k] != f) )
      return k;
  }
  return -1;
}

/*
 * Name: find - Search for any chars in the string e in string d
 * Input: d - pointer to a string to search through
 *        e - pointer to a list of chars to search for
 *        first - location in d to start search at
 * Output: Returns the location of the first occurence of a char in e in d.
 *         Returns -1 on errors
 * Example: find( "xrcedfg", "dg", 0 ) returns 4.
 */

int find( const char *d, const char *e, int first )
{
  int k, k2;
  
  for(k=first; k<strlen(d); k++) {
    for(k2=0; k2<strlen(e); k2++) {
      if( d[k] == e[k2] )
	return k;
    }
  }
  return -1;
}

/*
 * Name    : scopy
 * Descrip : Like the Pascal Copy function, scopy copies a portion of a
 *           string from src to dest, starting at index, and going for
 *           count characters.
 * Input   : dest - pointer to a string to recieve the copied portion
 *           src - pointer to a string to use as input
 *           index - character to start copying at
 *           count - number of characters to copy
 * Output  : dest contains the slice of src[index..index+count]
 * Notes   : None
 * Example : scopy( last, "Charlie Brown", 8, 5 ) - puts "Brown" in last
. */

char *scopy( char *dest, char *src, int index, int count )
{
  int k;
  
  for(k=0;k<=count;k++) {
    dest[k] = src[k+index];
  }
  dest[k] = '\0';
  return dest;
}

/*
 * Name: numstr - Returns number of substrings in a string
 * Input : d - a pointer to the string to search through
 * Output: returns number of substrings in string d
 * Example: numstr( "bob and    Figment  are THE  Bombz  " ) returns 6
 */
int numstr( const char *d )
{

  int k2, k3;
  int cnt = 0;

  k3 = -1;

  /* new version */  
  do {
    k2 = wspace( d, k3+1 ); /* find start of substring */
    if( k2 == -1 ) /* if there is no start, we return */
      return cnt;
    k3 = find( d, " \t", k2 ); /* find end of substring */
    if( k3 == -1 )
      return ++cnt;
    ++cnt;   /* increase counter if there is a start & a finish */
  } while( k3 != -1 );
  return -1;
}

/*
 * Name: parse - Returns specified substrings seperated by whitespace
 * Input: d - a pointer to the string to parse
 *		  i - the number of the substring you want
 *		  c - a pointer to the string where we place the substring
 * Output: Returns nonzero on errors and zero when there are no errors
 * Example: parse( " bob ate  cheese", 3, buffer ) places "cheese" in buffer
 */

char *parse( const char *src, int num )
{
  char *srccopy;
  char *ret = NULL;
  int currws, nextws;
  int i;
  
  srccopy = (char *)malloc(strlen(src)+1);
  strcpy( srccopy, src );
  
  total_trim( srccopy );
  
  if( !strcmp(srccopy, "") ) {
    return NULL;
  }
  
  currws = find( srccopy, " \t", 1 );

  if( currws == -1 ) { // no spaces found, so only one string available
    return srccopy;
  } 

  if( num == 1 ) {
    ret = new_strf( srccopy, 0, currws );
    free( srccopy );
    return ret;
  }
  
  /*
    01234567890123456789
    "hello said the fig"
  */          
  
  for( i=1; i<num; i++ ) {
    nextws = find( srccopy, " \t", currws+1 );
    if( i+1 != num ) {
      currws = nextws;
    }
  }

  if( nextws == -1 ) {  // last bit in src, no whitespace after
    ret = new_strf( srccopy, currws+1, strlen(srccopy)-currws-1 );
  } else {
    ret = new_strf( srccopy, currws+1, nextws-currws-1 );
  }

  free( srccopy );
  return ret;
}

/*
 * Name    : rtrim
 * Descrip : Removes all trailing whitespace from a string
 * Input   : str = pointer to a string to strip
 * Output  : Returns a pointer to str
 * Notes   : None.
 * Example : rtrim( "Bob was busy   " ) - returns "Bob was busy"
 */

char *rtrim( char *str )
{

  int i = strlen( str ) - 1;
  
  while( (isspace(str[i])) && (i>=0) )
    str[i--] = '\0';
  
  return str;
}

/*
 * Name    : ltrim
 * Descrip : Removes all leading whitespace from a string
 * Input   : str = pointer to a string to strip
 * Output  : Retruns a pointer to str
 * Notes   : None
 * Example : ltrim( "  Woof! " ) - returns "Woof! "
 */

char *ltrim( char *str )
{                                  
  int i;
  int spc = ws( str );

  if( spc == -1 ) {  // blank line
    return "";
  }
  
  for( i=0; i<strlen(str); i++ ) {
    str[i] = str[i+spc];
  }
  str[i] = 0;
  
  return str;
}

/*
 * Name    : rws
 * Descrip : Reverse WhiteSpace: Finds the last charater in a string which
 *           is not a space or a tab
 * Input   : str = pointer to a string to search through
 * Output  : Returns the position of the last non-whitespace character
 * Notes   : Just like, ws(), but backwards
 * Example : rws( "Hey, you!  " ) - returns 8
 */

int rws( const char *str)
{
  int k;
  
  for(k=strlen(str);k>-1;k--) {
    if( (str[k] != ' ') && (str[k] != '\t') )
      return k;
  }
  return -1;
}


/*
 * Name    : ws
 * Descrip : WhiteSpace: finds the first character which isn't a tab or space
 * Input   : str = pointer to string to use as input
 * Output  : Returns position of fitsrt non-whitespace character
 * Notes   : none
 * Example : ws( "   Howdy, world!" ) - returns 3
 */

int ws( const char *str)
{
  int k;

  for(k=0;k<strlen(str);k++) {
    if( (str[k] != ' ') && (str[k] != '\t') )
      return k;
  }
  return -1;
}


int wspace( const char *str, int first )
{
  return neither( str, ' ', '\t', first );
}


char *trim( char *str ) 
{
  if( !strcmp(str, "") ) {
    return "";
  }

  ltrim( str );
  rtrim( str );
  
  return str;
}



int findchar( const char *str, char c )
{
  int i;
 
  for(i=0; i<strlen(str); i++) {
    if( str[i] == c )
      return i;
  }
  return -1;
}



/*
**  total_trim() - Remove leading, trailing, & excess embedded spaces
**  from the Snippets collection (TRIM.C)
**  public domain by Bob Stout & Michael Dehlwes 
*/
char *total_trim( char *str )
{
  char *ibuf, *obuf;
 
  if( !strcmp(str, "") ) {
    return str;
  }

  if( str ) {
    for( ibuf = obuf = str; *ibuf; ){
      while (*ibuf && (isspace (*ibuf))) {
	ibuf++;
      }
      if (*ibuf && (obuf != str)) {
	*(obuf++) = ' ';
      }
      while (*ibuf && (!isspace (*ibuf))) {
	*(obuf++) = *(ibuf++);
      }
    }
    *obuf = 0;
  }
  return str;
}


#ifdef STANDALONE

int main( int argc, char *argv[] )
{
  char *tmp;
  const char *text = "  hello, said the       happy       figment!     ";
  int i;
  int num = numstr( text );
  
  printf( "%d bits in `text'\n", num );
  
  for( i=1; i<=num; i++ ) {
    tmp = parse( text, i );
    puts( tmp );
    free( tmp );
  }

  return 0;
}
#endif


/*   var.c - RPilot variable support */
  
strvar *new_strvar( char *name, char *val )
{
  strvar *ret;
  
  if( (ret = (strvar *)malloc(sizeof(strvar))) == NULL ) {
    err( NO_MEM, "" );
  }
  
  ret->name = new_str( name );
  ret->val = new_str( val );
  ret->next = NULL;

  return ret;
}

numvar *new_numvar( char *name, int val )
{
  numvar *ret;

  if( (ret = (numvar *)malloc(sizeof(numvar))) == NULL ) {
    err( NO_MEM, "" );
  }  

  ret->name = new_strf( name, 0, strlen(name) );
  ret->val = val;
  ret->next = NULL;
  return ret;
}

void set_strvar( char *name, char *val )
{
  strvar *s = (strvar *)rpi->strhead;
  strvar *prev;

  while( s != NULL ) {
    if( !strcasecmp(s->name, name) ) {
      free( s->val );
      s->val = new_str( val );
      return;
    }
    prev = s;
    s = (strvar *)s->next;
  }

  // if we can't find the variable, make a new one
  (strvar *)prev->next = new_strvar( name, val );
}

void set_numvar( char *name, int val )
{
  numvar *n = (numvar *)rpi->numhead;
  numvar *prev;

  while( n != NULL ) {
    if( !strcasecmp(n->name, name) ) {
      n->val = val;
      return;
    }
    prev = n;
    n = (numvar *)n->next;
  }

  (numvar *)prev->next = new_numvar( name, val );
}

char *get_stvr( char *name )
{
  strvar *s = (strvar *)rpi->strhead;

  while( s != NULL ) {
    if( !strcasecmp(s->name, name) ) {
      return s->val;
    } else {
      s = (strvar *)s->next;
    }
  }
  
  if( rpi->strict == TRUE ) {
    err( BAD_VAR, name );
  } else {
    set_strvar( name, "" );
    return "";
  }

  return NULL;
}

int get_nr( char *name )
{
  numvar *n = (numvar *)rpi->numhead;

  while( n != NULL ) {
    if( !strcasecmp(n->name, name) ) {
      return n->val;
    } else {
      n = (numvar *)n->next;
    }
  }

  if( rpi->strict == TRUE ) {
    err( BAD_VAR, name );
  } else {
    set_numvar( name, 0 );
    return 0;
  }

}

void prt_strvar( strvar *var )
{
  printf( "%s = \"%s\"\n", var->name, var->val );
}

void prt_numvar( numvar *var )
{
  printf( "%s = %d\n", var->name, var->val );
}

void prt_st_l( void )
{
  strvar *var = (strvar *)rpi->strhead;
  
  while( var != NULL ) {
    prt_strvar( var );
    var = (strvar *)var->next;
  }
}

void prt_nu_l( void )
{
  numvar *var = (numvar *)rpi->numhead;

  while( var != NULL ) {
    prt_numvar( var );
    var = (numvar *)var->next;
  }
}
