unit VariablesGlobals;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ConstantsGlobals;

var
  TOKEN_OPEN:string = '{';
  TOKEN_CLOSE:string = '}';
  GEN_SEP:char = '=';
  OVER_STATE:string = '@';
  VAR_ASSOC:string = '=';
  OVER_ASSOC:string = ':';
  OVER_PARAM:char = '|';
  FILES_SECURE_SEP:string = '|';
  OVER_EXTRA:string = '>';
  COMMENT_TOKEN:string = '#';
  DEF_IF_NOT:string = '';
  MAX_TRIES:integer = 40;
  PROCESSORS_FOLDER:string = 'Processors';
  EXTENSIONS_FOLDER:string = 'Extensions';
  TIME_STR:array of string;
  EXT_ACCEPTED:array[0..3] of string;
  PREDEF_OVERR:array[0..5] of string;
  RESERVED_WORDS: array [0..13] of string;
  LOOP_FUNCTION:string = 'LOOP';
  LINE_BREAK:string = 'LINE';
  FULL_DATE:string = DATE_INTERCHANGE_FORMAT;
  PI: double;

implementation
begin
  SetLength(TIME_STR,2);
  TIME_STR[0] := 'NOW';
  TIME_STR[1] := 'GEN';

  EXT_ACCEPTED[0] := '';
  EXT_ACCEPTED[1] := '.exe';
  EXT_ACCEPTED[2] := '.bat';
  EXT_ACCEPTED[3] := '.cmd';

  PREDEF_OVERR[0] := 'copyTo';
  PREDEF_OVERR[1] := 'outFileName';
  PREDEF_OVERR[2] := 'extension';
  PREDEF_OVERR[3] := 'overwrite';
  PREDEF_OVERR[4] := 'import';
  PREDEF_OVERR[5] := 'include';

  RESERVED_WORDS[0] := 'TRUE';
  RESERVED_WORDS[1] := 'FALSE';
  RESERVED_WORDS[2] := 'CONTAINS';
  RESERVED_WORDS[3] := 'EMPTY';
  RESERVED_WORDS[4] := 'EQ';
  RESERVED_WORDS[6] := 'GT';
  RESERVED_WORDS[7] := 'LT';
  RESERVED_WORDS[8] := 'GEQ';
  RESERVED_WORDS[9] := 'LEQ';
  RESERVED_WORDS[10] := LOOP_FUNCTION;
  RESERVED_WORDS[11] := LINE_BREAK;
  RESERVED_WORDS[12] := 'FULL_DATE';
  RESERVED_WORDS[13] := 'UTC_FORMAT';

  PI := System.Pi();


end.

