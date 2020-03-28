unit VariablesGlobals;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ConstantsGlobals, TemplateClass, QueueListClass;

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

  SPREAD_KEYS:string = '.';
  SPREAD_VALUES:string = '..';
  SPREAD_PAIRS:string = '...';
  SPREAD_LIST_COMMA:string = '*';
  SPREAD_LIST:string = '**';


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

  RESERVED_WORDS[0] := 'true';
  RESERVED_WORDS[1] := 'false';
  RESERVED_WORDS[2] := LOOP_FUNCTION;
  RESERVED_WORDS[3] := LINE_BREAK;
  RESERVED_WORDS[4] := 'FULL_DATE';
  RESERVED_WORDS[5] := 'UTC_FORMAT';

  PI := System.Pi();


end.

