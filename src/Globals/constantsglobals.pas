unit ConstantsGlobals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  EXT_SEP = '.';
  PARAM_OPEN = '(';
  PARAM_CLOSE = ')';
  PARAM_SEP = ',';
  STR_ENCLOSE = '''';
  ESCAPER = '\';
  ATTR_ACCESSOR = '.';
  EXT_FUNC_SEP = ':';
  REFER_VAR = '&';
  FROM_GEN_SET = '&';
  SELF_WORD = 'self';
  FUNCTION_ALLOWED = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  NUMBERS = '1234567890';
  FUNC_SYMBOLS = '._:';
  DATE_INTERCHANGE_FORMAT = 'yyyy-mm-dd hh:nn:ss.zzz';
  AS_STRING = True;
  FLOW_EXT = 'uflow';
  GEN_FILES_CALL_SEP = '|';
  SCRIPT_MODE_ENTER = '@{';
  SCRIPT_MODE_EXIT = '}';
  GEN_SUB_LEVEL = ':';

  { calling modes }
  SET_GROUP = '+';
  SET_SEP = '|';

  GENSET_CALL = '-gens';
  GENPATH_CALL = '-genpath';
  TEMPSET_CALL = '-templates';
  LOOK_SUB_FLAG = '-sub';
  PARAM_SET_DEFAULT = '-default';

  LIVE_CALL = '--persist';
  LIVE_CALL_S = '-p';
  GEN_AS_STRING = '--string';
  GEN_AS_STRING_S = '-s';
  SEPARATOR_CHANGE = ' -separator:';
  DEFAULT_CHANGE = ' -default:';

  ASC = 'ASC';
  DESC = 'DESC';



implementation

end.
