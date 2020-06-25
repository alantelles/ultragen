unit ExceptionsClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  E_INVALID_ARGS = 'Invalid number of arguments for this function';

type
  ELexicalError = class (Exception) end;
  EParserError = class (Exception) end;
  ESemanticError = class (Exception) end;
  ERunTimeError = class (Exception) end;
  EArgumentsError = class(Exception) end;
  EValueError = class (Exception) end;
  ETypeError = class (Exception) end;
  EListError = class(Exception) end;

implementation

end.

