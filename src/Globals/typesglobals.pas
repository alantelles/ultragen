unit TypesGlobals;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;


type
  TKVPair = record
    Key, Value: AnsiString;
    Constant: boolean;
  end;

  TDict = array of TKVPair;

  TParseResult = record
    Value:string;
    ParseOk: boolean;
  end;

  TErrorLocation = record
    LineNumber:integer;
    Line:string;
    TempName:string;
  end;



implementation

end.

