unit TypesGlobals;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;


type
  TKVPair = record
    Key, Value: AnsiString;
  end;

  TDict = array of TKVPair;

  TParseResult = record
    Value:string;
    ParseOk: boolean;
  end;



implementation

end.

