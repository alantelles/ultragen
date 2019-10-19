unit TypesGlobals;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;


type
  TKVPair = record
    Key, Value: string;
  end;

  TDict = array of TKVPair;

  TParseResult = record
    Value:string;
    ParseOk: boolean;
  end;



implementation

end.

