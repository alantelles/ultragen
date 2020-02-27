unit BooleansFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function StrToBoolean(AValue:string):boolean;

//booleans comparisons
function Equal(var Params:TStringList; neg:boolean=False):string;
function Greater(var Params:TStringList; neg:boolean=False):string;
function GreaterOrEq(var Params:TStringList; neg:boolean=False):string;
function Inverter(var Params:TStringList):string;
function LogicAnd(var Params:TStringList; neg:boolean=False):string;
function LogicOr(var Params:TStringList; neg:boolean=False):string;
function TernaryPrint(var Params:TStringList):string;
//end booleans comparisons

implementation

uses
  ConstantsGlobals;

function TernaryPrint(var Params:TStringList):string;
var
  IfFalse:string = '';
begin
  if Params.Count = 3 then
    IfFalse := Params[2];
  if Params[0] = LANG_TRUE then
    Result := Params[1]
  else
    Result := IfFalse;
end;

function Equal(var Params:TStringList; neg:boolean=False):string;
var
  Return:string=LANG_TRUE;
  NoNumeric:boolean;
  i:integer;
  a,b:double;
begin
  for i:=0 to Params.count - 2 do
  begin
    try
      a := StrToFloat(Params[i]);
      b := StrToFloat(Params[i+1]);
      if (Params[i][1] = '0') or (Params[i+1][1] = '0') then
        NoNumeric := True
      else
        NoNumeric := False;
    except
      NoNumeric := True;
    end;
    if NoNumeric then
    begin
      if ((not neg) and (Params[i] <> Params[i+1])) or
         ((neg) and (Params[i] = Params[i+1])) then
      begin
        Return := LANG_FALSE;
        break;
      end;
    end
    else
    begin
      if ((not neg) and (a <> b)) or
         ((neg) and (a = b)) then
      begin
        Return := LANG_FALSE;
        break;
      end;
    end
  end;
  Result := Return;
end;

function Greater(var Params:TStringList; neg:boolean=False):string;
var
  Return:string=LANG_TRUE;
  NoNumeric:boolean;
  i:integer;
  a,b:double;
begin
  for i:=0 to Params.count - 2 do
  begin
    try
      a := StrToFloat(Params[i]);
      b := StrToFloat(Params[i+1]);
      if (Params[i][1] = '0') or (Params[i+1][1] = '0') then
        NoNumeric := True
      else
        NoNumeric := False;
    except
      NoNumeric := True;
    end;
    if noNumeric then
    begin
      if ((not neg) and (Params[i] <= Params[i+1])) or
         ((neg) and (Params[i] >= Params[i+1])) then
      begin
        Return := LANG_FALSE;
        break;
      end;
    end
    else
    begin
      if ((not neg) and (a <= b)) or
         ((neg) and (a >= b)) then
      begin
        Return := LANG_FALSE;
        break;
      end;
    end;
  end;
  Result := Return;
end;

function Inverter(var Params:TStringList):string;
begin
  if Params[0] = LANG_TRUE then
    Result := LANG_FALSE
  else
    Result := LANG_TRUE;
end;

function GreaterOrEq(var Params:TStringList; neg:boolean=False):string;
var
  Return:string=LANG_TRUE;
  i:integer;
begin
  for i:=0 to Params.count - 2 do
  begin
    if ((not neg) and (Params[i] < Params[i+1])) or
       ((neg) and (Params[i] > Params[i+1])) then
    begin
      Return := LANG_FALSE;
      break;
    end;
  end;
  Result := Return;
end;

function LogicAnd(var Params:TStringList; neg:boolean=False):string;
var
  Return:string;
  Test:boolean=True;
  i:integer;
begin
  for i:=0 to Params.Count-1 do
  begin
    if (Params[i] <> LANG_TRUE) then
    begin
      Test := False;
      break;
    end;
  end;
  if Test then
  begin
    if not neg then
      Result := LANG_TRUE
    else
      Result := LANG_FALSE
  end
  else
  begin
    if not neg then
      Result := LANG_FALSE
    else
      Result := LANG_TRUE;
  end;
end;

function LogicOr(var Params:TStringList; neg:boolean=False):string;
var
  Return:string;
  Test:boolean=False;
  i:integer;
begin
  for i:=0 to Params.Count-1 do
  begin
    if (Params[i] = LANG_TRUE) then
    begin
      Test := True;
      break;
    end;
  end;
  if Test then
  begin
    if not neg then
      Result := LANG_TRUE
    else
      Result := LANG_FALSE
  end
  else
  begin
    if not neg then
      Result := LANG_FALSE
    else
      Result := LANG_TRUE;
  end;
end;


// project utils
function StrToBoolean(AValue:string):boolean;
begin
  if (AValue = LANG_TRUE) then
    Result := True
  else
    Result := False;
end;


end.

