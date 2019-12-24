unit BooleansFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function BooleanToInt(AValue:String):string;
function IntToBoolean(AValue:string):string;
function BooleanToStr(AValue:string;IfTrue:string='True';IfFalse:string='False'):string;
function StrToBoolean(AValue:string):boolean;

implementation

function BooleanToInt(AValue:String):string;
begin
  if (lowercase(AValue) = 'true') then
    Result := '1'
  else
    Result := '0';
end;

function StrToBoolean(AValue:string):boolean;
begin
  if (lowercase(AValue) = 'true') or (Avalue = '1') then
    Result := True
  else
    Result := False;
end;

function IntToBoolean(AValue:string):string;
begin
  try
    if (StrToInt(AValue) > 0) then
      Result := 'TRUE'
    else
      Result := 'FALSE';
  except
    Result := 'FALSE';
  end;
end;

function BooleanToStr(AValue:string;IfTrue:string='True';IfFalse:string='False'):string;
begin
  if (lowercase(AValue) = 'true') then
    Result := IfTrue
  else
    Result := IfFalse;
end;

end.

