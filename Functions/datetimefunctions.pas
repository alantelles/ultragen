unit DateTimeFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, DateUtils,

  { Globals }
  ConstantsGlobals;

function PrintDate(AFormat,ADateTime:String):string;
function PrintDateMs(ADateTime:string):string;

implementation

function PrintDate(AFormat,ADateTime:string):string;
begin
  Result := FormatDateTime(AFormat,ScanDateTime(DATE_INTERCHANGE_FORMAT,ADateTime));
end;

function PrintDateMs(ADateTime:String):string;
var
  ADateConv:TDateTime;
begin
  ADateConv := ScanDateTime(DATE_INTERCHANGE_FORMAT,ADateTime);
  Result := IntToStr(DateTimeToUnix(ADateConv) * 1000) + FormatDateTime('zzz',ADateConv);
end;

//

end.

