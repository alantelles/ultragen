unit DateTimeFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, DateUtils,

  { Globals }
  ConstantsGlobals;

function PrintDate(AFormat,ADateTime:String):string;
function PrintDateMs(ADateTime:string;TimeZone:real=0):string;

implementation

function PrintDate(AFormat,ADateTime:string):string;
begin
  if AFormat = 'FULL_DATE' then
    AFormat := DATE_INTERCHANGE_FORMAT
  else if AFormat = 'UTC_FORMAT' then
    AFormat := UTC_FORMAT;
  Result := FormatDateTime(AFormat,ScanDateTime(DATE_INTERCHANGE_FORMAT,ADateTime));
end;

function PrintDateMs(ADateTime:String;TimeZone:real=0):string;
var
  ADateConv:TDateTime;
begin
  ADateConv := ScanDateTime(DATE_INTERCHANGE_FORMAT,ADateTime);
  Result := FloatToStr(DateTimeToUnix(ADateConv) + (3600 * TimeZone)) + FormatDateTime('zzz',ADateConv);
end;

//

end.

