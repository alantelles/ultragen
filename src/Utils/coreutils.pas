unit CoreUtils;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils;

function BooleanToStr(AVal: boolean):string;

implementation

function BooleanToStr(AVal: boolean):string;
begin
  if AVal then
    Result := 'true'
  else
    Result := 'false';
end;

end.

