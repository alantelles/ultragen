unit MathFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function sum(a,b:int64):int64;
function sub(a,b:int64):int64;



implementation

function sum(a,b:int64):int64;
begin
  Result := a+b;
end;

function sub(a,b:int64):int64;
begin
  Result := a-b;
end;

end.

