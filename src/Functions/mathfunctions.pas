unit MathFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

function sum(a,b:double):double;
function sub(a,b:double):double;
function mult(a,b:double):double;
function divInt(a,b:double):double;
function divFloat(a,b:double):double;
function modNum(a,b:double):int64;
function pow(a:double;b:double=2):double;
function root(a:double; b:double=2):double;



implementation

function sum(a,b:double):double;
begin
  Result := a+b;
end;

function sub(a,b:double):double;
begin
  Result := a-b;
end;

function mult(a,b:double):double;
begin
  Result := a*b;
end;

function divInt(a,b:double):double;
var
  aC, bC:int64;
begin
  try
    aC := Floor64(a);
    bC := Floor64(b);
    Result := aC div bC;
	except
    Result := 0;
	end;
end;

function divFloat(a,b:double):double;
begin
  try
    Result := a / b;
	except
    Result := 0;
	end;
end;

function modNum(a,b:double):int64;
var
  aC, bC:int64;
begin
  try
    aC := Floor64(a);
    bC := Floor64(b);
    Result := aC mod bC;
	except
    Result := 0;
	end;
end;

function pow(a,b:double):double;
begin
  Result := power(a,b);
end;

function root(a:double; b:double=2):double;
begin
  if (a < 0) and (Floor64(b) mod 2 = 0) then
    Result := 0
  else
    Result := power(a,1/b);
end;
end.

