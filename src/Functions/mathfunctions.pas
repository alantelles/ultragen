unit MathFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

function sum(a,b:real):real;
function sub(a,b:real):real;
function mult(a,b:real):real;
function divInt(a,b:real):real;
function divFloat(a,b:real):real;
function modNum(a,b:real):int64;
function pow(a:real;b:real=2):real;
function root(a:real; b:real=2):real;



implementation

function sum(a,b:real):real;
begin
  Result := a+b;
end;

function sub(a,b:real):real;
begin
  Result := a-b;
end;

function mult(a,b:real):real;
begin
  Result := a*b;
end;

function divInt(a,b:real):real;
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

function divFloat(a,b:real):real;
begin
  try
    Result := a / b;
	except
    Result := 0;
	end;
end;

function modNum(a,b:real):int64;
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

function pow(a,b:real):real;
begin
  Result := power(a,b);
end;

function root(a:real; b:real=2):real;
begin
  if (a < 0) and (Floor64(b) mod 2 = 0) then
    Result := 0
  else
    Result := power(a,1/b);
end;
end.

