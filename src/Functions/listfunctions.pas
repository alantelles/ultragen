unit ListFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,StrUtils,

  { Globals }
  VariablesGlobals, ConstantsGlobals
  ;

function PrintCount(AListStr,ADelim:string):string;
function PrintRange(AnEnd:integer):string;
function PrintRange(AStart,AnEnd:integer):string;

implementation


function PrintRange(AnEnd:integer):string;
var
  Return:string='';
  i:integer;
begin
  if AnEnd > 0 then
  begin
    for i:=0 to AnEnd-1 do
      Return := Return + IntToStr(i)+',';
  end;
  Result := Copy(Return,1,Length(Return)-1);
end;

function PrintRange(AStart,AnEnd:integer):string;
var
  Return:string='';
  i:integer;
begin
  if AnEnd > AStart then
  begin
    for i:=AStart to AnEnd-1 do
      Return := Return + IntToStr(i)+',';
  end;
  Result := Copy(Return,1,Length(Return)-1);
end;

function PrintCount(AListStr,ADelim:string):string;
var
  AList:TStringList;
  Return:integer=-1;
begin
  AList := TStringList.Create;
  if ADelim = LINE_BREAK then
  begin
    AList.Text := AListStr;
    Return := AList.Count;
  end
  else
  begin
    AList.Delimiter := ADelim[1];
    AList.StrictDelimiter := True;
    AList.DelimitedText := AListStr;
    Return := AList.Count;
  end;

  AList.Free;
  Result := IntToStr(Return);
end;

end.

