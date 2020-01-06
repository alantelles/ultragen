unit FileHandlingUtils;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, StrUtils,
    ConstantsGlobals,
    VariablesGlobals;

function GetFileName(AFileName:string; WithExt:boolean=True; NewExt:string=''):string;
function GetFilePath (AFileName:string; ALevel: integer=-1; FullPathReturn:boolean=True):string;
function GetFileRelative(AFileName:string; ALevel: integer=-1; FullPathReturn: boolean=True):string;
procedure CreateDirTree(APath: string; ToFile:boolean=true);
function RemoveLastBackslash(AStr:string):string;
function PrintFileIfExists(AName,Ifnot, IfYes:string):string;

implementation
uses StringsFunctions;

function PrintFileIfExists(AName,IfNot,IfYes:string):string;
var
  Return,Aux:string;
begin
  Return := AName;
  if FileExists(AName) then
    Return := IfYes
  else
    Return := Ifnot;
  Result := Return;
end;

function RemoveLastBackslash(AStr:string):string;
var
  Ret:string;
begin
  Ret:=AStr;
  if (Length(AStr) > 0) and (AStr[Length(AStr)] = DirectorySeparator) then
     Ret := Copy(AStr,1,Length(AStr)-1);
  Result := Ret;
end;

function GetFileName(AFileName:string; WithExt:boolean=True; NewExt:string=''):string;
var
  Temp:string;
  BarPos:integer;
begin
  BarPos := RPos(DirectorySeparator, AFileName);
  if BarPos > 0 then
    Temp := Copy(AFileName, BarPos+1, Length(AFileName))
  else
    Temp := AFileName;
  if not WithExt then
  begin
     BarPos := RPos(EXT_SEP, Temp);
     if BarPos > 0 then
       Temp := Copy(Temp, 1, BarPos-1)
  end;
  if NewExt <> '' then
  begin
     Temp := Temp + NewExt;
  end;
  Result := Temp;
end;

function GetFilePath (AFileName:string; ALevel: integer=-1; FullPathReturn:boolean=True):string;
var
  ADelim:TStringList;
  Part: String = '';
  Temp, Ret: string;
  Limit, i: integer;
begin
  ADelim := TStringList.Create;
  ADelim.SkipLastLineBreak := True;
  ADelim.Delimiter := DirectorySeparator;
  ADelim.StrictDelimiter := True;
  ADelim.DelimitedText := AFileName;
  Limit := (ADelim.Count-1) + ALevel;
  i:=0;
  for Temp in ADelim do
  begin
    Part := Part + Temp + DirectorySeparator;
    if not FullPathReturn then
      Ret := Temp
    else
      Ret := Part;
    if i >= Limit then
      Break;
    Inc(i);
  end;
  ADelim.Free;
  Result := RemoveLastBackslash(Ret);
end;

function GetFileRelative (AFileName:string; ALevel: integer=-1; FullPathReturn:boolean=True):string;
var
  ADelim:TStringList;
  Part: String = '';
  Temp, Ret: string;
  Limit, i, j: integer;
begin
  ADelim := TStringList.Create;
  ADelim.SkipLastLineBreak := True;
  ADelim.Delimiter := DirectorySeparator;
  ADelim.StrictDelimiter := True;
  ADelim.DelimitedText := AFileName;
  Limit := (ADelim.Count-1) + ALevel;
  i:=0;
  for j:=limit to ADelim.Count-1 do
  begin
    Part := Part + ADelim[j] + DirectorySeparator;
    if not FullPathReturn then
      Ret := Temp
    else
      Ret := Part;
    if i >= Limit then
      Break;
    Inc(i);
  end;
  ADelim.Free;
  Result := RemoveLastBackslash(Ret);
end;

procedure CreateDirTree(APath: string; ToFile:boolean=true);
var
  ADelim: TStringList;
  Part: String = '';
  Temp: string;
begin
  if ToFile then
    APath := GetFilePath(APath);
  APath := RemoveLastBackslash(APath);
  ADelim := TStringList.Create;
  ADelim.SkipLastLineBreak := True;
  ADelim.Delimiter := DirectorySeparator;
  ADelim.StrictDelimiter := True;
  ADelim.DelimitedText := APath;
  for Temp in ADelim do
  begin
    Part := Part + Temp + DirectorySeparator;
    if not DirectoryExists(Part) then
       CreateDir(Part);
  end;
  ADelim.Free;
end;

end.

