unit StringsFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, md5;

function ReplaceStrExtended(var AList:TStringList):string;
function RepeatStr(AText: string; ACount: Integer; AJoiner:string=''): string;
function LeftZeros(ANumber:string; Zeros:integer): string;
function Slice(AStr:string; StrStart, StrEnd:integer):string;
function StrToMD5(AStr:string):string;
function SuperHash(AStr:string):string;
function Concat(var Params:TStringList):string;
procedure ReverseList(var AList:TstringList);
function DropLastLineBreak(s:string):string;
function Join(var Params:TStringList):string;

implementation

function Join(var Params:TStringList):string;
// Params[0] is the joiner, other are the parts
var
  j, Return:string;
  i:integer;
begin
  j := Params[0];
  Return := '';
  if Params.Count > 1 then
  begin
    for i:=1 to Params.Count - 1 do
    begin
      Return := Return + Params[i];
      if i < Params.Count-1 then
        Return := Return + j;
    end;
  end;
  Result := Return;
end;

function DropLastLineBreak(s:string):string;
var
  last2, ret:string;
begin
  last2 := Copy(s,Length(s)-2,2);
  if (Length(s) > 1) and (last2 = #13#10) then
    ret := Copy(s,1,Length(s)-2)
  else if (Length(s) > 0) then
    ret := Copy(s,1,Length(s)-1);
  Result := ret;
end;

function Concat(var Params:TStringList):string;
var
  Temp:string;
  Return:string='';
begin
  for Temp in Params do
    Return := Return+Temp;
  Result := Return;
end;

function StrToMD5(AStr:string):string;
begin
  Result := MD5Print(MD5String(AStr));
end;

function SuperHash(AStr:string):string;
var dump,temp:string;
   i,j, len:integer;
   Reverse:boolean=True;
   Test:boolean;
begin
    len := 8;
    temp := '';
    dump:=StrToMD5(AStr);
    for j:=1 to (16 div len) do
    begin
        for i:=1 to Length(dump) do
        begin
            if (i mod 2) = 0 then
                continue;
            if reverse then
            begin
                Test:=dump[i] > dump[i+1];
                Reverse:=False;
            end
            else
            begin
                Test:=dump[i] < dump[i+1];
                reverse:=true;
            end;
            if (test) then
                temp:=temp+dump[i]
            else
                temp:=temp+dump[i+1];
        end;
        dump:=temp;
        temp:='';
    end;
    Result := Dump;
end;

procedure ReverseList(var AList:TStringList);
var
  i,mid:integer;
  Aux:String;
begin
  if AList.Count > 0 then
  begin
    mid := AList.Count div 2;
    for i:=0 to mid-1 do
    begin
      Aux := AList[i];
      AList[i] := AList[(AList.Count-1)-i];
      AList[(AList.Count-1)+i] := Aux;
    end;
  end;
end;

function ReplaceStrExtended(var AList:TStringList):string;
var
  i:integer;
  Return, Repl:string;
begin
  Return := '';
  Repl := AList[AList.Count-2];
  Return := AList[AList.Count-1];
  for i:=0 to AList.Count-3 do
    Return := ReplaceStr(Return,AList[i],Repl);
  Result := Return;
end;

function RepeatStr(AText: string; ACount: Integer; AJoiner:string=''): string;
var
    i:integer;
    Ret:string='';
begin
    if ACount = 0 then
       Result:=Ret;
    for i:=1 to ACount do
    begin
      Ret:=Ret+AText;
      if i < ACount then
        Ret := Ret+AJoiner;
    end;
    Result:=Ret;
end;

function LeftZeros(ANumber:string; Zeros:integer): string;
var
  i:integer;
  Return: string;
begin
  // 54
  // 4
  // 0054
  if Length(ANumber) < Zeros then
  begin
    Zeros := Zeros - Length(ANumber);
    Return := RepeatStr('0',Zeros) + ANumber;
  end
  else
    Return := ANumber;
  Result := Return;
end;

function Slice(AStr:string; StrStart, StrEnd:integer):string;
var
  Return: string;
begin
  Return := '';
  if (StrStart >= 0) and (StrEnd > StrStart) then
  //Slice('abcdefgh',1,5) = bcde
    Return := Copy(AStr,StrStart+1,StrEnd-StrStart)
  else if (StrStart >= 0) and (StrEnd < 0) then
  //Slice('abcdefgh',2,-1) = cdefg
  //Slice('abcdefgh',4,-2) = ef
    Return := Copy(AStr,StrStart+1,Length(AStr)+StrEnd-StrStart)
  else if (StrStart < 0) and (StrEnd <= 0) then
  //Slice('abcdefgh',-6,-3) = cde
    Return := Copy(AStr,Length(AStr)+StrStart+1,(StrStart-StrEnd)*(-1));
  Result := Return;

end;



end.

