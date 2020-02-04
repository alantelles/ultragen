unit StringsFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, md5, DateUtils;

function ReplaceStrExtended(var AList:TStringList):string;
function RepeatStr(AText: string; ACount: Integer; AJoiner:string=''): string;
function LeftZeros(ANumber:string; Zeros:integer): string;
function Slice(AStr:string; StrStart, StrEnd:integer):string;
function StrToMD5(AStr:string):string;
function MiniHash(AStr:string):string;
function Concat(var Params:TStringList):string;
procedure ReverseList(var AList:TstringList);
function DropLastLineBreak(s:string):string;
function Join(var Params:TStringList):string;
function Explode(var Params:TStringList):string;
function InTag(var Params:TStringList):string;
function CreateSessionID:string;
function CreateRandomHash:String;
function OsDirSep(AStr:string):string;

// cast functions
function ToNumeric(AStr:string):string;
function ToBoolean(AStr:string):string;

implementation

uses ConstantsGlobals, VariablesGlobals;

function CreateRandomHash:string;
begin
  Result := CreateSessionID;
end;

function CreateSessionID:string;
var
  ASessionName:string;
  FullPath: string;
begin
  ASessionName :=  StrToMd5(
    FormatDateTime('yyyymmddhhnnsszzz',now)+
    IntToStr(Random(Int64(HourOf(now))))+
    IntToStr(MilliSecondOf(now))+
    IntToStr(Random(Int64(MinuteOf(now)+SecondOf(Now))))
  );
  ASessionName := ASessionName + StrToMd5(
    FormatDateTime('yyyymmddhhnnsszzz',now)+
    IntToStr(Random(Int64(HourOf(now))))+
    IntToStr(MilliSecondOf(now))+
    IntToStr(Random(Int64(MinuteOf(now)+SecondOf(Now))))
  );
  Result := ASessionName;

end;

function ToNumeric(AStr:string):string;
var
  Return:string;
begin
  if AStr = 'true' then
    Return := '1'
  else if AStr = 'false' then
    Return := '0'
  else if AStr = '' then
    Return := '0'
  else
  begin
    try
      Return := IntToStr(StrToInt(AStr));
    except
      try
        Return := FloatToStr(StrToFloat(AStr));
      except
        if Length(AStr) > 0 then
          Return := '1';
      end;
    end;
  end;
  Result := Return;
end;

function ToBoolean(AStr:string):string;
var
  Return:string;
begin
  if (AStr = '-1') or (AStr = '') then
    Return := 'false'
  else
    Return := 'true';
  Result := Return;
end;

function Explode(var Params:TStringList):string;
var
  Return:String='';
  Dump:TStringList;
  index:integer;
begin
  Dump := TStringList.Create;
  Dump.SkipLastLineBreak := True;
  if Params.Count = 3 then
    Dump.Delimiter := Params[2][1]
  else
    Dump.Delimiter := DEF_DELIM;
  Dump.StrictDelimiter := True;
  Dump.DelimitedText := Params[0];

  //Return := Dump.text;
  index := StrToInt(Params[1]);
  if (Dump.Count > index) and (index > -1) then
    Return := Dump[index];
  Dump.Free;
  Result := Return;
end;

function Join(var Params:TStringList):string;
// Params[0] is the joiner, other are the parts
var
  j, Return:string;
  i:integer;
begin
  j := Params[0];
  if j = LINE_BREAK then
    j := sLineBreak;
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

function InTag(var Params:TStringList):string;
var
  Return, open, close:string;
  i: integer;
begin
  open := '<'+Params[0];
  if Params.Count > 2 then
    for i:=2 to Params.Count - 1 do
      open := open +' '+Params[i];
  open := open+'>';
  close := '</'+Params[0]+'>';
  Result := open + Params[1] + close;
end;

function DropLastLineBreak(s:string):string;
var
  ret:string;
begin
  {$IFDEF WINDOWS}
  ret := Copy(s,1,Length(s)-2);
  {$ENDIF}
  {$IFDEF UNIX}
  ret := Copy(s,1,Length(s)-1);
  {$ENDIF}
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

function OsDirSep(AStr:string):string;
var
  Part:string;
begin
  {$IFDEF WINDOWS}
  Part := ReplaceStr(AStr,'/','\');
  {$ENDIF}
  {$IFDEF UNIX}
  Part := ReplaceStr(AStr,'\','/');
  {$ENDIF}
  Result := Part;
end;

function StrToMD5(AStr:string):string;
begin
  Result := MD5Print(MD5String(AStr));
end;

function MiniHash(AStr:string):string;
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
      try
        Aux := AList[i];
        AList[i] := AList[(AList.Count-1)-i];
        AList[(AList.Count-1)-i] := Aux;

      finally
      end;
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

