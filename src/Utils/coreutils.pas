unit CoreUtils;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils;

function BooleanToStr(AVal: boolean):string;
function StrToBoolean(AVal: string):boolean;
function DecodeUnicodeEscapes(EscapedString: String): String;
function GetOption(Arg:string):string;


implementation
uses StrUtils, JsonTools;

function GetOption(Arg:string):string;
var
  ret:string = '';
  start:string;
begin
  if Arg.StartsWith('--') then
    Ret := Arg.Substring(2);
  Result := Ret;
end;

function BooleanToStr(AVal: boolean):string;
begin
  if AVal then
    Result := 'true'
  else
    Result := 'false';
end;

function StrToBoolean(AVal: string):boolean;
begin
  writeln('a ... val', Aval);
  if AVal = 'false' then
    Result := False
  else
    Result := True;
end;

function DecodeUnicodeEscapes(EscapedString: String): String;
//kibbed from https://pt.stackoverflow.com/users/13245/gabriel-santos
var
  FoundPos: LongInt;
  HexCode: String;
  DecodedChars: String;
begin
  Result := EscapedString;
  FoundPos := Pos('\u', Result);
  while (FoundPos <> 0) and (FoundPos < Length(Result) - 4) do begin
    HexCode :=  Copy(Result, FoundPos + 2, 4);
    DecodedChars := WideChar(StrToInt('$' + HexCode));
    Result := AnsiReplaceStr(Result, '\u' + HexCode,
                             UTF8Encode(DecodedChars));
    FoundPos := Pos('\u', Result);
  end;
end;

procedure Traverse(ANode: TJsonNode);
var
  i: TJsonNode;
begin
  writeln('kind: ',Anode.Kind);
  for i in ANode do
  begin
    if I.Count > 0 then
      Traverse(i)
    else
    begin
      writeln('kind: ',i.Kind);
      writeln('"',i.Name, '":' ,i.Value, ',');
		end;
	end;
end;

end.

