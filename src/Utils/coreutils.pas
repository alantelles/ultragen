unit CoreUtils;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils;

function BooleanToStr(AVal: boolean):string;
function DecodeUnicodeEscapes(EscapedString: String): String;


implementation
uses StrUtils;

function BooleanToStr(AVal: boolean):string;
begin
  if AVal then
    Result := 'true'
  else
    Result := 'false';
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

end.

