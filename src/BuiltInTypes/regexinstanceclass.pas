unit RegexInstanceClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, InstanceOfClass;

type
  TRegexInstance = class (TInstanceOf)
    public
      constructor Create;
      function MatchExpr(ATest: string): boolean;
  end;

implementation
uses StringInstanceClass;

constructor TRegexInstance.Create;
begin
  inherited Create;
  FMembers.Add('expr', TStringInstance.Create('.*'));
end;

function TRegexInstance.MatchExpr(ATest: string):boolean;
var
  re: TRegExpr;
  Expr: string;
  Ret: boolean;
begin
  Expr := TInstanceOf(FMembers.Find('expr')).PStrValue;
  re := TRegExpr.Create(Expr);
  Ret := re.Exec(ATest);
  re.Free;
  Result := Ret;
end;

end.

