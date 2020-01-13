unit JsonToGenFunctions;

{$mode objfpc}{$H+}

interface

uses
  GenFileClass;

const
  OBJ_O = '{';
  OBJ_C = '}';
  LIST_O = '[';
  LIST_C = ']';
  SEP = ',';
  STR_ENC = '"';

type
  TJson2Gen = class
  private
    FGenFile: TGenFile;
    FJson:string;
  public
    constructor Create(AStr:String; var AGenFile:TGenFile);
    procedure ParseJson;
    procedure ParseValue(AStr:string;Key:string='');
    procedure ParseStream(AStr:string);
    procedure ParsePair(AStr:string);
    procedure ParseList(AStr:string);
    procedure ParseObj(Astr:string);
    function IsObj(AStr:string):boolean;
    function IsList(AStr:string):boolean;
    function Conform(AStr:String):string;
  end;



implementation

uses
  Classes, SysUtils;

constructor TJson2Gen.Create(AStr:string; var AGenFile:TGenFile);
begin
  FGenFile := AGenFile;
  FJson := AStr;
end;

function TJson2Gen.Conform(AStr:string):string;
begin
  if (Conform[1] = STR_ENC) and (Conform[Length(Conform)] = STR_ENC) then
    Result := Copy(AStr,2,Length(AStr)-2)
  else
    Result := AStr;
end;

procedure TJson2Gen.ParseJson;
begin
  ParseValue(FJson);
end;

procedure TJson2Gen.ParseValue(AStr:string;Key:string='');
begin
  if IsList(AStr) then
    ParseList(AStr)
  else if IsObj(AStr) then
    ParseObj(AStr)
  else
    FGenFile.SetValue(Key,Conform(AStr));
end;

procedure TJson2Gen.ParseStream(AStr:string);
begin

end;

procedure TJson2Gen.ParsePair(AStr:string);
begin

end;

procedure TJson2Gen.ParseList(AStr:string);
begin

end;

procedure TJson2Gen.ParseObj(Astr:string);
begin

end;

function TJson2Gen.IsObj(AStr:String):boolean;
begin
  if (AStr[1] = OBJ_O) and (AStr[Length(AStr)] = OBJ_C) then
    Result := True
  else
    Result := False;
end;

function TJson2Gen.IsList(AStr:String):boolean;
begin
  if (AStr[1] = LIST_O) and (AStr[Length(AStr)] = LIST_C) then
    Result := True
  else
    Result := False;
end;

end.

