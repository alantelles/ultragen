unit ListInstanceClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass;

type
  TListInstance = class (TInstanceOf)
    private
      FValue: TInstanceList;
      FMetName:string;
      FAddLocked, FChangeLocked: boolean;
      function LenList:integer;

    public
      property Count: integer read lenList;
      property PValue: TInstanceList read FValue write FValue;
      property PAddLocked: boolean read FAddLocked write FAddLocked;
      property PChangeLocked: boolean read FChangeLocked write FChangeLocked;
      constructor Create(AList: TInstanceList);
      constructor Create;
      procedure Clear;
      function AsString:string; override;
      function GetItem(AIndex: TIntegerInstance):TInstanceOf;
      function GetItem(AIndex: integer):TInstanceOf;
      procedure SetItem(AIndex: integer; AItem: TInstanceOf);
      function PopItem:TInstanceOf;
      procedure Add(AItem: TInstanceOf);
      procedure Prepend(AItem: TInstanceOf);
      procedure CopyList(var ANewList: TListInstance);
      procedure CopyInstance(var AReceiver: TInstanceOf); override;
      class function LoadText(AFileName: string): TListInstance;
      procedure WriteText(AFileName: string);

      function Execute: TInstanceOf;

      //functions
      function MapList: TListInstance;
      destructor Destroy; override;

  end;

implementation
uses
  StrUtils, CoreUtils, Tokens, ExceptionsClasses, StringInstanceCLass;

constructor TListInstance.Create(AList: TInstanceList);
var
  len, i: integer;
begin
  inherited Create;
  len := Length(AList);
  if len > 0 then
  begin
    SetLength(FValue, len);
    for i:=0 to len-1 do
    begin
      AList[i].CopyInstance(FValue[i]);
		end;
	end;
	FAddLocked := False;
  FChangeLocked := False;
end;



class function TListInstance.LoadText(AFileName: string): TListInstance;
var
  AStrList: TStringList;
  Aret: TListInstance;
  aLine: string;
begin
  AStrlist := TStringList.Create;
  AStrList.LoadFromFile(AFileName);
  AStrList.SkipLastLineBreak := True;
  ARet := TListInstance.Create;
  for ALine in AStrList do
    ARet.Add(TStringInstance.Create(ALine));
  AStrList.Free;
  Result := ARet;
end;

procedure TListInstance.WriteText(AFileName: string);
var
  AStrList: TStringList;
  aLine: TInstanceOf;
  i: integer;
begin
  AStrlist := TStringList.Create;
  AStrList.SkipLastLineBreak := True;
  if Count > 0 then
  begin
    for i:=0 to Count-1 do
      AStrList.Add(FValue[i].AsString);
  end;
  AStrList.SaveToFile(AFileName);
  AStrList.Free;
end;

procedure TListInstance.Clear;
var
  i: integer;
begin
  if Count > 0 then
  begin
    for i:=0 to Count-1 do
    begin
      FValue[i].Free;
    end;
  end;
  SetLength(FValue, 0);
end;

procedure TListInstance.CopyList(var ANewList: TListInstance);
var
  i, len: integer;
begin
  len := Length(FValue);
  if len > 0 then
  begin
    for i:=0 to len-1 do
    begin
      ANewList.Add(FValue[i]);
    end;
  end;
end;

procedure TListInstance.CopyInstance(var AReceiver: TInstanceOf);
var
  i, len: integer;
  Cast: TListInstance;
  Itens: TInstanceOf;
begin
  Cast := TListInstance.Create;
  len := Length(FValue);
  if len > 0 then
  begin
    for i:=0 to len-1 do
    begin
      Itens := TInstanceOf.Create;
      FValue[i].CopyInstance(Itens);
      Cast.Add(Itens);
    end;
  end;
  AReceiver := Cast;
end;

constructor TListInstance.Create;
begin

end;

destructor TListInstance.Destroy;
var
  len, i: integer;
begin
  len := Length(FValue);
  if len > 0 then
  begin
    for i:=0 to len-1 do
    begin
      if FValue[i] <> nil then
      begin
        if FValue[i].ClassNameIs('TIntegerInstance') or
           FValue[i].ClassNameIs('TBooleanInstance') or
           FValue[i].ClassNameIs('TStringInstance') or
           FValue[i].ClassNameIs('TFloatInstance') or
           FValue[i].ClassNameIs('TNullInstance') then
          FValue[i].Free;
      end;
    end;
  end;
  inherited;
end;

function TListInstance.PopItem:TInstanceOf;
var
  Ret, NewRet: TInstanceOf;
  Last: integer;
begin
  Last := lenList - 1;
  Ret := FValue[last];
  SetLength(FValue, last);
  Ret.CopyInstance(NewRet);
  Result := NewRet;
end;

procedure TListInstance.SetItem(Aindex:integer; AItem: TInstanceOf);
begin
  if Aindex > LenList - 1 then
    raise EListError.Create('Undefined index '+IntToStr(Aindex)+' at list', '', 1, 1);
  FValue[AIndex] := Aitem;
end;

procedure TListInstance.Add(AItem: TInstanceOf);
var
  len: integer;
  ToAdd: TInstanceOf;
begin
  ToAdd := AItem;
  {if AItem <> nil then
  begin
    if AItem.ClassNameIs('TIntegerInstance') then
      ToAdd := TIntegerInstance.Create(AItem.PIntValue)
    else if AItem.ClassNameIs('TBooleanInstance') then
      ToAdd := TBooleanInstance.Create(AItem.PBoolValue)
    else if AItem.ClassNameIs('TStringInstance') then
      ToAdd := TStringInstance.Create(AItem.PStrValue)
    else if AItem.ClassNameIs('TFloatInstance') then
      ToAdd := TFloatInstance.Create(AItem.PFloatValue)
    else if AItem.ClassNameIs('TNullInstance') then
      ToAdd := TNullInstance.Create;
  end;}
  len := Length(FValue);
  len := len + 1;
  SetLength(FValue, len);
  FValue[len - 1] := ToAdd;
end;

procedure TListInstance.Prepend(AItem: TInstanceOf);
var
  i, len: integer;
  ToAdd: TInstanceOf;
begin
  ToAdd := AItem;
  {if AItem <> nil then
  begin
    if AItem.ClassNameIs('TIntegerInstance') then
      ToAdd := TIntegerInstance.Create(AItem.PIntValue)
    else if AItem.ClassNameIs('TBooleanInstance') then
      ToAdd := TBooleanInstance.Create(AItem.PBoolValue)
    else if AItem.ClassNameIs('TStringInstance') then
      ToAdd := TStringInstance.Create(AItem.PStrValue)
    else if AItem.ClassNameIs('TFloatInstance') then
      ToAdd := TFloatInstance.Create(AItem.PFloatValue)
    else if AItem.ClassNameIs('TNullInstance') then
      ToAdd := TNullInstance.Create;
  end;}
  len := Length(FValue);
  len := len + 1;
  SetLength(FValue, len);
  if len > 0 then
  begin
    for i:=len-1 downto 1 do
    begin
      FValue[i] := FValue[i-1];
    end;
    FValue[0] := ToAdd;
  end;
end;

function TListInstance.GetItem(AIndex: TIntegerInstance):TInstanceOf;
var
  Ret: TInstanceOf;
begin
  Ret := FValue[AIndex.PValue];
  if Ret <> nil then
    Result := Ret
  else
    raise EListError.Create('Undefined index '+IntToStr(Aindex.PValue)+' at list', '', 1, 1);
end;

function TListInstance.GetItem(AIndex: integer):TInstanceOf;
var
  Ret: TInstanceOf;
begin
  Ret := FValue[AIndex];
  if Ret <> nil then
    Result := Ret
  else
    raise EListError.Create('Undefined index '+IntToStr(Aindex)+' at list', '', 1, 1);
end;

function TListInstance.LenList:integer;
var
  i: integer;
begin
  Result := Length(FValue);
end;

function TListInstance.AsString:string;
var
  AItem: TInstanceOf;
  Ret: TStringList;
begin
  Ret := TStringList.Create;
  Ret.SkipLastLineBreak := True;
  Ret.LineBreak := ', ';
  for AItem in FValue do
  begin
    if AItem.ClassNameIs('TStringInstance') then
      Ret.Add('''' + AItem.AsString + '''')
    else
      Ret.Add(AItem.AsString);
  end;
  Result := '[' + Ret.Text + ']';
end;

function TListInstance.Execute:TInstanceOf;
begin
  if FMetName = 'each' then
    Result := MapList;
end;

function TListInstance.MapList:TListInstance;
begin

end;

end.

