unit StackClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ARClass;

type

  TARList = array of TActivationRecord;

  TStack = class
    private
      FItems:TARList;
      FLevel: integer;
    public
      property PItems: TARList read FItems;
      property PLevel: integer read FLevel write FLevel;
      constructor Create;
      procedure Push(AActRec: TActivationRecord);
      function Pop: TActivationRecord;
      function Peek: TActivationRecord;
      function AsString:string;
  end;

implementation

constructor TStack.Create;
begin
  SetLength(FItems, 0);
  FLevel := 0
end;

function TStack.AsString:string;
var
  Ret: string = '';
  i, len: integer;
begin
  Ret := 'CALL STACK' + sLineBreak;
  len := Length(FItems);
  if len > 0 then
  begin
    for i := (len-1) downto 0 do
    begin
      Ret := Ret + 'Call to '+FItems[i].PName+ ' of level '+IntToStr(FItems[i].PNestingLevel)+' of type '+ FItems[i].PType + sLineBreak;
      writeln(Ret);
      FItems[i].AsString;
    end;
  end;
  Result := '';
end;

function TStack.Pop:TActivationRecord;
var
  Ret: TActivationRecord = nil;
  len:integer;
begin
  len := Length(FItems);
  if len > 0 then
  begin
    Ret := FItems[len - 1];
    SetLength(FItems, len - 1);
    FLevel := FLevel - 1;
  end;
  Result := Ret;
end;

function TStack.Peek:TActivationRecord;
var
  Ret: TActivationRecord = nil;
  len:integer;
begin
  len := Length(FItems);
  if len > 0 then
  begin
    Ret := FItems[len - 1];
  end;
  Result := Ret;
end;

procedure TStack.Push(AActRec:TActivationRecord);
var
  len: integer;
begin
  len := Length(FItems);
  SetLength(FItems, Len+1);
  FLevel := FLevel + 1;
  FItems[len] := AActRec;
end;

end.

