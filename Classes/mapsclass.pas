unit MapsClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  { Globals }
    VariablesGlobals,
    TypesGlobals;

{type
  TMap = class

  end;

procedure TMap.ResetMaps;
function TMap.MapExists(MapName:string;IsWild:boolean=False):integer;
function TMap.AddMap(Params:string;IsWild:boolean):integer;
procedure TMap.AddPair(Params:string;IsWild:boolean=False);
function TMap.KeyExists(MapIndex:integer;AKey:string):integer;
function TMap.ReturnValue(MapName,AKey:string):string;
function TMap.ReturnWildValue(MapName,ASearch:string):string;
procedure TMap.SetDefault(Params:string;IsWild:boolean=False);}

implementation
{uses MetaGenXUtils, StrUtils;
procedure ResetMaps;
begin
    SetLength(Maps,0);
end;
function MapExists(MapName:string;IsWild:boolean=False):integer;
var
   M:TMap;
   i:integer=-1;
   Ret:boolean = False;
begin
    for M in Maps do
    begin
        i:=i+1;
        if (M.Name = MapName) and (M.WILD = IsWild) then
        begin
            Ret:=True;
            Break;
        end;
    end;
    if Ret then
        Result:=i
    else
        Result:=-1;
end;

function AddMap(Params:string;IsWild:boolean):integer;
var
   Temp:string;
   Len:integer;
   Ret:integer;
begin
    Temp:=Trim(Params);
    Temp:=Copy(Temp,1,Pos(SEC_PARAM,Temp)-1); //MapName
    Ret:=MapExists(Temp,IsWild);
    if Ret = -1 then
    begin
        Len:=Length(Maps);
        SetLength(Maps,Len+1);
        Maps[Len].Name:=Temp;
        Maps[Len].MapDefault:='';
        Maps[Len].WILD:=IsWild;
        SetLength(Maps[Len].Pairs,0);
        Result:=Len;
    end
    else
        Result:=Ret;
end;

procedure AddPair(Params:string;IsWild:boolean=False);
var
   Key,Value:string;
   MapIndex,Len,KeyAvail:integer;
begin
    //@Map:MapName|Key>Value
    Params:=Trim(Params);
    MapIndex:=AddMap(Params,IsWild);
    Key:=Copy(Params,Pos(SEC_PARAM,Params)+1,(Pos(MULTI_PARAM,Params)-Pos(SEC_PARAM,Params)-1));
    Value:=Copy(Params,Pos(MULTI_PARAM,Params)+1,Length(Params));
    KeyAvail:=KeyExists(MapIndex,Key);
    if KeyAvail = -1 then
    begin
        Len:=Length(Maps[MapIndex].Pairs);
        SetLength(Maps[MapIndex].Pairs,Len+1);
        Maps[MapIndex].Pairs[Len].Key:=ParseLine(Key).Line;
        Maps[MapIndex].Pairs[Len].Value:=ParseLine(Value).Line;
    end
    else
    begin
        Maps[MapIndex].Pairs[KeyAvail].Key:=ParseLine(Key).Line;
        Maps[MapIndex].Pairs[KeyAvail].Value:=ParseLine(Value).Line;

    end;
end;

procedure SetDefault(Params:string;IsWild:boolean=False);
var
   Value:string;
   MapIndex:integer;
begin
    //@MapDefault:MapName|Value
    Params:=Trim(Params);
    MapIndex:=MapExists(Copy(Params,1,Pos(SEC_PARAM,Params)-1),IsWild);
    Value:=Copy(Params,Pos(SEC_PARAM,Params)+1,Length(Params));
    Maps[MapIndex].MapDefault:=ParseLine(Value).Line;
end;

function KeyExists(MapIndex:integer;AKey:string):integer;
var
    KeyIndex:integer=-1;
    Found:boolean=False;
    APair:TPairKV;
begin
    for APair in Maps[MapIndex].Pairs do
    begin
        Inc(KeyIndex);
        if APair.Key = AKey then
        begin
            Found:=True;
            Break;
        end;
    end;
    if Found then
        Result:=KeyIndex
    else
        Result:=-1;
end;

function ReturnValue(MapName,AKey:string):string;
var
    MapIndex,KeyIndex:integer;
begin
    MapIndex:=MapExists(MapName);
    if MapIndex > -1 then
    begin
      KeyIndex:=KeyExists(MapIndex,AKey);
      if KeyIndex > -1 then
          Result:=Maps[MapIndex].Pairs[KeyIndex].Value
      else
          Result:=Maps[MapIndex].MapDefault;
    end
    else
        Result:='';
end;

function ReturnWildValue(MapName,ASearch:string):string;
var
    MapIndex,KeyIndex,Jump,i:integer;
    APair:TPairKV;
    Temp,Right,AWild,Ret:string;
    WildLeft,WildRight,Match:boolean;
    Wilds:array of string;
begin
    MapIndex:=MapExists(MapName,WILD);
    if MapIndex > -1 then
    begin
        Ret:=Maps[MapIndex].MapDefault;
        if Length(Maps[MapIndex].Pairs) > 0 then
        begin
            for i:=0 to Length(Maps[MapIndex].Pairs)-1 do
            begin
                Right:=Maps[MapIndex].Pairs[i].Key;
                Match:=True;
                repeat
                    Jump:=Pos('*',Right);
                    if Jump = 1 then
                        Jump:=Pos('*',Copy(Right,2,Length(Right)))+1;
                    if (Jump = 1) or (Jump = 0) then
                        Jump:=Length(Right);
                    Temp:=Copy(Right,1,Jump);
                    Right:=Copy(Right,Length(Temp),Length(Right));
                    WildLeft:=Pos('*',Temp) = 1;
                    WildRight:=RPos('*',Temp) = Length(Temp);
                    AWild:=ReplaceStr(Temp,'*','');
                    if WildLeft and WildRight then
                        Match:=Pos(AWild,ASearch) > 0
                    else if WildRight then
                        Match:=Pos(AWild,ASearch) = 1
                    else if WildLeft then
                        Match:=Pos(AWild,ASearch) = (Length(ASearch) - Length(AWild))+1
                    else
                        Match:=(ASearch = AWild);
                    if not Match then
                       break;
                until Length(Right) = 1;
                if Match then
                begin
                    Ret:=Maps[MapIndex].Pairs[i].Value;
                    Break;
                end;
            end;
        end;
    end;
    Result:=Ret;
end;                }

end.

