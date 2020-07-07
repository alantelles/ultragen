unit FileExplorerInstanceClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass, ListInstanceClass;

type
  TFileExplorer = class (TInstanceOf)
    private
      FPath: string;
    public
      property PPath: string read FPath write FPath;
      function GetAllFiles:TListInstance;
      constructor Create;
  end;

implementation
uses
  StrUtils, FileUtil, StringInstanceClass;

constructor TFileExplorer.Create;
begin

end;

function TFileExplorer.GetAllFiles:TListInstance;
var
  AFileEx: TStringList;
  ent: string;
  ret: TListInstance;
begin
  AFileEx := TStringList.Create;
  FindAllFiles(AFileEx, FPath);
  Ret := TListInstance.Create();
  for ent in AFileEx do
    Ret.Add(TStringInstance.Create(ent));
  AFileEx.Free;
  Result := Ret;
end;

end.

