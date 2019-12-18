unit WebTemplateClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, TemplateClass;

type
  TWebTemplate = class(TTemplate)
    protected
      FSessionFile: string;
      FSessionId: string;
      FSessionPath:string;
    public
      property SessionFile:string read FSessionFile write FSessionFile;
      property SessionId:string read FSessionId write FSessionId;
      property SessionPath:string read FSessionPath write FSessionPath;
      constructor Create(AppLoader, ASessionFile, ASessionId, ASessionPath:string);
	end;

implementation
constructor TWebTemplate.Create(AppLoader, ASessionFile, ASessionId, ASessionPath:string);
begin
  Inherited Create(AppLoader);
  FSessionFile := ASessionFile;
  FSessionId := ASessionId;
  FSessionPath := ASessionPath;
end;

end.

