unit FCGIModule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb;


type
  TFCGIWebModule = class(TFPWebModule)
    procedure Request(Sender: TObject; ARequest: TRequest;
        AResponse: TResponse; var Handled: Boolean);
    constructor CreateNew(AOwner: TComponent; CreateMode: Integer); override;
  end;




implementation

uses
  TemplateClass, GenFileSetClass, GenFileClass, ConstantsGlobals;

constructor TFCGIWebModule.CreateNew(AOwner: TComponent; CreateMode: Integer);
begin
  inherited CreateNew(AOwner,CreateMode);
  OnRequest := @Request;
end;

procedure TFCGIWebModule.Request(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  RealRoute, Content, AppLoader:string;
  Params:TStringList;
  ATemp:TTemplate;
  AGenSet: TGenFileSet;
  AGenReq, AFileGen:TGenFile;

  i:integer;
  AnAlias:string;
begin

  RealRoute := ARequest.QueryFields.Values['ULTRAGEN_ROUTE'];
  AppLoader := ARequest.QueryFields.Values['APP_LOADER'];
  Params := TStringList.Create;
  ATemp := TTemplate.Create(AppLoader);
  AGenReq := TGenFile.Create;
  AFileGen := TGenFile.Create;
  AGenSet := TGenFileSet.Create;

  if ARequest.QueryFields.Count > 0 then
  begin
    for i := 0 to ARequest.QueryFields.Count - 1 do
      AGenReq.SetValue('_get' + GEN_SUB_LEVEL + ARequest.QueryFields.Names[i],
        ARequest.QueryFields.ValueFromIndex[i]);
  end;

  if ARequest.Files.Count > 0 then
  begin
    //alias: _files:[fieldName]:0
    for i:=0 to ARequest.Files.Count - 1 do
    begin
      AFileGen := TGenFile.Create;
      AFileGen.SetValue('fileName',ARequest.Files[i].FileName);
      AFileGen.SetValue('size',IntToStr(ARequest.Files[i].Size));
      AFileGen.SetValue('tempName',ARequest.Files[i].LocalFileName);
      AnAlias := '_files' + GEN_SUB_LEVEL + ARequest.Files[i].FieldName+ GEN_SUB_LEVEL +IntToStr(i);
      AGenSet.Add(AFileGen,AnAlias);
    end;
    {
    Property FieldName: String Read FFieldName Write FFieldName;
  Property FileName: String Read FFileName Write FFileName;
  Property Stream: TStream Read GetStream;
  Property Size: Int64 Read FSize Write FSize;
  Property ContentType: String Read FContentType Write FContentType;
  Property Disposition: String Read FDisposition Write FDisposition;
  Property LocalFileName: String Read FLocalFileName Write FLocalFileName;
  Property Description: String Read FDescription Write FDescription;
    }
  end;

  if ARequest.ContentFields.Count > 0 then
  begin
    for i := 0 to ARequest.ContentFields.Count - 1 do
    begin
      AGenReq.SetValue('_post' + GEN_SUB_LEVEL + ARequest.ContentFields.Names[i],
        ARequest.ContentFields.ValueFromIndex[i]);
    end;
  end;

  if ARequest.CookieFields.Count > 0 then
  begin
    for i := 0 to ARequest.CookieFields.Count - 1 do
      AGenReq.SetValue('_cookie' + GEN_SUB_LEVEL + ARequest.CookieFields.Names[i], ARequest.CookieFields.ValueFromIndex[i]);
  end;

  AGenReq.SetValue('_route', RealRoute);
  AGenReq.SetValue('_method', ARequest.Method);

  AGenReq.PairsLocked := True;
  AGenSet.Add(AGenReq, 'request');
  ATemp.ParseTemplate(AGenSet);
  Content := ATemp.ParsedLines.Text;
  ATemp.Free;
  Params.Free;
  AResponse.Content := Content;
  // AResponse.Contents.LoadFromFile('./public/zika3.html');
  // AResponse.Contents.Add(RealRoute);
  // AResponse.Contents.Add(ARequest.QueryString);
  Handled := True;
end;

initialization
  RegisterHTTPModule('ultra-fcgi', TFCGIWebModule, True);

end.

