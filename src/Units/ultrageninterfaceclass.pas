unit UltragenInterfaceClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils,
      ASTClass, LexerClass, ImpParserClass, InterpreterClass,
      StrUtils, LoggingClass, httpdefs;

type
  TUltraInterface = class
    public
      class function ParseString(AStringCode:string): TAST;
      class function ParseStringList(var AList: TStringList): TAST;
      class function ParseWebRequest(var ARequest: TRequest): TAST;
      class function InterpretScript(AFilePath: string; APreludes: TProgram = nil): string;
	end;

implementation

class function TUltraInterface.ParseString(AStringCode: string): TAST;
var
  AParser: TTParser;
  ALexer: TLexer;
  ATree: TAST;
begin
  ALexer := TLexer.Create(AStringCode, False);
  AParser := TTParser.Create(ALexer);
  ATree := AParser.ParseCode();
  AParser.Free;
  Result := ATree;
end;

class function TUltraInterface.InterpretScript(AFilePath: string; APreludes: TProgram = nil): string;
var
  len, i: integer;
  AParser: TTParser;
  ALexer: TLexer;
  ATree: TAST;
  AInter: TInterpreter;
  LiveOut: string;
begin
  ALexer := TLexer.Create(AFilePath);
  AParser := TTParser.Create(ALexer);
  ATree := AParser.ParseCode();
  if APreludes <> nil then
  begin
    len := Length(APreludes.PChildren);
	  if len > 0 then
	  begin
	    for i:=0 to len-1 do
	      TProgram(ATree).AddPrelude(APreludes.PChildren[i]);
	  end;
	end;
  AParser.Free;
	AInter := TInterpreter.Create(ATree);
	AInter.Interpret;
	LiveOut := AInter.PLive;
	AInter.Free;
  Result := LiveOut;
end;

class function TUltraInterface.ParseStringList(var AList: TStringList): TAST;
begin
  AList.LineBreak := #10;
  Result := ParseString(AList.Text);
end;

class function TUltraInterface.ParseWebRequest(var ARequest: TRequest): TAST;
var
  WebVars: TStringList;
  AParser: TTParser;
  ALexer: TLexer;
  ATree: TAST;
  len, i: integer;
  comma, K, V: string;
begin
  WebVars := TStringList.Create;
  WebVars.Add('$request = {');
  Webvars.Add('"route": "'+ARequest.URI+'", ');
  Webvars.Add('"method": "'+ARequest.Method+'", ');
  Webvars.Add('"querystring": "'+ARequest.QueryString+'", ');
  WebVars.Add('"query": {');
  len := ARequest.QueryFields.Count;
  if len > 0 then
  begin
    comma := ', ';
    for i:=0 to len - 1 do
    begin
      ARequest.QueryFields.GetNameValue(i, K, V);
      if i = len - 1 then
        comma := '';
      WebVars.Add('"'+K+'": "'+V+'"'+comma);
    end;
  end;
  WebVars.Add('}, ');
  WebVars.Add('"content_type": "'+ARequest.ContentType+'", ');
  WebVars.Add('"body_content": "'+ReplaceStr(ARequest.Content, '"', '\"')+'", ');
  WebVars.Add('"body": {');
  len := ARequest.ContentFields.Count;
  if len > 0 then
  begin
    comma := ', ';
    for i:=0 to len - 1 do
    begin
      ARequest.ContentFields.GetNameValue(i, K, V);
      writeln(k, ': ', v);
      if i = len - 1 then
        comma := '';
      WebVars.Add('"'+K+'": "'+V+'"'+comma);
    end;
  end;
  WebVars.Add('}');
  WebVars.Add('}');
  WebVars.Add('$request.lock()');
  ALexer := TLexer.Create(WebVars.Text, False);
  AParser := TTParser.Create(ALexer);
  ATree := AParser.ParseCode();
  AParser.Free;
  WebVars.Free;
  Result := ATree;
end;

end.

