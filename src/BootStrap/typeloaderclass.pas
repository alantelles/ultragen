unit TypeLoaderClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass, InterpreterClass, ARClass, ByteStreamClass;

type
  TTypeLoader = class
  public
    class procedure LoadType(AName: string; var AInter: Tinterpreter;
      var AActRec: TActivationRecord);
    class procedure LoadRequest(var AActRec: TActivationRecord);
    class procedure LoadFileSystem(var AActRec: TActivationRecord);
    class procedure LoadOS(var AActRec: TActivationRecord);
    class procedure LoadDateTime(var AActRec: TActivationRecord);
    class procedure LoadResponseHandler(var AActRec: TActivationRecord);
    class procedure LoadBrookResponseHandler(var AActRec: TActivationRecord);
    class procedure LoadServerInstance(var AActRec: TActivationRecord);
    class procedure LoadByteStream(var AActRec: TActivationRecord);
    class procedure LoadMarkdownParser(var AActRec: TActivationrecord);
    class procedure LoadBrookserver(var AActRec: TActivationRecord);
    class procedure LoadHelpers(var AActRec: TActivationRecord);
    class procedure LoadBrookUploaded(AActRec: TActivationRecord);
    class procedure LoadDBInstance(AActRec: TActivationRecord);
    class procedure LoadRegex(AActRec: TActivationRecord);
  end;

implementation

class procedure TTypeLoader.LoadRegex(AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TRegexInstance', True, False, False, True);
  AType := TDataType.Create('TRegexInstance', 'Regex');
  AType.PMembers.Add('match', AFunc);
  AActRec.AddMember('Regex', AType);
end;

//  ARegexFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TRegexInstance', True, False, False, True);
class procedure TTypeLoader.LoadDBInstance(AActRec: TActivationRecord);
var
  AFunc, AFuncSt: TFunctionInstance;
  AType: TDataType;
begin
  Atype := TDataType.Create('TDBInstance', 'DBConnection');
  AFuncSt := TFunctionInstance.Create('BuiltIn', nil, nil, 'TDBInstance',
    True, False, False, False);
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TDBInstance',
    True, False, False, True);
  AType.PMembers.Add('connect', AFunc);
  AType.PMembers.Add('query', AFunc);
  AType.PMembers.Add('close', AFunc);
  AType.PMembers.Add('create', AFuncSt);
  AActRec.AddMember('DBConnection', AType);
end;

class procedure TTypeLoader.LoadHelpers(var AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  Atype := TDataType.Create('THelpersInstance', 'Helpers');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'THelpersInstance',
    True, False, False, False);
  Atype.PMembers.Add('urlencode', AFunc);
  Atype.PMembers.Add('urldecode', AFunc);
  Atype.PMembers.Add('randomToken', AFunc);
  AActRec.AddMember('Helpers', AType);
end;

class procedure TTypeloader.LoadBrookUploaded(AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  AType := TDataType.Create('TBrookUploadedInstance', 'Uploaded');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TUploadedInstance' ,
    True, False, False, True);
  AType.PMembers.Add('save', AFunc);
  AType.PMembers.Add('name', AFunc);
  AType.PMembers.Add('size', AFunc);
  AType.PMembers.Add('contentType', AFunc);
  AActRec.AddMember('Uploaded', AType);
end;

class procedure TTypeLoader.LoadBrookserver(var AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  Atype := TDataType.Create('TBrookServerInstance', 'Server');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TBrookServerInstance',
    True, False, False, True);
  Atype.PMembers.Add('run', AFunc);
  AActrec.AddMember('Server', AType);
end;

class procedure TTypeLoader.LoadMarkdownParser(var AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  Atype := TDataType.Create('TMarkdownParserInstance', 'Markdown');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TMarkdownParserInstance',
    True, False, False, False);
  AType.PMembers.Add('parse', AFunc);
  AType.PMembers.Add('parseFile', AFunc);
  AActRec.AddMember('Markdown', AType);
end;

class procedure TTypeLoader.LoadOS(var AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  AType := TDataType.Create('TOSInstance', 'OS');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TOSInstance',
    True, False, False, False);
  AType.PMembers.Add('getEnv', AFunc);
  AActRec.AddMember('OS', AType);
end;

class procedure TTypeLoader.LoadByteStream(var AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  AType := TDataType.Create('TByteStreamInstance', 'ByteStream');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TByteStreamInstance',
    True, False, False, True);
  AType.PMembers.Add('save', AFunc);
  AType.PMembers.Add('read', AFunc);
  AType.PMembers.Add('write', AFunc);
  AType.PMembers.Add('length', AFunc);
  AActRec.AddMember('ByteStream', AType);
end;

class procedure TTypeLoader.LoadBrookResponseHandler(var AActRec: TActivationRecord);
var
  ADataType: TDataType;
  AFunc: TFunctionInstance;
begin
  ADataType := TDataType.Create('TBrookResponseInstance', 'AppResponse');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TBrookResponseInstance',
    True, False, False, False);

  ADataType.PMembers.Add('redirect', AFunc);
  ADataType.PMembers.Add('clientRedirect', AFunc);
  ADataType.PMembers.Add('static', AFunc);
  AActRec.AddMember('AppResponse', ADataType);
end;

class procedure TTypeLoader.LoadResponseHandler(var AActRec: TActivationRecord);
var
  ADataType: TDataType;
  AFunc: TFunctionInstance;
begin
  ADataType := TDataType.Create('TAppResponseInstance', 'AppResponse');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TAppResponseInstance',
    True, False, False, False);
  AdataType.PMembers.Add('$headers', TDictionaryInstance.Create(
    TActivationRecord.Create('AppResponseHeaders', AR_DICT, -1)));
  AdataType.PMembers.Add('$cookies', TDictionaryInstance.Create(
    TActivationRecord.Create('AppResponseCookies', AR_DICT, -1)));
  ADataType.PMembers.Add('redirect', AFunc);
  ADataType.PMembers.Add('clientRedirect', AFunc);
  ADataType.PMembers.Add('static', AFunc);
  AActRec.AddMember('AppResponse', ADataType);
end;

class procedure TTypeLoader.LoadServerInstance(var AActRec: TActivationRecord);
var
  AServerType: TDataType;
  AServerFunc: TFunctionInstance;
begin
  AServerType := TDataType.Create('TServerInstance', 'Server');
  AServerFunc := TFunctionInstance.Create('BuiltIn', nil, nil,
    'TServerInstance', True, False, False, True);
  AServerType.PMembers.Add('run', AServerFunc);
  AServerType.PMembers.Add('setStaticPath', AServerFunc);
  AServerType.PMembers.Add('setStaticPaths', AServerFunc);
  AActRec.AddMember('Server', AServerType);
end;

class procedure TTypeLoader.LoadDateTime(var AActRec: TActivationRecord);
var
  ADateTimeFunc, ADateTimeFuncStatic: TFunctionInstance;
  ADateTimeType: TDataType;
begin
  ADateTimeFuncStatic := TFunctionInstance.Create('BuiltIn', nil, nil,
    'TDateTimeInstance', True, False, False, False);
  ADateTimeFunc := TFunctionInstance.Create('BuiltIn', nil, nil,
    'TDateTimeInstance', True, False, False, True);
  ADateTimeType := TDataType.Create('TDateTimeInstance', 'DateTime');
  ADateTimeType.PMembers.Add('now', ADateTimeFuncStatic);
  ADateTimeType.PMembers.Add('year', ADateTimeFunc);
  ADateTimeType.PMembers.Add('month', ADateTimeFunc);
  ADateTimeType.PMembers.Add('day', ADateTimeFunc);
  ADateTimeType.PMembers.Add('hour', ADateTimeFunc);
  ADateTimeType.PMembers.Add('minute', ADateTimeFunc);
  ADateTimeType.PMembers.Add('second', ADateTimeFunc);
  ADateTimeType.PMembers.Add('milli', ADateTimeFunc);
  ADateTimeType.PMembers.Add('weekDay', ADateTimeFunc);
  ADateTimeType.PMembers.Add('weekDayAbbr', ADateTimeFunc);
  ADateTimeType.PMembers.Add('weekDayName', ADateTimeFunc);
  ADateTimeType.PMembers.Add('parse', ADateTimeFuncStatic);
  ADateTimeType.PMembers.Add('format', ADateTimeFunc);
  ADateTimeType.PMembers.Add('unixTime', ADateTimeFunc);
  ADateTimeType.PMembers.Add('compare', ADateTimeFunc);
  ADateTimeType.PMembers.Add('compareTime', ADateTimeFunc);
  ADateTimeType.PMembers.Add('compareDate', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addDays', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addMonts', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addYears', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addHours', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addMinutes', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addSeconds', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addMillis', ADateTimeFunc);
  ADateTimeType.PMembers.Add('addWeeks', ADateTimeFunc);
  ADateTimeType.PMembers.Add('isBetween', ADateTimeFunc);

  AActRec.AddMember('DateTime', ADateTimeType);
end;

class procedure TTypeLoader.LoadRequest(var AActRec: TActivationRecord);
var
  AHttpClientFuncStatic, AHttpClientFunc: TFunctionInstance;
  AHttpClientType, AHttpResponseType: TDataType;
begin
  AHttpClientFunc := TFunctionInstance.Create('BuiltIn', nil, nil,
    'THttpClientInstance', True, False, False, True);
  AHttpClientFuncStatic := TFunctionInstance.Create('BuiltIn', nil, nil,
    'THttpClientInstance', True, False, False, False);
  AHttpClientType := TDataType.Create('THttpClientInstance', 'Request');
  AHttpClientType.PMembers.Add('get', AHttpClientFuncStatic);
  AHttpClientType.PMembers.Add('post', AHttpClientFuncStatic);
  AHttpClientType.PMembers.Add('put', AHttpClientFuncStatic);
  AHttpClientType.PMembers.Add('delete', AHttpClientFuncStatic);
  AHttpClientType.PMembers.Add('run', AHttpClientFunc);
  AActrec.AddMember('Request', AHttpClientType);

  AHttpResponseType := TDataType.Create('THttpResponseInstance', 'Response');
  AActrec.AddMember('Response', AHttpResponseType);
end;

class procedure TTypeLoader.LoadFileSystem(var AActRec: TActivationRecord);
var
  AFSType: TDataType;
  AFSFunc: TFunctionInstance;
begin
  AFSType := TDataType.Create('TFileSystemInstance', 'FileSystem');
  AFSFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TFileSystemInstance',
    True, False, False, False);
  AFSType.PMembers.Add('loadText', AFSFunc);
  AFSType.PMembers.Add('mkdir', AFSFunc);
  AFSType.PMembers.Add('isFile', AFSFunc);
  AFSType.PMembers.Add('isDir', AFSFunc);
  AFSType.PMembers.Add('getAllFiles', AFSFunc);
  AFSType.PMembers.Add('copy', AFSFunc);
  AFSType.PMembers.Add('delete', AFSFunc);
  AActRec.AddMember('FileSystem', AFSType);
end;

class procedure TTypeLoader.LoadType(AName: string; var AInter: Tinterpreter;
  var AActRec: TActivationRecord);
begin
  if AName = 'Request' then
    TTypeLoader.LoadRequest(AActRec)
  else if AName = 'FileSystem' then
    TTypeLoader.LoadFileSystem(AActRec)
  {else if AName = 'Cookies' then
    TTypeLoader.LoadCookiesHandler(AActRec)}
  else if AName = 'OS' then
    TTypeLoader.LoadOS(AActRec)
  else if AName = 'ByteStream' then
    TTypeLoader.LoadByteStream(AActRec)
  else if AName = 'Helpers' then
    TTypeLoader.LoadHelpers(AActRec)
  else if AName = 'DateTime' then
    TTypeLoader.LoadDateTime(AActRec)
  else if AName = 'Server' then
    TTypeLoader.LoadServerInstance(AActRec)
  else if AName = 'AppResponse' then
    TTypeLoader.LoadResponseHandler(AActRec)
  else if AName = 'Regex' then
    TTypeLoader.LoadRegex(AActRec)
  else if AName = 'BrookServer' then
    TTypeLoader.LoadBrookserver(AActRec)
  else if AName = 'BrookAppResponse' then
    TTypeLoader.LoadBrookResponseHandler(AActRec)
  else if AName = 'Markdown' then
    TTypeLoader.LoadMarkdownParser(AActRec)
  else if AName = 'BrookUploaded' then
    TTypeLoader.LoadBrookUploaded(AActRec)
  else if AName = 'DBConnection' then
    TTypeLoader.LoadDBInstance(AActRec)
  else
    AInter.RaiseException('Type "' + AName + '" does not exist and can''t be loaded',
      'RunTime');
end;

end.
