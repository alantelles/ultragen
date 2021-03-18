unit TypeLoaderClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass, InterpreterClass, ARClass;

type TTypeLoader = class
  public
    class procedure LoadType(AName: string; var AInter: Tinterpreter; var AActRec: TActivationRecord);
    class procedure LoadRequest(var AActRec: TActivationRecord);
    class procedure LoadFileSystem(var AActRec: TActivationRecord);
    class procedure LoadOS(var AActRec: TActivationRecord);
    class procedure LoadDateTime(var AActRec: TActivationRecord);
    class procedure LoadResponseHandler(var AActRec: TActivationRecord);
    class procedure LoadServerInstance(var AActRec: TActivationRecord);
    // class procedure LoadCookiesHandler(var AActRec: TActivationrecord);
end;

implementation


class procedure TTypeLoader.LoadOS(var AActRec: TActivationRecord);
var
  AFunc: TFunctionInstance;
  AType: TDataType;
begin
  AType := TDataType.Create('TOSInstance', 'OS');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TOSInstance', True,False, False);
  AType.PMembers.Add('getEnv', AFunc);
  AActRec.AddMember('OS', AType);
end;

{class procedure TTypeLoader.LoadCookiesHandler(var AActRec: TActivationRecord);
var
  ADataType: TDataType;
  AFunc: TFunctionInstance;
begin
  ADataType := TDataType.Create('TCookiesHandlerInstance', 'Cookies');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TCookiesHandlerInstance', True, False, False);
  ADataType.PMembers.Add('set', AFunc);
  ADataType.PMembers.Add('get', AFunc);
  ADataType.PMembers.Add('unset', AFunc);
  AActRec.AddMember('Cookies', ADataType);
end;}

class procedure TTypeLoader.LoadResponseHandler(var AActRec: TActivationRecord);
var
  ADataType: TDataType;
  AFunc: TFunctionInstance;
begin
  ADataType := TDataType.Create('TAppResponseInstance', 'AppResponse');
  AFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TAppResponseInstance', True, False, False);
  ADataType.PMembers.Add('redirect', AFunc);
  ADataType.PMembers.Add('clientRedirect', AFunc);
  ADataType.PMembers.Add('setStatusCode', AFunc);
  ADataType.PMembers.Add('setStatusText', AFunc);
  ADataType.PMembers.Add('setContentType', AFunc);
  ADataType.PMembers.Add('setHeader', AFunc);
  ADataType.PMembers.Add('static', AFunc);
  AActRec.AddMember('AppResponse', ADataType);
end;

class procedure TTypeLoader.LoadServerInstance(var AActRec: TActivationRecord);
var
  AServerType: TDataType;
  AServerFunc: TFunctionInstance;
begin
  AServerType := TDataType.Create('TServerInstance', 'Server');
  AServerFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TServerInstance', True,False, False);
  AServerType.PMembers.Add('setPort', AServerFunc);
  AServerType.PMembers.Add('setRootFile', AServerFunc);
  AServerType.PMembers.Add('run', AServerFunc);
  AServerType.PMembers.Add('init', AServerFunc);
  AServerType.PMembers.Add('setTitle', AServerFunc);
  AServerType.PMembers.Add('setStaticPath', AServerFunc);
  AServerType.PMembers.Add('setStaticPaths', AServerFunc);
  AServerType.PMembers.Add('setExceptionHandler', AServerFunc);
  AServerType.PMembers.Add('setMimeTypesFile', AServerFunc);
  AServerType.PMembers.Add('setStopRoute', AServerFunc);
  AActRec.AddMember('Server', AServerType);
end;

class procedure TTypeLoader.LoadDateTime(var AActRec: TActivationRecord);
var
  ADateTimeFunc: TFunctionInstance;
  ADateTimeType: TDataType;
begin
  ADateTimeFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TDateTimeInstance', True, False, False);
  ADateTimeType := TDataType.Create('TDateTimeInstance', 'DateTime');
  ADateTimeType.PMembers.Add('now', ADateTimeFunc);
  ADateTimeType.PMembers.Add('year', ADateTimeFunc);
  ADateTimeType.PMembers.Add('month', ADateTimeFunc);
  ADateTimeType.PMembers.Add('day', ADateTimeFunc);
  ADateTimeType.PMembers.Add('hour', ADateTimeFunc);
  ADateTimeType.PMembers.Add('minute', ADateTimeFunc);
  ADateTimeType.PMembers.Add('second', ADateTimeFunc);
  ADateTimeType.PMembers.Add('milli', ADateTimeFunc);
  AActRec.AddMember('DateTime', ADateTimeType);
end;

class procedure TTypeLoader.LoadRequest(var AActRec: TActivationRecord);
var
  AHttpClientFunc: TFunctionInstance;
  AHttpClientType, AHttpResponseType: TDataType;
begin
  AHttpClientFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'THttpClientInstance', True, False, False);
  AHttpClientType := TDataType.Create('THttpClientInstance', 'Request');
  AHttpClientType.PMembers.Add('get', AHttpClientFunc);
  AHttpClientType.PMembers.Add('post', AHttpClientFunc);
  AHttpClientType.PMembers.Add('put', AHttpClientFunc);
  AHttpClientType.PMembers.Add('delete', AHttpClientFunc);
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
  AFSFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TFileSystemInstance', True, False, False);
  AFSType.PMembers.Add('loadText', AFSFunc);
  AFSType.PMembers.Add('mkdir', AFSFunc);
  AFSType.PMembers.Add('isFile', AFSFunc);
  AFSType.PMembers.Add('isDir', AFSFunc);
  AFSType.PMembers.Add('getAllFiles', AFSFunc);
  AActRec.AddMember('FileSystem', AFSType);
end;

class procedure TTypeLoader.LoadType(AName: string; var AInter: Tinterpreter; var AActRec: TActivationRecord);
begin
  if AName = 'Request' then
    TTypeLoader.LoadRequest(AActRec)
  else if AName = 'FileSystem' then
    TTypeLoader.LoadFileSystem(AActRec)
  {else if AName = 'Cookies' then
    TTypeLoader.LoadCookiesHandler(AActRec)}
  else if AName = 'OS' then
    TTypeLoader.LoadOS(AActRec)
  else if AName = 'DateTime' then
    TTypeLoader.LoadDateTime(AActRec)
  else if AName = 'Server' then
    TTypeLoader.LoadServerInstance(AActRec)
  else if AName = 'AppResponse' then
    TTypeLoader.LoadResponseHandler(AActRec)
  else
    AInter.RaiseException('Type "'+AName+'" does not exist and can''t be loaded', 'RunTime');
end;

end.


