AIntType := TDataType.Create('TIntegerInstance', 'Integer');
AIntFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TIntegerInstance', True);
AIntType.PMembers.Add('fixed', AIntFunc);
AIntType.PMembers.Add('cycle', AIntFunc);
AIntType.PMembers.Add('leftZeros', AIntFunc);
AActRec.AddMember('Integer', AIntType);
