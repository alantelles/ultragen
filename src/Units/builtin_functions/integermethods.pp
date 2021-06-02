AIntType := TDataType.Create('TIntegerInstance', 'Integer');
AIntFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TIntegerInstance', True, False, False, True);
AIntStFunc := TFunctionInstance.Create('BuiltIn', nil, nil, 'TIntegerInstance', True, False, False, False);
AIntType.PMembers.Add('fixed', AIntFunc);
AIntType.PMembers.Add('cycle', AIntFunc);
AIntType.PMembers.Add('leftZeros', AIntFunc);
AIntType.PMembers.Add('random', AIntStFunc);
AActRec.AddMember('Integer', AIntType);
