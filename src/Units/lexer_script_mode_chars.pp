         if (FCurrChar = ' ') or (FCurrChar = #13) or (FCurrChar = #9) then
		      begin
		        SkipSpace;
		        continue;
		      end;

		      if (FCurrChar = T_STRENC_SINGLE) then
		      begin
		        Result := GetString(T_STRENC_SINGLE);
		        exit;
		      end;

          if (FExtension <> '.ultra') and ((FCurrChar + Peek(1)) = '}}') then
				  begin
				      FInterpol := False;
              FScriptMode := False;
				      Advance;
				      Advance;
				      Result := TToken.Create(T_INTERPOLATION_END, '}}', FScriptLine, FLineChar, FFileName);
				      exit
				  end;

          if (FCurrChar + Peek(1)) = '0x' then
          begin
              Result := TToken.Create(TYPE_BYTE, GetHexaNum(), FScriptLine, FLineChar, FFileName);
              Exit;
          end;

		      {if (FCurrChar + Peek(1) = '$_') then
		      begin
		        Result := GetInnerAttribute();
		        exit
				  end;}

		      if (FCurrChar + Peek(Length(T_STRENC_MULTI) - 1)) = T_STRENC_MULTI then
		      begin
		        Result := GetString(T_STRENC_MULTI);
		        exit;
		      end;

          if (FCurrChar + Peek(3)) = T_LANG_TRUE then
          begin
            Advance(4);
            Result := TToken.Create(TYPE_BOOLEAN, T_LANG_TRUE, FScriptLine, FLineChar, FFileName);
            Exit;
          end;

          if (FCurrChar + Peek(4)) = T_LANG_FALSE then
          begin
            Advance(5);
            Result := TToken.Create(TYPE_BOOLEAN, T_LANG_FALSE, FScriptLine, FLineChar, FFileName);
            Exit;
          end;

		      if (FCurrChar = T_STRENC_DOUBLE) then
		      begin
		        Result := GetString(T_STRENC_DOUBLE);
		        exit;
		      end;

		      {if (FCurrChar + Peek(2)) = 'end' then
		      begin
		        Advance(3);

            try
              AuxStr := FScopeType[FSCopeType.Count-1];
		          FScopeType.Delete(FScopeType.Count-1);

            except
              EParseError('Using "end" in a non-block scope');
            end;
            Result := TToken.Create(T_END+AuxStr,T_END+AuxStr, FScriptLine, FLineChar, FFileName);
		        exit
		      end;}

          if (FCurrChar + Peek(2)) = '###' then
          begin
            Advance(3);
            PassBlockComment();
            Result := GetNextToken();
            exit
          end;


		      // and , or

		      if (FCurrChar + Peek(1)) = '&&' then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_AND, '&&', FScriptLine, FLineChar, FFileName);
		        exit
		      end;

		      if (FCurrChar + Peek(1)) = '||' then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_OR, '||', FScriptLine, FLineChar, FFileName);
		        exit
		      end;

		      // leq, geq, neq
		      if (FCurrChar = '<') and (Peek = '=') then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_LEQ, '<=', FScriptLine, FLineChar, FFileName);
		        exit
		      end;

		      if (FCurrChar = '>') and (Peek = '=') then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_GEQ, '>=', FScriptLine, FLineChar, FFileName);
		        exit
		      end;

		      if (FCurrChar = '!') and (Peek = '=') then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_NEQ, '!=', FScriptLine, FLineChar, FFileName);
		        exit
		      end;

          // inc/dec shortcuts

          if (FCurrChar = '+') and (Peek = '=') then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_SHORT_INC, '+=', FScriptLine, FLineChar, FFileName);
		        exit
		      end;

          if (FCurrChar = '-') and (Peek = '=') then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_SHORT_DEC, '-=', FScriptLine, FLineChar, FFileName);
		        exit
		      end;

          if (FCurrChar = '*') and (Peek = '=') then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_SHORT_MULT, '*=', FScriptLine, FLineChar, FFileName);
		        exit
		      end;

          if (FCurrChar = '/') and (Peek = '=') then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_SHORT_DIV, '/=', FScriptLine, FLineChar, FFileName);
		        exit
		      end;

          if (FCurrChar = T_LINE_COMMENT) then
		      begin
		        Advance;
            PassLineComment;
            Result := GetNextToken();
		        exit
		      end;

		      // end leq, neq, geq


		      // end and, or

		      if FCurrChar = '!' then
		      begin
		        Advance;
		        Result := TToken.Create(T_NOT, '!', FScriptLine, FLineChar, FFileName);
		        exit
		      end;

		      if FCurrChar = '>' then
		      begin
		        Advance;
		        Result := TToken.Create(T_GT, '>', FScriptLine, FLineChar, FFileName);
		        exit
		      end;

		      if FCurrChar = '<' then
		      begin
		        Advance;
		        Result := TToken.Create(T_LT, '<', FScriptLine, FLineChar, FFileName);
		        exit
		      end;

		      if (FCurrChar = '=') and (Peek = '=') then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_EQ, '==', FScriptLine, FLineChar, FFileName);
		        exit
		      end;

		      if FCurrChar = '=' then
		      begin
		        Advance;
		        Result := TToken.Create(T_ASSIGN, '=', FScriptLine, FLineChar, FFileName);
		        exit
		      end;

		      if (FCurrChar = #10)  then
		      begin
		        Advance;
		        FScriptLine := FScriptLine + 1;
		        FLineChar := 1;
		        Result := TToken.Create(T_NEWLINE, sLineBreak, FScriptLine, FLineChar, FFileName);
		        exit
		      end;

		      if Pos(FCurrChar, SET_NUMBERS) > 0 then
		      begin
		        AuxStr := GetNumber();
		        if (Pos('.', AuxStr) = 0) then
		          Result := TToken.Create(TYPE_INTEGER, AuxStr, FScriptLine, FLineChar, FFileName)
		        else
		          Result := TToken.Create(TYPE_FLOAT, AuxStr, FScriptLine, FLineChar, FFileName);
		        exit
				  end;

		      if FCurrChar = '+' then
		      begin
		        Advance;
		        Result := TToken.Create(T_PLUS, '+', FScriptLine, FLineChar, FFileName);
		        exit
				  end;

		      if FCurrChar = '*' then
		      begin
		        Advance;
		        Result := TToken.Create(T_MULT, '*', FScriptLine, FLineChar, FFileName);
		        exit
				  end;


		      if (FCurrChar + Peek(1)) = '//' then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_INT_DIV, '//', FScriptLine, FLineChar, FFileName);
		        exit
				  end;

		      if FCurrChar = '/' then
		      begin
		        Advance;
		        Result := TToken.Create(T_DIV, '/', FScriptLine, FLineChar, FFileName);
		        exit
				  end;

		      if FCurrChar = '%' then
		      begin
		        Advance;
		        Result := TToken.Create(T_MODULUS, '%', FScriptLine, FLineChar, FFileName);
		        exit
				  end;


		      if FCurrChar = '-' then
		      begin
		        Advance;
		        Result := TToken.Create(T_MINUS, '-', FScriptLine, FLineChar, FFileName);
		        exit
				  end;

		      if FCurrChar = '(' then
		      begin
		        Advance;
		        Result := TToken.Create(T_LPAREN, '(', FScriptLine, FLineChar, FFileName);
		        exit
				  end;

		      if FCurrChar = ')' then
		      begin
		        Advance;
		        Result := TToken.Create(T_RPAREN, ')', FScriptLine, FLineChar, FFileName);
		        exit
				  end;

		      if FCurrChar = '[' then
		      begin
		        Advance;
		        Result := TToken.Create(T_LIST_START, T_LIST_START, FScriptLine, FLineChar, FFileName);
		        Exit
		      end;

		      if FCurrChar = ']' then
		      begin
		        Advance;
		        Result := TToken.Create(T_LIST_END, T_LIST_END, FScriptLine, FLineChar, FFileName);
		        exit
		      end;

          if FCurrChar = ':' then
          begin
            Advance;
            Result := TToken.Create(T_DICT_ASSIGN, T_DICT_ASSIGN, FScriptLine, FLineChar, FFileName);
            exit
          end;


		      if Pos(FCurrChar, LETTERS + '_' + '$') > 0 then
		      begin
		        Result := GetId();
		        exit
		      end;

		      if FCurrChar = '.' then
		      begin
		        Advance;
		        Result := TToken.Create(T_ATTR_ACCESSOR, ATTR_ACCESSOR, FScriptLine, FLineChar, FFileName);
		        exit;
		      end;

          if FCurrChar = '@' then
          begin
            Advance;
            Result := GetModulePath();
            exit;
					end;

          if FCurrChar = '{' then
          begin
            Advance;
            Result := TToken.Create(T_DICT_START, '{', FScriptLine, FLineChar, FFileName);
            exit
          end;

          if FCurrChar = '}' then
          begin
            Advance;
            Result := TToken.Create(T_DICT_END, '}', FScriptLine, FLineChar, FFileName);
            exit
          end;

		      if (FCurrChar = ',') then
		      begin
		        Advance;
		        Result := TToken.Create(T_COMMA, ',', FScriptLine, FLineChar, FFileName);
		        exit
		      end;
