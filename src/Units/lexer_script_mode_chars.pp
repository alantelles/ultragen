         if FCurrChar = ' ' then
		      begin
		        SkipSpace;
		        continue;
		      end;

		      if (FCurrChar = T_STRENC_SINGLE) then
		      begin
		        Result := GetString(T_STRENC_SINGLE);
		        exit;
		      end;

          if (FCurrChar + Peek(1)) = '}}' then
				  begin
				      FInterpol := False;
              FScriptMode := False;
				      Advance;
				      Advance;
				      Result := TToken.Create(T_INTERPOLATION_END, '}}');
				      exit
				  end;

		      if (FCurrChar = T_LINE_COMMENT) then
		      begin
		        Advance;
		        Result := TToken.Create(T_COMMENT, PassLineComment);
		        exit
		      end;

		      if (FCurrChar = '$') then
		      begin
		        Result := GetInnerAttribute();
		        exit
				  end;

		      if (FCurrChar + Peek(Length(T_STRENC_MULTI) - 1)) = T_STRENC_MULTI then
		      begin
		        Result := GetString(T_STRENC_MULTI);
		        exit;
		      end;

		      if (FCurrChar = T_STRENC_DOUBLE) then
		      begin
		        Result := GetString(T_STRENC_DOUBLE);
		        exit;
		      end;

		      if (FCurrChar + Peek(2)) = 'end' then
		      begin
		        Advance(3);
		        AuxStr := FScopeType[FSCopeType.Count-1];
		        FScopeType.Delete(FScopeType.Count-1);
		        Result := TToken.Create(T_END+AuxStr,'end of block '+AuxStr);
		        exit
		      end;



		      // and , or

		      if (FCurrChar + Peek(1)) = '&&' then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_AND, '&&');
		        exit
		      end;

		      if (FCurrChar + Peek(1)) = '||' then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_OR, '||');
		        exit
		      end;

		      // leq, geq, neq
		      if (FCurrChar = '<') and (Peek = '=') then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_LEQ, '<=');
		        exit
		      end;

		      if (FCurrChar = '>') and (Peek = '=') then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_GEQ, '>=');
		        exit
		      end;

		      if (FCurrChar = '!') and (Peek = '=') then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_NEQ, '!=');
		        exit
		      end;

		      // end leq, neq, geq


		      // end and, or

		      if FCurrChar = '!' then
		      begin
		        Advance;
		        Result := TToken.Create(T_NOT, '!');
		        exit
		      end;

		      if FCurrChar = '>' then
		      begin
		        Advance;
		        Result := TToken.Create(T_GT, '>');
		        exit
		      end;

		      if FCurrChar = '<' then
		      begin
		        Advance;
		        Result := TToken.Create(T_LT, '<');
		        exit
		      end;

		      if (FCurrChar = '=') and (Peek = '=') then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_EQ, '==');
		        exit
		      end;

		      if FCurrChar = '=' then
		      begin
		        Advance;
		        Result := TToken.Create(T_ASSIGN, '=');
		        exit
		      end;
		      {$IFDEF UNIX}
		      if (FCurrChar = sLineBreak)  then
		      begin
		        Advance;
		        FScriptLine := FScriptLine + 1;
		        FLineChar := 1;
		        Result := TToken.Create(T_NEWLINE, sLineBreak);
		        exit
		      end;
		      {$ENDIF}
		      {$IFDEF Windows}
		      if (FCurrChar + Peek(1)) = sLineBreak  then
		      begin
		        Advance;
		        Advance;
		        FScriptLine := FScriptLine + 1;
		        FLineChar := 1;
		        Result := TToken.Create(T_NEWLINE, sLineBreak);
		        exit
		      end;
		      {$ENDIF}

		      if Pos(FCurrChar, SET_NUMBERS) > 0 then
		      begin
		        AuxStr := GetNumber();
		        if (Pos('.', AuxStr) = 0) then
		          Result := TToken.Create(TYPE_INTEGER, AuxStr)
		        else
		          Result := TToken.Create(TYPE_FLOAT, AuxStr);
		        exit
				  end;

		      if FCurrChar = '+' then
		      begin
		        Advance;
		        Result := TToken.Create(T_PLUS, '+');
		        exit
				  end;

		      if FCurrChar = '*' then
		      begin
		        Advance;
		        Result := TToken.Create(T_MULT, '*');
		        exit
				  end;


		      if (FCurrChar + Peek(1)) = '//' then
		      begin
		        Advance;
		        Advance;
		        Result := TToken.Create(T_INT_DIV, '//');
		        exit
				  end;

		      if FCurrChar = '/' then
		      begin
		        Advance;
		        Result := TToken.Create(T_DIV, '/');
		        exit
				  end;

		      if FCurrChar = '%' then
		      begin
		        Advance;
		        Result := TToken.Create(T_MODULUS, '%');
		        exit
				  end;


		      if FCurrChar = '-' then
		      begin
		        Advance;
		        Result := TToken.Create(T_MINUS, '-');
		        exit
				  end;

		      if FCurrChar = '(' then
		      begin
		        Advance;
		        Result := TToken.Create(T_LPAREN, '(');
		        exit
				  end;

		      if FCurrChar = ')' then
		      begin
		        Advance;
		        Result := TToken.Create(T_RPAREN, ')');
		        exit
				  end;

		      if FCurrChar = '[' then
		      begin
		        Advance;
		        Result := TToken.Create(T_LIST_START, T_LIST_START);
		        Exit
		      end;

		      if FCurrChar = ']' then
		      begin
		        Advance;
		        Result := TToken.Create(T_LIST_END, T_LIST_END);
		        exit
		      end;

          if FCurrChar = ':' then
          begin
            Advance;
            Result := TToken.Create(T_DICT_ASSIGN, T_DICT_ASSIGN);
            exit
          end;


		      if Pos(FCurrChar, LETTERS + '_' ) > 0 then
		      begin
		        Result := GetId();
		        exit
		      end;

		      if FCurrChar = '.' then
		      begin
		        Advance;
		        Result := TToken.Create(T_ATTR_ACCESSOR, ATTR_ACCESSOR);
		        exit;
		      end;

          if FCurrChar = '{' then
          begin
            Advance;
            Result := TToken.Create(T_DICT_START, '{');
            exit
          end;

          if FCurrChar = '}' then
          begin
            Advance;
            Result := TToken.Create(T_DICT_END, '}');
            exit
          end;

		      if (FCurrChar = ',') then
		      begin
		        Advance;
		        Result := TToken.Create(T_COMMA, ',');
		        exit
		      end;
