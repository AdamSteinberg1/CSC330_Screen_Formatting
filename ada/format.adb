with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Command_Line, Ada.Text_IO.Text_Streams, Ada.Strings.Unbounded, Ada.Strings.Fixed;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Command_line, Ada.Text_IO.Text_Streams, Ada.Strings.Unbounded, Ada.Strings.Fixed;

procedure format is

    procedure ProcessWord (word : in String; currLine, maxLine, minLine : in out Unbounded_String; currLineNum, maxLineNum, minLineNum : in out Integer) is
    begin
        if(currLine = "") then
            currLine := To_Unbounded_String(word);
            return;
        end if;

        if(word = "") then
            return;
        end if;

        if(Length(currLine) + word'Length + 1 <= 60) then
            currLine := currLine & " " & word;
        else
            Put(Item => currLineNum, Width => 8);
            Put_Line("  " & to_string(currLine));

        if (Length(currLine) <= Length(minLine)) then
            minLine := currLine;
            minLineNum := currLineNum;
        end if;
        if (Length(currLine) >= Length(maxLine)) then
            maxLine := currLine;
            maxLineNum := currLineNum;
        end if;
        currLineNum := currLineNum + 1;
        currLine := To_Unbounded_String(word);
        end if;
    end ProcessWord;

    function RemoveNumbers(word : in String) return String is
        result : Unbounded_String;
    begin
        result := To_Unbounded_String("");
        for c of word loop
            if (c /= '0' and
                c /= '1' and
                c /= '2' and
                c /= '3' and
                c /= '4' and
                c /= '5' and
                c /= '6' and
                c /= '7' and
                c /= '8' and
                c /= '9') then
                    result := result & c;
            end if;
        end loop;
        return to_string(result);
    end RemoveNumbers;


Input  : File_Type;
Buffer : Character;
tempWord : Unbounded_String;
currLine, maxLine, minLine : Unbounded_String;
currLineNum, maxLineNum, minLineNum : Integer;
begin
    if(Argument_Count <= 0) then
        Put_Line("Error there must be one command line argument");
        return;
    end if;

    currLine := To_Unbounded_String("");
    currLineNum := 1;
    maxLine := To_Unbounded_String("");
    minLine := To_Unbounded_String("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"); --60 characters
    maxLineNum := -1;
    minLinenum := -1;

   Open   (File => Input,  Mode => In_File,  Name => Argument(1));
   tempWord := To_Unbounded_String("");
   loop
      Buffer := Character'Input (Stream (Input));
      if(Buffer = ' ' or Buffer = Character'Val(10) or Buffer = Character'Val(13) ) then
        --process each word
        ProcessWord(RemoveNumbers(to_string(tempWord)), currLine, maxLine, minLine, currLineNum, maxLineNum, minLineNum);
        tempWord := To_unBounded_String("");
      else
        tempWord := tempWord & Buffer;
      end if;
   end loop;

exception --jump to here when done reading the file
   when End_Error =>
      if Is_Open(Input) then
         Close (Input);
      end if;

  ProcessWord(RemoveNumbers(to_string(tempWord)), currLine, maxLine, minLine, currLineNum, maxLineNum, minLineNum);
  Put(Item => currLineNum, Width => 8);
  Put_Line("  " & to_string(currLine));
  New_Line;

  if (Length(currLine) <= Length(minLine)) then
      minLine := currLine;
      minLineNum := currLineNum;
  end if;
  if (Length(currLine) >= Length(maxLine)) then
      maxLine := currLine;
      maxLineNum := currLineNum;
  end if;

  Put(head("LONG", 6));
  --Put(Item => maxLineNum, Width => 13);
  Put(head(Integer'Image(maxLineNum), 14));
  Put_Line(to_string(maxLine));

  Put(head("SHORT", 6));
  Put(head(Integer'Image(minLineNum), 14));
  Put_Line(to_string(minLine));

end format;
