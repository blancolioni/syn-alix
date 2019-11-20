with Ada.Directories;
with Ada.Strings.Fixed;

package body Syn.File_Writer is

   Right_Margin : constant := 78;
   Next_Temporary : Integer := 0;

   function "-" (Item : Ada.Strings.Unbounded.Unbounded_String)
                 return String
   is (Ada.Strings.Unbounded.To_String (Item));

   function "+" (Item : String)
                 return Ada.Strings.Unbounded.Unbounded_String
   is (Ada.Strings.Unbounded.To_Unbounded_String (Item));

   function Temporary_File
     (Path : String)
      return String;

   function File_Changed
     (Original_File_Path : String;
      New_File_Path      : String)
      return Boolean;

   -----------
   -- Close --
   -----------

   overriding
   procedure Close (Item : in out File_Writer) is
      Item_Path : constant String := -Item.Path;
      Temp_Path : constant String := -Item.Temp_Path;
   begin
      Item.Flush;
      Ada.Text_IO.Close (Item.File);

      if Item_Path /= Temp_Path then
         if File_Changed (Item_Path, Temp_Path) then
            Ada.Directories.Delete_File (Item_Path);
            Ada.Directories.Rename (Temp_Path, Item_Path);
         else
            Ada.Directories.Delete_File (Temp_Path);
         end if;
      end if;
   end Close;

   ---------
   -- Col --
   ---------

   overriding
   function Col (Writer : File_Writer) return Positive is
   begin
      return Writer.Line_Length + 1;
   end Col;

   ------------
   -- Create --
   ------------

   overriding
   procedure Create
     (Item : in out File_Writer;
      Path : in     String)
   is
   begin
      Item.Path := +Path;
      if Ada.Directories.Exists (Path) then
         Item.Temp_Path := +(Temporary_File (Path));
      else
         Item.Temp_Path := Item.Path;
      end if;
      Ada.Text_IO.Create (Item.File, Ada.Text_IO.Out_File, -Item.Temp_Path);
      Item.Line_Length := 0;
      Item.Optional_NL := 0;
   end Create;

   ------------------
   -- File_Changed --
   ------------------

   function File_Changed
     (Original_File_Path : String;
      New_File_Path      : String)
      return Boolean
   is
      use Ada.Text_IO;
      Old_File, New_File : File_Type;
      Changed            : Boolean := False;
   begin
      Open (Old_File, In_File, Original_File_Path);
      Open (New_File, In_File, New_File_Path);
      while not End_Of_File (Old_File) loop
         if End_Of_File (New_File) then
            Changed := True;
            exit;
         end if;

         declare
            Old_Line : constant String := Get_Line (Old_File);
            New_Line : constant String := Get_Line (New_File);
         begin
            if Old_Line /= New_Line then
               Changed := True;
               exit;
            end if;
         end;
      end loop;

      if not End_Of_File (New_File) then
         Changed := True;
      end if;
      Close (Old_File);
      Close (New_File);
      return Changed;
   end File_Changed;

   -----------
   -- Flush --
   -----------

   procedure Flush (Writer : in out File_Writer) is
   begin
      if Writer.Line_Length > 0 then
         Ada.Text_IO.New_Line;
      end if;
   end Flush;

   ------------
   -- Indent --
   ------------

   overriding
   function Indent (Writer : File_Writer) return Natural is
   begin
      return Writer.Indent;
   end Indent;

   ------------
   -- Indent --
   ------------

   overriding
   procedure Indent (Writer : in out File_Writer;
                     Value  : in     Natural)
   is
   begin
      Writer.Indent := Value;
   end Indent;

   ----------
   -- Line --
   ----------

   overriding function Line
     (Writer : File_Writer)
      return Positive
   is
   begin
      return Positive (Ada.Text_IO.Line (Writer.File));
   end Line;

   --------------
   -- New_Line --
   --------------

   overriding
   procedure New_Line (Writer : in out File_Writer) is
   begin
      if Writer.First_On_Line
        and then Writer.Have_Empty_Line
      then
         return;
      end if;

      if Writer.First_On_Line then
         Writer.Have_Empty_Line := True;
      end if;

      Ada.Text_IO.Put_Line (Writer.File,
                            Writer.Current_Line (1 .. Writer.Line_Length));
      Writer.Line_Length := 0;
      Writer.Optional_NL := 0;
      Writer.First_On_Line := True;

   end New_Line;

   -----------------------
   -- Optional_New_Line --
   -----------------------

   overriding
   procedure Optional_New_Line
     (Writer : in out File_Writer)
   is
   begin
      Writer.Optional_NL := Writer.Col;
   end Optional_New_Line;

   ---------
   -- Put --
   ---------

   overriding
   procedure Put
     (Writer : in out File_Writer;
      Text   : in     String)
   is
      Trimmed : constant String :=
                  Ada.Strings.Fixed.Trim (Text,
                                          Ada.Strings.Right);
   begin
      if Writer.First_On_Line
        and then Writer.Indent > 1
      then
         Writer.Current_Line (1 .. Writer.Indent) :=
         (others => ' ');
         Writer.Line_Length := Writer.Indent;
      end if;

      if Trimmed'Length > 0
        and then Writer.Line_Length + Writer.Pending_Spaces + Trimmed'Length
          >= Right_Margin
        and then Writer.Optional_NL > 0
      then
         declare
            Flushed_Length : constant Natural := Writer.Optional_NL - 1;
            Kept_Start     : constant Natural := Writer.Optional_NL;
            Flushed_Part : constant String :=
                             Writer.Current_Line (1 .. Flushed_Length);
            Kept_Part    : constant String :=
                             Writer.Current_Line
                               (Kept_Start .. Writer.Line_Length);
            Next_Start   : constant String (1 .. Writer.Indent + 2) :=
                             (others => ' ');
            Next_Line    : constant String :=
                             Next_Start & Kept_Part;

         begin
            Ada.Text_IO.Put_Line (Writer.File, Flushed_Part);
            Writer.Current_Line (1 .. Next_Line'Length) := Next_Line;
            Writer.Line_Length := Next_Line'Length;
            Writer.Optional_NL := 0;
         end;
      end if;

      if Trimmed'Length > 0
        and then Writer.Line_Length + Writer.Pending_Spaces + Trimmed'Length
          >= Right_Margin
        and then Ada.Strings.Fixed.Index (Trimmed, ".") > 0
      then
         for I in reverse Trimmed'Range loop
            if Trimmed (I) = '.'
              and then Writer.Line_Length + Writer.Pending_Spaces
                + (I - Trimmed'First)
              < Right_Margin
            then
               Writer.Put (Trimmed (Trimmed'First .. I - 1));
               Writer.New_Line;
               Writer.Put (Trimmed (I .. Trimmed'Last));
               return;
            end if;
         end loop;
      end if;

      if not Writer.First_On_Line
        and then Writer.Pending_Spaces > 0
      then
         Writer.Current_Line (Writer.Line_Length + 1 ..
                                Writer.Line_Length + Writer.Pending_Spaces) :=
             (others => ' ');
         Writer.Line_Length := Writer.Line_Length + Writer.Pending_Spaces;
         Writer.Pending_Spaces := 0;
      end if;

      Writer.First_On_Line := False;

      Writer.Pending_Spaces := Text'Length - Trimmed'Length;
      Writer.Current_Line (Writer.Line_Length + 1 ..
                             Writer.Line_Length + Trimmed'Length) :=
          Trimmed;
      Writer.Line_Length := Writer.Line_Length + Trimmed'Length;

      Writer.Have_Empty_Line :=
        Writer.Have_Empty_Line and then Trimmed'Length = 0;

   end Put;

   -------------
   -- Set_Col --
   -------------

   overriding
   procedure Set_Col (Writer : in out File_Writer;
                      Value  : in     Positive)
   is
   begin
      while Writer.Line_Length < Value - 1 loop
         Writer.Line_Length := Writer.Line_Length + 1;
         Writer.Current_Line (Writer.Line_Length) := ' ';
      end loop;
   end Set_Col;

   --------------------
   -- Temporary_File --
   --------------------

   function Temporary_File
     (Path : String)
      return String
   is
      Directory_Path : constant String :=
                         Ada.Directories.Containing_Directory
                           (Path);
   begin
      Next_Temporary := Next_Temporary - 1;
      return Ada.Directories.Compose
        (Directory_Path, "t" & Integer'Image (Next_Temporary) & ".txt");
   end Temporary_File;

end Syn.File_Writer;
