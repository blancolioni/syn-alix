package body Syn.Types is

   type Range_Type_Definition is
     new Type_Definition with
      record
         Low, High : access String;
      end record;

   overriding procedure Write
     (Item   : Range_Type_Definition;
      Writer : in out Writer_Interface'Class);

   type Subprogram_Type_Definition is
     new Type_Definition with
      record
         Signature : access Syn.Declarations.Subprogram_Declaration'Class;
      end record;

   overriding procedure Write
     (Item   : Subprogram_Type_Definition;
      Writer : in out Writer_Interface'Class);

   -------------------
   -- Add_Component --
   -------------------

   procedure Add_Component
     (To_Record      : in out Record_Type_Definition'Class;
      Component_Name : in String;
      Component_Type : in Subtype_Indication'Class;
      Is_Access      : in Boolean := False;
      Is_Constant    : in Boolean := False)
   is
   begin
      To_Record.Components.Append
        ((To_Record.Current_Case_Value,
         new String'(Component_Name),
         new Subtype_Indication'Class'(Component_Type),
         null,
         Is_Access, Is_Constant));
   end Add_Component;

   -------------------
   -- Add_Component --
   -------------------

   procedure Add_Component
     (To_Record      : in out Record_Type_Definition'Class;
      Component_Name : in String;
      Component_Type : in String;
      Is_Access      : in Boolean := False;
      Is_Constant    : in Boolean := False)
   is
   begin
      Add_Component (To_Record, Component_Name,
                     Named_Subtype (Component_Type),
                     Is_Access => Is_Access,
                     Is_Constant => Is_Constant);
   end Add_Component;

   -------------------
   -- Add_Component --
   -------------------

   procedure Add_Component
     (To_Record         : in out Record_Type_Definition'Class;
      Component_Name    : in String;
      Component_Type    : in String;
      Component_Default : in String)
   is
   begin
      To_Record.Add_Component (Component_Name,
                               Named_Subtype (Component_Type),
                               Object (Component_Default));
   end Add_Component;

   -------------------
   -- Add_Component --
   -------------------

   procedure Add_Component
     (To_Record      : in out Record_Type_Definition'Class;
      Component_Name : in String;
      Component_Type : in Subtype_Indication'Class;
      Component_Default : in Expression'Class)
   is
   begin
      To_Record.Components.Append
        ((To_Record.Current_Case_Value,
         new String'(Component_Name),
         new Subtype_Indication'Class'(Component_Type),
         new Expression'Class'(Component_Default),
         False, False));
   end Add_Component;

   ----------------
   -- Add_Parent --
   ----------------

   overriding procedure Add_Parent
     (Item : in out Record_Type_Definition;
      Name : in     String)
   is
   begin
      Item.Set_Tagged;
      Interface_Type_Definition (Item).Add_Parent (Name);
   end Add_Parent;

   -----------------
   -- Add_Variant --
   -----------------

   procedure Add_Variant
     (To_Record         : in out Record_Type_Definition'Class;
      Variant_Name      : in     String;
      Variant_Type      : in     String;
      Variant_Default   : in     String := "")
   is
   begin
      To_Record.Variant := True;
      To_Record.Variant_Name := new String'(Variant_Name);
      To_Record.Variant_Type := new String'(Variant_Type);
      if Variant_Default /= "" then
         To_Record.Variant_Default := new String'(Variant_Default);
      end if;
   end Add_Variant;

   --------------
   -- End_Case --
   --------------

   procedure End_Case (Rec : in out Record_Type_Definition'Class) is
   begin
      Rec.Current_Case_Value := null;
   end End_Case;

   -----------------
   -- Has_Variant --
   -----------------

   overriding
   function Has_Variant (Item : Record_Type_Definition) return Boolean is
   begin
      return Item.Variant;
   end Has_Variant;

   ---------------
   -- Is_Tagged --
   ---------------

   overriding function Is_Tagged
     (Item : Record_Type_Definition) return Boolean
   is
   begin
      return Item.Is_Tagged;
   end Is_Tagged;

   --------------------------
   -- New_Range_Definition --
   --------------------------

   function New_Range_Definition
     (Low, High : String)
      return Type_Definition'Class
   is
   begin
      return Range_Type_Definition'
        (Is_Abstract        => False,
         Is_Limited         => False,
         Is_Synchronized    => False,
         Is_Private         => False,
         Visible_Derivation => False,
         Low                => new String'(Low),
         High               => new String'(High));
   end New_Range_Definition;

   ------------------------------------
   -- New_Subprogram_Type_Definition --
   ------------------------------------

   function New_Subprogram_Type_Definition
     (Signature : Syn.Declarations.Subprogram_Declaration'Class)
      return Type_Definition'Class
   is
      Result : constant Subprogram_Type_Definition :=
        Subprogram_Type_Definition'
          (Is_Abstract        => False,
           Is_Limited         => False,
           Is_Synchronized    => False,
           Is_Private         => False,
           Visible_Derivation => False,
           Signature          =>
              new Syn.Declarations.Subprogram_Declaration'Class'
             (Signature));
   begin
      Result.Signature.Set_Anonymous;
      return Result;
   end New_Subprogram_Type_Definition;

   ----------------------
   -- Next_Case_Option --
   ----------------------

   procedure Next_Case_Option (Rec   : in out Record_Type_Definition'Class;
                               Value : in     String)
   is
   begin
      Rec.Case_Values.Append (Value);
      Rec.Current_Case_Value := new String'(Value);
   end Next_Case_Option;

   ------------------
   -- Set_Abstract --
   ------------------

   procedure Set_Abstract (Rec : in out Record_Type_Definition'Class) is
   begin
      Rec.Is_Abstract := True;
   end Set_Abstract;

   ----------------
   -- Set_Tagged --
   ----------------

   procedure Set_Tagged (Rec : in out Record_Type_Definition'Class) is
   begin
      Rec.Is_Tagged := True;
   end Set_Tagged;

   ----------------------------
   -- Set_Visible_Derivation --
   ----------------------------

   procedure Set_Visible_Derivation
     (Rec : in out Record_Type_Definition'Class)
   is
   begin
      Rec.Visible_Derivation := True;
   end Set_Visible_Derivation;

   ----------------
   -- Start_Case --
   ----------------

   procedure Start_Case (Rec   : in out Record_Type_Definition'Class;
                         Value : in     String)
   is
   begin
      Rec.Case_Values.Append (Value);
      Rec.Current_Case_Value := new String'(Value);
   end Start_Case;

   ---------------------
   -- Variant_Default --
   ---------------------

   overriding
   function Variant_Default (Item : Record_Type_Definition) return String is
   begin
      if Item.Variant_Default /= null then
         return Item.Variant_Default.all;
      else
         return "";
      end if;
   end Variant_Default;

   ------------------
   -- Variant_Name --
   ------------------

   overriding
   function Variant_Name (Item : Record_Type_Definition) return String is
   begin
      return Item.Variant_Name.all;
   end Variant_Name;

   ------------------
   -- Variant_Type --
   ------------------

   overriding
   function Variant_Type (Item : Record_Type_Definition) return String is
   begin
      return Item.Variant_Type.all;
   end Variant_Type;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Item        : Record_Type_Definition;
      Writer      : in out Writer_Interface'Class)
   is
      Max_Name_Length : Natural := 0;
      Colon_Indent    : Positive;
      First_Parent    : Boolean := True;

      procedure Write_Component
        (Component : Record_Component);

      ---------------------
      -- Write_Component --
      ---------------------

      procedure Write_Component
        (Component : Record_Component)
      is
      begin
         Writer.Put
           (To_Ada_Name (Component.Component_Name.all));
         Writer.Set_Col (Colon_Indent);
         Writer.Put (": ");
         Writer.Optional_New_Line;
         if Component.Component_Is_Access then
            Writer.Put ("access ");
         end if;
         Component.Component_Type.Write (Writer);
         if Component.Component_Default /= null then
            Writer.Put (" := ");
            Writer.Optional_New_Line;
            Component.Component_Default.Write (Writer);
         end if;

         Writer.Put (";");
         Writer.New_Line;
      end Write_Component;

   begin

      Writer.Optional_New_Line;

      if Item.Is_Abstract then
         Writer.Put ("abstract ");
      end if;

      if not Item.Parents.Is_Empty then

         if Item.Is_Limited then
            Writer.Put ("limited ");
         end if;

         Writer.Indent (Writer.Indent + 2);
         for Parent_Name of Item.Parents loop
            if First_Parent then
               Writer.Put ("new " & Parent_Name);
               First_Parent := False;
            else
               Writer.New_Line;
               Writer.Put ("and " & Parent_Name);
            end if;
         end loop;
         Writer.Put (" with");
         Writer.Indent (Writer.Indent + 1);
      else
         if Item.Is_Tagged then
            Writer.Put ("tagged");
         end if;

         if Item.Is_Limited then
            Writer.Put (" limited ");
         end if;

         Writer.Indent (Writer.Indent + 3);
      end if;

      if (Item.Is_Private or else Item.Visible_Derivation)
        and then Writer.Context = Package_Spec
      then
         Writer.Put (" private");
         Writer.Indent (Writer.Indent - 3);
      elsif Item.Components.Is_Empty then
         Writer.Put (" null record");
         Writer.Indent (Writer.Indent - 3);
      else
         Writer.New_Line;
         Writer.Put ("record");
         Writer.New_Line;
         Writer.Indent (Writer.Indent + 3);

         for Component of Item.Components loop

            declare
               Name : constant String := Component.Component_Name.all;
            begin
               if Name'Length > Max_Name_Length then
                  Max_Name_Length := Name'Length;
               end if;
            end;
         end loop;

         if Item.Variant then

            Colon_Indent := Max_Name_Length + Writer.Indent + 2;

            declare
               Have_Cases : Boolean := False;
            begin

               for Component of Item.Components loop
                  if Component.Component_Case_Value = null then
                     Write_Component (Component);
                  else
                     Have_Cases := True;
                  end if;
               end loop;

               if Have_Cases then
                  Colon_Indent := Colon_Indent + 6;

                  Writer.Put_Line ("case " & Item.Variant_Name.all & " is");
                  Writer.Indent (Writer.Indent + 3);

                  for S of Item.Case_Values loop
                     Writer.Put_Line ("when " & S & " =>");

                     Writer.Indent (Writer.Indent + 3);

                     declare
                        Got_Component : Boolean := False;
                     begin
                        for Component of Item.Components loop
                           if Component.Component_Case_Value /= null
                             and then Component.Component_Case_Value.all = S
                           then
                              Write_Component (Component);
                              Got_Component := True;
                           end if;
                        end loop;
                        if not Got_Component then
                           Writer.Put_Line ("null;");
                        end if;
                     end;
                     Writer.Indent (Writer.Indent - 3);
                  end loop;
                  Writer.Indent (Writer.Indent - 3);
                  Writer.Put_Line ("end case;");
               end if;
            end;

         else

            Colon_Indent := Max_Name_Length + Writer.Indent + 2;

            for Component of Item.Components loop
               Write_Component (Component);
            end loop;
         end if;

         Writer.Indent (Writer.Indent - 3);
         Writer.Put ("end record");
         Writer.Indent (Writer.Indent - 3);
      end if;
   end Write;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Item   : Range_Type_Definition;
      Writer : in out Writer_Interface'Class)
   is
   begin
      Writer.Put ("range ");
      Writer.Put (Item.Low.all);
      Writer.Put (" .. ");
      Writer.Put (Item.High.all);
   end Write;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Item   : Subprogram_Type_Definition;
      Writer : in out Writer_Interface'Class)
   is
   begin
      Writer.Put_Line ("access");
      Writer.Indent (Writer.Indent + 2);
      Item.Signature.Write (Writer);
      Writer.Indent (Writer.Indent - 2);
   end Write;

end Syn.Types;
