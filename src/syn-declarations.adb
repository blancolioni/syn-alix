with Ada.Strings.Fixed;

with Syn.Statements;

package body Syn.Declarations is

   type Use_Package_Declaration is
     new Declaration with
      record
         Package_Name : access String;
      end record;

   overriding
   procedure Write (Item        : Use_Package_Declaration;
                    Writer      : in out Writer_Interface'Class);

   type Use_Type_Declaration is
     new Declaration with
      record
         Type_Name : access String;
      end record;

   overriding
   procedure Write (Item        : Use_Type_Declaration;
                    Writer      : in out Writer_Interface'Class);

   type Pragma_Declaration is
     new Declaration with
      record
         Pragma_Name : access String;
         Pragma_Arg  : access String;
      end record;

   overriding
   procedure Write (Item        : Pragma_Declaration;
                    Writer      : in out Writer_Interface'Class);

   type Renaming_Declaration_Record is
     new Declaration with
      record
         New_Identifier     : access String;
         New_Type           : access String;
         Renamed_Expression : access Expression'Class;
      end record;

   overriding
   procedure Write (Item        : Renaming_Declaration_Record;
                    Writer      : in out Writer_Interface'Class);

   procedure Sort_Subprograms
     (Decs : in out Declaration_Vector.Vector);

   function Single_Identifier (Text : String)
                              return Defining_Identifier_List;

   function New_Formal_Argument
     (From_Object_Declaration : Object_Declaration'Class;
      Mode                    : Argument_Mode;
      Null_Exclusion          : Boolean := False;
      Is_Aliased              : Boolean := False)
     return Formal_Argument'Class;

   type Address_Representation_Declaration is
     new Declaration with
      record
         Object_Name  : access String;
         Address_Expr : access Expression'Class;
      end record;

   overriding procedure Write
     (Item        : Address_Representation_Declaration;
      Writer      : in out Writer_Interface'Class);

   -------------------------
   -- Add_Formal_Argument --
   -------------------------

   procedure Add_Formal_Argument
     (Subprogram : in out Subprogram_Declaration'Class;
      Argument   : in     Formal_Argument'Class)
   is
   begin
      Subprogram.Arguments.Append (Argument);
      if Argument.Defining_Identifiers_Length > Subprogram.Arg_Name_Width then
         Subprogram.Arg_Name_Width :=
           Argument.Defining_Identifiers_Length;
      end if;

   end Add_Formal_Argument;

   -------------------------
   -- Add_Formal_Argument --
   -------------------------

   procedure Add_Formal_Argument
     (Subprogram : in out Subprogram_Declaration'Class;
      Arg_Name   : in     String;
      Arg_Type   : in     String)
   is
   begin
      Subprogram.Add_Formal_Argument
        (New_Formal_Argument
           (Arg_Name,
            Named_Subtype (Arg_Type)));
   end Add_Formal_Argument;

   -------------------------
   -- Add_Formal_Argument --
   -------------------------

   procedure Add_Formal_Argument
     (Subprogram : in out Subprogram_Declaration'Class;
      Arg_Name   : in     String;
      Arg_Mode   : in     Argument_Mode;
      Arg_Type   : in     String)
   is
   begin
      Subprogram.Add_Formal_Argument
        (New_Formal_Argument
           (New_Object_Declaration (Arg_Name, Named_Subtype (Arg_Type)),
            Arg_Mode));
   end Add_Formal_Argument;

   -------------------------
   -- Add_Formal_Argument --
   -------------------------

   procedure Add_Formal_Argument
     (Subprogram     : in out Subprogram_Declaration'Class;
      Arg_Name       : in     String;
      Null_Exclusion : in     Boolean;
      Is_Aliased     : in     Boolean;
      Arg_Mode       : in     Argument_Mode;
      Arg_Type       : in     String)
   is
   begin
      Subprogram.Add_Formal_Argument
        (New_Formal_Argument
           (New_Object_Declaration (Arg_Name, Named_Subtype (Arg_Type)),
            Arg_Mode, Null_Exclusion, Is_Aliased));
   end Add_Formal_Argument;

   -------------------------
   -- Add_Formal_Argument --
   -------------------------

   procedure Add_Formal_Argument
     (Subprogram  : in out Subprogram_Declaration'Class;
      Arg_Name    : in     String;
      Arg_Type    : in     String;
      Arg_Default : in     Expression'Class)
   is
   begin
      Subprogram.Add_Formal_Argument
        (New_Formal_Argument
           (New_Constant_Declaration
              (Arg_Name, Arg_Type,
               Arg_Default),
            In_Argument));
   end Add_Formal_Argument;

   ---------------------------------
   -- Add_Generic_Actual_Argument --
   ---------------------------------

   procedure Add_Generic_Actual_Argument
     (Item  : in out Subprogram_Declaration'Class;
      Value : in     String)
   is
      pragma Assert (Item.Is_Instantiation);
   begin
      Item.Generic_Arguments.Append (Value);
   end Add_Generic_Actual_Argument;

   ---------------------------------
   -- Add_Generic_Actual_Argument --
   ---------------------------------

   procedure Add_Generic_Actual_Argument
     (Item  : in out Subprogram_Declaration'Class;
      Value : in     Integer)
   is
      pragma Assert (Item.Is_Instantiation);
      Image : constant String :=
                Ada.Strings.Fixed.Trim
                  (Integer'Image (Value),
                   Ada.Strings.Left);
   begin
      Item.Add_Generic_Actual_Argument (Image);
   end Add_Generic_Actual_Argument;

   ---------------------------
   -- Add_Local_Declaration --
   ---------------------------

   procedure Add_Local_Declaration
     (Subprogram : in out Subprogram_Declaration;
      Dec        : in     Declaration'Class)
   is
   begin
      Subprogram.Sub_Body.Add_Declaration (Dec);
   end Add_Local_Declaration;

   -------------------
   -- Add_Separator --
   -------------------

   procedure Add_Separator (Item : in out Package_Type) is
   begin
      Item.Append (New_Separator);
   end Add_Separator;

   -----------------------------------
   -- Address_Representation_Clause --
   -----------------------------------

   function Address_Representation_Clause
     (Object_Name : String;
      Address     : Expression'Class)
      return Declaration'Class
   is
   begin
      return Result : Address_Representation_Declaration do
         Result.Object_Name := new String'(Object_Name);
         Result.Address_Expr := new Expression'Class'(Address);
      end return;
   end Address_Representation_Clause;

   ------------
   -- Append --
   ------------

   procedure Append
     (To_Package : in out Package_Type;
      Item       : in     Declaration'Class)
   is
   begin
      Item.Check;
      To_Package.Declarations.Append (Item);
      To_Package.Has_Private :=
        To_Package.Has_Private or else Item.Has_Private_Part;
      To_Package.Has_Body :=
        To_Package.Has_Body or else Item.Has_Body;
   end Append;

   --------------------
   -- Append_To_Body --
   --------------------

   procedure Append_To_Body
     (To_Package : in out Package_Type;
      Item       : in     Declaration'Class)
   is
      Copy : Declaration'Class := Item;
   begin
      Copy.Check;
      Copy.Body_Only := True;
      To_Package.Declarations.Append (Copy);
      To_Package.Has_Body := True;
   end Append_To_Body;

   -----------
   -- Check --
   -----------

   overriding procedure Check (Item : Subprogram_Declaration) is
   begin
      pragma Assert (Item.Name /= null);
   end Check;

   ---------------------------------
   -- Defining_Identifiers_Length --
   ---------------------------------

   function Defining_Identifiers_Length
     (Item : Object_Declaration'Class)
     return Natural
   is
      Result : Natural := 0;
   begin
      for I in 1 .. Item.Objects.List.Last_Index loop
         if I > 1 then
            Result := Result + 2;   -- ", "
         end if;
         Result := Result + Item.Objects.List.Element (I)'Length;
      end loop;
      return Result;
   end Defining_Identifiers_Length;

   --------------
   -- Has_Body --
   --------------

   overriding
   function Has_Body
     (Item : Specification_Separator)
     return Boolean
   is
      pragma Unreferenced (Item);
   begin
      return False;
   end Has_Body;

   --------------
   -- Has_Body --
   --------------

   overriding
   function Has_Body
     (Item : Subtype_Declaration)
     return Boolean
   is
      pragma Unreferenced (Item);
   begin
      return False;
   end Has_Body;

   --------------
   -- Has_Body --
   --------------

   overriding
   function Has_Body
     (Item : Package_Type)
     return Boolean
   is
   begin
      return Item.Has_Body;
   end Has_Body;

   --------------
   -- Has_Body --
   --------------

   overriding
   function Has_Body
     (Item : Object_Declaration)
     return Boolean
   is
      pragma Unreferenced (Item);
   begin
      return False;
   end Has_Body;

   --------------
   -- Has_Body --
   --------------

   overriding
   function Has_Body
     (Item : Subprogram_Declaration)
     return Boolean
   is
   begin
      return not Item.Is_Abstract and then not Item.Is_Instantiation;
   end Has_Body;

   -------------------
   -- Has_Body_Spec --
   -------------------

   overriding
   function Has_Body_Spec
     (Item : Type_Declaration)
     return Boolean
   is
   begin
      return Item.Body_Only or else Item.Is_Deferred;
   end Has_Body_Spec;

   -------------------
   -- Has_Body_Spec --
   -------------------

   overriding
   function Has_Body_Spec
     (Item : Subprogram_Declaration)
      return Boolean
   is
   begin
      return Item.Body_Only;
   end Has_Body_Spec;

   ----------------------
   -- Has_Private_Part --
   ----------------------

   overriding
   function Has_Private_Part
     (Item : Specification_Separator)
     return Boolean
   is
      pragma Unreferenced (Item);
   begin
      return True;
   end Has_Private_Part;

   ----------------------
   -- Has_Private_Part --
   ----------------------

   overriding
   function Has_Private_Part
     (Item : Subtype_Declaration)
     return Boolean
   is
      pragma Unreferenced (Item);
   begin
      return False;
   end Has_Private_Part;

   ----------------------
   -- Has_Private_Part --
   ----------------------

   overriding
   function Has_Private_Part
     (Item : Object_Declaration)
     return Boolean
   is
   begin
      return Item.Is_Deferred;
   end Has_Private_Part;

   ----------------------
   -- Has_Private_Part --
   ----------------------

   overriding
   function Has_Private_Part
     (Item : Package_Type)
     return Boolean
   is
   begin
      return Item.Has_Private;
   end Has_Private_Part;

   ----------------------
   -- Has_Private_Part --
   ----------------------

   overriding
   function Has_Private_Part
     (Item : Type_Declaration)
     return Boolean
   is
   begin
      return Item.Is_Private;
   end Has_Private_Part;

   ----------------------
   -- Has_Private_Part --
   ----------------------

   overriding
   function Has_Private_Part
     (Item : Subprogram_Declaration)
     return Boolean
   is
      pragma Unreferenced (Item);
   begin
      return False;
   end Has_Private_Part;

   ----------------
   -- Identifier --
   ----------------

   function Identifier (Id : String) return Defining_Identifier_List is
   begin
      return Result : Defining_Identifier_List do
         Result.List.Append (Id);
      end return;
   end Identifier;

   -----------------------------------
   -- Instantiate_Generic_Procedure --
   -----------------------------------

   function Instantiate_Generic_Procedure
     (Instantiated_Name : String;
      Generic_Name      : String)
      return Subprogram_Declaration'Class
   is
   begin
      return Result : Subprogram_Declaration do
         Result.Name := new String'(Instantiated_Name);
         Result.Generic_Name := new String'(Generic_Name);
         Result.Has_Body    := False;
         Result.Is_Instantiation := True;
      end return;
   end Instantiate_Generic_Procedure;

   ---------------------------
   -- New_Abstract_Function --
   ---------------------------

   function New_Abstract_Function
     (Name        : String;
      Result_Type : Subtype_Indication'Class)
     return Subprogram_Declaration'Class
   is
   begin
      return Result : Subprogram_Declaration do
         Result.Name := new String'(Name);
         Result.Result_Type := new Subtype_Indication'Class'(Result_Type);
         Result.Is_Function := True;
         Result.Is_Abstract := True;
         Result.Has_Body    := False;
      end return;
   end New_Abstract_Function;

   ---------------------------
   -- New_Abstract_Function --
   ---------------------------

   function New_Abstract_Function
     (Name        : String;
      Argument    : Formal_Argument'Class;
      Result_Type : Subtype_Indication'Class)
     return Subprogram_Declaration'Class
   is
      Result : Subprogram_Declaration'Class :=
        New_Abstract_Function (Name, Result_Type);
   begin
      Result.Add_Formal_Argument (Argument);
      return Result;
   end New_Abstract_Function;

   ---------------------------
   -- New_Abstract_Function --
   ---------------------------

   function New_Abstract_Function
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class;
      Result_Type : Subtype_Indication'Class)
     return Subprogram_Declaration'Class
   is
      Result : Subprogram_Declaration'Class :=
        New_Abstract_Function (Name, Result_Type);
   begin
      Result.Add_Formal_Argument (Argument_1);
      Result.Add_Formal_Argument (Argument_2);
      return Result;
   end New_Abstract_Function;

   ---------------------------
   -- New_Abstract_Procedure --
   ---------------------------

   function New_Abstract_Procedure
     (Name        : String)
     return Subprogram_Declaration'Class
   is
   begin
      return Result : Subprogram_Declaration do
         Result.Name := new String'(Name);
         Result.Is_Abstract := True;
         Result.Has_Body    := False;
      end return;
   end New_Abstract_Procedure;

   ---------------------------
   -- New_Abstract_Procedure --
   ---------------------------

   function New_Abstract_Procedure
     (Name        : String;
      Argument    : Formal_Argument'Class)
     return Subprogram_Declaration'Class
   is
      Result : Subprogram_Declaration'Class :=
        New_Abstract_Procedure (Name);
   begin
      Result.Add_Formal_Argument (Argument);
      return Result;
   end New_Abstract_Procedure;

   ---------------------------
   -- New_Abstract_Procedure --
   ---------------------------

   function New_Abstract_Procedure
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class)
     return Subprogram_Declaration'Class
   is
      Result : Subprogram_Declaration'Class :=
        New_Abstract_Procedure (Name);
   begin
      Result.Add_Formal_Argument (Argument_1);
      Result.Add_Formal_Argument (Argument_2);
      return Result;
   end New_Abstract_Procedure;

   ---------------------------
   -- New_Abstract_Procedure --
   ---------------------------

   function New_Abstract_Procedure
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class;
      Argument_3  : Formal_Argument'Class)
     return Subprogram_Declaration'Class
   is
      Result : Subprogram_Declaration'Class :=
        New_Abstract_Procedure (Name);
   begin
      Result.Add_Formal_Argument (Argument_1);
      Result.Add_Formal_Argument (Argument_2);
      Result.Add_Formal_Argument (Argument_3);
      return Result;
   end New_Abstract_Procedure;

   -----------------------
   -- New_Child_Package --
   -----------------------

   function New_Child_Package
     (Parent : Package_Type;
      Name   : String)
      return Package_Type
   is
   begin
      return Result : Package_Type do
         Result.Name := new String'(Parent.Name.all & "." & Name);
      end return;
   end New_Child_Package;

   ------------------------------
   -- New_Constant_Declaration --
   ------------------------------

   function New_Constant_Declaration
     (Name        : String;
      Value       : Expression'Class)
      return Object_Declaration'Class
   is
   begin
      return Result : Object_Declaration do
         Result.Objects     := Identifier (Name);
         Result.Is_Aliased  := False;
         Result.Is_Constant := True;
         Result.Is_Deferred := False;
         Result.Object_Type := null;
         Result.Initialiser := new Expression'Class'(Value);
      end return;
   end New_Constant_Declaration;

   ------------------------------
   -- New_Constant_Declaration --
   ------------------------------

   function New_Constant_Declaration
     (Name        : String;
      Object_Type : String;
      Value       : Expression'Class)
      return Object_Declaration'Class
   is
   begin
      return New_Object_Declaration
        (Identifiers => Single_Identifier (Name),
         Is_Aliased  => False,
         Is_Constant => True,
         Is_Deferred => False,
         Object_Type => Named_Subtype (Object_Type),
         Initialiser => Value);
   end New_Constant_Declaration;

   ---------------------------------------
   -- New_Deferred_Constant_Declaration --
   ---------------------------------------

   function New_Deferred_Constant_Declaration
     (Name        : String;
      Object_Type : Subtype_Indication'Class;
      Value       : Expression'Class)
      return Object_Declaration'Class
   is
   begin
      return New_Object_Declaration
        (Single_Identifier (Name), False, True, True, Object_Type, Value);
   end New_Deferred_Constant_Declaration;

   ---------------------------------------
   -- New_Deferred_Constant_Declaration --
   ---------------------------------------

   function New_Deferred_Constant_Declaration
     (Name        : String;
      Object_Type : String;
      Value       : Expression'Class)
      return Object_Declaration'Class
   is
   begin
      return New_Object_Declaration
        (Single_Identifier (Name),
         False, True, True,
         Named_Subtype (Object_Type),
         Value);
   end New_Deferred_Constant_Declaration;

   -----------------------------------
   -- New_Deferred_Type_Declaration --
   -----------------------------------

   function New_Deferred_Type_Declaration
     (Identifier  : String;
      Definition  : Type_Definition'Class;
      Indefinite  : Boolean               := False)
      return Type_Declaration
   is
   begin
      return Result : Type_Declaration do
         Result.Name := new String'(Identifier);
         Result.Definition := new Type_Definition'Class'(Definition);
         Result.Is_Deferred := True;
         Result.Is_Indefinite := Indefinite;
      end return;
   end New_Deferred_Type_Declaration;

   -------------------------
   -- New_Formal_Argument --
   -------------------------

   function New_Formal_Argument
     (From_Object_Declaration : Object_Declaration'Class;
      Mode                    : Argument_Mode;
      Null_Exclusion          : Boolean := False;
      Is_Aliased              : Boolean := False)
      return Formal_Argument'Class
   is
   begin
      return Result : Formal_Argument do
         Object_Declaration (Result) :=
          Object_Declaration (From_Object_Declaration);
         Result.Is_Argument := True;
         Result.Mode        := Mode;
         Result.Null_Exclusion := Null_Exclusion;
         Result.Is_Aliased     := Is_Aliased;
      end return;
   end New_Formal_Argument;

   -------------------------
   -- New_Formal_Argument --
   -------------------------

   function New_Formal_Argument
     (Name          : String;
      Mode          : Argument_Mode;
      Argument_Type : Subtype_Indication'Class)
      return Formal_Argument'Class
   is
   begin
      return New_Formal_Argument
        (New_Object_Declaration (Name, Argument_Type), Mode);
   end New_Formal_Argument;

   -------------------------
   -- New_Formal_Argument --
   -------------------------

   function New_Formal_Argument
     (Name          : String;
      Argument_Type : Subtype_Indication'Class)
     return Formal_Argument'Class
   is
   begin
      return New_Formal_Argument
        (New_Object_Declaration (Name, Argument_Type),
         In_Argument);
   end New_Formal_Argument;

   -------------------------
   -- New_Formal_Argument --
   -------------------------

   function New_Formal_Argument
     (Name             : String;
      Argument_Type    : Subtype_Indication'Class;
      Argument_Default : Expression'Class)
     return Formal_Argument'Class
   is
   begin
      return New_Formal_Argument
        (New_Object_Declaration (Name, Argument_Type, Argument_Default),
         In_Argument);
   end New_Formal_Argument;

   -------------------------------
   -- New_Full_Type_Declaration --
   -------------------------------

   function New_Full_Type_Declaration
     (Identifier  : String;
      Definition  : Type_Definition'Class)
      return Type_Declaration
   is
   begin
      return Result : Type_Declaration do
         Result.Name := new String'(Identifier);
         Result.Definition := new Type_Definition'Class'(Definition);
         Result.Is_Private := False;
      end return;
   end New_Full_Type_Declaration;

   ------------------
   -- New_Function --
   ------------------

   function New_Function
     (Name        : String;
      Result_Type : Subtype_Indication'Class;
      Block       : Blocks.Block_Type'Class)
      return Subprogram_Declaration'Class
   is
   begin
      return Result : Subprogram_Declaration do
         Result.Name := new String'(Name);
         Result.Result_Type := new Subtype_Indication'Class'(Result_Type);
         Result.Sub_Body    := new Blocks.Block_Type'Class'(Block);
         Result.Is_Function := True;
         Result.Is_Abstract := False;
         Result.Has_Body    := True;
      end return;
   end New_Function;

   ------------------
   -- New_Function --
   ------------------

   function New_Function
     (Name        : String;
      Result_Type : String;
      Block       : Blocks.Block_Type'Class)
      return Subprogram_Declaration'Class
   is
   begin
      return New_Function (Name,
                           Named_Subtype (Result_Type),
                           Block);
   end New_Function;

   ------------------
   -- New_Function --
   ------------------

   function New_Function
     (Name        : String;
      Argument    : Formal_Argument'Class;
      Result_Type : String;
      Block       : Blocks.Block_Type'Class)
      return Subprogram_Declaration'Class
   is
      Result : Subprogram_Declaration'Class :=
                 New_Function (Name,
                               Named_Subtype (Result_Type),
                               Block);
   begin
      Result.Add_Formal_Argument (Argument);
      return Result;
   end New_Function;

   ------------------
   -- New_Function --
   ------------------

   function New_Function
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class;
      Result_Type : String;
      Block       : Blocks.Block_Type'Class)
      return Subprogram_Declaration'Class
   is
      Result : Subprogram_Declaration'Class :=
        New_Function (Name,
                      Named_Subtype (Result_Type),
                      Block);
   begin
      Result.Add_Formal_Argument (Argument_1);
      Result.Add_Formal_Argument (Argument_2);
      return Result;
   end New_Function;

   ------------------
   -- New_Function --
   ------------------

   function New_Function
     (Name        : String;
      Result_Type : String;
      Result      : Expression'Class)
      return Subprogram_Declaration'Class
   is
      Block : Syn.Blocks.Block_Type;
   begin
      Block.Add_Statement
        (Syn.Statements.New_Return_Statement
           (Result));

      return Fn : Subprogram_Declaration'Class :=
        New_Function (Name, Result_Type, Block)
      do
         Fn.Expression_Body := new Expression'Class'(Result);
      end return;

   end New_Function;

   ------------------------
   -- New_Inout_Argument --
   ------------------------

   function New_Inout_Argument
     (Name          : String;
      Argument_Type : Subtype_Indication'Class)
     return Formal_Argument'Class
   is
   begin
      return New_Formal_Argument
        (New_Object_Declaration (Name, Argument_Type),
         Inout_Argument);
   end New_Inout_Argument;

   ----------------------------
   -- New_Object_Declaration --
   ----------------------------

   function New_Object_Declaration
     (Name        : String;
      Object_Type : Subtype_Indication'Class)
     return Object_Declaration'Class
   is
   begin
      return New_Object_Declaration
        (Identifiers => Identifier (Name),
         Is_Aliased  => False,
         Is_Constant => False,
         Is_Deferred => False,
         Object_Type => Object_Type);
   end New_Object_Declaration;

   ----------------------------
   -- New_Object_Declaration --
   ----------------------------

   function New_Object_Declaration
     (Name        : String;
      Object_Type : Subtype_Indication'Class;
      Initialiser : Expression'Class)
     return Object_Declaration'Class
   is
   begin
      return New_Object_Declaration
        (Identifiers => Identifier (Name),
         Is_Aliased  => False,
         Is_Constant => False,
         Is_Deferred => False,
         Object_Type => Object_Type,
         Initialiser => Initialiser);
   end New_Object_Declaration;

   ----------------------------
   -- New_Object_Declaration --
   ----------------------------

   function New_Object_Declaration
     (Name        : String;
      Object_Type : String)
     return Object_Declaration'Class
   is
   begin
      return New_Object_Declaration (Name, Named_Subtype (Object_Type));
   end New_Object_Declaration;

   ----------------------------
   -- New_Object_Declaration --
   ----------------------------

   function New_Object_Declaration
     (Name        : String;
      Object_Type : String;
      Initialiser : Expression'Class)
     return Object_Declaration'Class
   is
   begin
      return New_Object_Declaration
        (Identifiers => Identifier (Name),
         Is_Aliased  => False,
         Is_Constant => False,
         Is_Deferred => False,
         Object_Type => Named_Subtype (Object_Type),
         Initialiser => Initialiser);
   end New_Object_Declaration;

   ----------------------------
   -- New_Object_Declaration --
   ----------------------------

   function New_Object_Declaration
     (Identifiers : Defining_Identifier_List;
      Is_Aliased  : Boolean;
      Is_Constant : Boolean;
      Is_Deferred : Boolean;
      Object_Type : Subtype_Indication'Class;
      Initialiser : Expression'Class)
      return Object_Declaration'Class
   is
   begin
      return Result : Object_Declaration do
         Result.Objects     := Identifiers;
         Result.Is_Aliased  := Is_Aliased;
         Result.Is_Constant := Is_Constant;
         Result.Is_Deferred := Is_Deferred;
         Result.Object_Type := new Subtype_Indication'Class'(Object_Type);
         Result.Initialiser := new Expression'Class'(Initialiser);
      end return;
   end New_Object_Declaration;

   ----------------------------
   -- New_Object_Declaration --
   ----------------------------

   function New_Object_Declaration
     (Identifiers : Defining_Identifier_List;
      Is_Aliased  : Boolean;
      Is_Constant : Boolean;
      Is_Deferred : Boolean;
      Object_Type : Subtype_Indication'Class)
      return Object_Declaration'Class
   is
   begin
      return Result : Object_Declaration do
         Result.Objects     := Identifiers;
         Result.Is_Aliased  := Is_Aliased;
         Result.Is_Constant := Is_Constant;
         Result.Is_Deferred := Is_Deferred;
         Result.Object_Type := new Subtype_Indication'Class'(Object_Type);
         Result.Initialiser := null;
      end return;
   end New_Object_Declaration;

   ----------------------
   -- New_Out_Argument --
   ----------------------

   function New_Out_Argument
     (Name          : String;
      Argument_Type : Subtype_Indication'Class)
     return Formal_Argument'Class
   is
   begin
      return New_Formal_Argument
        (New_Object_Declaration (Name, Argument_Type),
         Out_Argument);
   end New_Out_Argument;

   -------------------------------
   -- New_Package_Type --
   -------------------------------

   function New_Package_Type
     (Name   : String)
      return Package_Type
   is
   begin
      return Result : Package_Type do
         Result.Name := new String'(Name);
      end return;
   end New_Package_Type;

   ----------------
   -- New_Pragma --
   ----------------

   function New_Pragma (Pragma_Name : String;
                        Argument    : String)
                        return Declaration'Class
   is
   begin
      return Result : Pragma_Declaration do
         Result.Pragma_Name := new String'(Pragma_Name);
         Result.Pragma_Arg := new String'(Argument);
      end return;
   end New_Pragma;

   ----------------------------------
   -- New_Private_Type_Declaration --
   ----------------------------------

   function New_Private_Type_Declaration
     (Identifier  : String;
      Definition  : Type_Definition'Class;
      Indefinite  : Boolean               := False)
      return Type_Declaration
   is
   begin
      return Result : Type_Declaration do
         Result.Name := new String'(Identifier);
         Result.Definition := new Type_Definition'Class'(Definition);
         Result.Is_Private := True;
         Result.Is_Indefinite := Indefinite;
      end return;
   end New_Private_Type_Declaration;

   -------------------
   -- New_Procedure --
   -------------------

   function New_Procedure
     (Name        : String;
      Block       : Blocks.Block_Type'Class)
      return Subprogram_Declaration'Class
   is
   begin
      return Result : Subprogram_Declaration do
         Result.Name := new String'(Name);
         Result.Sub_Body    := new Blocks.Block_Type'Class'(Block);
         Result.Is_Function := False;
         Result.Is_Abstract := False;
         Result.Has_Body    := True;
      end return;
   end New_Procedure;

   -------------------
   -- New_Procedure --
   -------------------

   function New_Procedure
     (Name        : String;
      Argument    : Formal_Argument'Class;
      Block       : Blocks.Block_Type'Class)
      return Subprogram_Declaration'Class
   is
   begin
      return Result : Subprogram_Declaration do
         Result.Name := new String'(Name);
         Result.Add_Formal_Argument (Argument);
         Result.Sub_Body    := new Blocks.Block_Type'Class'(Block);
         Result.Is_Function := False;
         Result.Is_Abstract := False;
         Result.Has_Body    := True;
      end return;
   end New_Procedure;

   -------------------
   -- New_Procedure --
   -------------------

   function New_Procedure
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class;
      Block       : Blocks.Block_Type'Class)
      return Subprogram_Declaration'Class
   is
   begin
      return Proc : Subprogram_Declaration'Class :=
        New_Procedure (Name, Block)
      do
         Proc.Add_Formal_Argument (Argument_1);
         Proc.Add_Formal_Argument (Argument_2);
      end return;
   end New_Procedure;

   function New_Procedure
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class;
      Argument_3  : Formal_Argument'Class;
      Block       : Blocks.Block_Type'Class)
      return Subprogram_Declaration'Class
   is
   begin
      return Proc : Subprogram_Declaration'Class :=
        New_Procedure (Name, Block)
      do
         Proc.Add_Formal_Argument (Argument_1);
         Proc.Add_Formal_Argument (Argument_2);
         Proc.Add_Formal_Argument (Argument_3);
      end return;
   end New_Procedure;

   -------------------
   -- New_Separator --
   -------------------

   function New_Separator return Specification_Separator is
   begin
      return Result : Specification_Separator do
         null;
      end return;
   end New_Separator;

   function New_Subtype_Declaration
     (Identifier  : String;
      Definition  : Subtype_Indication'Class)
      return Subtype_Declaration
   is
   begin
      return Result : Subtype_Declaration do
         Result.Name := new String'(Identifier);
         Result.Definition := new Subtype_Indication'Class'(Definition);
      end return;
   end New_Subtype_Declaration;

   ------------------------
   -- Pseudo_Declaration --
   ------------------------

   overriding
   function Pseudo_Declaration
     (Item : Specification_Separator)
     return Boolean
   is
      pragma Unreferenced (Item);
   begin
      return True;
   end Pseudo_Declaration;

   function Renaming_Declaration
     (New_Identifier : String;
      New_Type       : String;
      Renamed_Expression : Expression'Class)
      return Declaration'Class
   is
   begin
      return Result : Renaming_Declaration_Record do
         Result.New_Identifier := new String'(New_Identifier);
         Result.New_Type := new String'(New_Type);
         Result.Renamed_Expression :=
           new Expression'Class'(Renamed_Expression);
      end return;
   end Renaming_Declaration;

   -----------------
   -- Set_Aliased --
   -----------------

   procedure Set_Aliased (Item : in out Object_Declaration'Class) is
   begin
      Item.Is_Aliased := True;
   end Set_Aliased;

   -------------------
   -- Set_Anonymous --
   -------------------

   procedure Set_Anonymous (Item : in out Subprogram_Declaration'Class) is
   begin
      Item.Is_Anonymous := True;
   end Set_Anonymous;

   ------------------
   -- Set_Constant --
   ------------------

   procedure Set_Constant (Item : in out Object_Declaration'Class) is
   begin
      Item.Is_Constant := True;
   end Set_Constant;

   ----------------------------
   -- Set_Generic_Instantion --
   ----------------------------

   procedure Set_Generic_Instantiation
     (Item                    : in out Subprogram_Declaration'Class;
      Instantiated_Subprogram : in     String)
   is
   begin
      Item.Is_Instantiation := True;
      Item.Has_Body := False;
      Item.Generic_Name := new String'(Instantiated_Subprogram);
   end Set_Generic_Instantiation;

   --------------------
   -- Set_Overriding --
   --------------------

   procedure Set_Overriding
     (Item : in out Subprogram_Declaration)
   is
   begin
      Item.Is_Overriding := True;
   end Set_Overriding;

   -----------------
   -- Set_Private --
   -----------------

   procedure Set_Private
     (Item : in out Package_Type)
   is
   begin
      Item.Is_Private := True;
   end Set_Private;

   ----------------------
   -- Set_Private_Spec --
   ----------------------

   overriding procedure Set_Private_Spec
     (Item : in out Type_Declaration)
   is
   begin
      Item.Is_Private := True;
      Item.Definition.Is_Private := True;
      Item.Private_Spec := True;
   end Set_Private_Spec;

   -----------------------
   -- Single_Identifier --
   -----------------------

   function Single_Identifier (Text : String)
                              return Defining_Identifier_List
   is
   begin
      return Result : Defining_Identifier_List do
         Result.List.Append (Text);
      end return;
   end Single_Identifier;

   ----------------------
   -- Sort_Subprograms --
   ----------------------

   procedure Sort_Subprograms
     (Decs : in out Declaration_Vector.Vector)
   is
      function Less (Left, Right : Declaration'Class) return Boolean;

      ----------
      -- Less --
      ----------

      function Less (Left, Right : Declaration'Class) return Boolean is
      begin
         return Subprogram_Declaration'Class (Left).Name.all
           < Subprogram_Declaration'Class (Right).Name.all;
      end Less;

      package Sorting is new Declaration_Vector.Generic_Sorting (Less);
   begin
      Sorting.Sort (Decs);
   end Sort_Subprograms;

   -----------------
   -- Use_Package --
   -----------------

   function Use_Package
     (Package_Name : String)
      return  Declaration'Class
   is
   begin
      return Result : Use_Package_Declaration do
         Result.Package_Name := new String'(Package_Name);
      end return;
   end Use_Package;

   --------------
   -- Use_Type --
   --------------

   function Use_Type (Type_Name : String)
                      return  Declaration'Class
   is
   begin
      return Result : Use_Type_Declaration do
         Result.Type_Name := new String'(Type_Name);
      end return;
   end Use_Type;

   ------------------
   -- With_Package --
   ------------------

   procedure With_Package
     (Item         : in out Package_Type;
      Withed       : in     String;
      Private_With : Boolean := False;
      Body_With    : Boolean := False;
      Use_Package  : Boolean := False)
   is
      New_With : With_Context_Clause;
   begin

      if To_Ada_Name (Withed) = To_Ada_Name (Item.Name.all) then
         return;
      end if;

      for Context_Clause of Item.Withed_Packages loop
         if Context_Clause.Withed_Package.all = Withed then
            return;
         end if;
      end loop;

      New_With.Withed_Package := new String'(To_Ada_Name (Withed));
      New_With.Is_Private     := Private_With;
      New_With.Is_Body        := Body_With;
      New_With.Is_Used        := Use_Package;
      Item.Withed_Packages.Append (New_With);
   end With_Package;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : Specification_Separator;
                    Writer      : in out Writer_Interface'Class)
   is
      pragma Unreferenced (Item);
   begin
      Writer.New_Line;
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write
     (Item        : Package_Type;
      Writer      : in out Writer_Interface'Class)
   is
      Top_Level : constant Boolean :=
                    Writer.Context = Compilation_Unit;
   begin

      for Is_Body in Boolean loop
         exit when Is_Body and then not Item.Has_Body;

         if Top_Level then
            Writer.Create (To_File_Name (Item.Name, Spec => not Is_Body));
            Writer.Context := (if Is_Body then Package_Body else Package_Spec);

            declare
               Got_With : Boolean := False;
            begin

               for Context of Item.Withed_Packages loop
                  if (Context.Is_Body and then Writer.Context = Package_Body)
                    or else (not Context.Is_Body
                             and then Writer.Context = Package_Spec)
                  then
                     if Context.Is_Private then
                        Writer.Put ("private ");
                     end if;
                     Writer.Put ("with " &
                                   Context.Withed_Package.all &
                                   ";");
                     if Context.Is_Used then
                        Writer.Set_Col (40);
                        Writer.Optional_New_Line;
                        Writer.Put ("use "
                                    & Context.Withed_Package.all
                                    & ";");
                     end if;
                     Writer.New_Line;
                     Got_With := True;
                  end if;
               end loop;

               if Got_With then
                  Writer.New_Line;
               end if;
            end;

            if not Is_Body
              and then Item.Is_Private
            then
               Writer.Put ("private ");
            end if;
         end if;

         Writer.Put ("package ");
         if Is_Body then
            Writer.Put ("body ");
         end if;

         Writer.Put (To_Ada_Name (Item.Name));
         Writer.Put_Line (" is");

         if Item.Is_Instantiation then
            Writer.Indent (Writer.Indent + 2);
            Writer.Put ("new " & Item.Generic_Name.all);
            Writer.Indent (Writer.Indent + 4);
            Writer.New_Line;
            declare
               First : Boolean := True;
            begin
               for S of Item.Generic_Arguments loop
                  if First then
                     Writer.Put (" (");
                     First := False;
                  else
                     Writer.Put_Line (",");
                  end if;
                  Writer.Put (S);
               end loop;
               if not First then
                  Writer.Put (")");
               end if;
               Writer.Indent (Writer.Indent - 6);
               if Top_Level then
                  Writer.Put_Line (";");
               end if;
            end;
         else
            Writer.Indent (3);
            Writer.New_Line;

            if Is_Body then

               Writer.Context := Package_Body_Specs;
               for Dec of Item.Declarations loop
                  if Dec.Has_Output (Writer) then
                     Dec.Write (Writer);
                     if not Dec.Pseudo_Declaration then
                        Writer.Put_Line (";");
                     end if;
                  end if;
               end loop;

               Writer.Context := Package_Body;

            end if;

            declare
               Decs : Declaration_Vector.Vector;
            begin

               if not Is_Body then

                  for Dec of Item.Declarations loop
                     if Dec.Has_Output (Writer) then
                        Dec.Write (Writer);
                        if not Dec.Pseudo_Declaration then
                           Writer.Put_Line (";");
                        end if;
                     end if;
                  end loop;

               else

                  for Dec of Item.Declarations loop
                     if Dec not in Subprogram_Declaration'Class then
                        if Dec.Has_Output (Writer) then
                           Dec.Write (Writer);
                           if not Dec.Pseudo_Declaration then
                              Writer.Put_Line (";");
                           end if;
                        end if;
                     else
                        Decs.Append (Dec);
                     end if;
                  end loop;

                  Sort_Subprograms (Decs);

                  for Dec of Decs loop
                     if Dec.Has_Output (Writer) then
                        Dec.Write (Writer);
                        Writer.Put_Line (";");
                     end if;
                  end loop;
               end if;
            end;

            Writer.Indent (0);

            if not Is_Body and then Item.Has_Private then

               Writer.New_Line;
               Writer.Put_Line ("private");
               Writer.New_Line;

               Writer.Context := Package_Private;
               Writer.Indent (3);

               for Dec of Item.Declarations loop
                  if Dec.Has_Output (Writer) then
                     Dec.Write (Writer);
                     if not Dec.Pseudo_Declaration then
                        Writer.Put_Line (";");
                     end if;

                  end if;
               end loop;

               Writer.Indent (0);

            end if;

            Writer.New_Line;
            Writer.Put ("end ");
            Writer.Put (To_Ada_Name (Item.Name));
            Writer.Put (";");
            Writer.New_Line;
         end if;

         if Top_Level then
            Writer.Close;
            Writer.Context := Compilation_Unit;
         end if;

      end loop;

   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write
     (Item        : Object_Declaration;
      Writer      : in out Writer_Interface'Class)
   is
   begin

      if (Writer.Context = Package_Spec
          and then not Item.Body_Only
          and then not Item.Private_Spec)
        or else (Writer.Context = Package_Private
          and then not Item.Body_Only
          and then Item.Private_Spec)
        or else (Writer.Context = Package_Body_Specs and then Item.Body_Only)
        or else Writer.Context = Block
        or else Item.Is_Argument
        or else (Has_Private_Part (Declaration'Class (Item))
                   and then Writer.Context = Package_Private)
      then

         declare
            First : Boolean := True;
         begin
            for Id of Item.Objects.List loop
               if First then
                  Writer.Put (To_Ada_Name (Id));
               else
                  Writer.Put (", " & To_Ada_Name (Id));
                  First := False;
               end if;
            end loop;
         end;

         if Writer.Tabs (1) /= 0 then
            Writer.Set_Col (Writer.Tabs (1));
         end if;

         Writer.Put (" : ");
         Writer.Optional_New_Line;

         if Item.Is_Aliased then
            Writer.Put ("aliased ");
         end if;

         if Item.Is_Constant
           and then not Item.Is_Argument
         then
            Writer.Put ("constant ");
            Writer.Optional_New_Line;
         end if;

         if Item.Is_Argument
           and then (not Writer.Is_Set (Suppress_Argument_Mode)
                     or else Item.Mode /= In_Argument)
         then
            case Item.Mode is
               when In_Argument =>
                  Writer.Put ("in     ");
               when Out_Argument =>
                  Writer.Put ("   out ");
               when Inout_Argument =>
                  Writer.Put ("in out ");
               when Access_Argument =>
                  if Item.Null_Exclusion then
                     Writer.Put ("not null ");
                  end if;
                  Writer.Put ("access ");
                  if Item.Is_Constant then
                     Writer.Put ("constant ");
                  end if;
                  Writer.Optional_New_Line;
            end case;
         end if;

         if Item.Object_Type /= null then
            Item.Object_Type.Write (Writer);
         end if;

         if Item.Initialiser /= null
           and then (not Item.Is_Deferred
                       or else Writer.Context = Package_Private)
         then
            Writer.Put (" := ");
            Writer.Optional_New_Line;
            Item.Initialiser.Write (Writer);
         end if;
      end if;

   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : Subprogram_Declaration;
                    Writer      : in out Writer_Interface'Class)
   is
      Arg_Start_Column  : Natural;
      Only_In_Arguments : Boolean := True;
   begin
      if (Writer.Context = Package_Private and then
            not Item.Private_Spec)
        or else (Writer.Context = Package_Body and then
                   not Item.Has_Body)
      then
         return;
      end if;

      if Writer.Context = Package_Body then
         declare
            Dashes : constant String (1 .. Item.Name'Length + 6) :=
                       (others => '-');
            Name   : constant String (Dashes'Range) :=
                       ("-- " & To_Ada_Name (Item.Name) & " --");
         begin
            Writer.New_Line;
            Writer.Put_Line (Dashes);
            Writer.Put_Line (Name);
            Writer.Put_Line (Dashes);
            Writer.New_Line;
         end;
      end if;

      if Item.Is_Overriding then
         Writer.Put ("overriding ");
      end if;

      if Item.Is_Function then
         Writer.Put ("function");
      else
         Writer.Put ("procedure");
      end if;

      if not Item.Is_Anonymous then
         Writer.Put (" ");
         Writer.Put (To_Ada_Name (Item.Name));
      end if;

      for Arg of Item.Arguments loop
         if Arg.Mode /= In_Argument then
            Only_In_Arguments := False;
         end if;
      end loop;

      Writer.Push_Flag (Suppress_Argument_Mode, Only_In_Arguments);

      if Item.Arguments.Last_Index > 0 then
         if Item.Arguments.Last_Index = 1 then
            Writer.Optional_New_Line;
            Writer.Put (" (");
            Arg_Start_Column := Writer.Col - 1;
            Item.Arguments.Element (1).Write (Writer);
            Writer.Put (")");
            Writer.Optional_New_Line;
         else
            declare
               First : Boolean := True;
            begin
               Writer.Indent (Writer.Indent + 2);
               Arg_Start_Column := Writer.Indent;
               Writer.Tabs (1) := Arg_Start_Column +
                 Item.Arg_Name_Width + 2;
               for Argument of Item.Arguments loop
                  if not First then
                     Writer.Put (";");
                  end if;

                  Writer.New_Line;
                  Writer.Put ((if First then "(" else " "));
                  First := False;
                  Argument.Write (Writer);
               end loop;
               Writer.Indent (Writer.Indent - 2);
               Writer.Put (")");
               Writer.Tabs (1) := 0;
            end;
         end if;
      end if;

      Writer.Pop_Flag (Suppress_Argument_Mode);

      if Item.Is_Function then
         if Item.Arguments.Last_Index > 1 then
            Writer.New_Line;
            Writer.Set_Col (Arg_Start_Column + 2);
         else
            Writer.Optional_New_Line;
            Writer.Put (" ");
         end if;
         Writer.Put ("return ");
         Item.Result_Type.Write (Writer);
      end if;

      if Item.Is_Anonymous then
         null;
      elsif Item.Is_Abstract then
         Writer.New_Line;
         Writer.Put ("   is abstract");
      elsif Writer.Context = Package_Body
        or else Item.Is_Instantiation
      then
         if Item.Arguments.Last_Index > 1
           or else Item.Expression_Body /= null
         then
            Writer.New_Line;
            Writer.Put ("is");
         else
            Writer.Put (" is");
         end if;

         if Item.Expression_Body /= null then
            Writer.Put (" (");
            Item.Expression_Body.Write (Writer);
            Writer.Put (")");
         else
            Writer.New_Line;

            if Item.Is_Instantiation then
               Writer.Indent (Writer.Indent + 2);
               Writer.Put ("new " & Item.Generic_Name.all);
               Writer.Indent (Writer.Indent + 4);
               Writer.New_Line;
               declare
                  First : Boolean := True;
               begin
                  for S of Item.Generic_Arguments loop
                     if First then
                        Writer.Put (" (");
                        First := False;
                     else
                        Writer.Put_Line (",");
                     end if;
                     Writer.Put (S);
                  end loop;
                  if not First then
                     Writer.Put (")");
                  end if;
                  Writer.Indent (Writer.Indent - 6);
               end;
            else
               Item.Sub_Body.Write (Writer);
               Writer.Put (" ");
               Writer.Put (Item.Name.all);
            end if;
         end if;
      end if;

   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : Subtype_Declaration;
                    Writer      : in out Writer_Interface'Class)
   is
   begin
      Writer.Put ("subtype ");
      Writer.Put (Item.Name.all);
      Writer.Put (" is ");
      Writer.Optional_New_Line;
      Item.Definition.Write (Writer);
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write
     (Item        : Type_Declaration;
      Writer      : in out Writer_Interface'Class)
   is
   begin
      if (Writer.Context = Package_Spec
          and then not Item.Private_Spec)
        or else (Writer.Context = Package_Private and then
                   (Item.Has_Private_Part
                    or else Item.Private_Spec))
        or else (Writer.Context = Package_Body_Specs and then
                   Item.Body_Only)
      then
         Writer.Put ("type ");
         Writer.Put (To_Ada_Name (Item.Name));
         if Item.Is_Indefinite
           and then Writer.Context = Package_Spec
         then
            Writer.Put (" (<>)");
         elsif Item.Definition.Has_Variant then
            Writer.Optional_New_Line;
            Writer.Put (" (" & Item.Definition.Variant_Name
                        & " : " & Item.Definition.Variant_Type);
            if Item.Definition.Variant_Default /= "" then
               Writer.Put (" := " & Item.Definition.Variant_Default);
            end if;
            Writer.Put (")");
         end if;

         Writer.Put (" is ");
         Writer.Optional_New_Line;

         if (Item.Is_Private
             and then Writer.Context = Package_Spec)
           and then not Item.Definition.Visible_Derivation
         then
            if Item.Definition.Is_Abstract then
               Writer.Put ("abstract ");
            end if;
            if Item.Definition.Is_Tagged then
               Writer.Put ("tagged ");
            end if;
            if Item.Definition.Is_Limited then
               Writer.Put ("limited ");
            end if;
            Writer.Put ("private");
         else
            Item.Definition.Write (Writer);
         end if;

         if not Item.Aspects.Is_Empty
           and then Writer.Context = Package_Spec
         then
            Writer.New_Line;
            Writer.Put ("with ");
            Writer.Indent (Writer.Indent + 5);
            declare
               First : Boolean := True;
            begin
               for A of Item.Aspects loop
                  if First then
                     First := False;
                  else
                     Writer.Put (",");
                     Writer.New_Line;
                  end if;
                  Writer.Put (A.Name.all & " => ");
                  A.Value.Write (Writer);
               end loop;
            end;
            Writer.Indent (Writer.Indent - 5);
         end if;

      end if;
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : Formal_Argument;
                    Writer      : in out Writer_Interface'Class)
   is
   begin
      Object_Declaration (Item).Write (Writer);
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : Use_Package_Declaration;
                    Writer      : in out Writer_Interface'Class)
   is
   begin
      Writer.Put ("use " & Item.Package_Name.all);
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : Use_Type_Declaration;
                    Writer      : in out Writer_Interface'Class)
   is
   begin
      Writer.Put ("use type " & Item.Type_Name.all);
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : Renaming_Declaration_Record;
                    Writer      : in out Writer_Interface'Class)
   is
   begin
      Writer.Put_Line (Item.New_Identifier.all & " : " & Item.New_Type.all);
      Writer.Indent (Writer.Indent + 2);
      Writer.Put ("renames ");
      Item.Renamed_Expression.Write (Writer);
      Writer.Indent (Writer.Indent - 2);
   end Write;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : Pragma_Declaration;
                    Writer      : in out Writer_Interface'Class)
   is
   begin
      Writer.Put ("pragma ");
      Writer.Put (Item.Pragma_Name.all);
      Writer.Put (" (");
      Writer.Put (Item.Pragma_Arg.all);
      Writer.Put (")");
   end Write;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Item        : Address_Representation_Declaration;
      Writer      : in out Writer_Interface'Class)
   is
   begin
      Writer.Put ("for ");
      Writer.Put (Item.Object_Name.all);
      Writer.Put ("'Address use ");
      Item.Address_Expr.Write (Writer);
   end Write;

end Syn.Declarations;
