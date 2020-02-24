private with Ada.Containers.Vectors;

with Syn.Blocks;

package Syn.Declarations is

   function Use_Type (Type_Name : String)
                      return  Declaration'Class;

   function Use_Package (Package_Name : String)
                         return  Declaration'Class;

   function New_Pragma (Pragma_Name : String;
                        Argument    : String)
                        return Declaration'Class;

   type Specification_Separator is
     new Declaration with private;

   overriding
   function Has_Private_Part
     (Item : Specification_Separator)
     return Boolean;

   overriding
   function Has_Body
     (Item : Specification_Separator)
     return Boolean;

   overriding
   procedure Write (Item        : Specification_Separator;
                    Writer      : in out Writer_Interface'Class);

   overriding
   function Pseudo_Declaration
     (Item : Specification_Separator)
     return Boolean;

   function New_Separator return Specification_Separator;

   function Renaming_Declaration
     (New_Identifier : String;
      New_Type       : String;
      Renamed_Expression : Expression'Class)
      return Declaration'Class;

   type Object_Declaration is new Declaration with private;

   overriding
   function Has_Private_Part
     (Item : Object_Declaration)
     return Boolean;

   overriding
   function Has_Body
     (Item : Object_Declaration)
     return Boolean;

   overriding
   procedure Write (Item        : Object_Declaration;
                    Writer      : in out Writer_Interface'Class);

   procedure Set_Aliased (Item : in out Object_Declaration'Class);
   procedure Set_Constant (Item : in out Object_Declaration'Class);

   type Defining_Identifier_List is private;

   function Identifier (Id : String) return Defining_Identifier_List;

   function New_Object_Declaration
     (Identifiers : Defining_Identifier_List;
      Is_Aliased  : Boolean;
      Is_Constant : Boolean;
      Is_Deferred : Boolean;
      Object_Type : Subtype_Indication'Class;
      Initialiser : Expression'Class)
     return Object_Declaration'Class;

   function New_Object_Declaration
     (Identifiers : Defining_Identifier_List;
      Is_Aliased  : Boolean;
      Is_Constant : Boolean;
      Is_Deferred : Boolean;
      Object_Type : Subtype_Indication'Class)
     return Object_Declaration'Class;

   function New_Object_Declaration
     (Name        : String;
      Object_Type : Subtype_Indication'Class;
      Initialiser : Expression'Class)
     return Object_Declaration'Class;

   function New_Object_Declaration
     (Name        : String;
      Object_Type : Subtype_Indication'Class)
     return Object_Declaration'Class;
   function New_Object_Declaration
     (Name        : String;
      Object_Type : String)
     return Object_Declaration'Class;

   function New_Object_Declaration
     (Name        : String;
      Object_Type : String;
      Initialiser : Expression'Class)
     return Object_Declaration'Class;

   function New_Constant_Declaration
     (Name        : String;
      Value       : Expression'Class)
     return Object_Declaration'Class;

   function New_Constant_Declaration
     (Name        : String;
      Object_Type : String;
      Value       : Expression'Class)
     return Object_Declaration'Class;

   function New_Deferred_Constant_Declaration
     (Name        : String;
      Object_Type : Subtype_Indication'Class;
      Value       : Expression'Class)
     return Object_Declaration'Class;

   function New_Deferred_Constant_Declaration
     (Name        : String;
      Object_Type : String;
      Value       : Expression'Class)
     return Object_Declaration'Class;

   type Type_Declaration is new Declaration with private;

   overriding
   function Has_Private_Part
     (Item : Type_Declaration)
     return Boolean;

   overriding
   function Has_Body_Spec
     (Item : Type_Declaration)
     return Boolean;

   overriding procedure Set_Private_Spec
     (Item : in out Type_Declaration);

   overriding
   procedure Write (Item        : Type_Declaration;
                    Writer      : in out Writer_Interface'Class);

   function New_Full_Type_Declaration
     (Identifier  : String;
      Definition  : Type_Definition'Class)
     return Type_Declaration;

   function New_Private_Type_Declaration
     (Identifier  : String;
      Definition  : Type_Definition'Class;
      Indefinite  : Boolean               := False)
     return Type_Declaration;

   function New_Deferred_Type_Declaration
     (Identifier  : String;
      Definition  : Type_Definition'Class;
      Indefinite  : Boolean               := False)
      return Type_Declaration;

   type Subtype_Declaration is new Declaration with private;

   overriding
   function Has_Private_Part
     (Item : Subtype_Declaration)
     return Boolean;

   overriding
   function Has_Body
     (Item : Subtype_Declaration)
     return Boolean;

   overriding
   procedure Write (Item        : Subtype_Declaration;
                    Writer      : in out Writer_Interface'Class);

   function New_Subtype_Declaration
     (Identifier  : String;
      Definition  : Subtype_Indication'Class)
     return Subtype_Declaration;

   type Argument_Mode is (In_Argument, Out_Argument, Inout_Argument,
                          Access_Argument);

   type Formal_Argument is new Object_Declaration with private;

   function New_Formal_Argument
     (Name          : String;
      Argument_Type : Subtype_Indication'Class)
     return Formal_Argument'Class;

   function New_Formal_Argument
     (Name          : String;
      Mode          : Argument_Mode;
      Argument_Type : Subtype_Indication'Class)
     return Formal_Argument'Class;

   function New_Formal_Argument
     (Name             : String;
      Argument_Type    : Subtype_Indication'Class;
      Argument_Default : Expression'Class)
      return Formal_Argument'Class;

   function New_In_Argument
     (Name          : String;
      Argument_Type : Subtype_Indication'Class)
     return Formal_Argument'Class
     renames New_Formal_Argument;

   function New_Out_Argument
     (Name          : String;
      Argument_Type : Subtype_Indication'Class)
     return Formal_Argument'Class;

   function New_Inout_Argument
     (Name          : String;
      Argument_Type : Subtype_Indication'Class)
     return Formal_Argument'Class;

   overriding
   procedure Write (Item        : Formal_Argument;
                    Writer      : in out Writer_Interface'Class);

   type Subprogram_Declaration is new Declaration with private;

   overriding procedure Check (Item : Subprogram_Declaration);

   overriding
   function Has_Private_Part
     (Item : Subprogram_Declaration)
     return Boolean;

   overriding
   function Has_Body
     (Item : Subprogram_Declaration)
      return Boolean;

   overriding
   function Has_Body_Spec
     (Item : Subprogram_Declaration)
     return Boolean;

   overriding
   procedure Write (Item        : Subprogram_Declaration;
                    Writer      : in out Writer_Interface'Class);

   function Is_Anonymous (Item : Subprogram_Declaration'Class) return Boolean;
   procedure Set_Anonymous (Item : in out Subprogram_Declaration'Class);

   procedure Add_Local_Declaration
     (Subprogram : in out Subprogram_Declaration;
      Dec        : in     Declaration'Class);

   function New_Abstract_Function
     (Name        : String;
      Argument    : Formal_Argument'Class;
      Result_Type : Subtype_Indication'Class)
     return Subprogram_Declaration'Class;

   function New_Abstract_Function
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class;
      Result_Type : Subtype_Indication'Class)
     return Subprogram_Declaration'Class;

   function New_Abstract_Procedure
     (Name        : String;
      Argument    : Formal_Argument'Class)
     return Subprogram_Declaration'Class;

   function New_Abstract_Procedure
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class)
     return Subprogram_Declaration'Class;

   function New_Abstract_Procedure
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class;
      Argument_3  : Formal_Argument'Class)
      return Subprogram_Declaration'Class;

   function New_Abstract_Function
     (Name        : String;
      Result_Type : Subtype_Indication'Class)
     return Subprogram_Declaration'Class;

   function New_Abstract_Procedure
     (Name        : String)
     return Subprogram_Declaration'Class;

   function New_Function
     (Name        : String;
      Result_Type : Subtype_Indication'Class;
      Block       : Blocks.Block_Type'Class)
     return Subprogram_Declaration'Class;

   function New_Function
     (Name        : String;
      Result_Type : String;
      Block       : Blocks.Block_Type'Class)
     return Subprogram_Declaration'Class;

   function New_Function
     (Name        : String;
      Result_Type : String;
      Result      : Expression'Class)
     return Subprogram_Declaration'Class;

   function New_Function
     (Name        : String;
      Argument    : Formal_Argument'Class;
      Result_Type : String;
      Block       : Blocks.Block_Type'Class)
     return Subprogram_Declaration'Class;

   function New_Function
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class;
      Result_Type : String;
      Block       : Blocks.Block_Type'Class)
      return Subprogram_Declaration'Class;

   function New_Procedure
     (Name        : String;
      Argument    : Formal_Argument'Class;
      Block       : Blocks.Block_Type'Class)
     return Subprogram_Declaration'Class;

   function New_Procedure
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class;
      Block       : Blocks.Block_Type'Class)
      return Subprogram_Declaration'Class;

   function New_Procedure
     (Name        : String;
      Argument_1  : Formal_Argument'Class;
      Argument_2  : Formal_Argument'Class;
      Argument_3  : Formal_Argument'Class;
      Block       : Blocks.Block_Type'Class)
      return Subprogram_Declaration'Class;

   function New_Procedure
     (Name        : String;
      Block       : Blocks.Block_Type'Class)
     return Subprogram_Declaration'Class;

   function Instantiate_Generic_Procedure
     (Instantiated_Name : String;
      Generic_Name      : String)
     return Subprogram_Declaration'Class;

   procedure Add_Formal_Argument
     (Subprogram : in out Subprogram_Declaration'Class;
      Argument   : in     Formal_Argument'Class);

   procedure Add_Formal_Argument
     (Subprogram : in out Subprogram_Declaration'Class;
      Arg_Name   : in     String;
      Arg_Type   : in     String);

   procedure Add_Formal_Argument
     (Subprogram : in out Subprogram_Declaration'Class;
      Arg_Name   : in     String;
      Arg_Mode   : in     Argument_Mode;
      Arg_Type   : in     String);

   procedure Add_Formal_Argument
     (Subprogram     : in out Subprogram_Declaration'Class;
      Arg_Name       : in     String;
      Null_Exclusion : in     Boolean;
      Is_Aliased     : in     Boolean;
      Arg_Mode       : in     Argument_Mode;
      Arg_Type       : in     String);

   procedure Add_Formal_Argument
     (Subprogram  : in out Subprogram_Declaration'Class;
      Arg_Name    : in     String;
      Arg_Type    : in     String;
      Arg_Default : in     Expression'Class);

   procedure Set_Overriding
     (Item : in out Subprogram_Declaration);

   procedure Set_Generic_Instantiation
     (Item                    : in out Subprogram_Declaration'Class;
      Instantiated_Subprogram : in     String);

   procedure Add_Generic_Actual_Argument
     (Item  : in out Subprogram_Declaration'Class;
      Value : in     String);

   procedure Add_Generic_Actual_Argument
     (Item  : in out Subprogram_Declaration'Class;
      Value : in     Integer);

   type Package_Type is new Subprogram_Declaration with private;

   overriding
   procedure Write (Item        : Package_Type;
                    Writer      : in out Writer_Interface'Class);

   procedure Add_Separator (Item : in out Package_Type);

   function New_Child_Package
     (Parent : Package_Type;
      Name   : String)
     return Package_Type;

   function New_Package_Type
     (Name   : String)
     return Package_Type;

   procedure With_Package
     (Item         : in out Package_Type;
      Withed       : in     String;
      Private_With : Boolean := False;
      Body_With    : Boolean := False;
      Use_Package  : Boolean := False);

   procedure Set_Private
     (Item : in out Package_Type);

   procedure Append
     (To_Package : in out Package_Type;
      Item       : in     Declaration'Class);

   procedure Append_To_Body
     (To_Package : in out Package_Type;
      Item       : in     Declaration'Class);

   overriding
   function Has_Private_Part
     (Item : Package_Type)
     return Boolean;

   overriding
   function Has_Body
     (Item : Package_Type)
     return Boolean;

   function Address_Representation_Clause
     (Object_Name : String;
      Address     : Expression'Class)
      return Declaration'Class;

private

   type Specification_Separator is
     new Declaration with null record;

   type Defining_Identifier_List is
      record
         List : String_Vector.Vector;
      end record;

   type Object_Declaration is new Declaration with
      record
         Objects        : Defining_Identifier_List;
         Object_Type    : access Subtype_Indication'Class;
         Initialiser    : access Expression'Class;
         Is_Constant    : Boolean;
         Is_Aliased     : Boolean;
         Is_Deferred    : Boolean;
         Is_Argument    : Boolean := False;
         Null_Exclusion : Boolean := False;
         Mode           : Argument_Mode := In_Argument;
      end record;

   function Defining_Identifiers_Length
     (Item : Object_Declaration'Class)
     return Natural;

   type Type_Declaration is new Declaration with
      record
         Name          : String_Access;
         Definition    : access Type_Definition'Class;
         Is_Private    : Boolean := False;
         Is_Deferred   : Boolean := False;
         Is_Indefinite : Boolean := False;
      end record;

   type Subtype_Declaration is new Declaration with
      record
         Name        : String_Access;
         Definition  : access Subtype_Indication'Class;
      end record;

   type Formal_Argument is new Object_Declaration with null record;

   package Formal_Argument_Vectors is
      new Ada.Containers.Indefinite_Vectors (Positive, Formal_Argument'Class);

   type Subprogram_Declaration is new Declaration with
      record
         Name              : String_Access;
         Generic_Name      : String_Access;
         Arguments         : Formal_Argument_Vectors.Vector;
         Generic_Arguments : String_Vector.Vector;
         Result_Type       : access Subtype_Indication'Class;
         Sub_Body          : access Blocks.Block_Type'Class;
         Expression_Body   : access Expression'Class;
         Is_Function       : Boolean := False;
         Is_Abstract       : Boolean := False;
         Is_Private        : Boolean := False;
         Is_Overriding     : Boolean := False;
         Is_Instantiation  : Boolean := False;
         Is_Anonymous      : Boolean := False;
         Arg_Name_Width    : Natural := 0;
         Has_Body          : Boolean := False;
      end record;

   function Is_Anonymous (Item : Subprogram_Declaration'Class) return Boolean
   is (Item.Is_Anonymous);

   type With_Context_Clause is
      record
         Withed_Package : String_Access;
         Is_Private     : Boolean        := False;
         Is_Body        : Boolean        := False;
         Is_Used        : Boolean        := False;
         Elaborate_All  : Boolean        := False;
      end record;

   package Context_Clause_Vectors is
      new Ada.Containers.Vectors (Positive, With_Context_Clause);

   type Package_Type is new Subprogram_Declaration with
      record
         Withed_Packages      : Context_Clause_Vectors.Vector;
         Declarations         : Declaration_Vector.Vector;
         Formal_Arguments     : Declaration_Vector.Vector;
         Is_Generic           : Boolean                    := False;
         Has_Private          : Boolean                    := False;
      end record;

end Syn.Declarations;
