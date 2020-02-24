private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Unbounded;

package Syn is

   type Write_Flag is (Suppress_Argument_Mode);

   type Writer_Interface is abstract tagged limited private;

   procedure Create (Writer : in out Writer_Interface;
                     Path   : in     String)
      is abstract;

   procedure Close (Writer : in out Writer_Interface)
      is abstract;

   procedure Put (Writer : in out Writer_Interface;
                  Text   : in     String)
      is abstract;

   procedure Put_Line (Writer : in out Writer_Interface'Class;
                       Text   : in     String);

   procedure New_Line (Writer : in out Writer_Interface)
      is abstract;

   function Col (Writer : Writer_Interface) return Positive
      is abstract;

   procedure Set_Col (Writer : in out Writer_Interface;
                      Value  : in     Positive)
     is abstract;

   function Line (Writer : Writer_Interface) return Positive
      is abstract;

   function Indent (Writer : Writer_Interface) return Natural
      is abstract;

   procedure Indent (Writer : in out Writer_Interface;
                     Value  : in     Natural)
      is abstract;

   procedure Optional_New_Line
     (Writer : in out Writer_Interface)
   is null;

   procedure Push_Flag
     (Writer : in out Writer_Interface'Class;
      Flag   : Write_Flag;
      Value  : Boolean);

   procedure Pop_Flag
     (Writer : in out Writer_Interface'Class;
      Flag   : Write_Flag);

   function Is_Set
     (Writer : Writer_Interface'Class;
      Flag   : Write_Flag)
      return Boolean;

   procedure Indent (Writer : in out Writer_Interface'Class);

   type Syntax_Root is abstract tagged private;

   procedure Write (Item        : Syntax_Root;
                    Writer      : in out Writer_Interface'Class)
      is abstract;

   type Type_Definition is abstract new Syntax_Root with private;

   procedure Set_Limited (Item  : in out Type_Definition'Class);

   function Has_Variant (Item : Type_Definition) return Boolean;
   function Variant_Name (Item : Type_Definition) return String;
   function Variant_Type (Item : Type_Definition) return String;
   function Variant_Default (Item : Type_Definition) return String;

   function Is_Tagged (Item : Type_Definition) return Boolean;
   function Visible_Derivation (Item : Type_Definition) return Boolean;

   type Enumeration_Type_Definition is
     new Type_Definition with private;

   overriding
   procedure Write (Item        : Enumeration_Type_Definition;
                    Writer      : in out Writer_Interface'Class);

   procedure New_Literal (Item    : in out Enumeration_Type_Definition;
                          Literal : in String);

   type Derived_Type_Definition is new Type_Definition with private;

   overriding
   procedure Write (Item        : Derived_Type_Definition;
                    Writer      : in out Writer_Interface'Class);

   function New_Derived_Type (Derived_From : String)
                             return Derived_Type_Definition;

   type Access_Type_Definition is new Type_Definition with private;

   overriding
   procedure Write (Item        : Access_Type_Definition;
                    Writer      : in out Writer_Interface'Class);

   function New_Access_Type (Access_To  : String;
                             Access_All : Boolean)
                             return Access_Type_Definition;

   type Interface_Type_Definition is new Type_Definition with private;

   procedure Add_Parent
     (To_Interface : in out Interface_Type_Definition;
      Name         : in     String);

   overriding
   procedure Write (Item        : Interface_Type_Definition;
                    Writer      : in out Writer_Interface'Class);

   type Declaration is abstract new Syntax_Root with private;

   procedure Check (D : Declaration) is null;

   procedure Set_Private_Spec (D : in out Declaration);

   function Has_Private_Part
     (Item : Declaration)
     return Boolean;

   function Has_Body
     (Item : Declaration)
     return Boolean;

   function Has_Body_Spec
     (Item : Declaration)
      return Boolean;

   function Has_Output
     (Item : Declaration;
      Writer : Writer_Interface'Class)
     return Boolean;

   function Pseudo_Declaration
     (Item : Declaration)
     return Boolean;

   type Statement is abstract new Syntax_Root with private;

   procedure Set_Label (Item    : in out Statement;
                        Label   : in     String);

   type Statement_Sequencer is interface;

   procedure Append (To_Sequencer : in out Statement_Sequencer;
                     Item         : in     Statement'Class)
   is abstract;

   procedure Iterate
     (Sequence : Statement_Sequencer;
      Process  : not null access
        procedure (S : Statement'Class))
   is abstract;

   procedure Append (To : in out Statement_Sequencer'Class;
                     S  : in     String);

   procedure Append_Pragma
     (To       : in out Statement_Sequencer'Class;
      Name     : String;
      Argument : String);

   type Subtype_Indication is tagged private;

   function Named_Subtype (Name : String)
                          return Subtype_Indication'Class;

   function Class_Wide_Subtype (Name : String)
                                return Subtype_Indication'Class;

   type Expression is abstract new Syntax_Root with private;

   function Constrained_Subtype
     (Name      : String;
      Low, High : Expression'Class)
      return Subtype_Indication'Class;

   type Literal_Expression is new Expression with private;

   overriding
   procedure Write (Item        : Literal_Expression;
                    Writer      : in out Writer_Interface'Class);

   function Literal (Value : Integer) return Expression'Class;
   function Literal (Value : String) return Expression'Class;
   function Literal (Value : Boolean) return Expression'Class;

   function Value (Image : String) return Expression'Class;

   function Current_Line return Expression'Class;

   function Object
     (Name         : String;
      Dereferenced : Boolean := False)
      return Expression'Class;

   function Constrained_Array
     (Name         : String;
      Low, High    : Expression'Class)
      return Expression'Class;

   procedure Add_Aspect
     (D     : in out Declaration'Class;
      Name  : in String;
      Value : in Expression'Class);

   procedure Add_Aspect
     (D     : in out Declaration'Class;
      Name  : in String;
      Value : in String);

private

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   type String_Access is access String;

   package String_Vector is
      new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Syntax_Root is abstract tagged null record;

   type Type_Definition is abstract new Syntax_Root with
      record
         Is_Abstract        : Boolean := False;
         Is_Limited         : Boolean := False;
         Is_Synchronized    : Boolean := False;
         Is_Private         : Boolean := False;
         Visible_Derivation : Boolean := False;
      end record;

   function Visible_Derivation (Item : Type_Definition) return Boolean
   is (Item.Visible_Derivation);

   type Aspect is
      record
         Name     : access String;
         Value    : access Expression'Class;
      end record;

   package Aspect_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Aspect);

   type Declaration is abstract new Syntax_Root with
      record
         Aspects      : Aspect_Lists.List;
         Body_Only    : Boolean := False;
         Private_Spec : Boolean := False;
      end record;

   type Subtype_Indication is new Syntax_Root with
      record
         Base_Type   : String_Access;
         Class_Wide  : Boolean        := False;
         Constrained : Boolean        := False;
         Low         : access Expression'Class;
         High        : access Expression'Class;
      end record;

   overriding
   procedure Write (Item        : Subtype_Indication;
                    Writer      : in out Writer_Interface'Class);

   type Statement is abstract new Syntax_Root with
      record
         Label : String_Access;
      end record;

   type Expression is abstract new Syntax_Root with null record;

   type Literal_Expression is
     new Expression with
      record
         Literal      : String_Access;
         Dereferenced : Boolean := False;
         Low, High    : access Expression'Class;
      end record;

   type Meta_Expression is abstract new Expression with null record;

   type Current_Line_Expression is
     new Meta_Expression with null record;

   overriding
   procedure Write
     (Item        : Current_Line_Expression;
      Writer      : in out Writer_Interface'Class);

   type Enumeration_Type_Definition is new Type_Definition with
      record
         Literals : String_Vector.Vector;
      end record;

   type Derived_Type_Definition is new Type_Definition with
      record
         Base_Type : String_Access;
      end record;

   type Access_Type_Definition is new Type_Definition with
      record
         Access_To_Type : String_Access;
         Access_All     : Boolean;
      end record;

   type Interface_Type_Definition is new Type_Definition with
      record
         Parents : String_Vector.Vector;
      end record;

   package Declaration_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive,
                                            Declaration'Class);

   package Statement_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive,
                                            Statement'Class);

   package Declaration_Vector is
      new Ada.Containers.Indefinite_Vectors (Positive, Declaration'Class);

   type Actual_Argument is
      record
         Name   : access String;
         Value  : access Expression'Class;
      end record;

   package Actual_Argument_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Actual_Argument);

   type Write_Context is (Compilation_Unit,
                          Package_Spec, Package_Private,
                          Package_Body, Package_Body_Specs,
                          Block);

   type Tab_Stops is array (1 .. 10) of Natural;

   type Write_Flags is array (Write_Flag) of Boolean;
   type Flag_Stack is array (1 .. 20) of Write_Flags;

   type Writer_Interface is abstract tagged limited
      record
         Context  : Write_Context := Compilation_Unit;
         Tabs     : Tab_Stops     := (others => 0);
         Flags    : Flag_Stack := (others => (others => False));
         Flag_Top : Natural := 0;
      end record;

   function Is_Set
     (Writer : Writer_Interface'Class;
      Flag   : Write_Flag)
      return Boolean
   is (Writer.Flag_Top > 0 and then Writer.Flags (Writer.Flag_Top) (Flag));

   function To_Ada_Name (Text : String_Access) return String;
   function To_File_Name (Text : String_Access;
                          Spec : Boolean)
                         return String;
   function To_Ada_Name (Text : String) return String;

end Syn;
