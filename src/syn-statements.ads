private with Ada.Containers.Vectors;

with Syn.Blocks;

package Syn.Statements is

   type Sequence_Of_Statements is
     new Syntax_Root and Statement_Sequencer with private;

   overriding
   procedure Append (To : in out Sequence_Of_Statements;
                     S  : in     Statement'Class);

   overriding
   procedure Write (Item   : Sequence_Of_Statements;
                    Writer : in out Writer_Interface'Class);

   function Pragma_Statement
     (Pragma_Name : String;
      Argument    : String)
      return Statement'Class;

   function Declare_Statement
     (Block : Syn.Blocks.Block_Type'Class)
      return Statement'Class;

   type If_Statement_Record is
     new Statement with private;

   function If_Statement
     (Condition  : Expression'Class;
      True_Part  : Sequence_Of_Statements;
      False_Part : Sequence_Of_Statements)
      return If_Statement_Record'Class;

   function If_Statement
     (Condition  : Expression'Class;
      True_Part  : Sequence_Of_Statements)
      return If_Statement_Record'Class;

   function If_Statement
     (Condition  : Expression'Class;
      True_Part  : Statement'Class)
      return If_Statement_Record'Class;

   function If_Statement
     (Condition  : Expression'Class;
      True_Part  : Statement'Class;
      False_Part : Statement'Class)
      return If_Statement_Record'Class;

   procedure Add_Elsif
     (To_Statement    : in out If_Statement_Record'Class;
      Condition       : in     Expression'Class;
      Elsif_Statement : in     Statement'Class);

   procedure Add_Elsif
     (To_Statement     : in out If_Statement_Record'Class;
      Condition        : in     Expression'Class;
      Elsif_Statements : in     Sequence_Of_Statements);

   function While_Statement
     (Condition  : Expression'Class;
      While_Body : Sequence_Of_Statements'Class)
      return Statement'Class;

   function While_Statement
     (Condition  : Expression'Class;
      While_Body : Statement'Class)
      return Statement'Class;

   function For_Loop
     (Loop_Variable : String;
      Low, High     : Expression'Class;
      Reversed      : Boolean;
      Loop_Body     : Sequence_Of_Statements'Class)
      return Statement'Class;

   function Iterate
     (Loop_Variable  : String;
      Container_Name : String;
      Iterate_Body   : Sequence_Of_Statements'Class)
      return Statement'Class;

   function Iterate
     (Loop_Variable  : String;
      Container_Name : String;
      Iterate_Body   : Statement'Class)
      return Statement'Class;

   type Case_Statement_Record is
     new Statement with private;

   function Case_Statement (Case_Expression : String)
                            return Case_Statement_Record'Class;

   procedure Add_Case_Option
     (Statement : in out Case_Statement_Record'Class;
      Value     : in     String;
      Stats     : in     Sequence_Of_Statements'Class);

   procedure Add_Case_Option
     (To_Case : in out Case_Statement_Record'Class;
      Value   : in     String;
      Stat    : in     Statement'Class);

   procedure Add_Others_Option
     (Statement : in out Case_Statement_Record'Class;
      Stats     : in     Sequence_Of_Statements'Class);

   overriding
   procedure Write (Item : Case_Statement_Record;
                    Writer : in out Writer_Interface'Class);

   function Raise_Statement (Exception_Name : String;
                             Message        : String)
                             return Statement'Class;

   type Null_Statement is
     new Statement with private;

   overriding
   procedure Write (Item   : Null_Statement;
                    Writer : in out Writer_Interface'Class);

   function New_Null_Statement return Null_Statement;

   function New_Return_Statement
      return Statement'Class;

   function New_Return_Statement
     (Result : Expression'Class)
      return Statement'Class;

   function New_Return_Statement
     (Return_Variable   : String;
      Variable_Type     : String;
      Return_Statements : Sequence_Of_Statements'Class)
      return Statement'Class;

   type Assignment_Statement is new Statement with private;

   overriding
   procedure Write (Item   : Assignment_Statement;
                    Writer : in out Writer_Interface'Class);

   function New_Assignment_Statement
     (Target : String;
      Value  : Expression'Class)
      return Statement'Class;

   type Procedure_Call_Statement is
     new Statement with private;

   overriding
   procedure Write (Item   : Procedure_Call_Statement;
                    Writer : in out Writer_Interface'Class);

   function New_Procedure_Call_Statement
     (Procedure_Name : String)
      return Procedure_Call_Statement;

   function New_Procedure_Call_Statement
     (Procedure_Name : String;
      Argument       : Expression'Class)
      return Procedure_Call_Statement;

   function New_Procedure_Call_Statement
     (Procedure_Name : String;
      Argument_1     : Expression'Class;
      Argument_2     : Expression'Class)
      return Procedure_Call_Statement;

   function New_Procedure_Call_Statement
     (Procedure_Name : String;
      Argument_1     : Expression'Class;
      Argument_2     : Expression'Class;
      Argument_3     : Expression'Class)
      return Procedure_Call_Statement;

   function New_Procedure_Call_Statement
     (Procedure_Name : String;
      Argument_1     : Expression'Class;
      Argument_2     : Expression'Class;
      Argument_3     : Expression'Class;
      Argument_4     : Expression'Class)
      return Procedure_Call_Statement;

   function New_Procedure_Call_Statement
     (Procedure_Name : String;
      Argument_1     : Expression'Class;
      Argument_2     : Expression'Class;
      Argument_3     : Expression'Class;
      Argument_4     : Expression'Class;
      Argument_5     : Expression'Class)
      return Procedure_Call_Statement;

   procedure Add_Actual_Argument
     (Call      : in out Procedure_Call_Statement;
      Name      : in     String;
      Value     : in     Expression'Class);

   procedure Add_Actual_Argument
     (Call      : in out Procedure_Call_Statement;
      Value     : in     Expression'Class);

   procedure Add_Actual_Argument
     (Call      : in out Procedure_Call_Statement;
      Value     : in     String);

private

   type Sequence_Of_Statements is
     new Syntax_Root
     and Statement_Sequencer
   with
      record
         Sequence : Statement_Vectors.Vector;
      end record;

   overriding procedure Iterate
     (Block   : Sequence_Of_Statements;
      Process : not null access
        procedure (S : Statement'Class));

   type Null_Statement is new Statement with null record;

   function New_Null_Statement return Null_Statement
   is (others => <>);

   type Assignment_Statement is new Statement with
      record
         Target : access String;
         Expr   : access Expression'Class;
      end record;

   type Procedure_Call_Statement is
     new Statement with
      record
         Name      : access String;
         Arguments : Actual_Argument_Lists.List;
      end record;

   type Case_Option is
      record
         Value : access Expression'Class;
         Stats : Sequence_Of_Statements;
      end record;

   package Case_Option_Vector is
     new Ada.Containers.Vectors (Positive, Case_Option);

   type Case_Statement_Record is
     new Statement with
      record
         Case_Expression : access Expression'Class;
         Case_Options    : Case_Option_Vector.Vector;
      end record;

   type Elsif_Option is
      record
         Condition : access Expression'Class;
         Stats     : Sequence_Of_Statements;
      end record;

   package Elsif_Option_Vector is
      new Ada.Containers.Vectors (Positive, Elsif_Option);

   type If_Statement_Record is
     new Statement with
      record
         Condition  : access Expression'Class;
         True_Part  : Sequence_Of_Statements;
         Elsifs     : Elsif_Option_Vector.Vector;
         False_Part : Sequence_Of_Statements;
      end record;

   overriding
   procedure Write (Item        : If_Statement_Record;
                    Writer      : in out Writer_Interface'Class);

end Syn.Statements;
