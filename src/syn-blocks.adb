with Syn.Statements;

package body Syn.Blocks is

   ---------------------
   -- Add_Declaration --
   ---------------------

   procedure Add_Declaration
     (Block : in out Block_Type;
      Item  : in     Declaration'Class)
   is
   begin
      Block.Declarations.Append (Item);
   end Add_Declaration;

   -------------------
   -- Add_Statement --
   -------------------

   procedure Add_Statement
     (Block : in out Block_Type;
      Item  : in     Statement'Class)
   is
   begin
      Block.Statements.Append (Item);
   end Add_Statement;

   -------------------
   -- Add_Statement --
   -------------------

   procedure Add_Statement
     (Block : in out Block_Type;
      Item  : in     String)
   is
   begin
      Block.Statements.Append
        (Syn.Statements.New_Procedure_Call_Statement (Item));
   end Add_Statement;

   ------------------
   -- Create_Block --
   ------------------

   function Create_Block
     (Item : Statement'Class)
      return Block_Type'Class
   is
      Result : Block_Type;
   begin
      Result.Add_Statement (Item);
      return Result;
   end Create_Block;

   function Create_Block
     (Item : Statement_Sequencer'Class)
      return Block_Type'Class
   is
      Result : Block_Type;

      procedure Add (S : Statement'Class);

      ---------
      -- Add --
      ---------

      procedure Add (S : Statement'Class) is
      begin
         Result.Append (S);
      end Add;

   begin
      Item.Iterate (Add'Access);
      return Result;
   end Create_Block;

   -------------
   -- Iterate --
   -------------

   overriding procedure Iterate
     (Block   : Block_Type;
      Process : not null access
        procedure (S : Statement'Class))
   is
   begin
      for S of Block.Statements loop
         Process (S);
      end loop;
   end Iterate;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item        : Block_Type;
                    Writer      : in out Writer_Interface'Class)
   is
   begin
      Writer.Context := Block;
      Writer.Indent (Writer.Indent + 3);
      for D of Item.Declarations loop
         D.Write (Writer);
         Writer.Put_Line (";");
      end loop;
      Writer.Indent (Writer.Indent - 3);
      Writer.Put_Line ("begin");
      Writer.Indent (Writer.Indent + 3);
      for S of Item.Statements loop
         S.Write (Writer);
         Writer.Put_Line (";");
      end loop;
      Writer.Indent (Writer.Indent - 3);
      Writer.Put ("end");
      Writer.Context := Package_Body;
   end Write;

end Syn.Blocks;
