unit unitAddContact;

{$mode ObjFPC}{$H+}

interface

uses
  UnitData, Utils,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LazLogger,
  SQLDB;

type

  { TFrmAddContact }

  TFrmAddContact = class(TForm)
    ButtonSave: TButton;
    ButtonCancel: TButton;
    EditID: TEdit;
    EditFirst: TEdit;
    EditLast: TEdit;
    Label_ID: TLabel;
    LabelFirst: TLabel;
    LabelLast: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
  private

  public

  end;

var
  FrmAddContact: TFrmAddContact;

implementation

{$R *.lfm}

{ TFrmAddContact }


procedure TFrmAddContact.ButtonSaveClick(Sender: TObject);
var
  insertId : Integer;
begin
  if (editFirst.Text <> '') or (editLast.Text <> '') then
  begin

    try
      try
        with DataModule1.QueryInsert do
        begin
             Close();
             Sql.Clear();
             // NOTES:
             //   To "Open" a query, Sql.text must be set to a select statment.
             //   This is because open makes the result set available, and insert
             //   statements do not have a result set.
             //
             //   The InsertSql.Text property also exists, apparently use Insert()
             //    instead of ExecSQL
             Sql.Text := 'INSERT INTO People(First, Last) ' +
                         'VALUES(' + QuotedStr(EditFirst.Text) + ',' +
                                     QuotedStr(EditLast.Text) + ')';
             DebugLn(Sql.text);
             ExecSql();           // Insert/Update use execSQL

             // ApplyUpdates(); - Needed to save when editing an existing record
             if (Transaction.Active) then
                 begin
                 // Cast the transaction property of the query to TSQLTr...
                 TSQLTransaction(Transaction).Commit();
                 end;


             insertId := Utils.GetLastInsertID(DataModule1.QueryInsert);
             EditID.Text := IntToStr(insertId);
        end;
      except
        on E: Exception do
        begin
          if (Datamodule1.QueryInsert.Transaction.Active) then
              begin
              TSQLTransaction(Datamodule1.QueryInsert.Transaction).Rollback();
              end;
          Utils.ShowException(E);
        end;
      end;
    finally
      // Nothing needed here.
    end;
  end;
end;

procedure TFrmAddContact.ButtonCancelClick(Sender: TObject);
begin
  Self.Close();
end;

end.

