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
    LabelUserPrompt: TLabel;
    Label_ID: TLabel;
    LabelFirst: TLabel;
    LabelLast: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
     mEditMode : Boolean;
  public
     function GetEditMode(): Boolean;
     procedure SetEditMode(Value : Boolean);
  end;

var
  FrmAddContact: TFrmAddContact;

implementation

{$R *.lfm}

{ TFrmAddContact }


procedure TFrmAddContact.ButtonSaveClick(Sender: TObject);
var
  InsertId : Integer;
begin
  if (editFirst.Text = '') and (editLast.Text = '') then
      begin
      MessageDlg('Must have at least one non blank name to save!', mtInformation, mbOkCancel, 0);
      end
  else
  begin
    if (mEditMode) then
        begin
        InsertId := StrToInt(EditId.Text);
        Utils.EditUser(DataModule1.QueryInsert, InsertId, EditFirst.Text, EditLast.Text);
        end
    else
        begin
        InsertId := Utils.AddUser(DataModule1.QueryInsert, EditFirst.Text, EditLast.Text);
        EditID.Text := IntToStr(InsertId);
        end;
    Self.Close();

  end;
end;

procedure TFrmAddContact.FormShow(Sender: TObject);
begin
  if (mEditMode = true) then
      begin
        LabelUserPrompt.Caption := 'Editing Existing User:';
        FrmAddContact.Caption := 'Edit Contact';
      end
  else
      begin
        LabelUserPrompt.Caption := 'Create New Contact?';
        FrmAddContact.Caption := 'Add Contact';

      end;
end;

procedure TFrmAddContact.ButtonCancelClick(Sender: TObject);
begin
  Self.Close();
end;

function TFrmAddContact.GetEditMode(): Boolean;
begin
  GetEditMode := mEditMode;
end;


procedure TFrmAddContact.SetEditMode(Value : Boolean);
begin
  if (mEditMode <> Value) then
      begin
      mEditMode := value;
      end;
end;

end.

