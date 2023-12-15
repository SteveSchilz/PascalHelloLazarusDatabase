unit UnitAddPHone;

{$mode ObjFPC}{$H+}

interface

uses
  UnitData, Utils,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFrmAddPhone }

  TFrmAddPhone = class(TForm)
    ButtonSave: TButton;
    ButtonCancel: TButton;
    ComboBoxType: TComboBox;
    EditNumber: TEdit;
    LabelUserPrompt: TLabel;
    LabelType: TLabel;
    LabelNumber: TLabel;
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InitializeTypeDropDown();
    // TODO: Move the editMode stuff to a base class and then
    //       inherit from it here and in UnitAddContact
  private
     mEditMode: Boolean;
  public
     function GetEditMode(): Boolean;
     procedure SetEditMode(Value : Boolean);
  end;

var
  FrmAddPhone: TFrmAddPhone;

implementation

{$R *.lfm}

{ TFrmAddPhone }

procedure TFrmAddPhone.ButtonCancelClick(Sender: TObject);
begin
  self.close();
end;

procedure TFrmAddPhone.ButtonSaveClick(Sender: TObject);

begin
  if ( ValidatePhone(EditNumber.Text) = false ) then
     begin
       MessageDlg('Must have a valid 10 digit phone number to save!', mtInformation, mbOkCancel, 0);
     end
  else
      begin
          //TODO: Save the dang number!
           self.close();
      end;

end;

procedure TFrmAddPhone.FormCreate(Sender: TObject);
begin
    InitializeTypeDropDown();
end;

procedure TFrmAddPhone.FormShow(Sender: TObject);
begin
    if (mEditMode = true) then
      begin
        LabelUserPrompt.Caption := 'Editing Existing Phone:';
        FrmAddPhone.Caption := 'Edit Phone';
      end
  else
      begin
        LabelUserPrompt.Caption := 'Add New Phone?';
        FrmAddPhone.Caption := 'Add Phone';

      end;

end;


procedure TFrmAddPhone.InitializeTypeDropDown();
begin
  try
    try
        ComboBoxType.Items.Clear();
        ComboBoxType.Items.BeginUpdate;
        DataModule1.QueryPhoneType.Open;
        try
            while not DataModule1.QueryPhoneType.EOF do
            begin
                ComboBoxType.Items.Add(DataModule1.QueryPhoneType.FieldByName('Type').AsString);
                DataModule1.QueryPhoneType.Next;
            end;
        finally
            ComboBoxType.Items.EndUpdate;
            ComboBoxType.ItemIndex := 0;
        end;
    finally
        DataModule1.QueryPhoneType.Close;
    end;

  except
    on E: Exception do
     begin
         Utils.ShowException(E);
     end;
   end;
end;

function TFrmAddPhone.GetEditMode(): Boolean;
begin
  GetEditMode := mEditMode;
end;


procedure TFrmAddPhone.SetEditMode(Value : Boolean);
begin
  if (mEditMode <> Value) then
      begin
      mEditMode := value;
      end;
end;


end.

