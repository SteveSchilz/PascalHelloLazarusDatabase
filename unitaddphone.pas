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
     mPersonId: Integer;
     mPhoneId: Integer;
     mPhoneTypeIds : Array of integer;

  public
     function GetEditMode(): Boolean;
     procedure SetEditMode(Value : Boolean);
     procedure SetPersonId(Id: Integer);
     procedure SetPhoneId(Id: Integer);
     procedure SetPhoneTypeId(Id: Integer);
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
var
  Number: Int64;

begin
  Number := ValidatePhone(EditNumber.Text);
  if ( Number = -1 ) then
     begin
       MessageDlg('Must have a valid 10 digit phone number to save!', mtInformation, mbOkCancel, 0);
     end
  else
      begin

          if (mEditMode = false) then
          begin
               WriteLn(Format('Add Phone: PersonId: %d Ph Type Index: %d, Ph: %d',
                              [mPersonId, mPhoneTypeIds[ComboBoxType.ItemIndex], Number]));
               Utils.AddPhone(DataModule1.QueryInsert, mPersonId, IntToStr(Number), mPhoneTypeIds[ComboBoxType.ItemIndex]);
          end
          else
          begin
              WriteLn(Format('Edit Phone: Id: %d, PersonId: %d Ph Type Index: %d, Ph: %s',
                             [mPhoneId, mPersonId, mPhoneTypeIds[ComboBoxType.ItemIndex], EditNumber.Text]));
              Utils.EditPhone(DataModule1.QueryInsert, mPhoneId,  mPersonId, EditNumber.Text, mPhoneTypeIds[ComboBoxType.ItemIndex]);
          end;

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
var
  count : LongInt;   // Note: Count is a long int, but we only expect a few (< 10 rows)
  i : Integer;
begin
  try
    try
        i := 0;

        ComboBoxType.Items.Clear();
        ComboBoxType.Items.BeginUpdate;
        try
            DataModule1.InitializePhoneTypesQuery();
            count := DataModule1.QueryPhoneType.RecordCount;
            SetLength(mPhoneTypeIds, count);

            while not DataModule1.QueryPhoneType.EOF do
            begin
                ComboBoxType.Items.Add(DataModule1.QueryPhoneType.FieldByName('Type').AsString);
                mPhoneTypeIds[i] := DataModule1.QueryPhoneType.FieldByName('Id').AsInteger;
                WriteLn(Format('mPhoneTypeIds[%d] = %d', [i, mPhoneTypeIds[i]]));
                i := i + 1;
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

procedure TFrmAddPhone.SetPersonId(Id: Integer);
begin
  mPersonId := Id;
end;

procedure TFrmAddPhone.SetPhoneId(Id: Integer);
begin
     mPhoneId := Id;
end;

(* SetPhoneTypeId
 *
 * @param: Id : Actual Phone type ID from PhoneTypes Table
 *
 * Sets ComboBox ItemIndex to match the selected Item Id using mPhoneTypeIds[]
 *)
procedure TFrmAddPhone.SetPhoneTypeId(Id: Integer);
var
  i: Integer;
begin
     i := ComboBoxType.Items.Count;
     while (i > 0) do
     begin
         Writeln(Format('Comparing %d', [mPhoneTypeIds[i-1]]));
         if (mPhoneTypeIds[i-1] = Id) then
             begin
             Writeln('Matches: ',  Id);
             ComboBoxType.ItemIndex := i-1;
             end;
         i := i - 1;
     end;
end;

end.

