unit UnitAddPHone;

{$mode ObjFPC}{$H+}

interface

uses
  UnitData, Utils,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFrmAddPhone }

  TFrmAddPhone = class(TForm)
    ButtonAddPhone: TButton;
    ButtonCancelPhone: TButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    LabelType: TLabel;
    LabelNumber: TLabel;
    procedure ButtonCancelPhoneClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InitializeTypeDropDown();
  private

  public

  end;

var
  FrmAddPhone: TFrmAddPhone;

implementation

{$R *.lfm}

{ TFrmAddPhone }

procedure TFrmAddPhone.ButtonCancelPhoneClick(Sender: TObject);
begin
  self.close();
end;

procedure TFrmAddPhone.FormCreate(Sender: TObject);
begin
    InitializeTypeDropDown();
end;


procedure TFrmAddPhone.InitializeTypeDropDown();
begin
  try
    try
        ComboBox1.Items.Clear();
        ComboBox1.Items.BeginUpdate;
        DataModule1.QueryPhoneType.Open;
        try
            while not DataModule1.QueryPhoneType.EOF do
            begin
                ComboBox1.Items.Add(DataModule1.QueryPhoneType.FieldByName('Type').AsString);
                DataModule1.QueryPhoneType.Next;
            end;
        finally
            ComboBox1.Items.EndUpdate;
            ComboBox1.ItemIndex := 0;
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

end.

