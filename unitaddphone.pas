unit UnitAddPHone;

{$mode ObjFPC}{$H+}

interface

uses
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

end.

