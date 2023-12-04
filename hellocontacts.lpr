program helloContacts;

{$mode objfpc}{$H+}
                   //
                   // NOTE: I sometimes have to restart the IDE to get it to compile resources o_O
                   //
{$R resources.rc}  // This compiles resources.rc to hellocontacts.res using windres compiler
{$R *.res}         // This loads the project icon, which is a Lazarus Resource

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1, unitData, unitAddContact, unitAddPhone, Utils;


begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TFormContacts, FormContacts);
  Application.CreateForm(TFrmAddContact, FrmAddContact);
  Application.CreateForm(TFrmAddPhone, FrmAddPhone);
  Application.Run;
end.

