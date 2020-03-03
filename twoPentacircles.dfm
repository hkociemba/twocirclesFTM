object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 336
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    527
    336)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 208
    Top = 303
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Start'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 529
    Height = 297
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Button2: TButton
    Left = 328
    Top = 303
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = Button2Click
  end
end
