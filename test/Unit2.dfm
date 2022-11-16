object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 623
  ClientWidth = 824
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 232
    Top = 160
    Width = 577
    Height = 449
    ActivePage = Log
    TabOrder = 0
    object Grid: TTabSheet
      Caption = 'Grid'
      object grd: TStringGrid
        Left = 0
        Top = 0
        Width = 569
        Height = 421
        Align = alClient
        TabOrder = 0
      end
    end
    object Log: TTabSheet
      Caption = 'Log'
      ImageIndex = 1
      object Memo1: TMemo
        Left = 0
        Top = 37
        Width = 569
        Height = 384
        Align = alBottom
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Edit1: TEdit
        Left = 12
        Top = 10
        Width = 301
        Height = 21
        TabOrder = 1
        Text = 'Edit1'
      end
      object BitBtn1: TBitBtn
        Left = 356
        Top = 6
        Width = 75
        Height = 25
        Caption = 'BitBtn1'
        TabOrder = 2
        OnClick = BitBtn1Click
      end
      object Button1: TButton
        Left = 437
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Button1'
        TabOrder = 3
      end
    end
  end
end
