object MainForm: TMainForm
  Left = 197
  Top = 111
  Caption = '!Paint for Windows'
  ClientHeight = 547
  ClientWidth = 934
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 591
    Top = 0
    Height = 547
    Align = alRight
    ExplicitLeft = 614
    ExplicitTop = -8
  end
  object Memo1: TMemo
    Left = 594
    Top = 0
    Width = 340
    Height = 547
    Align = alRight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'System'
    Font.Pitch = fpFixed
    Font.Style = []
    Lines.Strings = (
      'Welcome to !Paint for Windows'
      '--------------------------------------'
      ''
      'This is version 1.00 (beta release 2).'
      ''
      'This is intending to be a RISC OS'
      'Sprite convertor for Windows, which'
      'loads a Sprite file and outputs'
      'Windows bitmaps.'
      ''
      'This is still under development and,'
      'as such, has much to do:'
      '* Does not deal with masks;'
      '* Does not handle 256 colour partial'
      '  palettes (currently uses default'
      '  palette).'
      '* Does not handle any sprites with a'
      '  mode flag 7 or above.'
      ''
      'plus lots more.'
      ''
      'Contact: gerald@hollypops.co.uk or'
      'geraldholdsworth on the Stardot.com'
      'forums.'
      ''
      'This pane is used to display debugging'
      'information.')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object ScrollBox1: TScrollBox
    Left = 34
    Top = 0
    Width = 557
    Height = 547
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 34
    Height = 547
    Align = alLeft
    TabOrder = 2
    object sb_OpenFile: TSpeedButton
      Left = 4
      Top = 4
      Width = 23
      Height = 22
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        555555555555555555550000000000055555777777777775F555003333333330
        5555775F555555575F550B033333333305557F75F555555575F50FB033333333
        30557F575F555555575F0BFB0333333333057F5575FFFFFFFF7F0FBFB0000000
        00007F555777777777770BFBFFFBFB0555557F555555557F55550FBFBFBFBF05
        55557F555FFFFF7F55550BFB00000005555575FF777777755FFF500055555555
        0005577755555555777F5555555555555005555555555F55577F555555550555
        05055555555575FF757555555555500055555555555557775555555555555555
        5555555555555555555555555555555555555555555555555555}
      NumGlyphs = 2
      OnClick = sb_OpenFileClick
    end
    object sb_Save: TSpeedButton
      Left = 4
      Top = 28
      Width = 23
      Height = 22
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333FFFFFFFFFFFFF33000077777770033377777777777773F000007888888
        00037F3337F3FF37F37F00000780088800037F3337F77F37F37F000007800888
        00037F3337F77FF7F37F00000788888800037F3337777777337F000000000000
        00037F3FFFFFFFFFFF7F00000000000000037F77777777777F7F000FFFFFFFFF
        00037F7F333333337F7F000FFFFFFFFF00037F7F333333337F7F000FFFFFFFFF
        00037F7F333333337F7F000FFFFFFFFF00037F7F333333337F7F000FFFFFFFFF
        00037F7F333333337F7F000FFFFFFFFF07037F7F33333333777F000FFFFFFFFF
        0003737FFFFFFFFF7F7330099999999900333777777777777733}
      NumGlyphs = 2
      OnClick = sb_SaveClick
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'All Files (*.*)|*.*'
    Left = 104
    Top = 152
  end
end
