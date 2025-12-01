object FmInicializacao: TFmInicializacao
  Left = 0
  Top = 0
  Caption = 'FmInicializacao'
  ClientHeight = 736
  ClientWidth = 1021
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object PnMenu: TPanel
    Left = 0
    Top = 0
    Width = 993
    Height = 105
    TabOrder = 0
    object PnMenuGeral: TPanel
      Left = 0
      Top = 0
      Width = 297
      Height = 96
      TabOrder = 0
      Visible = False
      object BtnMenu: TBitBtn
        Left = 16
        Top = 21
        Width = 75
        Height = 65
        Caption = 'Menu'
        TabOrder = 0
        OnClick = BtnMenuClick
      end
      object BtnPedidos: TBitBtn
        Left = 97
        Top = 21
        Width = 193
        Height = 65
        Caption = 'Pedidos'
        TabOrder = 1
        OnClick = BtnPedidosClick
      end
    end
    object PnMenuGerencial: TPanel
      Left = 296
      Top = 0
      Width = 727
      Height = 96
      TabOrder = 1
      Visible = False
      object BtnGerenciamentoUsuarios: TBitBtn
        Left = 24
        Top = 21
        Width = 75
        Height = 65
        Caption = 'Usu'#225'rios'
        TabOrder = 0
        OnClick = BtnGerenciamentoUsuariosClick
      end
      object BtnGerenciamentoClientes: TBitBtn
        Left = 105
        Top = 21
        Width = 75
        Height = 65
        Caption = 'Clientes'
        TabOrder = 1
        OnClick = BtnGerenciamentoClientesClick
      end
      object BtnGerenciamentoIngredientes: TBitBtn
        Left = 186
        Top = 21
        Width = 75
        Height = 65
        Caption = 'Ingredientes'
        TabOrder = 2
        OnClick = BtnGerenciamentoIngredientesClick
      end
      object BtnGerenciamentoProduto: TBitBtn
        Left = 267
        Top = 21
        Width = 75
        Height = 65
        Caption = 'Produto'
        TabOrder = 3
        OnClick = BtnGerenciamentoProdutoClick
      end
    end
  end
  object PnDadosGerenciais: TPanel
    Left = 0
    Top = 102
    Width = 993
    Height = 547
    TabOrder = 1
    Visible = False
  end
  object PnLogin: TPanel
    Left = 248
    Top = 132
    Width = 465
    Height = 177
    TabOrder = 2
    object lblUsuario: TLabel
      Left = 40
      Top = 24
      Width = 43
      Height = 15
      Caption = 'Usu'#225'rio:'
    end
    object lblSenha: TLabel
      Left = 40
      Top = 56
      Width = 35
      Height = 15
      Caption = 'Senha:'
    end
    object EdtUsuario: TEdit
      Left = 86
      Top = 16
      Width = 315
      Height = 23
      TabOrder = 0
    end
    object EdtSenha: TMaskEdit
      Left = 86
      Top = 48
      Width = 121
      Height = 23
      PasswordChar = '*'
      TabOrder = 1
      Text = ''
    end
    object BtnLogin: TButton
      Left = 40
      Top = 85
      Width = 161
      Height = 25
      Caption = 'Entrar'
      TabOrder = 2
      OnClick = BtnLoginClick
    end
  end
end
