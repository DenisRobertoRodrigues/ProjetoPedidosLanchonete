object FormCliente: TFormCliente
  Left = 0
  Top = 0
  Caption = 'Cadastro de Clientes'
  ClientHeight = 600
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 900
    Height = 60
    Align = alTop
    BevelOuter = bvNone
    Color = clNavy
    ParentBackground = False
    TabOrder = 0
    object lblTitulo: TLabel
      Left = 16
      Top = 16
      Width = 320
      Height = 29
      Caption = 'Gerenciamento de Clientes'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -24
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 560
    Width = 900
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlCenter: TPanel
    Left = 0
    Top = 60
    Width = 900
    Height = 500
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 900
      Height = 500
      ActivePage = tsLista
      Align = alClient
      TabOrder = 0
      object tsLista: TTabSheet
        Caption = 'Lista de Clientes'
        object pnlFiltro: TPanel
          Left = 0
          Top = 0
          Width = 892
          Height = 50
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblFiltro: TLabel
            Left = 16
            Top = 16
            Width = 50
            Height = 13
            Caption = 'Pesquisar:'
          end
          object edtFiltro: TEdit
            Left = 88
            Top = 13
            Width = 300
            Height = 21
            TabOrder = 0
            OnChange = edtFiltroChange
          end
          object chkApenasAtivos: TCheckBox
            Left = 408
            Top = 15
            Width = 120
            Height = 17
            Caption = 'Apenas Ativos'
            Checked = True
            State = cbChecked
            TabOrder = 1
            OnClick = chkApenasAtivosClick
          end
        end
        object GridClientes: TStringGrid
          Left = 0
          Top = 50
          Width = 892
          Height = 372
          Align = alClient
          ColCount = 6
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
          TabOrder = 1
          OnDblClick = GridClientesDblClick
        end
        object pnlBotoesLista: TPanel
          Left = 0
          Top = 422
          Width = 892
          Height = 50
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 2
          object btnNovo: TButton
            Left = 16
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Novo'
            TabOrder = 0
            OnClick = btnNovoClick
          end
          object btnEditar: TButton
            Left = 122
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Editar'
            TabOrder = 1
            OnClick = btnEditarClick
          end
          object btnExcluir: TButton
            Left = 228
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Excluir'
            TabOrder = 2
            OnClick = btnExcluirClick
          end
          object btnAtualizar: TButton
            Left = 334
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Atualizar'
            TabOrder = 3
            OnClick = btnAtualizarClick
          end
        end
      end
      object tsCadastro: TTabSheet
        Caption = 'Cadastro'
        ImageIndex = 1
        object lblNome: TLabel
          Left = 24
          Top = 24
          Width = 31
          Height = 13
          Caption = 'Nome:'
        end
        object lblContato: TLabel
          Left = 24
          Top = 75
          Width = 43
          Height = 13
          Caption = 'Contato:'
        end
        object lblDocumento: TLabel
          Left = 24
          Top = 126
          Width = 66
          Height = 13
          Caption = 'CPF ou CNPJ:'
        end
        object edtNome: TEdit
          Left = 24
          Top = 43
          Width = 400
          Height = 21
          TabOrder = 0
        end
        object edtContato: TEdit
          Left = 24
          Top = 94
          Width = 300
          Height = 21
          TabOrder = 1
        end
        object edtDocumento: TEdit
          Left = 24
          Top = 145
          Width = 200
          Height = 21
          MaxLength = 18
          TabOrder = 2
          OnExit = edtDocumentoExit
          OnKeyPress = edtDocumentoKeyPress
        end
        object chkAtivo: TCheckBox
          Left = 24
          Top = 186
          Width = 97
          Height = 17
          Caption = 'Ativo'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object pnlBotoesCadastro: TPanel
          Left = 0
          Top = 422
          Width = 892
          Height = 50
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 4
          object btnSalvar: TButton
            Left = 24
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Salvar'
            TabOrder = 0
            OnClick = btnSalvarClick
          end
          object btnCancelar: TButton
            Left = 130
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Cancelar'
            TabOrder = 1
            OnClick = btnCancelarClick
          end
        end
      end
    end
  end
end
